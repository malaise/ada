with Ada.Exceptions;
with Basic_Proc, Argument, Mixed_Str, Images,
     Event_Mng, Ip_Addr, Socket, Tcp_Util;
with Trace;
procedure T_Tcp_Send is
  Arg_Error : exception;

  -- Socket
  Protocol : constant Socket.Protocol_List := Socket.Tcp_Header;
  Sock, Accept_Sock : Socket.Socket_Dscr;
  Remote_Host : Tcp_Util.Remote_Host;
  Remote_Port : Tcp_Util.Remote_Port;
  Local_Port : Tcp_Util.Local_Port;
  Port_Num : Tcp_Util.Port_Num;

  -- Options
  Next_Arg : Positive;
  Blocking : Boolean := False;
  Send_Timeout : Duration := 0.0;
  Default_Timeout : constant Duration := 5.1;

  -- Connect synchronous result
  Result : Boolean;

  -- Sequence number
  type Sequence_Number is mod Integer'Last;
  function Image is new Images.Mod_Image (Sequence_Number);

  -- Trace logger
  Logger : Trace.Logger;

  -- Signal callback
  Sig : Boolean := False;
  procedure Signal_Cb is
  begin
    Logger.Log_Info ("Aborted.");
    Sig := True;
  end Signal_Cb;

  -- Message type
  type Message_Type is record
    Seq : Sequence_Number;
    Str : String (1 .. 16 * 1024);
  end record;
  Message : Message_Type;
  package Reception is new Tcp_Util.Reception (Message_Type);
  function My_Send is new Tcp_Util.Send (Message_Type);

  -- Reception callbacks
  procedure Disconnect_Cb  (Dscr : in Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
  begin
    Logger.Log_Info ("Disconnection Cb");
    Sock := Socket.No_Socket;
  end Disconnect_Cb;

  function Read_Cb (Dscr : Socket.Socket_Dscr;
                    Msg  : Message_Type;
                    Len  : Natural) return Boolean is
    pragma Unreferenced (Dscr, Len);
  begin
    if Msg.Seq = Message.Seq then
      Logger.Log_Info ("Read Cb got " & Image (Msg.Seq));
    else
      Logger.Log_Info ("Read Cb got seq " & Image (Msg.Seq)
                                & " while expecting " & Image (Message.Seq));
    end if;
    Message.Seq := Msg.Seq + 1;
    return True;
  end Read_Cb;

  -- Acception callback
  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       New_Dscr        : in Socket.Socket_Dscr) is
    pragma Unreferenced (Local_Port_Num, Local_Dscr, Remote_Port_Num,
                         Remote_Host_Id);
    use type Socket.Socket_Dscr;
    Tmp_Dscr : Socket.Socket_Dscr;
  begin
    if Sock /= Socket.No_Socket then
      Logger.Log_Info ("Accept Cb rejecting new connection");
      Tmp_Dscr := New_Dscr;
      Tmp_Dscr.Close;
    else
      Logger.Log_Info ("Accept Cb accepting connection");
      Sock := New_Dscr;
      Sock.Set_Blocking (Socket.Full_Blocking);
      Message.Seq := 0;
      Reception.Set_Callbacks (Sock,
             Read_Cb'Unrestricted_Access,
             Disconnect_Cb'Unrestricted_Access);
    end if;
  end Accept_Cb;

  -- Connection
  In_Overflow : Boolean := False;
  procedure Connect_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                        Remote_Port_Num : in Tcp_Util.Port_Num;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr) is
    pragma Unreferenced (Remote_Port_Num, Remote_Host_Id);
  begin
    if Connected then
      Sock := Dscr;
      if not Blocking then
        Sock.Set_Blocking (Socket.Non_Blocking);
      end if;
    end if;
    Logger.Log_Info ("Connection Cb " & Mixed_Str (Connected'Img));
  end Connect_Cb;

  procedure Connect is
  begin
    Logger.Log_Info ("Connecting");
    In_Overflow := False;
    Message.Seq := 0;
    Result := Tcp_Util.Connect_To (Protocol,
                                   Remote_Host, Remote_Port,
                                   Connect_Cb'Unrestricted_Access,
                                   1.0, 0);
    Logger.Log_Info ("Connect result " & Mixed_Str (Result'Img));
  end Connect;

  -- Emission
  procedure End_Overflow_Cb (Dscr : in  Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
  begin
    Logger.Log_Info ("End of overflow Cb");
    In_Overflow  := False;
  end End_Overflow_Cb;

  procedure Send_Err_Cb (Dscr : in  Socket.Socket_Dscr;
                         Conn_Lost : in Boolean) is
    pragma Unreferenced (Dscr);
  begin
    Logger.Log_Info ("Send error Cb " & Mixed_Str (Conn_Lost'Img));
    -- Socket will be closed by Tcp_Util
    Sock := Socket.No_Socket;
    Connect;
  end Send_Err_Cb;

  procedure Send is
    Res : Boolean;
    use type Socket.Socket_Dscr;
  begin
    if Sock = Socket.No_Socket then
      Logger.Log_Info ("Not sending cause not connected");
      return;
    elsif In_Overflow then
      Logger.Log_Info ("Not sending cause in overflow");
      return;
    end if;
    Logger.Log_Info ("Sending");
    Logger.Log_Debug ("Sending");
    Res := My_Send (Sock,
             End_Overflow_Cb'Unrestricted_Access,
             Send_Err_Cb'Unrestricted_Access,
             Send_Timeout, Message);
    Message.Seq := Message.Seq + 1;
    Logger.Log_Debug ("Send -> " & Mixed_Str (Res'Img));
    In_Overflow := not Res;
    Logger.Log_Info ("Send result " & Mixed_Str (Res'Img));
  exception
    when Socket.Soc_Conn_Lost =>
      Logger.Log_Debug ("Send -> Socket.Soc_Conn_Lost");
      Logger.Log_Info ("Sending exception Conn_Lost, closing");
      Sock.Close;
      Connect;
    when Tcp_Util.Timeout_Error =>
      Logger.Log_Debug ("Send -> Tcp_Util.Timeout_Error");
      Logger.Log_Info ("Sending exception Timeout_Error, closing");
      Sock.Close;
      Connect;
    when Error : others =>
      Logger.Log_Debug ("Send -> "
                      & Mixed_Str (Ada.Exceptions.Exception_Name (Error)));
      Logger.Log_Info ("Sending exception: "
        & Mixed_Str (Ada.Exceptions.Exception_Name (Error)) & ".");
  end Send;

begin
  Logger.Init;
  Logger.Add_Mask (Trace.Infos);
  Logger.Log_Debug ("Init");
  -- General init
  Message := (0, (others => ' '));

  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  if Argument.Get_Nbre_Arg < 1 then
    raise Arg_Error;
  end if;

  -- Accepter mode
  if Argument.Get_Nbre_Arg = 2
  and then Argument.Get_Parameter (1) = "-a" then
    begin
      Local_Port := Ip_Addr.Parse (Argument.Get_Parameter (2));
    exception
      when others =>
        raise Arg_Error;
    end;
    Logger.Log_Info ("Accepting");
    Tcp_Util.Accept_From (Protocol,
                          Local_Port,
                          Accept_Cb'Unrestricted_Access,
                          Accept_Sock,
                          Port_Num);
    Event_Mng.Pause (-1);
    return;
  end if;

  -- Sender mode
  Next_Arg := 1;
  loop
    declare
      Arg : constant String := Argument.Get_Parameter (Next_Arg);
    begin
      if Arg = "-b" then
        Blocking := True;
      elsif Arg = "-t" then
        Send_Timeout := Default_Timeout;
      elsif Next_Arg = Argument.Get_Nbre_Arg then
        -- Last arg = dest
        exit;
      else
        raise Arg_Error;
      end if;
    end;
    Next_Arg := Next_Arg + 1;
  end loop;

  begin
    Ip_Addr.Parse (Argument.Get_Parameter (Next_Arg),
                   Remote_Host, Remote_Port);
  exception
    when others =>
      raise Arg_Error;
  end;
  Connect;

  loop
    Event_Mng.Pause (1_000);
    exit when Sig;
    Send;
  end loop;

  Logger.Log_Debug ("Done.");
exception
  when Arg_Error =>
    Logger.Log_Fatal ("Invalid_Argument");
    Logger.Log_Info ("Usage: "
          & Argument.Get_Program_Name & " -a <port>");
    Logger.Log_Info ("   or: "
          & Argument.Get_Program_Name & " [ -b ] [ -t ] <host>:<port>");
    Basic_Proc.Set_Error_Exit_Code;
  when Error: others =>
    Logger.Log_Fatal ("Exception: "
                   & Ada.Exceptions.Exception_Name (Error));
    Basic_Proc.Set_Error_Exit_Code;
end T_Tcp_Send;

