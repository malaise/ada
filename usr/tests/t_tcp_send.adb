with Ada.Exceptions;
with Basic_Proc, Argument, Mixed_Str,
     Event_Mng, Ip_Addr, Socket, Tcp_Util;
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

  -- Connect synchronous result
  Result : Boolean;

  -- Signal callback
  Sig : Boolean := False;
  procedure Signal_Cb is
  begin
    Basic_Proc.Put_Line_Output ("Aborted.");
    Sig := True;
  end Signal_Cb;

  -- Message type
  subtype Message_Type is string (1 .. 16 * 1024);
  Message : Message_Type;
  package Reception is new Tcp_Util.Reception (Message_Type);
  function My_Send is new Tcp_Util.Send (Message_Type);

  -- Reception callbacks
  procedure Disconnect_Cb  (Dscr : in Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
  begin
    Basic_Proc.Put_Line_Output ("Disconnection Cb");
    Sock := Socket.No_Socket;
  end Disconnect_Cb;

  procedure Read_Cb (Dscr    : in Socket.Socket_Dscr;
                     Message : in Message_Type;
                     Length  : in Natural) is
    pragma Unreferenced (Dscr, Message, Length);
  begin
    Basic_Proc.Put_Line_Output ("Read Cb");
  end Read_Cb;

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
      Basic_Proc.Put_Line_Output ("Accept Cb rejecting new connection");
      Tmp_Dscr := New_Dscr;
      Tmp_Dscr.Close;
    else
      Basic_Proc.Put_Line_Output ("Accept Cb accepting connection");
      Sock := New_Dscr;
      Reception.Set_Callbacks (Sock,
             Read_Cb'Unrestricted_Access, 
             Disconnect_Cb'Unrestricted_Access);
    end if;
  end Accept_Cb;

  -- Emission callbacks
  In_Overflow : Boolean := False;
  procedure End_Overflow_Cb (Dscr : in  Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
  begin
    Basic_Proc.Put_Line_Output ("End of overflow Cb");
    In_Overflow  := False;
  end End_Overflow_Cb;

  procedure Send_Err_Cb (Dscr : in  Socket.Socket_Dscr;
                         Conn_Lost : in Boolean) is
    pragma Unreferenced (Dscr);
  begin
    Basic_Proc.Put_Line_Output ("Send error Cb " & Mixed_Str (Conn_Lost'Img));
    Sock := Socket.No_Socket;
  end Send_Err_Cb;

  procedure Connect_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                        Remote_Port_Num : in Tcp_Util.Port_Num;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr) is
    pragma Unreferenced (Remote_Port_Num, Remote_Host_Id);
  begin
    if Connected then
      Sock := Dscr;
      if not Blocking then
        Sock.Set_Blocking (False);
      end if;
    end if;
    Basic_Proc.Put_Line_Output ("Connection Cb " & Mixed_Str (Connected'Img));
  end Connect_Cb;

  procedure Connect is
  begin
    Basic_Proc.Put_Line_Output ("Connecting");
    Result := Tcp_Util.Connect_To (Protocol,
                                   Remote_Host, Remote_Port,
                                   1.0, 0,
                                   Connect_Cb'Unrestricted_Access);
    Basic_Proc.Put_Line_Output ("Connect result " & Mixed_Str (Result'Img));
  end Connect;

  procedure Send is
    Res : Boolean;
    use type Socket.Socket_Dscr;
  begin
    if Sock = Socket.No_Socket then
      Basic_Proc.Put_Line_Output ("Not sending cause connected");
      return;
    elsif In_Overflow then
      Basic_Proc.Put_Line_Output ("Not sending cause in Overflow");
      return;
    end if;
    Basic_Proc.Put_Line_Output ("Sending");
    Res := My_Send (Sock,
             End_Overflow_Cb'Unrestricted_Access,
             Send_Err_Cb'Unrestricted_Access,
             Send_Timeout, Message);
    In_Overflow := not Res;
    Basic_Proc.Put_Line_Output ("Send result " & Mixed_Str (Res'Img));
  exception
    when Socket.Soc_Conn_Lost =>
      Basic_Proc.Put_Line_Output ("Sending exception Conn_Lost, closing");
      Sock.Close;
      Connect;
    when Tcp_Util.Timeout_Error =>
      Basic_Proc.Put_Line_Output ("Sending exception Timeout_Error, closing");
      Sock.Close;
      Connect;
    when Error : others =>
      Basic_Proc.Put_Line_Output ("Sending exception: " 
        & Mixed_Str (Ada.Exceptions.Exception_Name (Error)) & ".");
  end Send;

begin
  -- General init
  Message := (others => ' ');
  
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
    Basic_Proc.Put_Line_Output ("Accepting");
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
        Send_Timeout := 0.1;
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
                   Remote_host, Remote_Port);
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

exception
  when Arg_Error =>
    Basic_Proc.Put_Line_Output ("Usage: "
          & Argument.Get_Program_Name & " -a <port>");
    Basic_Proc.Put_Line_Output ("   or: "
          & Argument.Get_Program_Name & " [ -b ] [ -t ] <host>:<port>");
  when Error: others =>
    Basic_Proc.Put_Line_Output ("Exception: "
                   & Ada.Exceptions.Exception_Name (Error));
end T_Tcp_Send;

