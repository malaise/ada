with Ada.Exceptions, Ada.Characters.Latin_1;
with Argument, Basic_Proc, Async_Stdin, Event_Mng, Socket, Tcp_Util, Ip_Addr;
procedure T_Async is

  procedure Usage is
  begin
    Async_Stdin.Put_Line_Err ("Usage: "
       & Argument.Get_Program_Name & " -c <host>:<port>  |  -s <port>");
    Async_Stdin.Put_Line_Err ("  <host> ::= <host_name> | <host_address>");
    Async_Stdin.Put_Line_Err ("  <port> ::= <port_name> | <port_num>");
  end Usage;
  Arg_Error : exception;

  -- Client or server mode
  Server_Mode : Boolean;

  -- Give_up on connection failure, signal, Async_Stdin error
  Give_Up : Boolean := False;

  -- Common message type
  subtype Message_Type is String (1 .. 1024);

  -- TCP stuff
  Local_Port_Def : Tcp_Util.Local_Port;
  Remote_Host_Def : Tcp_Util.Remote_Host;
  Remote_Port_Def : Tcp_Util.Remote_Port;
  Port_Num : Tcp_Util.Port_Num;
  Tcp_Soc : Socket.Socket_Dscr;
  In_Overflow : Boolean;
  procedure Open;
  procedure Close;

  -- End of sending overflow
  procedure End_Ovf_Cb (Dscr : in  Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
    use type Socket.Socket_Dscr;
  begin
    In_Overflow := False;
  end End_Ovf_Cb;

  -- Sending fatal error
  procedure Send_Err_Cb (Dscr : in  Socket.Socket_Dscr;
                         Conn_Lost : in Boolean) is
    pragma Unreferenced (Dscr);
  begin
    if Conn_Lost then
      Async_Stdin.Put_Line_Err ("Sending msg failed cause connection lost");
    else
      Async_Stdin.Put_Line_Err ("Sending msg failed cause timeout");
    end if;
  end Send_Err_Cb;


  -- For sending on Tcp
  function My_Send is new Tcp_Util.Send (Message_Type);
  procedure Send (Msg : in Message_Type; Len : in Positive) is
  begin
    if not Tcp_Soc.Is_Open then
      return;
    end if;
    if not In_Overflow then
      if not My_Send (Tcp_Soc, End_Ovf_Cb'Unrestricted_Access,
                      Send_Err_Cb'Unrestricted_Access, 1.0, Msg, Len) then
        In_Overflow := True;
      end if;
    end if;
  exception
    when Error : others =>
      Async_Stdin.Put_Line_Err ("Sending " & Msg(1 .. Len) & " failed cause "
        & Ada.Exceptions.Exception_Name (Error) & ".");
  end Send;

  -- When a message to read
  procedure Read_Cb (Dscr : in Socket.Socket_Dscr;
                     Msg : in Message_Type;
                     Len : in Natural) is
    pragma Unreferenced (Dscr);
  begin
    if Len = 1 and then Msg(1) = Ada.Characters.Latin_1.Eot then
      -- Single Ctrl D => Close connection
      Async_Stdin.Put_Line_Err ("closing");
      Close;
      if Server_Mode then
        Open;
      else
        Give_Up := True;
      end if;
    else
      Async_Stdin.Put_Out (Msg(1 .. Len));
    end if;
  end Read_Cb;
  package My_Rece is new Tcp_Util.Reception (Message_Type);

  -- When Soc_Read_0
  procedure Discon_Cb (Dscr : in Socket.Socket_Dscr) is
    pragma Unreferenced (Dscr);
  begin
    Async_Stdin.Put_Line_Err ("disconnected");
    -- Tcp_Util closes the socket
    Tcp_Soc := Socket.No_Socket;
    if Server_Mode then
      -- Accept connections again
      Open;
    else
      Give_Up := True;
    end if;
  end Discon_Cb;

  procedure Accept_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                       Local_Dscr      : in Socket.Socket_Dscr;
                       Remote_Host_Id  : in Tcp_Util.Host_Id;
                       Remote_Port_Num : in Tcp_Util.Port_Num;
                       New_Dscr        : in Socket.Socket_Dscr) is
    pragma Unreferenced (Local_Port_Num, Local_Dscr, Remote_Port_Num,
                         Remote_Host_Id);
    use type Socket.Socket_Dscr;
  begin
    Tcp_Soc := New_Dscr;
    Tcp_Soc.Set_Blocking (Socket.Non_Blocking);
    In_Overflow := False;
    My_Rece.Set_Callbacks (Tcp_Soc, Read_Cb'Unrestricted_Access,
                                  Discon_Cb'Unrestricted_Access);
    Async_Stdin.Put_Line_Err ("connected");
    -- Only one connection at a time
    Tcp_Util.Abort_Accept (Socket.Tcp, Port_Num);
  end Accept_Cb;

  -- Connection report Cb
  procedure Conn_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                     Remote_Port_Num : in Tcp_Util.Port_Num;
                     Connected       : in Boolean;
                     Dscr            : in Socket.Socket_Dscr) is
    pragma Unreferenced (Remote_Port_Num, Remote_Host_Id);
  begin
    if Connected then
      Tcp_Soc := Dscr;
      Tcp_Soc.Set_Blocking (Socket.Non_Blocking);
      In_Overflow := False;
      My_Rece.Set_Callbacks (Tcp_Soc, Read_Cb'Unrestricted_Access,
                                    Discon_Cb'Unrestricted_Access);
      Async_Stdin.Put_Line_Err ("connected");
    else
      -- COnnection failure => Abort
      Give_Up := True;
    end if;
  end Conn_Cb;

  -- Open (accept) Tcp connection
  procedure Open is
    Acc_Soc : Socket.Socket_Dscr;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    if Server_Mode then
      loop
        begin
          Tcp_Util.Accept_From (Socket.Tcp,
                                Local_Port_Def,
                                Accept_Cb'Unrestricted_Access,
                                Acc_Soc,
                                Port_Num);
          exit;
        exception
          when Socket.Soc_Addr_In_Use =>
            Async_Stdin.Put_Line_Err ("Cannot accept. Maybe Close-wait. Waiting");
            Event_Mng.Wait (20_000);
        end;
      end loop;
    else
      Dummy := Tcp_Util.Connect_To (Socket.Tcp,
                                    Remote_Host_Def,
                                    Remote_Port_Def,
                                    1.0,
                                    1,
                                    Conn_Cb'Unrestricted_Access);
    end if;
  end Open;

  procedure Close is
  begin
    if Server_Mode then
      begin
        Tcp_Util.Abort_Accept (Socket.Tcp, Port_Num);
      exception
        when Tcp_Util.No_Such =>
          null;
      end;
    else
      begin
        Tcp_Util.Abort_Connect (Remote_Host_Def, Remote_Port_Def);
      exception
        when Tcp_Util.No_Such =>
          null;
      end;
    end if;
    if Tcp_Soc.Is_Open then
      begin
        My_Rece.Remove_Callbacks (Tcp_Soc);
      exception
       when Tcp_Util.No_Such =>
         null;
      end;
      begin
        Tcp_Util.Abort_Send_And_Close (Tcp_Soc);
      exception
       when Tcp_Util.No_Such =>
         null;
      end;
    end if;
    if Tcp_Soc.Is_Open then
      Tcp_Soc.Close;
    end if;
  end Close;

  -- Async stdin stuff
  function Async_Cb (Str : String) return Boolean is
    Msg : Message_Type;
  begin
    if Str = "" then
      -- Async stdin error
      Give_Up := True;
      return True;
    else
      Msg(1 .. Str'Length) := Str;
      Send (Msg, Str'Length);
      return False;
    end if;
  end Async_Cb;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Give_Up := True;
  end Signal_Cb;

begin

  -- Parse Arg, port name or num
  if Argument.Get_Nbre_Arg /= 2 then
    raise Arg_Error;
  end if;

  if Argument.Get_Parameter (1) = "-s" then
    Server_Mode := True;
    declare
      use type Tcp_Util.Remote_Port_List;
    begin
      Remote_Port_Def := Ip_Addr.Parse (Argument.Get_Parameter (2));
      if Remote_Port_Def.Kind = Tcp_Util.Port_Name_Spec then
        Local_Port_Def := (Tcp_Util.Port_Name_Spec, Remote_Port_Def.Name);
      else
        Local_Port_Def := (Tcp_Util.Port_Num_Spec, Remote_Port_Def.Num);
      end if;
    exception
      when others =>
        raise Arg_Error;
    end;
  elsif Argument.Get_Parameter (1) = "-c" then
    begin
      Server_Mode := False;
      Ip_Addr.Parse (Argument.Get_Parameter (2),
                     Remote_Host_Def, Remote_Port_Def);
    exception
      when others =>
        raise Arg_Error;
    end;
  else
    raise Arg_Error;
  end if;

  -- Init
  Give_Up := False;
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
  Open;
  Async_Stdin.Set_Async (Async_Cb'Unrestricted_Access, Message_Type'Length);

  -- Main loop
  loop
    exit when Event_Mng.Wait (Event_Mng.Infinite_Ms) and then Give_Up;
  end loop;

  -- Close
  Close;
  Async_Stdin.Set_Async (null);

exception
  when Arg_Error =>
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  when Error : others =>
    Async_Stdin.Put_Line_Err ("Exception: "
                   & Ada.Exceptions.Exception_Name (Error) & " raised.");
    Basic_Proc.Set_Error_Exit_Code;
end T_Async;

