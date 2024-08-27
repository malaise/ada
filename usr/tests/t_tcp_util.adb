-- Test Tcp_Util, asynchronous connection, overflow...
-- Role (client or server) port and server host are set by arguments
-- Message is fixed ("Ah que coucou!")
with Ada.Exceptions;
with Basic_Proc, As.U, Argument, Lower_Str, Event_Mng, Socket,
     Socket_Util, Tcp_Util, Timers;
procedure T_Tcp_Util is
  Arg_Error : exception;

  Protocol : constant Socket.Protocol_List := Socket.Tcp_Header;
  -- Protocol : constant Socket.Protocol_List := Socket.Tcp_Header_Afux;
  Server : Boolean;
  Server_Name : As.U.Asu_Us;
  Server_Port_Name : As.U.Asu_Us;
  Server_Port_Num : Socket.Port_Num;
  Client_Port_Name : As.U.Asu_Us;
  Client_Port_Num : Socket.Port_Num;
  Local_Port : Socket_Util.Local_Port;
  Remote_Port : Socket_Util.Remote_Port;
  Client_Port : Socket_Util.Local_Port;

  Give_Up : Boolean;
  In_Ovf : Boolean := False;
  Lost : Boolean := False;
  Server_Nb_Overflow : Natural := 0;

  Delay_Try : constant Duration := 10.0;
  Nb_Try : constant := 9;
  Ttl : constant := 5;

  Accept_Dscr, The_Dscr : Socket.Socket_Dscr;

  type Message_Type is record
    Len : Positive;
    Num : Positive;
    Str : String (1 .. 80);
    Dummy : String (1 .. 11_000);
  end record;
  Message, Received_Message : Message_Type;
  Str : constant String := "Ah que coucou!";
  procedure My_Read is new Socket.Receive (Message_Type);
  function My_Send is new Tcp_Util.Send (Message_Type);

  function Read_Cb (Fd : in Event_Mng.File_Desc; Unused_Read : in Boolean)
                   return Boolean;
  procedure End_Ovf_Cb (Dscr : in  Socket.Socket_Dscr);

  -- Signal received
  Sig : Boolean := False;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Basic_Proc.Put_Line_Output ("Aborted.");
    Sig := True;
  end Signal_Cb;

  procedure Wait (Dur : in Duration) is
  begin
    if Event_Mng.Wait (Integer (Dur) * 1_000) then
      Basic_Proc.Put_Line_Output ("Timer/Event");
    else
      Basic_Proc.Put_Line_Output ("Timeout");
    end if;
  end Wait;

  procedure Connect;

  -- Timer for connecting
  function Timer_Exp (Dummy_Id : in Timers.Timer_Id;
                      Dummy_Data : in Timers.Timer_Data) return Boolean is
  begin
    Connect;
    return False;
  end Timer_Exp;

  procedure Send_Err_Cb (Dscr : in  Socket.Socket_Dscr;
                         Conn_Lost : in Boolean) is
    Dummy_Timer : Timers.Timer_Id;
    Appt : constant Timers.Delay_Rec := (Delay_Kind => Timers.Delay_Sec,
                                         Delay_Seconds => 1.0,
                                         others => <>);
  begin
    Basic_Proc.Put_Line_Output ("Send_Err_Cb with lost_conn=" & Conn_Lost'Img);
    -- Tcp_Util will close Dscr
    Event_Mng.Del_Fd_Callback (Dscr.Get_Fd, True);
    The_Dscr := Socket.No_Socket;
    Lost := True;
    Dummy_Timer := Timers.Create (Appt, Timer_Exp'Unrestricted_Access);
  end Send_Err_Cb;

  function Send (Msg : in String) return Boolean is
  begin
    if not The_Dscr.Is_Open then
      Basic_Proc.Put_Line_Output (Msg & " not sending cause not open");
      return False;
    end if;
    if Lost then
      Basic_Proc.Put_Line_Output (Msg & " not sending cause lost connection");
      return False;
    end if;
    if Server then
      if not In_Ovf then
        if My_Send (The_Dscr, End_Ovf_Cb'Unrestricted_Access,
                    Send_Err_Cb'Unrestricted_Access, 1.0, Message) then
          Basic_Proc.Put_Line_Output (Msg & " sent num "
                                    & Integer'Image(Message.Num));
          return True;
        else
          In_Ovf := True;
          Basic_Proc.Put_Line_Output (
              Msg & "                     sending overflow");
        end if;
      else
         Basic_Proc.Put_Line_Output (Msg & " not sending cause in overflow");
      end if;
      return False;
    else
      -- Client
      if In_Ovf then
        Basic_Proc.Put_Line_Output (Msg & " not sending cause in overflow");
        return False;
      end if;
      while My_Send (The_Dscr, End_Ovf_Cb'Unrestricted_Access,
                     Send_Err_Cb'Unrestricted_Access, 1.0, Message) loop
        Basic_Proc.Put_Line_Output (Msg & " sent num "
                                  & Integer'Image(Message.Num));
        Message.Num := Message.Num + 1;
      end loop;
      In_Ovf := True;
      Basic_Proc.Put_Line_Output (
          Msg & "                     sending overflow");
    end if;
    return False;
  exception
    when Error : others =>
      Basic_Proc.Put_Line_Output ("Sending " & Msg & " failed cause "
        & Lower_Str (Ada.Exceptions.Exception_Name (Error)) & ".");
      return False;
  end Send;

  procedure End_Ovf_Cb (Dscr : in  Socket.Socket_Dscr) is
    use type Socket.Socket_Dscr;
  begin
    if Dscr /= The_Dscr then
      Basic_Proc.Put_Line_Output ("End of overflow on invalid dscr");
      return;
    end if;
    Basic_Proc.Put_Line_Output (
       "End of overflow Cb ->                    End of overflow");
    In_Ovf := False;
  end End_Ovf_Cb;

  procedure Connect_Cb (Unused_Remote_Host_Id  : in Socket_Util.Host_Id;
                        Unused_Remote_Port_Num : in Socket_Util.Port_Num;
                        Connected              : in Boolean;
                        Dscr                   : in Socket.Socket_Dscr) is
    Dummy : Boolean;
  begin
    if Connected then
      The_Dscr := Dscr;
      Event_Mng.Add_Fd_Callback (Dscr.Get_Fd,
                            True,
                            Read_Cb'Unrestricted_Access);
      The_Dscr.Set_Blocking (Socket.Non_Blocking);
      Basic_Proc.Put_Line_Output ("Connected and non blocking");
      Lost := False;
      In_Ovf := False;
      if The_Dscr.Get_Ttl /= Ttl then
        Basic_Proc.Put_Line_Output ("TTL lost!");
      end if;
    else
      Basic_Proc.Put_Line_Output ("Not connected");
      Give_Up := True;
      return;
    end if;
    if not Server then
      Dummy := Send ("First request");
    end if;
  end Connect_Cb;

  procedure Connect is
    Host : Socket_Util.Remote_Host(Socket_Util.Host_Name_Spec);
    Dummy_Res : Boolean;
  begin
    Host.Name := Server_Name;
    Dummy_Res := Tcp_Util.Connect_To (Protocol,
                                      Host, Remote_Port,
                                      Connect_Cb'Unrestricted_Access,
                                      Delay_Try, Nb_Try, Ttl, Client_Port);

  end Connect;

  procedure Accept_Cb (Unused_Local_Port_Num  : in Socket_Util.Port_Num;
                       Unused_Local_Dscr      : in Socket.Socket_Dscr;
                       Unused_Remote_Host_Id  : in Socket_Util.Host_Id;
                       Unused_Remote_Port_Num : in Socket_Util.Port_Num;
                       New_Dscr               : in Socket.Socket_Dscr) is
    use type Socket.Socket_Dscr;
    Tmp_Dscr : Socket.Socket_Dscr;
  begin
    if The_Dscr /= Socket.No_Socket then
      Basic_Proc.Put_Line_Output ("Rejected");
      Tmp_Dscr := New_Dscr;
      Tmp_Dscr.Close;
    else
      The_Dscr := New_Dscr;
      Event_Mng.Add_Fd_Callback (New_Dscr.Get_Fd,
                            True,
                            Read_Cb'Unrestricted_Access);
      The_Dscr.Set_Blocking (Socket.Non_Blocking);
      Basic_Proc.Put_Output ("Accepted and non blocking");
      Lost := False;
      In_Ovf := False;
    end if;
  end Accept_Cb;

  function Read_Cb (Fd : in Event_Mng.File_Desc; Unused_Read : in Boolean)
           return Boolean is
    Len : Natural;
    Dummy_Res : Boolean;
    use type Event_Mng.File_Desc;
  begin
    if Server then
      Basic_Proc.Put_Output ("Server: ");
    else
      Basic_Proc.Put_Output ("Client: ");
    end if;
    if not The_Dscr.Is_Open or else Fd /= The_Dscr.Get_Fd then
      Basic_Proc.Put_Line_Output ("Read Cb -> Unknown fd");
      return False;
    end if;

    Received_Message.Str := (others => ' ');
    begin
      My_Read (The_Dscr, Received_Message, Len, False);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        Basic_Proc.Put_Line_Output ("Read Cb -> disconnected: Closing");
        Event_Mng.Del_Fd_Callback (Fd, True);
        if In_Ovf then
          Tcp_Util.Abort_Send_And_Close (The_Dscr);
          In_Ovf := False;
        else
          The_Dscr.Close;
        end if;
        if not Server then
          Basic_Proc.Put_Line_Output (" - Waiting");
          delay 3.0;
          Connect;
        else
          Basic_Proc.New_Line_Output;
        end if;
        return True;
      when Socket.Soc_Would_Block =>
        Basic_Proc.Put_Line_Output ("Read Cb ->                     underflow");
        return False;
    end;

    Message := Received_Message;
    Basic_Proc.Put_Line_Output ("receives: >"
                   & Message.Str(1 .. Message.Len)
                   & "< num "
                   & Positive'Image(Message.Num));
    if Message.Str(1 .. Message.Len) /= Str then
      Basic_Proc.Put_Line_Output ("Incorrect message received");
      raise Program_Error;
    end if;

    -- Will exit with 0 (if no further error)
    Basic_Proc.Set_Ok_Exit_Code;

    -- Send / reply
    if not Server then
      Basic_Proc.Put_Line_Output ("      Working");
      delay 0.3;
      Dummy_Res := Send ("Request");
    else
      Basic_Proc.Put_Line_Output ("      Working");
      delay 0.1;
      Basic_Proc.Put_Line_Output ("      Replying");
      Message.Num := Message.Num + 10;
      if not Send ("Reply") and then In_Ovf then
        if Lost then
          Basic_Proc.Put_Line_Output ("      Not responding to lost client");
          return False;
        end if;
        if Server_Nb_Overflow < 50 then
          Basic_Proc.Put_Line_Output (
              "      Not responding to client in overflow");
          Server_Nb_Overflow := Server_Nb_Overflow + 1;
        else
          -- Cancel with this bad client
          Basic_Proc.Put_Line_Output ("      Closing with client in overflow");
          Event_Mng.Del_Fd_Callback (Fd, True);
          Tcp_Util.Abort_Send_And_Close (The_Dscr);
          In_Ovf := False;
          Server_Nb_Overflow := 0;
        end if;
      end if;
    end if;
    return False;
  end Read_Cb;

begin
  -- Will exit with 2 if no message received
  Basic_Proc.Set_Exit_Code (2);

  -- Server or client
  begin
    Argument.Get_Parameter (Server_Name, 1, "c");
    Server := False;
  exception
    when Argument.Argument_Not_Found =>
    begin
      Argument.Get_Parameter (Server_Name, 1, "s");
      Server := True;
    exception
      when others =>
        raise Arg_Error;
    end;
    when others =>
      raise Arg_Error;
  end;
  if Server and then not Server_Name.Is_Null then
    raise Arg_Error;
  elsif not Server and then Server_Name.Is_Null then
    raise Arg_Error;
  end if;

  -- Port name or num
  begin
    Argument.Get_Parameter (Server_Port_Name, 1, "P");
    if Server_Port_Name.Is_Null then
      raise Arg_Error;
    end if;
  exception
    when Argument.Argument_Not_Found =>
    begin
      Server_Port_Num := Socket.Port_Num'Value(Argument.Get_Parameter (1, "p"));
    exception
      when others =>
        raise Arg_Error;
    end;
    when others =>
      raise Arg_Error;
  end;

  -- Set ports
  if not Server_Port_Name.Is_Null then
    Local_Port := (Kind => Socket_Util.Port_Name_Spec,
                   Name => Server_Port_Name);
    Remote_Port := (Kind => Socket_Util.Port_Name_Spec,
                    Name => Local_Port.Name);
  else
    Local_Port := (Kind => Socket_Util.Port_Num_Spec, Num => Server_Port_Num);
    Remote_Port := (Kind => Socket_Util.Port_Num_Spec, Num => Server_Port_Num);
  end if;

  -- Set ports
  if not Server_Port_Name.Is_Null then
    Local_Port := (Kind => Socket_Util.Port_Name_Spec,
                   Name => Server_Port_Name);
    Remote_Port := (Kind => Socket_Util.Port_Name_Spec,
                    Name => Local_Port.Name);
  else
    Local_Port := (Kind => Socket_Util.Port_Num_Spec, Num => Server_Port_Num);
    Remote_Port := (Kind => Socket_Util.Port_Num_Spec, Num => Server_Port_Num);
  end if;

  -- Optional client port name or num
  begin
    Argument.Get_Parameter (Client_Port_Name, 1, "L");
    if Client_Port_Name.Is_Null then
      raise Arg_Error;
    end if;
    Client_Port := (Socket_Util.Port_Name_Spec, Client_Port_Name);
  exception
    when Argument.Argument_Not_Found =>
      begin
        Client_Port_Num := Socket.Port_Num'Value (
            Argument.Get_Parameter (1, "l"));
        Client_Port := (Socket_Util.Port_Num_Spec, Client_Port_Num);
      exception
        when Argument.Argument_Not_Found =>
          null;
      end;
    when others =>
      raise Arg_Error;
  end;

  -- Init
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
  Give_Up := False;
  if Server then
    loop
      begin
        Tcp_Util.Accept_From (Protocol,
                              Local_Port,
                              Accept_Cb'Unrestricted_Access,
                              Accept_Dscr,
                              Server_Port_Num);
        exit;
      exception
        when Socket.Soc_Addr_In_Use =>
          Basic_Proc.Put_Line_Output (
              "Cannot accept. Maybe Close-wait. Waiting");
          Wait (20.0);
      end;
      exit when Give_Up or else Sig;
    end loop;
  else
    Message.Num := 1;
    Message.Str (1 .. Str'Length) := Str;
    Message.Len := Str'Length;
    Connect;
  end if;


  -- Main loop
  if not Give_Up and then not Sig then
    loop
      Wait (1.0);
      exit when Give_Up or else Sig;
    end loop;
  end if;

  begin
    Tcp_Util.Abort_Accept(Protocol, Server_Port_Num);
  exception
    when Tcp_Util.No_Such => null;
  end;

  if The_Dscr.Is_Open then
    if Event_Mng.Fd_Callback_Set (The_Dscr.Get_Fd, True) then
      Event_Mng.Del_Fd_Callback (The_Dscr.Get_Fd, True);
    end if;
    if Event_Mng.Fd_Callback_Set (The_Dscr.Get_Fd, False) then
      Event_Mng.Del_Fd_Callback (The_Dscr.Get_Fd, False);
    end if;
    The_Dscr.Close;
  end if;
exception
  when Arg_Error =>
    Basic_Proc.Put_Line_Output ("Usage: "
            & Argument.Get_Program_Name & " <mode> <port> [ <client_port> ]");
    Basic_Proc.Put_Line_Output (" <mode> ::= -c<server_host> | -s");
    Basic_Proc.Put_Line_Output (" <port> ::= -P<port_name> | -p<port_num>");
    Basic_Proc.Put_Line_Output (" <client_port> ::= -L<port_name> | -l<port_num>");
    Basic_Proc.Set_Error_Exit_Code;
  when Error : others =>
    Basic_Proc.Put_Line_Output ("Exception: "
                   & Ada.Exceptions.Exception_Name (Error));
    Basic_Proc.Set_Error_Exit_Code;
end T_Tcp_Util;

