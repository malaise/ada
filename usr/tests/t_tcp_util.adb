with Ada.Exceptions, Ada.Text_Io;
with As.U, Argument, Lower_Str, Event_Mng, Socket, Tcp_Util;
procedure T_Tcp_Util is
  Arg_Error : exception;

  Protocol : constant Socket.Protocol_List := Socket.Tcp_Header;
  -- Protocol : constant Socket.Protocol_List := Socket.Tcp_Header_Afux;
  Server : Boolean;
  Server_Name : As.U.Asu_Us;
  Server_Port_Name : As.U.Asu_Us;
  Server_Port_Num : Socket.Port_Num;
  Local_Port : Tcp_Util.Local_Port;
  Remote_Port : Tcp_Util.Remote_Port;

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

  function Read_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
                   return Boolean;
  procedure End_Ovf_Cb (Dscr : in  Socket.Socket_Dscr);

  -- Signal received
  Sig : Boolean := False;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Ada.Text_Io.Put_Line ("Aborted.");
    Sig := True;
  end Signal_Cb;

  procedure Connect;
  procedure Send_Err_Cb (Dscr : in  Socket.Socket_Dscr;
                         Conn_Lost : in Boolean) is
  begin
    Ada.Text_Io.Put_Line ("Send_Err_Cb with lost_conn=" & Conn_Lost'Img);
    Event_Mng.Del_Fd_Callback (Dscr.Get_Fd, True);
    The_Dscr := Socket.No_Socket;
    Lost := True;
    Connect;
  end Send_Err_Cb;

  function Send (Msg : in String) return Boolean is
  begin
    if not The_Dscr.Is_Open then
      Ada.Text_Io.Put_Line (Msg & " not sending cause not open");
      return False;
    end if;
    if Lost then
      Ada.Text_Io.Put_Line (Msg & " not sending cause lost connection");
      return False;
    end if;
    if Server then
      if not In_Ovf then
        if My_Send (The_Dscr, End_Ovf_Cb'Unrestricted_Access,
                    Send_Err_Cb'Unrestricted_Access, 1.0, Message) then
          Ada.Text_Io.Put_Line (Msg & " sent num "
                                    & Integer'Image(Message.Num));
          return True;
        else
          In_Ovf := True;
          Ada.Text_Io.Put_Line (Msg & "                     sending overflow");
        end if;
      else
         Ada.Text_Io.Put_Line (Msg & " not sending cause in overflow");
      end if;
      return False;
    else
      -- Client
      if In_Ovf then
        Ada.Text_Io.Put_Line (Msg & " not sending cause in overflow");
        return False;
      end if;
      while My_Send (The_Dscr, End_Ovf_Cb'Unrestricted_Access,
                     Send_Err_Cb'Unrestricted_Access, 1.0, Message) loop
        Ada.Text_Io.Put_Line (Msg & " sent num " & Integer'Image(Message.Num));
        Message.Num := Message.Num + 1;
      end loop;
      In_Ovf := True;
      Ada.Text_Io.Put_Line (Msg & "                     sending overflow");
    end if;
    return False;
  exception
    when Error : others =>
      Ada.Text_Io.Put_Line ("Sending " & Msg & " failed cause "
        & Lower_Str (Ada.Exceptions.Exception_Name (Error)) & ".");
      return False;
  end Send;

  procedure End_Ovf_Cb (Dscr : in  Socket.Socket_Dscr) is
    use type Socket.Socket_Dscr;
  begin
    if Dscr /= The_Dscr then
      Ada.Text_Io.Put_Line ("End of overflow on invalid dscr");
      return;
    end if;
    Ada.Text_Io.Put_Line (
       "End of overflow Cb ->                    End of overflow");
    In_Ovf := False;
  end End_Ovf_Cb;

  procedure Connect_Cb (Remote_Host_Id  : in Tcp_Util.Host_Id;
                        Remote_Port_Num : in Tcp_Util.Port_Num;
                        Connected       : in Boolean;
                        Dscr            : in Socket.Socket_Dscr) is
    pragma Unreferenced (Remote_Port_Num, Remote_Host_Id);
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    if Connected then
      The_Dscr := Dscr;
      Event_Mng.Add_Fd_Callback (Dscr.Get_Fd,
                            True,
                            Read_Cb'Unrestricted_Access);
      The_Dscr.Set_Blocking (Socket.Non_Blocking);
      Ada.Text_Io.Put_Line ("Connected and non blocking");
      Lost := False;
      In_Ovf := False;
      if The_Dscr.Get_Ttl /= Ttl then
        Ada.Text_Io.Put_Line ("TTL lost!");
      end if;
    else
      Ada.Text_Io.Put_Line ("Not connected");
      Give_Up := True;
      return;
    end if;
    if not Server then
      Dummy := Send ("First request");
    end if;
  end Connect_Cb;

  procedure Connect is
    Host : Tcp_Util.Remote_Host(Tcp_Util.Host_Name_Spec);
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    Host.Name := Server_Name;
    Result := Tcp_Util.Connect_To (Protocol,
                                   Host, Remote_Port,
                                   Connect_Cb'Unrestricted_Access,
                                   Delay_Try, Nb_Try, Ttl);

  end Connect;

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
    if The_Dscr /= Socket.No_Socket then
      Ada.Text_Io.Put_Line ("Rejected");
      Tmp_Dscr := New_Dscr;
      Tmp_Dscr.Close;
    else
      The_Dscr := New_Dscr;
      Event_Mng.Add_Fd_Callback (New_Dscr.Get_Fd,
                            True,
                            Read_Cb'Unrestricted_Access);
      The_Dscr.Set_Blocking (Socket.Non_Blocking);
      Ada.Text_Io.Put ("Accepted and non blocking");
      Lost := False;
      In_Ovf := False;
    end if;
  end Accept_Cb;

  procedure Wait (Dur : in Duration) is
  begin
    if Event_Mng.Wait (Integer (Dur) * 1_000) then
      Ada.Text_Io.Put_Line ("Timer/Event");
    else
      Ada.Text_Io.Put_Line ("Timeout");
    end if;
  end Wait;

  function Read_Cb (Fd : in Event_Mng.File_Desc; Read : in Boolean)
           return Boolean is
    pragma Unreferenced (Read);
    Len : Natural;
    Res : Boolean;
    pragma Unreferenced (Res);
    use type Event_Mng.File_Desc;
  begin
    if Server then
      Ada.Text_Io.Put ("Server: ");
    else
      Ada.Text_Io.Put ("Client: ");
    end if;
    if not The_Dscr.Is_Open or else Fd /= The_Dscr.Get_Fd then
      Ada.Text_Io.Put_Line ("Read Cb -> Unknown fd");
      return False;
    end if;

    Received_Message.Str := (others => ' ');
    begin
      My_Read (The_Dscr, Received_Message, Len, False);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        Ada.Text_Io.Put_Line ("Read Cb -> disconnected: Closing");
        Event_Mng.Del_Fd_Callback (Fd, True);
        if In_Ovf then
          Tcp_Util.Abort_Send_And_Close (The_Dscr);
          In_Ovf := False;
        else
          The_Dscr.Close;
        end if;
        if not Server then
          Ada.Text_Io.Put_Line (" - Waiting");
          delay 3.0;
          Connect;
        else
          Ada.Text_Io.New_Line;
        end if;
        return True;
      when Socket.Soc_Would_Block =>
        Ada.Text_Io.Put_Line ("Read Cb ->                     underflow");
        return False;
    end;

    Message := Received_Message;
    Ada.Text_Io.Put_Line ("receives: >"
                   & Message.Str(1 .. Message.Len)
                   & "< num "
                   & Positive'Image(Message.Num));
    if Message.Str(1 .. Message.Len) /= Str then
      Ada.Text_Io.Put_Line ("Incorrect message received");
      raise Program_Error;
    end if;


    if not Server then
      Ada.Text_Io.Put_Line ("      Working");
      delay 0.3;
      Res := Send ("Request");
    else
      Ada.Text_Io.Put_Line ("      Working");
      delay 0.1;
      Ada.Text_Io.Put_Line ("      Replying");
      Message.Num := Message.Num + 10;
      if not Send ("Reply") and then In_Ovf then
        if Lost then
          Ada.Text_Io.Put_Line ("      Not responding to lost client");
          return False;
        end if;
        if Server_Nb_Overflow < 50 then
          Ada.Text_Io.Put_Line ("      Not responding to client in overflow");
          Server_Nb_Overflow := Server_Nb_Overflow + 1;
        else
          -- Cancel with this bad client
          Ada.Text_Io.Put_Line ("      Closing with client in overflow");
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
    Local_Port := (Kind => Tcp_Util.Port_Name_Spec, Name => Server_Port_Name);
    Remote_Port := (Kind => Tcp_Util.Port_Name_Spec, Name => Local_Port.Name);
  else
    Local_Port := (Kind => Tcp_Util.Port_Num_Spec, Num => Server_Port_Num);
    Remote_Port := (Kind => Tcp_Util.Port_Num_Spec, Num => Server_Port_Num);
  end if;

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
          Ada.Text_Io.Put_Line ("Cannot accept. Maybe Close-wait. Waiting");
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
    Ada.Text_Io.Put_Line ("Usage: "
            & Argument.Get_Program_Name & " <mode> <port>");
    Ada.Text_Io.Put_Line (" <mode> ::= -c<server_host> | -s");
    Ada.Text_Io.Put_Line (" <port> ::= -P<port_name> | -p<port_num>");
  when Error : others =>
    Ada.Text_Io.Put_Line ("Exception: "
                   & Ada.Exceptions.Exception_Name (Error));
end T_Tcp_Util;

