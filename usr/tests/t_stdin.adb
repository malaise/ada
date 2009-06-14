with Ada.Exceptions, Ada.Characters.Latin_1;
with Argument, Socket, Event_Mng, Async_Stdin;

procedure T_Stdin is

  Arg_Error : exception;

  Port : Socket.Port_Num;

  Soc : Socket.Socket_Dscr;
  Fd : Event_Mng.File_Desc := 0;

  Go_On : Boolean;

  -- Signal received
  Sig : Boolean := False;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Async_Stdin.Put_Line_Out ("Aborted.");
    Sig := True;
  end Signal_Cb;


  subtype Message_Type is String(1 .. 1024);
  procedure My_Send is new Socket.Send(Message_Type);
  procedure My_Receive is new Socket.Receive(Message_Type);

  function Socket_Cb(F : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is
    pragma Unreferenced (Read);
    use type Event_Mng.File_Desc;
    Message : Message_Type;
    Message_Len : Natural;
  begin
    if F /= Fd then
      return False;
    end if;
    begin
      My_Receive(Soc, Message, Message_Len, False);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        Async_Stdin.Put_Line_Out("Disconnected");
        Go_On := False;
        return True;
    end;
    Async_Stdin.Put_Out (Message(1 .. Message_Len));
    return False;
  end Socket_Cb;


  function Stdin_Cb(Str : in String) return Boolean is
    Message : Message_Type;
    Len : Natural := Str'Length;
    Last : Natural := Str'Last;
  begin
    if Len = 0 then
      Go_On := False;
      return True;
    end if;
    if Len >= 1 and then Str(Last) = Ada.Characters.Latin_1.Eot then
      Len := Len - 1;
      Last := Last - 1;
      Go_On := False;
    end if;
    Message (1 .. Len) := Str (Str'First .. Last);
    My_Send(Soc, Message, Len);
    return True;
  end Stdin_Cb;

begin

  -- Check arguments and get port
  if Argument.Get_Nbre_Arg = 2 then
    begin
      Port := Socket.Port_Num'Value(Argument.Get_Parameter(2));
    exception
      when others =>
        raise Arg_Error;
    end;
  else
    raise Arg_Error;
  end if;

  -- Set async stdin
  begin
    Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access, 21);
  exception
    when Async_Stdin.Error =>
      Async_Stdin.Put_Line_Out("Cannot set stdin async");
      return;
  end;

  -- Connect
  Socket.Open(Soc, Socket.Tcp_Header);
  Fd := Socket.Fd_Of(Soc);
  Async_Stdin.Put_Line_Out("Connecting to Host "
                & Argument.Get_Parameter(Occurence => 1)
                & " Port" & Socket.Port_Num'Image(Port));
  begin
    Socket.Set_Destination_Name_And_Port(Soc,
             False, Argument.Get_Parameter(1), Port);
  exception
    when Socket.Soc_Conn_Refused =>
      Async_Stdin.Put_Line_Out("Connection refused");
      Socket.Close(Soc);
      Async_Stdin.Set_Async;
      return;
  end;
  Event_Mng.Add_Fd_Callback(Fd, True, Socket_Cb'Unrestricted_Access);
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);


  -- Main loop
  Go_On := True;
  loop
    if Event_Mng.Wait(-1) then
      exit when not Go_On;
    end if;
    exit when Sig;
  end loop;

  if Event_Mng.Fd_Callback_Set (Fd, True) then
    Event_Mng.Del_Fd_Callback(Fd, True);
    Socket.Close(Soc);
  end if;
  Async_Stdin.Set_Async;

exception
  when Arg_Error =>
    Async_Stdin.Put_Line_Err ("Usage: " & Argument.Get_Program_Name
                   & " <host_name> <port_num>");

  when Error : others =>
    Async_Stdin.Put_Line_Err ("Exception: " & Ada.Exceptions.Exception_Name(Error));
    Async_Stdin.Set_Async;
end T_Stdin;

