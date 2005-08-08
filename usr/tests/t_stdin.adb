with Ada.Text_Io, Ada.Exceptions, Ada.Characters.Latin_1;
with Argument, Socket, Event_Mng, Tcp_Util, Async_Stdin;

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
    Ada.Text_Io.Put_Line ("Aborted.");
    Sig := True;
  end Signal_Cb;


  subtype Message_Type is String(Async_Stdin.Max_Chars_Range);
  procedure My_Send is new Socket.Send(Message_Type);
  procedure My_Receive is new Socket.Receive(Message_Type);

  function Socket_Cb(F : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is
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
        Ada.Text_Io.Put_Line("Disconnected");
        Go_On := False;
        return True;
    end;
    Ada.Text_Io.Put(Message(1 .. Message_Len));
    return False;
  end Socket_Cb;


  function Stdin_Cb(Str : in String) return Boolean is
    Message : Message_Type;
    Len : Natural := Str'Length;
  begin
    if Len = 0 then
      Go_On := False;
      return True;
    end if;
    if Len >= 1 and then Str(Str'Length) = Ada.Characters.Latin_1.Eot then
      Len := Len - 1;
      Go_On := False;
    end if;
    Message (1 .. Len) := Str (1 .. Len);
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
      Ada.Text_Io.Put_Line("Cannot set stdin async");
      return;
  end;

  -- Connect
  Socket.Open(Soc, Socket.Tcp_Header);
  Fd := Socket.Fd_Of(Soc);
  Ada.Text_Io.Put_Line("Connecting to Host "
                & Argument.Get_Parameter(Occurence => 1)
                & " Port" & Socket.Port_Num'Image(Port));
  begin
    Socket.Set_Destination_Name_And_Port(Soc,
             False, Argument.Get_Parameter(1), Port);
  exception
    when Socket.Soc_Conn_Refused =>
      Ada.Text_Io.Put_Line("Connection refused");
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
    Ada.Text_Io.Put_Line("Usage: " & Argument.Get_Program_Name
                   & " <host_name> <port_num>");

  when Error : others =>
    Ada.Text_Io.Put_Line("Exception: " & Ada.Exceptions.Exception_Name(Error));
    Async_Stdin.Set_Async;
end T_Stdin;

