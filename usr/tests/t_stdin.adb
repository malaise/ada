with Ada.Text_Io, Ada.Exceptions;
with Argument, Socket, X_Mng, Sys_Calls, Tcp_Util;

procedure T_Stdin is

  Arg_Error : exception;

  Port : Socket.Port_Num;

  Soc : Socket.Socket_Dscr;
  Fd : X_Mng.File_Desc;

  Go_On : Boolean;

  subtype Message_Type is String(1 .. 1024);
  procedure My_Send is new Socket.Send(Message_Type);
  procedure My_Receive is new Socket.Receive(Message_Type);

  function Socket_Cb(F : in X_Mng.File_Desc; Read : in Boolean)
                     return Boolean is
    use type X_Mng.File_Desc;
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


  function Stdin_Cb(F : in X_Mng.File_Desc; Read : in Boolean)
                    return Boolean is
    Message : Message_Type;
    Message_Len : Natural;
  begin
    Ada.Text_Io.Get_Line(Message, Message_Len);
    My_Send(Soc, Message, Message_Len);
    Message(1) := Ascii.Lf;
    My_Send(Soc, Message, 1);

    return False;
  exception
    when Ada.Text_Io.End_Error =>
      Go_On := False;
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

  -- Connect
  Socket.Open(Soc, Socket.Tcp);
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
      return;
  end;
  X_Mng.X_Add_Callback(Fd, True, Socket_Cb'Unrestricted_Access);
  X_Mng.X_Add_Callback(Sys_Calls.Stdin, True, Stdin_Cb'Unrestricted_Access);


  -- Main loop
  loop
    if X_Mng.Select_No_X(-1) then
      exit when not Go_On;
    end if;
  end loop;

  X_Mng.X_Del_Callback(Fd, True);
  X_Mng.X_Del_Callback(Sys_Calls.Stdin, True);
  Socket.Close(Soc);

exception
  when Arg_Error =>
    Ada.Text_Io.Put_Line("Usage: " & Argument.Get_Program_Name
                   & " <host_name> <port_num>");

  when Error : others =>
    Ada.Text_Io.Put_Line("Exception: " & Ada.Exceptions.Exception_Name(Error));
end T_Stdin;

