with Ada.Exceptions;
with My_Io, Argument, Socket, Event_Mng, Text_Handler;

procedure T_Tcp is

  Arg_Error : exception;
  Server : Boolean;
  Server_Name : Text_Handler.Text (80);

  Soc, Accept_Soc : Socket.Socket_Dscr;
  Fd, Accept_Fd : Event_Mng.File_Desc := 0;
  Server_Port_Name : constant String := "test_tcp";

  -- Signal received
  Sig : Boolean := False;

  -- Signal callback
  procedure Signal_Cb is
  begin
    My_Io.Put_Line ("Aborted.");
    Sig := True;
  end Signal_Cb;

  type Message_Type is record
    Len : Positive;
    Num : Positive;
    Str : String (1 .. 80);
  end record;

  Str : constant String := "Ah que coucou!";
  Message : Message_Type;

  procedure My_Send is new Socket.Send (Message_Type);
  procedure My_Receive is new Socket.Receive (Message_Type);

  function Call_Back (F : in Event_Mng.File_Desc; Read : in Boolean)
  return Boolean is
    use type Event_Mng.File_Desc;
    Message_Len : Natural;
  begin
    My_Io.Put ("In callback - ");
    if F = Accept_Fd then
      if Socket.Is_Open (Soc) and then Socket.Is_Connected (Soc) then
        My_Io.Put_Line ("rejects new connection");
        declare
          Tmp_Soc : Socket.Socket_Dscr;
        begin
          Socket.Accept_Connection (Accept_Soc, Tmp_Soc);
          Socket.Close (Tmp_Soc);
        end;
        return False;
      end if;
      Socket.Accept_Connection (Accept_Soc, Soc);
      Fd := Socket.Fd_Of (Soc);
      Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
      My_Io.Put_Line ("accepts connection");
      return True;
    end if;
      
    if F /= Fd then
      My_Io.Put_Line ("Not same Fd");
      raise Program_Error;
    end if;
    if Server then
      My_Io.Put ("Server");
    else
      My_Io.Put ("Client");
    end if;
    begin
      My_Receive (Soc, Message, Message_Len, False);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        My_Io.Put_Line (" receives disconnection: Closing");
        Event_Mng.Del_Fd_Callback (Fd, True);
        Socket.Close (Soc);
        return True;
    end;
    My_Io.Put_Line (" receives: >"
                   & Message.Str(1 .. Message.Len)
                   & "< num "
                   & Positive'Image(Message.Num));
    if not Server then
      return False;
    end if;
    My_Io.Put_Line ("      Working");
    delay 5.0;
    My_Io.Put_Line ("      Replying");
    Message.Num := Message.Num + 1;
    My_Send (Soc, Message);
    My_Io.Put_Line ("      Reply sent");
    return False;
  end Call_Back;

  function Client_Connect return Boolean is
  begin
    Socket.Open (Soc, Socket.Tcp_Header);
    Fd := Socket.Fd_Of (Soc);
    My_Io.Put_Line ("Client connecting");
    begin
      Socket.Set_Destination_Name_And_Service (Soc,
             False, Text_Handler.Value (Server_Name), Server_Port_Name);
    exception
      when Socket.Soc_Conn_Refused =>
        My_Io.Put_Line ("Client connection has failed. Closing");
        Socket.Close (Soc);
        return False;
    end;
    Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
    return True;
  end Client_Connect;

  procedure Client_Send is
  begin
    if not Socket.Is_Open (Soc) then
      if not Client_Connect then
        return;
      end if;
    end if;
    Message.Str (1 .. Str'Length) := Str;
    Message.Len := Str'Length;
    My_Io.Put_Line ("Client sending");
    begin
      My_Send (Soc, Message, 30);
    exception
      when Socket.Soc_Conn_Lost =>
         My_Io.Put_Line ("Client sending disconnection: Closing");
         Event_Mng.Del_Fd_Callback (Fd, True);
         Socket.Close (Soc);
      when others =>
         My_Io.Put_Line ("Client sending has failed!");
    end;
  end Client_Send;

begin

  -- Server or client
  if Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-s" then
    Server := True;
  elsif Argument.Get_Nbre_Arg = 2 and then Argument.Get_Parameter = "-c" then
    Server := False;
    Argument.Get_Parameter (Server_Name, 2);
  else
    raise Arg_Error;
  end if;

  -- Link, set server dest in client, client sends
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
  if Server then
    -- Create socket, add callback
    Socket.Open (Accept_Soc, Socket.Tcp_Header);
    loop
      begin
        Socket.Link_Service (Accept_Soc, Server_Port_Name);
        exit;
      exception
        when Socket.Soc_Addr_In_Use =>
           My_Io.Put_Line ("Address in use, waiting");
           delay 20.0;
      end;
    end loop;
    Accept_Fd := Socket.Fd_Of (Accept_Soc);
    Event_Mng.Add_Fd_Callback (Accept_Fd, True, Call_Back'Unrestricted_Access);
  else
    Message.Num := 1;
    Client_Send;
  end if;


  -- Main loop
  Main:
  loop
    for I in 1 .. 10 loop
      if Event_Mng.Wait (1000) then
        My_Io.Put_Line ("Fd event");
      else
        My_Io.Put_Line ("Timeout");
      end if;
      exit Main when Sig;
    end loop;
    if not Server then
      Client_Send;
    end if;
  end loop Main;

  if Event_Mng.Fd_Callback_Set (Fd, True) then
    Event_Mng.Del_Fd_Callback (Fd, True);
    Socket.Close (Soc);
  end if;

exception
  when Arg_Error =>
    My_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                   & " -c <server_host> | -s");

  when Error : others =>
    My_Io.Put_Line ("Exception: " & Ada.Exceptions.Exception_Name (Error));
end T_Tcp;

