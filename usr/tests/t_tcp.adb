with Ada.Exceptions;
with My_Io, Argument, Socket, X_Mng, Text_Handler;

procedure T_Tcp is

  Arg_Error : exception;
  Server : Boolean;
  Server_Name : Text_Handler.Text (80);

  Soc, Accept_Soc : Socket.Socket_Dscr;
  Fd, Accept_Fd : X_Mng.File_Desc;
  Server_Port_Name : constant String := "test_tcp";

  type Message_Type is record
    Len : Positive;
    Num : Positive;
    Str : String (1 .. 80);
  end record;

  Str : Constant String := "Ah que coucou!";
  Message : Message_Type;

  procedure My_Send is new Socket.Send (Message_Type);
  procedure My_Receive is new Socket.Receive (Message_Type);

  procedure Call_Back (F : in X_Mng.File_Desc) is
    use type X_Mng.File_Desc;
    Message_Len : Natural;
    Received : Boolean;
  begin
    My_Io.Put ("In callback - ");
    if F = Accept_Fd then
      Socket.Accept_Connection (Accept_Soc, Soc);
      Fd := Socket.Fd_Of (Soc);
      X_Mng.X_Add_Callback (Fd, Call_Back'Unrestricted_Access);
      My_Io.Put_Line (" accepts");
      return;
    end if;
      
    if F /= Fd then
      My_Io.Put_Line ("Not same Fd");
      raise Program_Error;
    end if;
    My_Receive (Soc, Message, Message_Len, Received, False);
    if Server then
      My_Io.Put ("Server");
    else
      My_Io.Put ("Client");
    end if;
    if not Received or else Message_Len = 0 then
      My_Io.Put_Line (" receives nothing: Closing");
      X_Mng.X_Del_Callback (Fd);
      Socket.Close (Soc);
      return;
    end if;
    My_Io.Put_Line (" receives: >"
                   & Message.Str(1 .. Message.Len)
                   & "< num "
                   & Positive'Image(Message.Num));
    if not Server then
      return;
    end if;
    My_Io.Put_Line ("      Working");
    delay 5.0;
    My_Io.Put_Line ("      Replying");
    Message.Num := Message.Num + 1;
    My_Send (Soc, Message);
    My_Io.Put_Line ("      Reply sent");
  end Call_Back;

  function Client_Connect return Boolean is
  begin
    Socket.Open (Soc, Socket.Tcp);
    Fd := Socket.Fd_Of (Soc);
    My_Io.Put_Line ("Client connecting");
    Socket.Set_Destination_Name_And_Service (Soc,
           False, Text_Handler.Value (Server_Name), Server_Port_Name);
    if not Socket.Is_Connected (Soc) then
      My_Io.Put_Line ("Client connection has failed. Closing");
      Socket.Close (Soc);
      return False;
    else
      X_Mng.X_Add_Callback (Fd, Call_Back'Unrestricted_Access);
      return True;
    end if;
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
      when Socket.Socket_Error =>
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
  if Server then
    -- Create socket, add callback
    Socket.Open (Accept_Soc, Socket.Tcp);
    Socket.Link_Service (Accept_Soc, Server_Port_Name);
    Accept_Fd := Socket.Fd_Of (Accept_Soc);
    X_Mng.X_Add_Callback (Accept_Fd, Call_Back'Unrestricted_Access);
  else
    Message.Num := 1;
    Client_Send;
  end if;


  -- Main loop
  loop
    for i in 1 .. 10 loop
      if X_Mng.Select_No_X (1000) then
        My_Io.Put_Line ("Fd event");
      else
        My_Io.Put_Line ("Timeout");
      end if;
    end loop;
    if not Server then
      Client_Send;
    end if;
  end loop;

  X_Mng.X_Del_Callback (Fd);
  Socket.Close (Soc);

exception
  when Arg_Error =>
    My_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                   & " -c <server_host> | -s");

  when Error : others =>
    My_Io.Put_Line ("Exception: " & Ada.Exceptions.Exception_Name (Error));
end T_Tcp;

