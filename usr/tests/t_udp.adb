with Ada.Exceptions;
with My_Io, Argument, Socket, X_Mng, Text_Handler;

procedure T_Udp is

  Arg_Error : exception;
  Server : Boolean;
  Server_Name : Text_Handler.Text (80);

  Soc : Socket.Socket_Dscr;
  Fd  : X_Mng.File_Desc;

  Server_Port_Name : constant String := "test_udp";

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
    if F /= Fd then
      My_Io.Put_Line ("Not same Fd");
      raise Program_Error;
    end if;
    My_Io.Put ("In callback - ");
    My_Receive (Soc, Message, Message_Len, Received, Set_For_Reply => Server);
    if Server then
      My_Io.Put ("Server");
    else
      My_Io.Put ("Client");
    end if;
    if not Received then
      My_Io.Put_Line (" receives nothing");
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
  end Call_Back;

  procedure Send is
  begin
    Message.Str (1 .. Str'Length) := Str;
    Message.Len := Str'Length;
    My_Send (Soc, Message, 30);
  end Send;

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

  -- Create socket, add callback
  Socket.Open (Soc, Socket.Udp);
  Fd := Socket.Fd_Of (Soc);
  X_Mng.X_Add_Callback (Fd, Call_Back'Unrestricted_Access);
  
  -- Link, set server dest in client, client sends
  if Server then
    Socket.Link_Service (Soc, Server_Port_Name);
  else
    Socket.Link_Dynamic (Soc);
    Socket.Set_Destination_Name_And_Service (Soc,
           False, Text_Handler.Value (Server_Name), Server_Port_Name);
    Message.Num := 1;
    Send;
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
      Send;
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
end T_Udp;

