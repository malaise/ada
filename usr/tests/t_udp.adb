with Ada.Exceptions;
with My_Io, Argument, Socket, X_Mng, Text_Handler;

procedure T_Udp is

  Arg_Error : exception;
  Server : Boolean;
  Server_Name : Text_Handler.Text (80);

  Soc : Socket.Socket_Dscr;
  Fd  : X_Mng.File_Desc := 0;

  Server_Port_Name : constant String := "test_udp";

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

  Str : Constant String := "Ah que coucou!";
  Message : Message_Type;

  procedure My_Send is new Socket.Send (Message_Type);
  procedure My_Receive is new Socket.Receive (Message_Type);

  function Call_Back (F : in X_Mng.File_Desc; Read : in Boolean)
  return Boolean is
    use type X_Mng.File_Desc;
    Message_Len : Natural;
  begin
    if F /= Fd then
      My_Io.Put_Line ("Not same Fd");
      raise Program_Error;
    end if;
    My_Io.Put ("In callback - ");
    if Server then
      My_Io.Put ("Server");
    else
      My_Io.Put ("Client");
    end if;
    begin
      My_Receive (Soc, Message, Message_Len, Set_For_Reply => Server);
    exception
      when Socket.Soc_Conn_Lost =>
        My_Io.Put_Line (" receives disconnection");
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
    return False;
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
  X_Mng.X_Add_Callback (Fd, True, Call_Back'Unrestricted_Access);
  X_Mng.X_Set_Signal (Signal_Cb'Unrestricted_Access);

  
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
  Main:
  loop
    for i in 1 .. 10 loop
      if X_Mng.Select_No_X (1000) then
        My_Io.Put_Line ("Fd event");
      else
        My_Io.Put_Line ("Timeout");
      end if;
      exit Main when Sig;
    end loop;
    if not Server then
      Send;
    end if;
  end loop Main;

  if X_Mng.X_Callback_Set (Fd, True) then
    X_Mng.X_Del_Callback (Fd, True);
    Socket.Close (Soc);
  end if;

exception
  when Arg_Error =>
    My_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                   & " -c <server_host> | -s");

  when Error : others =>
    My_Io.Put_Line ("Exception: " & Ada.Exceptions.Exception_Name (Error));
end T_Udp;

