with Ada.Exceptions;
with My_Io, Argument, Socket, Event_Mng, Text_Handler;

procedure T_Udp is

  Arg_Error : exception;
  Server : Boolean;
  Server_Name : Text_Handler.Text (80);
  Port_Name : Text_Handler.Text (80);

  Soc : Socket.Socket_Dscr;
  Fd  : Event_Mng.File_Desc := 0;

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

  function Call_Back (F : in Event_Mng.File_Desc; Read : in Boolean)
  return Boolean is
    use type Event_Mng.File_Desc;
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
  if Argument.Get_Nbre_Arg < 3
  or else Argument.Get_Parameter (Occurence => 1) /= "-p" then
    raise Arg_Error;
  end if;
  Argument.Get_Parameter (Port_Name, 2);

  if Argument.Get_Nbre_Arg = 3
  and then Argument.Get_Parameter (Occurence => 3) = "-s" then
    Server := True;
  elsif Argument.Get_Nbre_Arg = 4
  and then Argument.Get_Parameter (Occurence => 3) = "-s" then
    Server := True;
    Argument.Get_Parameter (Server_Name, 4);
  elsif Argument.Get_Nbre_Arg = 4
  and then Argument.Get_Parameter (Occurence => 3) = "-c" then
    Server := False;
    Argument.Get_Parameter (Server_Name, 4);
  else
    raise Arg_Error;
  end if;

  -- Create socket, add callback
  Socket.Open (Soc, Socket.Udp);
  Fd := Socket.Fd_Of (Soc);
  Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  
  -- Link, set server dest in client, client sends
  if Server then
    if not Text_Handler.Empty (Server_Name) then
      -- Ipm lan
      Socket.Set_Destination_Name_And_Service (Soc,
           True, Text_Handler.Value (Server_Name),
           Text_Handler.Value(Port_Name));
    end if;
    Socket.Link_Service (Soc, Text_Handler.Value(Port_Name));

  else
    Socket.Link_Dynamic (Soc);
    -- Check host
    declare
      Dest_Host_Id : Socket.Host_Id;
    begin
      Dest_Host_Id := Socket.Host_Id_Of (Text_Handler.Value (Server_Name));
      Socket.Set_Destination_Name_And_Service (Soc,
         False, Text_Handler.Value (Server_Name),
         Text_Handler.Value(Port_Name));
    exception
      when Socket.Soc_Name_Not_Found =>
        Socket.Set_Destination_Name_And_Service (Soc,
         True, Text_Handler.Value (Server_Name),
         Text_Handler.Value(Port_Name));
    end;
    Message.Num := 1;
    Send;
  end if;


  -- Main loop
  Main:
  loop
    for i in 1 .. 10 loop
      if Event_Mng.Wait (1000) then
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

  if Event_Mng.Fd_Callback_Set (Fd, True) then
    Event_Mng.Del_Fd_Callback (Fd, True);
    Socket.Close (Soc);
  end if;

exception
  when Arg_Error =>
    My_Io.Put_Line ("Usage: " & Argument.Get_Program_Name & " <port> <mode>");
    My_Io.Put_Line ("  <port> ::= -p <port_name>");
    My_Io.Put_Line ("  <mode> ::= -c <server_host_or_lan> "
                             & "| -s [ <server_lan> ]");

  when Error : others =>
    My_Io.Put_Line ("Exception: " & Ada.Exceptions.Exception_Name (Error));
end T_Udp;

