with Ada.Exceptions;
with My_Io, Argument, Udp, X_Mng, Text_Handler;

procedure T_Udp is

  Arg_Error : exception;
  Server : Boolean;
  Server_Name : Text_Handler.Text (80);

  Soc : Udp.Socket_Dscr;
  Fd  : X_Mng.File_Desc;

  Server_Port_Name : constant String := "test_udp";

  subtype Message_Type is String (1 .. 80);
  Message : Message_Type;
  Message_Len : Positive;

  procedure My_Send is new Udp.Send (Message_Type);
  procedure My_Receive is new Udp.Receive (Message_Type);

  procedure Call_Back (F : in X_Mng.File_Desc) is
    use type X_Mng.File_Desc;
  begin
    if F /= Fd then
      My_Io.Put_Line ("Not same Fd");
      raise Program_Error;
    end if;
    My_Io.Put ("In callback - ");
    My_Receive (Soc, Message, Message_Len, Set_For_Reply => True);
    if Server then
      My_Io.Put ("Server");
    else
      My_Io.Put ("Client");
    end if;
    My_Io.Put_Line (" receives: >" & Message(1 .. Message_Len) & "<");
    delay 5.0;
    My_Send (Soc, Message, Message_Len);
  end Call_Back;

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
  Udp.Open (Soc);
  Fd := Udp.Fd_Of (Soc);
  X_Mng.X_Add_Callback (Fd, Call_Back'Unrestricted_Access);
  
  -- Link, set server dest in client, client sends
  if Server then
    Udp.Link_Service (Soc, Server_Port_Name);
  else
    Udp.Link_Dynamic (Soc);
    Udp.Set_Destination_Name_And_Service (Soc,
           False, Text_Handler.Value (Server_Name), Server_Port_Name);
    declare
      Str : Constant String := "Ah que coucou!";
    begin
      Message_Len := Str'Length;
      Message (1 .. Message_Len) := Str;
    end;
    My_Send (Soc, Message, Message_Len);
  end if;


  -- Main loop
  loop
    X_Mng.Select_No_X (1000);
    My_Io.Put_Line ("Timeout");
  end loop;

  X_Mng.X_Del_Callback (Fd);
  Udp.Close (Soc);

exception
  when Arg_Error =>
    My_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                   & " -c <server_host> | -s");

  when Error : others =>
    My_Io.Put_Line ("Exception: " & Ada.Exceptions.Exception_Name (Error));
end T_Udp;

