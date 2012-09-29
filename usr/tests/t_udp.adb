with Ada.Exceptions;
with As.U, Basic_Proc, Argument, Socket, Event_Mng;

procedure T_Udp is

  Arg_Error : exception;
  Server : Boolean;
  Server_Name : As.U.Asu_Us;
  Port_Name : As.U.Asu_Us;

  Soc : Socket.Socket_Dscr;
  Fd  : Event_Mng.File_Desc := 0;

  -- Signal received
  Sig : Boolean := False;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Basic_Proc.Put_Line_Output ("Aborted.");
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
    pragma Unreferenced (Read);
    use type Event_Mng.File_Desc;
    Message_Len : Natural;
  begin
    if F /= Fd then
      Basic_Proc.Put_Line_Output ("Not same Fd");
      raise Program_Error;
    end if;
    Basic_Proc.Put_Output ("In callback - ");
    if Server then
      Basic_Proc.Put_Output ("Server");
    else
      Basic_Proc.Put_Output ("Client");
    end if;
    begin
      My_Receive (Soc, Message, Message_Len, Set_For_Reply => Server);
    exception
      when Socket.Soc_Conn_Lost =>
        Basic_Proc.Put_Line_Output (" receives disconnection");
        return True;
    end;
    Basic_Proc.Put_Line_Output (" receives: >"
                   & Message.Str(1 .. Message.Len)
                   & "< num "
                   & Positive'Image(Message.Num));
    if not Server then
      return False;
    end if;
    Basic_Proc.Put_Line_Output ("      Working");
    delay 5.0;
    Basic_Proc.Put_Line_Output ("      Replying");
    Message.Num := Message.Num + 1;
    My_Send (Soc, Message);
    return False;
  end Call_Back;

  procedure Send is
  begin
    Message.Str := (others => ' ');
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
  Argument.Get_Parameter (Port_Name, Occurence => 2);

  if Argument.Get_Nbre_Arg = 3
  and then Argument.Get_Parameter (Occurence => 3) = "-s" then
    Server := True;
  elsif Argument.Get_Nbre_Arg = 4
  and then Argument.Get_Parameter (Occurence => 3) = "-s" then
    Server := True;
    Argument.Get_Parameter (Server_Name, Occurence => 4);
  elsif Argument.Get_Nbre_Arg = 4
  and then Argument.Get_Parameter (Occurence => 3) = "-c" then
    Server := False;
    Argument.Get_Parameter (Server_Name, Occurence => 4);
  else
    raise Arg_Error;
  end if;

  -- Create socket, add callback
  Soc.Open (Socket.Udp);
  Basic_Proc.Put_Line_Output ("Socket created in mode " & Soc.Get_Blocking'Img);

  Fd := Soc.Get_Fd;
  Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);


  -- Link, set server dest in client, client sends
  if Server then
    if not Server_Name.Is_Null then
      -- Ipm lan
      Soc.Set_Destination_Name_And_Service (
           True, Server_Name.Image, Port_Name.Image);
    end if;
    Soc.Link_Service (Port_Name.Image);

  else
    Soc.Link_Dynamic;
    -- Check if it is a IPM/LAN first, because networks is unlikely resolved
    --  by a DNS, so we get immediate result
    begin
      Soc.Set_Destination_Name_And_Service (True,
         Server_Name.Image, Port_Name.Image);
    exception
      when Socket.Soc_Name_Not_Found =>
        Soc.Set_Destination_Name_And_Service (False,
           Server_Name.Image, Port_Name.Image);
    end;
    Message.Num := 1;
    Send;
  end if;


  -- Main loop
  Main:
  loop
    for I in 1 .. 10 loop
      if Event_Mng.Wait (1000) then
        Basic_Proc.Put_Line_Output ("Fd event");
      else
        Basic_Proc.Put_Line_Output ("Timeout");
      end if;
      exit Main when Sig;
    end loop;
    if not Server then
      Send;
    end if;
  end loop Main;

  if Event_Mng.Fd_Callback_Set (Fd, True) then
    Event_Mng.Del_Fd_Callback (Fd, True);
    Soc.Close;
  end if;

exception
  when Arg_Error =>
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name & " <port> <mode>");
    Basic_Proc.Put_Line_Output ("  <port> ::= -p <port_name>");
    Basic_Proc.Put_Line_Output ("  <mode> ::= -c <server_host_or_lan> "
                             & "| -s [ <server_lan> ]");

  when Error : others =>
    Basic_Proc.Put_Line_Output ("Exception: " & Ada.Exceptions.Exception_Name (Error));
end T_Udp;

