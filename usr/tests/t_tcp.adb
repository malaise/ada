with Ada.Exceptions;
with As.U, Basic_Proc, Argument, Socket, Event_Mng;

procedure T_Tcp is

  Arg_Error : exception;
  Server : Boolean;
  Server_Name : As.U.Asu_Us;

  Soc, Accept_Soc : Socket.Socket_Dscr;
  Fd, Accept_Fd : Event_Mng.File_Desc := 0;
  Server_Port_Name : constant String := "test_tcp";
  Protocol : constant Socket.Protocol_List := Socket.Tcp_Header;
  -- Protocol : constant Socket.Protocol_List := Socket.Tcp_Header_Afux;

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
    Basic_Proc.Put_Output ("In callback - ");
    if F = Accept_Fd then
      if Soc.Is_Open and then Soc.Is_Connected then
        Basic_Proc.Put_Line_Output ("rejects new connection");
        declare
          Tmp_Soc : Socket.Socket_Dscr;
        begin
          Accept_Soc.Accept_Connection (Tmp_Soc);
          Tmp_Soc.Close;
        exception
          when Socket.Soc_Would_Block =>
            null;
        end;
        return False;
      end if;
      begin
        Accept_Soc.Accept_Connection (Soc);
      exception
        when Socket.Soc_Would_Block =>
          return False;
      end;
      Fd := Soc.Get_Fd;
      Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
      Basic_Proc.Put_Line_Output ("accepts connection");
      return True;
    end if;

    if F /= Fd then
      Basic_Proc.Put_Line_Output ("Not same Fd");
      raise Program_Error;
    end if;
    if Server then
      Basic_Proc.Put_Output ("Server");
    else
      Basic_Proc.Put_Output ("Client");
    end if;
    begin
      My_Receive (Soc, Message, Message_Len, False);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        Basic_Proc.Put_Line_Output (" receives disconnection: Closing");
        Event_Mng.Del_Fd_Callback (Fd, True);
        Soc.Close;
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
    Basic_Proc.Put_Line_Output ("      Reply sent");
    return False;
  exception
    when Socket.Soc_Conn_Lost =>
      Basic_Proc.Put_Line_Output ("      Connection lost: Closing.");
      Event_Mng.Del_Fd_Callback (Fd, True);
      Soc.Close;
      return False;
  end Call_Back;

  function Client_Connect return Boolean is
  begin
    Soc.Open (Protocol);
    Fd := Soc.Get_Fd;
    Basic_Proc.Put_Line_Output ("Client connecting");
    begin
      Soc.Set_Destination_Name_And_Service (False,
             Server_Name.Image, Server_Port_Name);
    exception
      when Socket.Soc_Conn_Refused =>
        Basic_Proc.Put_Line_Output ("Client connection has failed. Closing");
        Soc.Close;
        return False;
    end;
    Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
    return True;
  end Client_Connect;

  procedure Client_Send is
  begin
    if not Soc.Is_Open then
      if not Client_Connect then
        return;
      end if;
    end if;
    Message.Str (1 .. Str'Length) := Str;
    Message.Len := Str'Length;
    Basic_Proc.Put_Line_Output ("Client sending");
    begin
      My_Send (Soc, Message, 30);
    exception
      when Socket.Soc_Conn_Lost =>
         Basic_Proc.Put_Line_Output ("Client sending disconnection: Closing");
         Event_Mng.Del_Fd_Callback (Fd, True);
         Soc.Close;
      when others =>
         Basic_Proc.Put_Line_Output ("Client sending has failed!");
    end;
  end Client_Send;

begin

  -- Server or client
  if Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-s" then
    Server := True;
  elsif Argument.Get_Nbre_Arg = 2 and then Argument.Get_Parameter = "-c" then
    Server := False;
    Argument.Get_Parameter (Server_Name, Occurence => 2);
  else
    raise Arg_Error;
  end if;

  -- Link, set server dest in client, client sends
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
  if Server then
    -- Create socket, add callback
    Accept_Soc.Open (Protocol);
    loop
      begin
        Accept_Soc.Link_Service (Server_Port_Name);
        exit;
      exception
        when Socket.Soc_Addr_In_Use =>
           Basic_Proc.Put_Line_Output ("Address in use, waiting");
           delay 20.0;
      end;
    end loop;
    Accept_Fd := Accept_Soc.Get_Fd;
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
        Basic_Proc.Put_Line_Output ("Fd event");
      else
        Basic_Proc.Put_Line_Output ("Timeout");
      end if;
      exit Main when Sig;
    end loop;
    if not Server then
      Client_Send;
    end if;
  end loop Main;

  if Accept_Soc.Is_Open then
    if Event_Mng.Fd_Callback_Set (Accept_Fd, True) then
      Event_Mng.Del_Fd_Callback (Accept_Fd, True);
    end if;
    Accept_Soc.Close;
  end if;

  if Soc.Is_Open then
    if Event_Mng.Fd_Callback_Set (Fd, True) then
      Event_Mng.Del_Fd_Callback (Fd, True);
    end if;
    Soc.Close;
  end if;

exception
  when Arg_Error =>
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                   & " -c <server_host> | -s");

  when Error : others =>
    Basic_Proc.Put_Line_Output ("Exception: " & Ada.Exceptions.Exception_Name (Error));
end T_Tcp;

