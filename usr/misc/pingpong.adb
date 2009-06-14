with Ada.Exceptions, Ada.Calendar, Ada.Text_Io;
with Argument, Socket, Tcp_Util, Ip_Addr, Event_Mng, Date_Image, String_Mng;
procedure Pingpong is
  Arg_Error : exception;
  Soc : Socket.Socket_Dscr;
  Fd  : Event_Mng.File_Desc := 0;

  procedure Put (Message : in String) is
  begin
    Ada.Text_Io.Put_Line (Date_Image (Ada.Calendar.Clock) & " PingPong -> " & Message);
  end Put;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Put ("Aborted.");
  end Signal_Cb;

  -- Ping request is "Ping" "<ping_sender>"
  -- Pong reply   is "Pong" "<pong_sender>"
  subtype Ping_Kind is String (1 ..4);
  Ping : constant Ping_Kind := "Ping";
  Pong : constant Ping_Kind := "Pong";
  subtype Host_Str is String (1 .. 255);
  type Message_Type is record
    Kind : Ping_Kind;
    Host_Id : Socket.Host_Id;
    Host_Name_Len : Natural;
    Host_Name : Host_Str;
  end record;

  procedure My_Send is new Socket.Send (Message_Type);
  procedure My_Receive is new Socket.Receive (Message_Type);

  Local_Host_Id : constant Socket.Host_Id := Socket.Local_Host_Id;
  Local_Host_Name : constant String := Socket.Local_Host_Name;
  procedure Fill_Host (Message : in out Message_Type) is
  begin
    Message.Host_Id := Local_Host_Id;
    Message.Host_Name_Len := Local_Host_Name'Length;
    Message.Host_Name (1 .. Message.Host_Name_Len) := Local_Host_Name;
  end Fill_Host;

  function Call_Back (F : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is
    pragma Unreferenced (Read);
    use type Event_Mng.File_Desc, Socket.Host_Id;
    Message : Message_Type;
    Message_Len : Natural;
  begin
    if F /= Fd then
      Put ("Not same Fd");
      raise Program_Error;
    end if;
    begin
      My_Receive (Soc, Message, Message_Len, Set_For_Reply => False);
    exception
      when Socket.Soc_Conn_Lost =>
        Put ("Receives disconnection");
        return False;
    end;
    if Message.Host_Id = Local_Host_Id then
      -- No answer nor log of our own ping or pong
     return False;
    end if;
    Put ("Receives: " & Message.Kind & " from "
       & Message.Host_Name (1 .. Message.Host_Name_Len));
    if Message.Kind = Pong then
      -- No answer to Pong
      return False;
    end if;
    -- Answer to Ping
    Message.Kind := Pong;
    Fill_Host (Message);
    My_Send (Soc, Message);
    return False;
  end Call_Back;

  procedure Send_Ping is
    Message : Message_Type;
  begin
    Message.Kind := Ping;
    Fill_Host (Message);
    Put ("Sends Ping");
    My_Send (Soc, Message);
  end Send_Ping;

  Period : Positive := 1;

begin
  if Argument.Get_Nbre_Arg = 2 then
    Period := Positive'Value (Argument.Get_Parameter (2));
  elsif Argument.Get_Nbre_Arg /= 1 then
    raise Arg_Error;
  end if;

  -- Create socket, add callback
  Socket.Open (Soc, Socket.Udp);
  Fd := Socket.Fd_Of (Soc);
  Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  -- Ipm address and port
  declare
    Addr : constant String := Argument.Get_Parameter (1);
    Index : constant Natural := String_Mng.Locate (Addr, ":");
    Lan : constant Tcp_Util.Remote_Host
        := Ip_Addr.Parse (Addr(1 .. Index - 1));
    Port : constant Tcp_Util.Remote_Port
           := Ip_Addr.Parse (Addr(Index + 1 .. Addr'Last));
    use type Tcp_Util.Remote_Port_List, Tcp_Util.Remote_Host_List;
  begin
    if Lan.Kind /= Tcp_Util.Host_Id_Spec
    or else Port.Kind /= Tcp_Util.Port_Num_Spec then
      raise Ip_Addr.Parse_Error;
    end if;
    Socket.Set_Destination_Host_And_Port (Soc, Lan.Id, Port.Num);
    Socket.Link_Port (Soc, Port.Num);
  end;
  Put ("Initialized on " & Local_Host_Name);

  -- Main loop
  loop
    Send_Ping;
    exit when Event_Mng.Wait (Period * 1_000);
  end loop;

  -- Close
  if Event_Mng.Fd_Callback_Set (Fd, True) then
    Event_Mng.Del_Fd_Callback (Fd, True);
    Socket.Close (Soc);
  end if;


exception
  when Error:others =>
    Put ("Exception: " & Ada.Exceptions.Exception_Name (Error) & " raised");
    Put ("Invalid arguments. Usage: " & Argument.Get_Program_Name
     & " <ipm_addr>:<port_num> [ <period_sec> ] ");
end Pingpong;

