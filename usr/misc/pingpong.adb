with Ada.Exceptions, Ada.Calendar;
with Argument, Socket, Tcp_Util, Ip_Addr, Event_Mng, Date_Image, String_Mng,
     Basic_Proc;
procedure Pingpong is
  Arg_Error : exception;
  Soc : Socket.Socket_Dscr;
  Fd  : Event_Mng.File_Desc := 0;
  Nb_Options : Natural;
  Iface : Tcp_Util.Remote_Host;
  Period : Positive;
  Send_Mode, Receive_Mode : Boolean;

  procedure Put (Message : in String) is
  begin
    Basic_Proc.Put_Line_Output (Date_Image (Ada.Calendar.Clock)
                              & " PingPong -> " & Message);
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
    if Send_Mode then
      -- Answer to Ping if not only receive_mode
      Message.Kind := Pong;
      Fill_Host (Message);
      My_Send (Soc, Message);
    end if;
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

  use type Tcp_Util.Remote_Host_List;
begin
  -- Parse arguments
  Nb_Options := 0;

  -- Interface
  begin
    Iface := Ip_Addr.Parse (Argument.Get_Parameter (1, "i"));
    Nb_Options := Nb_Options + 1;
  exception
    when Argument.Argument_Not_Found =>
      Iface := (Kind => Tcp_Util.Host_Id_Spec, Id => Socket.No_Host);
    when others =>
      raise Arg_Error;
  end;

  -- Period
  begin
    Period := Positive'Value (Argument.Get_Parameter (1, "p"));
    Nb_Options := Nb_Options + 1;
  exception
    when Argument.Argument_Not_Found =>
      Period := 1;
  end;

  -- Mode
  Send_Mode := True;
  Receive_Mode := True;
  if Argument.Is_Set (1, "s") then
   if Argument.Get_Parameter (1, "s") /= "" then
     raise Arg_Error;
   end if;
   -- Only send
   Receive_Mode := False;
   Nb_Options := Nb_Options + 1;
  end if;
  if Argument.Is_Set (1, "r") then
   if Argument.Get_Parameter (1, "r") /= "" then
     raise Arg_Error;
   end if;
   if Argument.Is_Set (1, "s") then
     -- Not both
     raise Arg_Error;
   end if;
   -- Only receive
   Send_Mode := False;
   Nb_Options := Nb_Options + 1;
  end if;

  -- No other options are supported
  --  only one extra arg, the address that is parsed here after
  if Argument.Get_Nbre_Arg /= Nb_Options + 1 then
    raise Arg_Error;
  end if;

  -- Set interface from host
  if Iface.Kind = Tcp_Util.Host_Name_Spec then
    -- Set host id
    begin
      Iface := (
          Kind => Tcp_Util.Host_Id_Spec,
          Id => Socket.Host_Id_Of (Tcp_Util.Name_Of (Iface.Name)));
    exception
      when Socket.Soc_Name_Not_Found =>
        Basic_Proc.Put_Line_Error ("Error: Unknown interface name "
                                 & Tcp_Util.Name_Of (Iface.Name));
      raise;
    end;
  end if;

  -- Create socket, add callback
  Socket.Open (Soc, Socket.Udp);
  Fd := Socket.Fd_Of (Soc);
  Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  -- Ipm address and port
  declare
    Addr : constant String
         := Argument.Get_Parameter (Param_Key => Argument.Not_Key);
    Index : constant Natural := String_Mng.Locate (Addr, ":");
    Lan : constant Tcp_Util.Remote_Host
        := Ip_Addr.Parse (Addr(1 .. Index - 1));
    Port : constant Tcp_Util.Remote_Port
           := Ip_Addr.Parse (Addr(Index + 1 .. Addr'Last));
    Port_Num : Socket.Port_Num;
    use type Tcp_Util.Remote_Port_List, Socket.Host_Id;
  begin
    -- Compute port num
    if Port.Kind /= Tcp_Util.Port_Num_Spec then
      begin
        Port_Num := Socket.Port_Num_Of (Tcp_Util.Name_Of (Port.Name),
                                        Socket.Udp);
      exception
        when Socket.Soc_Name_Not_Found =>
          Basic_Proc.Put_Line_Error ("Error: Unknown port name "
                                 & Tcp_Util.Name_Of (Port.Name));
          raise;
      end;
    else
      Port_Num := Port.Num;
    end if;

    -- Set interface
    if Iface.Id /= Socket.No_Host then
      if Send_Mode then
        Socket.Set_Sending_Ipm_Interface (Soc, Iface.Id);
      end if;
      if Receive_Mode then
        Socket.Set_Reception_Ipm_Interface (Soc, Iface.Id);
      end if;
    end if;

    -- Always set dist
    if Lan.Kind = Tcp_Util.Host_Name_Spec then
      begin
        Socket.Set_Destination_Name_And_Port (Soc, True,
                 Tcp_Util.Name_Of (Lan.Name), Port_Num);
      exception
        when Socket.Soc_Name_Not_Found =>
          Basic_Proc.Put_Line_Error ("Error: Unknown LAN name "
                                   & Tcp_Util.Name_Of (Lan.Name));
          raise;
      end;
    else
      Socket.Set_Destination_Host_And_Port (Soc, Lan.Id, Port_Num);
    end if;
    -- Bind if reeive
    if Receive_Mode then
      Socket.Link_Port (Soc, Port_Num);
    end if;
  end;
  Put ("Initialized on " & Local_Host_Name);

  -- Main loop
  loop
    if Send_Mode then
      Send_Ping;
    end if;
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
  & " <ipm_addr>:<port_num> [ -i<interface> ] [ -p<period_sec> ] [ -s | -r ]");
end Pingpong;

