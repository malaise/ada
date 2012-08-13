-- Send ping-pong between hosts
-- By default send a ping each period and replies to ping
-- On option only sends pings or only replies pong
with Ada.Exceptions, Ada.Calendar;
with As.U,
     Argument, Argument_Parser,
     Basic_Proc,
     Date_Image, Dur_Image, String_Mng, Mixed_Str,
     Socket, Tcp_Util, Ip_Addr, Event_Mng, Timers,
     Hashed_List.Unique;
procedure Pingpong is

  -- Argument parsing
  Keys : constant Argument_Parser.The_Keys_Type := (
    1 => (False, 'h', As.U.Tus ("help"), False),
    2 => (True,  'i', As.U.Tus ("interface"), False, True, As.U.Asu_Null),
    3 => (True,  'p', As.U.Tus ("period"), False, True, As.U.Asu_Null),
    4 => (False, 'r', As.U.Tus ("reply"), False),
    5 => (False, 's', As.U.Tus ("send"), False),
    6 => (False, 'a', As.U.Tus ("average"), False),
    7 => (False, 'd', As.U.Tus ("debug"), False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  Soc : Socket.Socket_Dscr;
  Fd  : Event_Mng.File_Desc := 0;
  Use_Iface : Boolean;
  Iface : Tcp_Util.Remote_Host;
  One_Shot : Boolean;
  Period : Timers.Period_Range;
  Send_Mode : Boolean;
  Average : Boolean;
  Debug : Boolean;

  -- Usage and Error
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " [ { <option> } ] <mode> <lan>:<port>");
    Basic_Proc.Put_Line_Error ("   or: " & Argument.Get_Program_Name
      & " " & Argument_Parser.Image (Keys(1)));
    Basic_Proc.Put_Line_Error (
        "  <option>    ::= <interface> | <period>");
    Basic_Proc.Put_Line_Error (
        "  <interface> ::= " & Argument_Parser.Image (Keys(2)));
    Basic_Proc.Put_Line_Error (
        "  <period>    ::= " & Argument_Parser.Image (Keys(3)));
    Basic_Proc.Put_Line_Error (
        "  <mode>      ::= " & Argument_Parser.Image (Keys(4))
      & " | " & Argument_Parser.Image (Keys(5)));
    Basic_Proc.Put_Line_Error (
        "  <average>    ::= " & Argument_Parser.Image (Keys(6)));
    Basic_Proc.Put_Line_Error (
        "  <debug>      ::= " & Argument_Parser.Image (Keys(7)));
    Basic_Proc.Put_Line_Error (
        "  <lan>        ::= <ipm_lan_name> | <ipm_lan_address>");
    Basic_Proc.Put_Line_Error (
        "  <port>       ::= <udp_port_name> | <port_num>");
  end Usage;

  Abort_Error : exception;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Abort_Error;
  end Error;

  procedure Put (Message : in String) is
  begin
    Basic_Proc.Put_Line_Output ("PingPong at "
      & Date_Image (Ada.Calendar.Clock) & ": " & Message);
  end Put;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Put ("Aborted.");
  end Signal_Cb;

  -- Message exchanged
  -- Ping request is Ping Ping_Sender Ping_Stamp
  -- Pong reply   is Pong Pong_Sender Ping_Stamp Pong_Stamp
  subtype Host_Str is String (1 .. 255);
  type Ping_Kind_List is (Ping, Pong);
  type Message_Type is record
    Kind : Ping_Kind_List;
    Host_Id : Socket.Host_Id;
    Host_Name : Host_Str;
    Host_Name_Len : Positive;
    Ping_Stamp, Pong_Stamp : Ada.Calendar.Time;
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

  -- Average delta time info on each remote host
  type Info_Type is record
    Host : Message_Type;
    Nb_Samples : Natural;
    Average_Delta : Duration;
  end record;
  type Info_Access is access all Info_Type;
  procedure Set (To : out Info_Type; Val : in Info_Type) is
  begin
    To := Val;
  end Set;
  function "=" (Current : Info_Type; Criteria : Info_Type) return Boolean is
  begin
    return Current.Host.Host_Name_Len = Criteria.Host.Host_Name_Len
    and then Current.Host.Host_Name(1 .. Current.Host.Host_Name_Len)
           = Criteria.Host.Host_Name(1 .. Criteria.Host.Host_Name_Len);
  end "=";
  function Key_Image (Element : Info_Type) return String is
  begin
    return Element.Host.Host_Name(1 .. Element.Host.Host_Name_Len);
  end Key_Image;
  package H_Info_List_Mng is new Hashed_List (Info_Type, Info_Access, Set,
                                            "=", Key_Image);
  package Info_List_Mng is new H_Info_List_Mng.Unique;
  Info_List : Info_List_Mng.Unique_List_Type;

  -- Message reception (ping or pong)
  function Call_Back (F : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is
    pragma Unreferenced (Read);
    use type Event_Mng.File_Desc, Socket.Host_Id, Ada.Calendar.Time;
    Message : Message_Type;
    Message_Len : Natural;
    End_Stamp : Ada.Calendar.Time;
    Delta_Time : Duration;
    Found : Boolean;
    Info : Info_Type;
    Info_Acc : Info_Access;
    Txt : As.U.Asu_Us;
  begin
    if F /= Fd then
      Put ("Not same Fd");
      raise Program_Error;
    end if;
    if Debug then
      Put ("Receiving");
    end if;
    begin
      My_Receive (Soc, Message, Message_Len, Set_For_Reply => not Send_Mode,
                  Set_Ipm_Iface => Use_Iface);
    exception
      when Socket.Soc_Conn_Lost =>
        Put ("Receives disconnection");
        return False;
    end;
    if Message.Host_Id = Local_Host_Id then
      -- No answer nor log of our own ping or pong
     return False;
    end if;
    if Debug then
      Put ("Receives: " & Mixed_Str (Message.Kind'Img) & " from "
         & Message.Host_Name (1 .. Message.Host_Name_Len));
    end if;
    if Message.Kind = Ping then
      if not Send_Mode then
        -- Reply mode: answer to Ping if not only Send_mode
        if Debug then
          Put ("Sends Pong");
        end if;
        Message.Kind := Pong;
        Fill_Host (Message);
        Message.Pong_Stamp := Ada.Calendar.Clock;
        My_Send (Soc, Message);
      end if;
      return False;
    end if;
    -- Handle Pong
    End_Stamp := Ada.Calendar.Clock;
    -- Compute delta Pong - (Ping + End) / 2
    Delta_Time := ( (Message.Pong_Stamp - Message.Ping_Stamp)
                  + (Message.Pong_Stamp - End_Stamp) ) / 2.0;
    if Average then
      -- Compute average
      Info.Host := Message;
      Info_List.Search (Info, Found);
      if not Found then
        -- First insertion
        Info.Nb_Samples := 1;
        Info.Average_Delta := Delta_Time;
        Info_List.Insert (Info);
      else
        Info_List.Get_Access_Current (Info_Acc);
        -- Average with previous value
        Info_Acc.Average_Delta :=
          (Info_Acc.Average_Delta * Info.Nb_Samples + Delta_Time)
          / Duration (Info_Acc.Nb_Samples + 1);
        Info_Acc.Nb_Samples := Info_Acc.Nb_Samples + 1;
        Delta_Time := Info_Acc.Average_Delta;
      end if;
      Txt := As.U.Tus ("Average delta");
    else
      Txt := As.U.Tus ("Current delta");
    end if;
    Put (Txt.Image & " of " & Message.Host_Name (1 .. Message.Host_Name_Len)
         & " is " & Dur_Image (Delta_Time, 3, True));
    return False;
  end Call_Back;

  procedure Send_Ping is
    Message : Message_Type;
  begin
    if Debug then
      Put ("Sends Ping");
    end if;
    Message.Kind := Ping;
    Fill_Host (Message);
    Message.Ping_Stamp := Ada.Calendar.Clock;
    My_Send (Soc, Message);
  end Send_Ping;

  use type Tcp_Util.Remote_Host_List;

begin

  -- Parse arguments
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error (Arg_Dscr.Get_Error);
  end if;

  -- Help
  if Arg_Dscr.Is_Set (1) then
    Usage;
    return;
  end if;

  -- Interface
  if Arg_Dscr.Is_Set (2) then
    begin
      Iface := Ip_Addr.Parse (Arg_Dscr.Get_Option (2, 1));
    exception
      when others =>
        Error ("Invalid interface specification");
    end;
    Use_Iface := True;
  else
    -- Default interface
    Iface := (Kind => Tcp_Util.Host_Id_Spec, Id => Socket.No_Host);
    Use_Iface := False;
  end if;

  -- Period
  if Arg_Dscr.Is_Set (3) then
    begin
      Period := Timers.Period_Range'Value (Arg_Dscr.Get_Option (3, 1));
    exception
      when others =>
        Error ("Invalid period specification");
    end;
  else
    -- No period => single shot
    Period := 0.0;
  end if;

  -- Mode
  Send_Mode := True;
  if Arg_Dscr.Is_Set (4) then
    if Arg_Dscr.Is_Set (5) then
      Error ("Send and reply modes are mutually exclusive");
    end if;
    -- Only receive
    Send_Mode := False;
    if Arg_Dscr.Is_Set (3) then
      Error ("Reply mode and period options are mutually exclusive");
    end if;
  elsif Arg_Dscr.Is_Set (5) then
    -- Only send
    Send_Mode := True;
  else
    Error ("Send or reply mode is required");
  end if;

  -- Single shot
  if Period /= 0.0 then
    One_Shot := False;
  else
    One_Shot := Send_Mode;
    Period := 1.0;
  end if;

  -- Average mode
  Average := Arg_Dscr.Is_Set (6);
  if Average and then not Send_Mode then
    Error ("Average requires sending");
  end if;
  if Average and then One_Shot then
    Error ("Average requires non null period");
  end if;

  Debug := Arg_Dscr.Is_Set (7);

  -- No other options are supported
  --  only one extra arg, the address that is parsed here after
  -- Must be after options
  if Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    Error ("LAN and port must appear after options");
  end if;
  if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 1 then
    Error ("LAN and port must be defined once and only once");
  end if;

  -- Set interface from host
  if Iface.Kind = Tcp_Util.Host_Name_Spec then
    -- Set host id
    begin
      Iface := (
          Kind => Tcp_Util.Host_Id_Spec,
          Id => Socket.Host_Id_Of (Iface.Name.Image));
    exception
      when Socket.Soc_Name_Not_Found =>
        Error("Unknown interface name " & Iface.Name.Image);
      raise;
    end;
  end if;

  -- Create socket, add callback
  Soc.Open (Socket.Udp);
  Fd := Soc.Get_Fd;
  Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  -- Ipm address and port
  declare
    Addr : constant String
         := Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index);
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
        Port_Num := Socket.Port_Num_Of (Port.Name.Image, Socket.Udp);
      exception
        when Socket.Soc_Name_Not_Found =>
          Error ("Unknown port name " & Port.Name.Image);
          raise;
      end;
    else
      Port_Num := Port.Num;
    end if;

    -- Set interface
    if Use_Iface then
      if Send_Mode then
        Soc.Set_Sending_Ipm_Interface (Iface.Id);
      else
        Soc.Set_Reception_Ipm_Interface (Iface.Id);
      end if;
    end if;

    -- Always set dest
    if Lan.Kind = Tcp_Util.Host_Name_Spec then
      begin
        Soc.Set_Destination_Name_And_Port (True, Lan.Name.Image, Port_Num);
      exception
        when Socket.Soc_Name_Not_Found =>
          Error ("Unknown LAN name " & Lan.Name.Image);
          raise;
      end;
    else
      Soc.Set_Destination_Host_And_Port (Lan.Id, Port_Num);
    end if;
    -- Bind
    Soc.Link_Port (Port_Num);
  end;
  Put ("Initialized on " & Local_Host_Name);

  -- Main loop
  loop
    if Send_Mode then
      Send_Ping;
    end if;
    exit when Event_Mng.Wait (Integer (Period * 1000.0))
    or else One_Shot;
  end loop;

  -- Close
  if Event_Mng.Fd_Callback_Set (Fd, True) then
    Event_Mng.Del_Fd_Callback (Fd, True);
    Soc.Close;
  end if;

exception
  when Abort_Error =>
    null;
  when Err:others =>
    Error ("Exception " & Ada.Exceptions.Exception_Name (Err) & " raised.");
end Pingpong;

