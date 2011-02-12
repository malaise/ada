with Ada.Exceptions, Ada.Text_Io, Ada.Calendar;
with As.U, Argument, Basic_Proc, Date_Image, Normal, Int_Image,
     Upper_Str, String_Mng, Text_Line, Sys_Calls,
     Socket, Event_Mng, Ip_Addr, Tcp_Util, Timers;

procedure Udp_Spy is

  -- Argument error
  Arg_Error : exception;

  -- Parsed Host and Port
  Host_Name : As.U.Asu_Us;
  Host : Tcp_Util.Remote_Host;
  Port_Name : As.U.Asu_Us;
  Port : Tcp_Util.Remote_Port;
  Port_Num : Socket.Port_Num;

  -- Kind of dump
  type Dump_Kind_List is (Header, Short, Full, Binary);
  Dump_Mode : Dump_Kind_List;
  Short_Data_Len : constant Natural := 64;
  Put_Host_Name : Boolean;

  -- Optional interface
  Iface : Tcp_Util.Remote_Host;
  -- For parsing arguments
  Nb_Options : Natural;

  -- Stdout through Text_Line
  File : Text_Line.File_Type;

  use type Socket.Host_Id;
  use type Tcp_Util.Remote_Port_List, Tcp_Util.Remote_Host_List;

  package Byte_Io is new Ada.Text_Io.Integer_Io (Socket.Byte);

  function Port_Image is new Int_Image (Socket.Port_Num);
  function Inte_Image is new Int_Image (Integer);

  -- Put host (name or IP @) that has send last message on socket
  function Dest_Image (S : Socket.Socket_Dscr; Allow_Name : Boolean)
           return String is
    Host_Id : constant Socket.Host_Id := S.Get_Destination_Host;
  begin
    if Put_Host_Name and then Allow_Name then
      -- Try to get host name and put it
      begin
        return Socket.Host_Name_Of (Host_Id)
           & ":" & Port_Image (S.Get_Destination_Port);
      exception
        when Socket.Soc_Name_Not_Found =>
          -- Will put IP address instead of host name
          null;
      end;
    end if;
    -- Put host IP address
    return Ip_Addr.Image (Socket.Id2Addr (Host_Id))
         & ":" & Port_Image (S.Get_Destination_Port);
  end Dest_Image;

  -- Current date image
  function Curr_Date_Image return String is
    Date : String := Date_Image (Ada.Calendar.Clock);
  begin
    -- Date is "YYyy/Mm/Dd Hh:Mm:Ss.mmm"
    Date (11) := '/';
    return "At " & Date (9 .. Date'Last);
  end Curr_Date_Image;

  -- The socket and its fd
  Soc : Socket.Socket_Dscr;
  Fd  : Event_Mng.File_Desc := 0;

  -- Signal received
  Signal : Boolean := False;
  -- Signal callback
  procedure Signal_Cb is
  begin
    Signal := True;
  end Signal_Cb;

  -- Timeout expired
  Timeout : Boolean := False;
  Tid : Timers.Timer_Id;
  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Timeout := True;
    return True;
  end Timer_Cb;

  -- Exit codes
  Error_Code : constant := 2;
  Packet_Received_Code : constant := 1;
  No_Packet_Code : constant := 0;

  -- Data received on socket
  type Data_Type is array (1 .. 9999) of Socket.Byte;
  Data : Data_Type;
  procedure My_Receive is new Socket.Receive (Data_Type);
  Packet_Received : Boolean := False;

  -- Dump all or some of the Data
  -- "  xx xx xx xx xx xx xx xx   xx xx xx xx xx xx xx xx   ................"
  procedure Dump_Data (Len : in Natural) is
    -- "16#xx#"
    Byte_Str : String (1 .. 6);
    Hexa_Str : String (1 .. 49) := (others => ' ');
    Char_Str : String (1 .. 16) := (others => ' ');
    function Hexa_Index (I : Positive) return Positive is
      T : constant Natural := (I - 1) rem 16;
    begin
      if T < 8 then
        return T * 3 + 1;
      else
        return T * 3 + 3;
      end if;
    end Hexa_Index;
    O : Positive;
    B : Socket.Byte;
    use type Socket.Byte;
  begin
    for I in 1 .. Len loop
      B := Data(I);
      -- Hexa dump
      O := Hexa_Index (I);
      Byte_Io.Put (Byte_Str, B, 16);
      if B < 16 then
        -- " 16#e#" -> "16#0e#"
        Byte_Str := Byte_Str(2 .. 4) & '0' & Byte_Str(5 .. 6);
      end if;
      Hexa_Str(O .. O + 1) := Upper_Str (Byte_Str(4 .. 5));
      -- Ascii dump
      O := (I - 1) rem 16 + 1;
      if B < 32 or else B > 126 then
        Char_Str(O) := '.';
      else
        Char_Str(O) := Character'Val(B);
      end if;
      -- Flush
      if O = 16 or else I = Len then
        Text_Line.Put_Line (File, "  " & Hexa_Str & "   " & Char_Str);
        Hexa_Str := (others => ' ');
        Char_Str := (others => ' ');
      end if;
    end loop;
  end Dump_Data;

  -- Callback on socket reception
  function Call_Back (F : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is
    pragma Unreferenced (Read);
    use type Event_Mng.File_Desc;
    Data_Len : Natural;
  begin
    if F /= Fd then
      Basic_Proc.Put_Line_Error ("Error: read callback on unknown fd.");
      raise Program_Error;
    end if;
    My_Receive (Soc, Data, Data_Len,
                Set_For_Reply => True,
                Set_Ipm_Iface => False);
    Packet_Received := True;
    -- Put header
    if Dump_Mode /= Binary then
      Text_Line.Put (File, Curr_Date_Image);
      Text_Line.Put (File, " got " & Normal (Data_Len, 4) & " bytes from ");
      Text_Line.Put_Line (File, Dest_Image (Soc, True));
    end if;
    -- Put data
    if Dump_Mode = Header then
      null;
    elsif Dump_Mode = Binary then
      -- Binary dump
      for I in 1 .. Data_Len loop
        Text_Line.Put (File, Character'Val (Data(I)) & "");
      end loop;
    elsif Dump_Mode = Short and then Data_Len > Short_Data_Len then
      Dump_Data (Short_Data_Len);
    else
      Dump_Data (Data_Len);
    end if;
    Text_Line.Flush (File);
    return False;
  exception
    when Socket.Soc_Conn_Lost =>
      return True;
  end Call_Back;

begin

  -- Parse arguments
  Nb_Options := 0;
  if Argument.Get_Nbre_Arg = 0
  or else Argument.Get_Nbre_Arg > 4 then
    raise Arg_Error;
  end if;

  -- Parse Dump mode
  -- Default
  Dump_Mode := Short;
  begin
    if Argument.Get_Parameter (1, "d") = "h" then
      Dump_Mode := Header;
      Nb_Options := Nb_Options + 1;
    elsif Argument.Get_Parameter (1, "d") = "s" then
      Dump_Mode := Short;
      Nb_Options := Nb_Options + 1;
    elsif Argument.Get_Parameter (1, "d") = "f" then
      Dump_Mode := Full;
      Nb_Options := Nb_Options + 1;
    elsif Argument.Get_Parameter (1, "d") = "b" then
      Dump_Mode := Binary;
      Nb_Options := Nb_Options + 1;
    else
      raise Arg_Error;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      null;
  end;

  -- Parse Put_Host_Name option
  Put_Host_Name := False;
  begin
    if Argument.Get_Parameter (1, "h") = "n" then
      Put_Host_Name := True;
      Nb_Options := Nb_Options + 1;
    else
      raise Arg_Error;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      null;
  end;

  -- Parse Interface
  -- Default
  begin
    Iface := Ip_Addr.Parse (Argument.Get_Parameter (1, "i"));
    Nb_Options := Nb_Options + 1;
  exception
    when Argument.Argument_Not_Found =>
      Iface := (Kind => Tcp_Util.Host_Id_Spec, Id => Socket.No_Host);
    when others =>
      raise Arg_Error;
  end;

  -- Parse Timeout
  if Argument.Is_Set (1, "t") then
    Nb_Options := Nb_Options + 1;
    begin
      Tid.Create (
          (Clock => null,
           Period => Timers.No_Period,
           Delay_Kind => Timers.Delay_Sec,
           Delay_Seconds => Duration'Value (Argument.Get_Parameter (1, "t"))),
          Callback => Timer_Cb'Unrestricted_Access);
    exception
      when others =>
        raise Arg_Error;
    end;
  end if;

  -- No other options are supported
  --  only one extra arg, the address, which is parsed here after
  if Argument.Get_Nbre_Arg /= Nb_Options + 1 then
    raise Arg_Error;
  end if;

  -- Parse address <Lan>:<Port>
  declare
    Address : constant String
            := Argument.Get_Parameter (Param_Key => Argument.Not_Key);
    Index : Natural;
  begin
    Index := String_Mng.Locate (Address, ":");
    Host_Name := As.U.Tus (Address(1 .. Index - 1));
    Port_Name := As.U.Tus (Address(Index + 1 .. Address'Last));
  exception
    when others =>
      raise Arg_Error;
  end;

  -- Open output flow (stdout)
  begin
    Text_Line.Open (File, Text_Line.Out_File, Sys_Calls.Stdout);
  exception
    when others =>
      Basic_Proc.Put_Line_Error ("Error: cannot open output flow.");
      raise;
  end;

  -- Create socket, add socket and sigterm callbacks
  Soc.Open (Socket.Udp);
  Fd := Soc.Get_Fd;
  Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  -- Set interface
  if Iface.Kind = Tcp_Util.Host_Name_Spec then
    -- Set host id
    begin
      Iface := (
          Kind => Tcp_Util.Host_Id_Spec,
          Id => Socket.Host_Id_Of (Iface.Name.Image));
    exception
      when Socket.Soc_Name_Not_Found =>
        Basic_Proc.Put_Line_Error ("Error: Unknown interface name "
                                 & Iface.Name.Image);
      raise;
    end;
  end if;
  if Iface.Id /= Socket.No_Host then
    Soc.Set_Reception_Ipm_Interface (Iface.Id);
  end if;

  -- Set port num
  Port := Ip_Addr.Parse (Port_Name.Image);
  if Port.Kind = Tcp_Util.Port_Name_Spec then
    begin
      Port_Num := Socket.Port_Num_Of (Port.Name.Image, Socket.Udp);
    exception
      when Socket.Soc_Name_Not_Found =>
        Basic_Proc.Put_Line_Error ("Error: Unknown port name "
                                 & Port.Name.Image);
        raise;
    end;
  else
    Port_Num := Port.Num;
  end if;

  -- Link to Lan name or num
  Host := Ip_Addr.Parse (Host_Name.Image);
  -- See if Server is Id or Name, if it is a Host or LAN name
  if Host.Kind = Tcp_Util.Host_Name_Spec then
    begin
      Soc.Set_Destination_Name_And_Port (True, Host.Name.Image, Port_Num);
    exception
      when Socket.Soc_Name_Not_Found =>
        Basic_Proc.Put_Line_Error ("Error: Unknown LAN name "
                                 & Host.Name.Image);
        raise;
    end;
  else
    Soc.Set_Destination_Host_And_Port (Host.Id, Port_Num);
  end if;
  Soc.Link_Port (Port_Num);

  -- Put "Ready on..." end-of-init message
  Basic_Proc.Put_Error (Curr_Date_Image & " listening on ");
  Basic_Proc.Put_Error (Dest_Image (Soc, False));
  if Iface.Id /= Socket.No_Host then
    Basic_Proc.Put_Error (" interface "
                        & Ip_Addr.Image(Socket.Id2Addr(Iface.Id)));
  end if;
  Basic_Proc.Put_Error (" for ");
  case Dump_Mode is
    when Header =>
      Basic_Proc.Put_Error ("header");
    when Short =>
      Basic_Proc.Put_Error ("short");
    when Full =>
      Basic_Proc.Put_Error ("full");
    when Binary =>
      Basic_Proc.Put_Error ("binary");
  end case;
  Basic_Proc.Put_Line_Error (" dumps.");

  -- Main loop until sigterm/sigint or timeout
  loop
    Event_Mng.Wait (-1);
    exit when Signal or else Timeout;
  end loop;

  -- Close
  Text_Line.Close (File);
  Tid.Delete_If_Exists;
  if Event_Mng.Fd_Callback_Set (Fd, True) then
    Event_Mng.Del_Fd_Callback (Fd, True);
    Soc.Close;
  end if;

  -- Put cause of exit
  if Signal then
    Basic_Proc.Put_Line_Error (Curr_Date_Image & " stopped.");
  elsif Timeout then
    Basic_Proc.Put_Line_Error (Curr_Date_Image & " timed out.");
  else
    Basic_Proc.Put_Line_Error (Curr_Date_Image & " aborted?");
  end if;

  -- Set exit code v.s. at least a packet has been received
  if Packet_Received then
    Basic_Proc.Set_Exit_Code (Packet_Received_Code);
  else
    Basic_Proc.Set_Exit_Code (No_Packet_Code);
  end if;

exception
  when Arg_Error =>
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " <address> [ <interface> ] [ <dump_mode> ] [ <host_name> ] [ <timeout> ]");
    Basic_Proc.Put_Line_Error ("  <address>   ::= <lan>:<port>");
    Basic_Proc.Put_Line_Error ("  <lan>       ::= <lan_name_or_num> ");
    Basic_Proc.Put_Line_Error ("  <port>      ::= <port_name_or_num>");
    Basic_Proc.Put_Line_Error ("  <interface> ::= -i<host_name_or_num>");
    Basic_Proc.Put_Line_Error ("  <dump_mode> ::= -dh | -ds | -df | -db");
    Basic_Proc.Put_Line_Error ("  <host_name> ::= -hn");
    Basic_Proc.Put_Line_Error ("  <timeout> ::= -t<duration>");
    Basic_Proc.Put_Line_Error ("Dump received headers, short ("
      & Inte_Image(Short_Data_Len) & " Bytes, default), full or binary,");
    Basic_Proc.Put_Line_Error ("  put sending hosts IP address or name.");
    Basic_Proc.Set_Exit_Code (Error_Code);
  when Error : others =>
    Basic_Proc.Put_Line_Error ("Exception: "
        & Ada.Exceptions.Exception_Name (Error));
    Basic_Proc.Set_Exit_Code (Error_Code);
end Udp_Spy;

