with Ada.Exceptions, Ada.Text_Io, Ada.Calendar;
with Argument, Basic_Proc, Text_Handler, Date_Image, Normal, Int_Image,
     Upper_Str, String_Mng, Text_Line, Sys_Calls,
     Socket, Event_Mng, Ip_Addr, Tcp_Util;

procedure Udp_Spy is

  Arg_Error : exception;

  Host_Name : Text_Handler.Text (80);
  Host : Tcp_Util.Remote_Host;
  Port_Name : Text_Handler.Text (80);
  Port : Tcp_Util.Remote_Port;
  Port_Num : Socket.Port_Num;
  type Dump_Kind_List is (Header, Short, Full, Binary);
  Dump_Mode : Dump_Kind_List;
  Iface : Tcp_Util.Remote_Host;
  File : Text_Line.File_Type;
  Short_Data_Len : constant Natural := 64;
  use type Socket.Host_Id;
  use type Tcp_Util.Remote_Port_List, Tcp_Util.Remote_Host_List;

  package Byte_Io is new Ada.Text_Io.Integer_Io (Socket.Byte);

  function Byte_Image is new Int_Image (Socket.Byte);
  function Port_Image is new Int_Image (Socket.Port_Num);
  function Inte_Image is new Int_Image (Integer);

  function Dest_Image (S : Socket.Socket_Dscr) return String is
    Ip_Add : constant Socket.Ip_Address
            := Socket.Id2Addr (Socket.Get_Destination_Host (S));
  begin
    return Ip_Addr.Image (Ip_Add)
         & ":" & Port_Image (Socket.Get_Destination_Port (S));
  end Dest_Image;

  -- Current date image
  function Curr_Date_Image return String is
    Date : String := Date_Image (Ada.Calendar.Clock);
  begin
    -- Date is "YYyy/Mm/Dd Hh:Mm:Ss.mmm"
    Date (11) := '/';
    return "At " & Date (9 .. Date'Last);
  end Curr_Date_Image;

  Soc : Socket.Socket_Dscr;
  Fd  : Event_Mng.File_Desc := 0;

  -- Signal received
  Sig : Boolean := False;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Sig := True;
  end Signal_Cb;

  type Data_Type is array (1 .. 9999) of Socket.Byte;
  Data : Data_Type;
  procedure My_Receive is new Socket.Receive (Data_Type);

  -- Dump all or some of the Data
  -- "  xx xx xx xx xx xx xx xx   xx xx xx xx xx xx xx xx   ................"
  procedure Dump_Data (Len : in Natural) is
    -- "16#xx#"
    Byte_Str : String (1 .. 6);
    Hexa_Str : String (1 .. 49) := (others => ' ');
    Char_Str : String (1 .. 16) := (others => ' ');
    function Hexa_Index (I : Positive) return Positive is
      T : Natural := (I - 1) rem 16;
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

  function Call_Back (F : in Event_Mng.File_Desc; Read : in Boolean)
                     return Boolean is
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
    -- Put header
    if Dump_Mode /= Binary then
      Text_Line.Put (File, Curr_Date_Image);
      Text_Line.Put (File, " got " & Normal (Data_Len, 4) & " bytes from ");
      Text_Line.Put_Line (File, Dest_Image (Soc));
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
  if Argument.Get_Nbre_Arg = 0
  or else Argument.Get_Nbre_Arg > 3 then
    raise Arg_Error;
  end if;
  if Argument.Get_Nbre_Arg = 2
  and then not Argument.Is_Set (Param_Key => "d")
  and then not Argument.Is_Set (Param_Key => "i") then
    -- 2 Args, one shall be d or h
    raise Arg_Error;
  elsif Argument.Get_Nbre_Arg = 3
  and then (not Argument.Is_Set (Param_Key => "d")
    or else not Argument.Is_Set (Param_Key => "i") ) then
    -- 3 Args, there shall be d and h
    raise Arg_Error;
  end if;

  -- Parse address <Lan>:<Port>
  declare
    Address : constant String
            := Argument.Get_Parameter (Param_Key => Argument.Not_Key);
    Index : Natural;
  begin
    Index := String_Mng.Locate (Address, ":");
    Text_Handler.Set (Host_Name, Address(1 .. Index - 1));
    Text_Handler.Set (Port_Name, Address(Index + 1 .. Address'Last));
  exception
    when others =>
      raise Arg_Error;
  end;

  -- Dump mode
  -- Default
  begin
    Dump_Mode := Short;
    if Argument.Get_Parameter (1, "d") = "h" then
      Dump_Mode := Header;
    elsif Argument.Get_Parameter (1, "d") = "s" then
      Dump_Mode := Short;
    elsif Argument.Get_Parameter (1, "d") = "f" then
      Dump_Mode := Full;
    elsif Argument.Get_Parameter (1, "d") = "b" then
      Dump_Mode := Binary;
    else
      raise Arg_Error;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      null;
  end;

  -- Interface
  -- Default
  Iface := (Kind => Tcp_Util.Host_Id_Spec, Id => Socket.No_Host);
  begin
    Iface := Ip_Addr.Parse (Argument.Get_Parameter (1, "i"));
  exception
    when Argument.Argument_Not_Found =>
      null;
  end;

  -- Open output flow
  begin
    Text_Line.Open (File, Text_Line.Out_File, Sys_Calls.Stdout);
  exception
    when others =>
      Basic_Proc.Put_Line_Error ("Error: cannot open output flow.");
      raise;
  end;

  -- Create socket, add callback
  Socket.Open (Soc, Socket.Udp);

  Fd := Socket.Fd_Of (Soc);
  Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  -- Set interface
  if Iface.Kind = Tcp_Util.Host_Name_Spec then
    -- Set host id
    begin
      Iface := (
          Kind => Tcp_Util.Host_Id_Spec,
          Id => Socket.Host_Id_Of (Tcp_Util.Name_Of (Iface.Name)));
    exception
      when Socket.Soc_Name_Not_Found =>
        Basic_Proc.Put_Line_Error ("Error: Unknown host name "
                                 & Tcp_Util.Name_Of (Iface.Name));
      raise;
    end;
  end if;
  if Iface.Id /= Socket.No_Host then
    Socket.Set_Reception_Ipm_Interface (Soc, Iface.Id);
  end if;

  -- Set port num
  Port := Ip_Addr.Parse (Text_Handler.Value(Port_Name));
  if Port.Kind = Tcp_Util.Port_Name_Spec then
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

  -- Link to Lan name or num
  Host := Ip_Addr.Parse (Text_Handler.Value (Host_Name));
  -- See if Server is Id or Name, if it is a Host or LAN name
  if Host.Kind = Tcp_Util.Host_Name_Spec then
    begin
      Socket.Set_Destination_Name_And_Port (Soc, True,
               Tcp_Util.Name_Of (Host.Name), Port_Num);
    exception
      when Socket.Soc_Name_Not_Found =>
        Basic_Proc.Put_Line_Error ("Error: Unknown LAN name "
                                 & Tcp_Util.Name_Of (Host.Name));
        raise;
    end;
  else
    Socket.Set_Destination_Host_And_Port (Soc, Host.Id, Port_Num);
  end if;
  Socket.Link_Port (Soc, Port_Num);

  -- Put Ready on...
  Basic_Proc.Put_Error (Curr_Date_Image & " listening on ");
  Basic_Proc.Put_Error (Dest_Image (Soc));
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

  -- Main loop
  loop
    Event_Mng.Wait (-1);
    exit when Sig;
  end loop;

  -- Close
  Text_Line.Close (File);
  if Event_Mng.Fd_Callback_Set (Fd, True) then
    Event_Mng.Del_Fd_Callback (Fd, True);
    Socket.Close (Soc);
  end if;

  Basic_Proc.Put_Line_Error (Curr_Date_Image & " stopped.");

exception
  when Arg_Error =>
    Basic_Proc.Put_Line_Error ("Usage: "
       & Argument.Get_Program_Name & " <address> [ <interface> ] [ <dump_mode> ]");
    Basic_Proc.Put_Line_Error ("  <address>   ::= <lan>:<port>");
    Basic_Proc.Put_Line_Error ("  <lan>       ::= <lan_name_or_num> ");
    Basic_Proc.Put_Line_Error ("  <port>      ::= <port_name_or_num>");
    Basic_Proc.Put_Line_Error ("  <interface> ::= -i<host_name_or_num>");
    Basic_Proc.Put_Line_Error ("  <dump_mode> ::= -dh | -ds | -df | -db");
    Basic_Proc.Put_Line_Error ("Dump received headers, short ("
      & Inte_Image(Short_Data_Len) & " Bytes, default), full or binary.");
    Basic_Proc.Set_Error_Exit_Code;
  when Error : others =>
    Basic_Proc.Put_Line_Error ("Exception: "
        & Ada.Exceptions.Exception_Name (Error));
    Basic_Proc.Set_Error_Exit_Code;
end Udp_Spy;

