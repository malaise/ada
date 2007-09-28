with Ada.Exceptions, Ada.Text_Io, Ada.Calendar;
with Argument, Basic_Proc, Text_Handler, Date_Image, Normal, Int_Image,
     Upper_Str, Socket, Event_Mng, Ip_Addr, Tcp_Util;

procedure Udp_Spy is

  Arg_Error : exception;

  Host_Name : Text_Handler.Text (80);
  Host : Tcp_Util.Remote_Host;
  Port_Name : Text_Handler.Text (80);
  Port : Tcp_Util.Remote_Port;
  Port_Num : Socket.Port_Num;
  use type Tcp_Util.Remote_Port_List, Tcp_Util.Remote_Host_List;
  type Dump_Kind_List is (Header, Short, Full, Binary);
  Dump_Mode : Dump_Kind_List;
  Short_Data_Len : constant Natural := 64;

  package Byte_Io is new Ada.Text_Io.Integer_Io (Socket.Byte);

  function Byte_Image is new Int_Image (Socket.Byte);
  function Port_Image is new Int_Image (Socket.Port_Num);

  function Dest_Image (S : Socket.Socket_Dscr) return String is
    Ip_Addr : constant Socket.Ip_Address
            := Socket.Id2Addr (Socket.Get_Destination_Host (S));
  begin
    return Byte_Image (Ip_Addr.A) & "."
         & Byte_Image (Ip_Addr.B) & "."
         & Byte_Image (Ip_Addr.C) & "."
         & Byte_Image (Ip_Addr.D) & ":"
         & Port_Image (Socket.Get_Destination_Port (S));
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
    Ada.Text_Io.Put_Line (Curr_Date_Image & " aborted.");
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
        Ada.Text_Io.Put_Line ("  " & Hexa_Str & "   " & Char_Str);
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
      Basic_Proc.Put_Line_Error ("Read callback: unknown fd");
      raise Program_Error;
    end if;
    My_Receive (Soc, Data, Data_Len, Set_For_Reply => True);
    -- Put header
    if Dump_Mode /= Binary then
      Ada.Text_Io.Put (Curr_Date_Image);
      Ada.Text_Io.Put (" got " & Normal (Data_Len, 4) & " bytes from ");
      Ada.Text_Io.Put_Line (Dest_Image (Soc));
    end if;
    -- Put data
    if Dump_Mode = Header then
      null;
    elsif Dump_Mode = Binary then
      -- Binary dump
      for I in 1 .. Data_Len loop
        Ada.Text_Io.Put (Character'Val (Data(I)));
      end loop;
    elsif Dump_Mode = Short and then Data_Len > Short_Data_Len then
      Dump_Data (Short_Data_Len);
    else
      Dump_Data (Data_Len);
    end if;
    return False;
  exception
    when Socket.Soc_Conn_Lost =>
      return True;
  end Call_Back;

begin

  -- Parse arguments
  begin
    if Argument.Get_Nbre_Arg /= 2
    and then Argument.Get_Nbre_Arg /= 3 then
      raise Arg_Error;
    end if;

    Argument.Get_Parameter (Port_Name, 1, "p");
    Argument.Get_Parameter (Host_Name, 1, "l");
    -- Dump mode
    if Argument.Get_Nbre_Arg = 2 then
      -- Default
      Dump_Mode := Short;
    else
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
    end if;
  exception
    when Argument.Argument_Not_Found =>
      raise Arg_Error;
  end;

  -- Create socket, add callback
  Socket.Open (Soc, Socket.Udp);

  Fd := Socket.Fd_Of (Soc);
  Event_Mng.Add_Fd_Callback (Fd, True, Call_Back'Unrestricted_Access);
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  -- Set port num
  Port := Ip_Addr.Parse (Text_Handler.Value(Port_Name));
  if Port.Kind = Tcp_Util.Port_Name_Spec then
    Port_Num := Socket.Port_Num_Of (Tcp_Util.Name_Of (Port.Name),
             Socket.Udp);
  else
    Port_Num := Port.Num;
  end if;

  -- Link to Lan name or num
  Host := Ip_Addr.Parse (Text_Handler.Value (Host_Name));
  -- See if Server is Id or Name, if it is a Host or LAN name
  if Host.Kind = Tcp_Util.Host_Name_Spec then
    Socket.Set_Destination_Name_And_Port (Soc, True,
             Tcp_Util.Name_Of (Host.Name), Port_Num);
  else
    Socket.Set_Destination_Host_And_Port (Soc, Host.Id, Port_Num);
  end if;
  Socket.Link_Port (Soc, Port_Num);

  -- Put Ready on...
  Basic_Proc.Put_Error (Curr_Date_Image & " listening on ");
  Basic_Proc.Put_Error (Dest_Image (Soc));
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

  if Event_Mng.Fd_Callback_Set (Fd, True) then
    Event_Mng.Del_Fd_Callback (Fd, True);
    Socket.Close (Soc);
  end if;

exception
  when Arg_Error =>
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
       & " <port> <lan> [ <dump_mode> ]");
    Basic_Proc.Put_Line_Error ("  <port> ::= -p <port_name_or_num>");
    Basic_Proc.Put_Line_Error ("  <lan>  ::= -l <lan_name_or_num> ");
    Basic_Proc.Put_Line_Error ("  <dump_mode> ::= -dh | -ds | -df | -db");
  when Error : others =>
    Basic_Proc.Put_Line_Error ("Exception: "
        & Ada.Exceptions.Exception_Name (Error));
end Udp_Spy;

