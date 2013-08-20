-- Listen to a TCP port, accepts one connection at a time
--  and put packets received
with Ada.Exceptions, Ada.Calendar;
with Argument, Basic_Proc, Images, Normal,
     Upper_Str, Text_Line, Sys_Calls,
     Socket, Event_Mng, Ip_Addr, Tcp_Util, Timers, Hexa_Utils;

procedure Tcp_Spy is

  Protocol : constant Tcp_Util.Tcp_Protocol_List :=  Socket.Tcp;

  -- Argument error
  Arg_Error : exception;

  -- Parsed Port
  Port : Tcp_Util.Local_Port;
  Port_Num : Tcp_Util.Port_Num;

  -- Kind of dump
  type Dump_Kind_List is (Header, Short, Full, Binary);
  Dump_Mode : Dump_Kind_List;
  Short_Data_Len : constant Natural := 64;
  Put_Host_Name : Boolean;

  -- For parsing arguments
  Nb_Options : Natural;

  -- Stdout through Text_Line
  File : Text_Line.File_Type;

  use type Socket.Host_Id;
  use type Tcp_Util.Remote_Port_List, Tcp_Util.Remote_Host_List;

  function Port_Image is new Images.Int_Image (Socket.Port_Num);
  function Inte_Image is new Images.Int_Image (Integer);

  -- Current date image
  function Curr_Date_Image return String is
    Date : String := Images.Date_Image (Ada.Calendar.Clock);
  begin
    -- Date is "YYyy/Mm/Dd Hh:Mm:Ss.mmm"
    Date (11) := '/';
    return "At " & Date (9 .. Date'Last);
  end Curr_Date_Image;

  -- Log a message
  Put_Date : Boolean := False;
  procedure Log (Msg : in String) is
  begin
    if Put_Date then
      Basic_Proc.Put_Error (Curr_Date_Image & " ");
    end if;
    Basic_Proc.Put_Line_Error (Msg);
  end Log;

  -- The socket and its fd
  Dscr : Socket.Socket_Dscr;

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
  Packet_Received : Boolean := False;

  -- Dump all or some of the Data
  -- "  xx xx xx xx xx xx xx xx   xx xx xx xx xx xx xx xx   ................"
  procedure Dump_Data (Data : in Data_Type; Len : in Natural) is
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
      Hexa_Str(O .. O + 1) := Upper_Str (Hexa_Utils.Image (Integer(B), 2));
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

  function Host_Image (Host_Id : Tcp_Util.Host_Id) return String is
  begin
    if Put_Host_Name then
      begin
        return Socket.Host_Name_Of (Host_Id);
      exception
        when Socket.Soc_Name_Not_Found =>
          null;
      end;
    end if;
    return Ip_Addr.Image (Host_Id);
  end Host_Image;

  -- Acception, reception, and disconnection callbacks
  procedure Acception_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                          Local_Dscr      : in Socket.Socket_Dscr;
                          Remote_Host_Id  : in Tcp_Util.Host_Id;
                          Remote_Port_Num : in Tcp_Util.Port_Num;
                          New_Dscr        : in Socket.Socket_Dscr);
  package Data_Reception is new Tcp_Util.Reception (Data_Type);
  function Reception_Cb (Dscr   : Socket.Socket_Dscr;
                         Data   : Data_Type;
                         Length : Natural) return Boolean;
  procedure Disconnection_Cb (Of_Dscr : in Socket.Socket_Dscr);

  -- Callback on acception
  procedure Acception_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                          Local_Dscr      : in Socket.Socket_Dscr;
                          Remote_Host_Id  : in Tcp_Util.Host_Id;
                          Remote_Port_Num : in Tcp_Util.Port_Num;
                          New_Dscr        : in Socket.Socket_Dscr) is
    pragma Unreferenced (Local_Port_Num, Local_Dscr);
  begin
    -- Allow only one connection at a time
    Tcp_Util.Abort_Accept (Protocol, Port_Num);
    -- Put partner address
    Log ("Accepted connection from " & Host_Image (Remote_Host_Id)
       & " port " & Port_Image (Remote_Port_Num));
    -- Set reception and disconnection callbacks
    Dscr := New_Dscr;
    Data_Reception.Set_Callbacks (Dscr, Reception_Cb'Unrestricted_Access,
                                  Disconnection_Cb'Unrestricted_Access);
  end Acception_Cb;

  -- Callback on disconnection
  procedure Disconnection_Cb (Of_Dscr : in Socket.Socket_Dscr) is
    Dummy_Dscr : Socket.Socket_Dscr;
    use type Socket.Socket_Dscr;
  begin
    if Of_Dscr /= Socket.No_Socket then
      Log ("Disconnection.");
    end if;
    -- Accept again
    Dscr := Socket.No_Socket;
    Tcp_Util.Accept_From (Protocol, Port, Acception_Cb'Unrestricted_Access,
                          Dummy_Dscr, Port_Num);
  end Disconnection_Cb;

  -- Callback on socket reception
  function Reception_Cb (Dscr   : Socket.Socket_Dscr;
                         Data   : Data_Type;
                         Length : Natural) return Boolean is
    pragma Unreferenced (Dscr);
  begin
    Packet_Received := True;
    -- Put header
    if Dump_Mode /= Binary then
      Log ("Got " & Normal (Length, 4) & " bytes");
    end if;
    -- Put data
    if Dump_Mode = Header then
      null;
    elsif Dump_Mode = Binary then
      -- Binary dump
      for I in 1 .. Length loop
        Text_Line.Put (File, Character'Val (Data(I)) & "");
      end loop;
    elsif Dump_Mode = Short and then Length > Short_Data_Len then
      Dump_Data (Data, Short_Data_Len);
    else
      Dump_Data (Data, Length);
    end if;
    Text_Line.Flush (File);
    return False;
  exception
    when Socket.Soc_Conn_Lost =>
      return True;
  end Reception_Cb;

begin

  -- Parse arguments
  Nb_Options := 0;
  if Argument.Get_Nbre_Arg = 0
  or else Argument.Get_Nbre_Arg > 4 then
    raise Arg_Error;
  end if;

  -- Parse Date option
  if Argument.Is_Set (1, "D") then
    Put_Date := True;
    Nb_Options := Nb_Options + 1;
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

  -- Parse <Port>
  begin
    Port := Ip_Addr.Parse (Argument.Get_Parameter (
                               Param_Key => Argument.Not_Key));
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

  -- Set sigterm callbacks
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  -- Start accepting
  Disconnection_Cb (Socket.No_Socket);

  -- Put "Ready on..." end-of-init message
  Log ("Accepting on port " & Port_Image (Port_Num) & " for " &
    (case Dump_Mode is
       when Header => ("header"),
       when Short => ("short"),
       when Full => ("full"),
       when Binary => ("binary")) & " dumps.");

  -- Main loop until sigterm/sigint or timeout
  loop
    Event_Mng.Wait (-1);
    exit when Signal or else Timeout;
  end loop;

  -- Close
  Text_Line.Close (File);
  Tid.Delete_If_Exists;
  if Dscr.Is_Open then
    Data_Reception.Remove_Callbacks (Dscr);
    Dscr.Close;
  else
    Tcp_Util.Abort_Accept (Protocol, Port_Num);
  end if;

  -- Put cause of exit
  if Signal then
    Log ("Stopped.");
  elsif Timeout then
    Log ("Timed out.");
  else
    Log ("Aborted?");
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
      & " <port> [ <date> ] [ <dump_mode> ] [ <host_name> ] [ <timeout> ]");
    Basic_Proc.Put_Line_Error ("  <port>      ::= <port_name_or_num>");
     Basic_Proc.Put_Line_Error("  <date>      ::= -D");
    Basic_Proc.Put_Line_Error ("  <dump_mode> ::= -dh | -ds | -df | -db");
    Basic_Proc.Put_Line_Error ("  <host_name> ::= -hn");
    Basic_Proc.Put_Line_Error ("  <timeout> ::= -t<duration>");
    Basic_Proc.Put_Line_Error ("Dump received headers, short ("
      & Inte_Image(Short_Data_Len) & " Bytes, default), full or binary,");
    Basic_Proc.Put_Line_Error ("  put connecting host IP address or name.");
    Basic_Proc.Set_Exit_Code (Error_Code);
  when Error : others =>
    Basic_Proc.Put_Line_Error ("Exception: "
        & Ada.Exceptions.Exception_Name (Error));
    Basic_Proc.Set_Exit_Code (Error_Code);
end Tcp_Spy;

