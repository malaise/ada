-- Compress or uncompress stdin to stdout
with Basic_Proc, Sys_Calls, Argument, Argument_Parser, C_Types,
     As.U, Images, Trace.Loggers, Lz4L, Long_Longs;
procedure Alz4L is

  subtype Ll_Integer is Long_Longs.Ll_Integer;
  subtype Ll_Natural is Long_Longs.Ll_Natural;
  subtype Ll_Positive is Long_Longs.Ll_Positive;

  -- Max buffer size in Mega Bytes
  Buffer_Unit : constant := 1024 * 1024;
  Max_Buffer_Size : constant := Ll_Integer'Last / Buffer_Unit;

  -- Default buffer size in Mega Bytes
  Buffer_Size : Ll_Positive := 5 * 1024;

  Logger : Trace.Loggers.Logger;

  procedure Help is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        &  " [ -s <buffer_size> ] -c | -d | -h");
    Basic_Proc.Put_Line_Error (" -c | --compress              : Compress stdin to stdout");
    Basic_Proc.Put_Line_Error (" -d | --decompress            : Uncompress stdin to stdout");
    Basic_Proc.Put_Line_Error (" -s <MB> | --buffer_size=<MB> : Buffer size in MB " &
      "(max=" & Images.Llint_Image (Max_Buffer_Size) & ", default=" &
      Images.Llint_Image (Buffer_Size) & ")");
    Basic_Proc.Put_Line_Error (" -h | --help                  : Display this help");
  end Help;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Help;
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Argument parsing
  Keys : constant Argument_Parser.The_Keys_Type := (
    01 => (False, 'h', As.U.Tus ("help"),        False),
    02 => (False, 'c', As.U.Tus ("compress"),    False),
    03 => (False, 'd', As.U.Tus ("decompress"),  False),
    04 => (True,  's', As.U.Tus ("buffer_size"), False, True, As.U.Tus ("MB")));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Do we compress
  Compress : Boolean;

  -- Buffers and lengths
  type Buffer_Access is access Lz4L.Byte_Array;
  Inb, Outb : Buffer_Access;
  Inl, Outl: Ll_Natural;

  procedure Parse_Size (Str : in String) is
  begin
    Buffer_Size := Ll_Positive'Value (Str);
    if Buffer_Size > Max_Buffer_Size then
      Error ("Buffer size too large (Max "
           & Images.Long_Image (Max_Buffer_Size) & ")");
      return;
    end if;
  exception
    when others =>
      Error ("Invalid buffer size");
      return;
  end Parse_Size;

  -- Read at most N bytes, return the number of bytes read
  function Read (Buffer : Lz4L.Byte_Array; N : Ll_Positive) return Ll_Natural is
    Index : Ll_Positive;
    Remain : Ll_Natural;
    Res, To_Read : Natural;
  begin
    Index := Buffer'First;
    Remain := N;
    loop
      if Remain <= Ll_Natural (Natural'Last) then
        To_Read := Natural (Remain);
      else
       To_Read := Natural'Last;
      end if;
      -- Read Remain bytes until read returns 0 or buffer full
      Res := Sys_Calls.Read (Sys_Calls.Stdin, Buffer(Index)'Address, To_Read);
      exit when Res = 0;
      Index := Index + Ll_Natural (Res);
      Remain := Remain - Ll_Natural (Res);
      exit when Remain = 0;
    end loop;
    return N - Remain;
  end Read;

  -- Write N bytes
  procedure Write (Buffer : in Lz4L.Byte_Array; N : in Ll_Positive) is
    Index : Ll_Natural;
    Remain : Ll_Natural;
    Res, To_Write : Natural;
  begin
    Index := Buffer'First;
    Remain := N;
    loop
      if Remain <= Ll_Natural (Natural'Last) then
        To_Write := Natural (Remain);
      else
       To_Write := Natural'Last;
      end if;
      -- Write Remain bytes until Remain becomes 0
      Res := Sys_Calls.Write (Sys_Calls.Stdout, Buffer(Index)'Address, To_Write);
      Index := Index + Ll_Natural (Res);
      Remain := Remain - Ll_Natural (Res);
      exit when Remain = 0;
    end loop;
  end Write;

  use type C_Types.Byte;
begin
  Logger.Init ("Alz4l");

  -- Parse arguments
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error (Arg_Dscr.Get_Error);
    return;
  end if;
  -- Help
  if Arg_Dscr.Is_Set (1) then
    Help;
    return;
  end if;
  -- Arg
  if Arg_Dscr.Is_Set (Argument_Parser.No_Key_Index) then
    Error ("Unexpected argument "
         & Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index));
    return;
  end if;
  -- (un)compress
  if (Arg_Dscr.Is_Set (2) and then Arg_Dscr.Is_Set (3))
  or else (not Arg_Dscr.Is_Set (2) and then not Arg_Dscr.Is_Set (3)) then
    Error ("Expecting either compress or decompress mode");
    return;
  end if;
  Compress := Arg_Dscr.Is_Set (2);
  -- Buffer size
  if Arg_Dscr.Is_Set (4) then
    Parse_Size (Arg_Dscr.Get_Option (4));
  end if;


  -- Create buffers
  Inb := new Lz4L.Byte_Array(1 .. Buffer_Size * Buffer_Unit);
  Outb := new Lz4L.Byte_Array(1 .. Buffer_Size * Buffer_Unit);

  -- Read input
  Inl := Read (Inb.all, Inb'Length);
  Logger.Log_Debug ("Read " & Inl'Img & " bytes");
  if Inl = Inb'Length then
    -- Buffer filled
    Error ("Input buffer too small");
    return;
  end if;

  -- (Un)compress
  if Compress then
    Lz4L.Compress (Inb(1 .. Inl), Outb.all, Outl);
    Logger.Log_Debug ("Compressed into " & Outl'Img & " bytes");
  else
    Lz4L.Uncompress (Inb(1..Inl), Outb.all, Outl);
    Logger.Log_Debug ("Uncompressed into " & Outl'Img & " bytes");
  end if;

  -- Write output
  if Outl > 0 then
    Write (Outb.all, Outl);
  end if;

exception
  when others =>
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end Alz4L;

