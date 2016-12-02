-- Compress or uncompress stdin to stdout
with Basic_Proc, Sys_Calls, Argument, Argument_Parser, C_Types, Bit_Ops,
     As.U, Images, Trace.Loggers, Lzf, Lz4, Snappy;
procedure Azf is

  -- Max buffer size in Mega Bytes
  Max_Buffer_Size : constant := 1024;

  -- Default buffer size in Mega Bytes
  Buffer_Size : Positive := 1;

  Logger : Trace.Loggers.Logger;

  procedure Help is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        &  " [ -s <buffer_size> | -H ] -c | -d | -h");
    Basic_Proc.Put_Line_Error (" -c | --compress              : Compress stdin to stdout");
    Basic_Proc.Put_Line_Error (" -d | --decompress            : Uncompress stdin to stdout");
    Basic_Proc.Put_Line_Error (" -H | --headers               : Use lzf header (and buffer of 64 kB)");
    Basic_Proc.Put_Line_Error (" -s <MB> | --buffer_size=<MB> : Set buffer size in Mega Bytes " &
      "(max=" & Images.Integer_Image (Max_Buffer_Size) &
      ", def=" & Images.Integer_Image (Buffer_Size) & ")");
    Basic_Proc.Put_Line_Error (" --lz4                        : Use lz4 instead of lzf");
    Basic_Proc.Put_Line_Error (" --snappy                     : Use snappy instead of lzf");
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
    04 => (True,  's', As.U.Tus ("buffer_size"), False, True, As.U.Tus ("MB")),
    05 => (False, 'H', As.U.Tus ("headers"),     False),
    06 => (False, Argument_Parser.No_Key_Char, As.U.Tus ("lz4"), False),
    07 => (False, Argument_Parser.No_Key_Char, As.U.Tus ("snappy"), False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Do we compress
  Compress : Boolean;

  -- Header mode
  Header_Mode : Boolean := False;


  -- Use lzf, lz4 or snappy
  type Algo_List is (Lzf_Algo, Lz4_Algo, Snappy_Algo);
  Algo : Algo_List;

  -- Header: "Z V", then either "0 Uh Ul" or "1 Ch Cl Uh Ul"
  --  High and low bytes of uncompressed and compressed length
  Header : Lzf.Byte_Array (1 .. 7);
  -- So max len of a data block is <len on 2 bytes> - 7
  Max_Len_Header : constant := 16#FFFF# - Header'Length;
  -- The minimum block length: header length (5, not-compressed header)
  --                         + 1 data
  Min_Len_Header : constant := 6;

  -- Buffers and lengths
  Buffer_Unit : constant := 1024 * 1024;
  type Buffer_Access is access Lzf.Byte_Array;
  Inb, Outb : Buffer_Access;
  Inl, Outl, Expected: Natural;

  procedure Parse_Size (Str : in String) is
  begin
    Buffer_Size := Positive'Value (Str);
    if Buffer_Size > Max_Buffer_Size then
      Error ("Buffer size too large (Max "
           & Images.Integer_Image (Max_Buffer_Size) & ")");
      return;
    end if;
  exception
    when others =>
      Error ("Invalid buffer size");
      return;
  end Parse_Size;

  -- Read at most N bytes, return the number of bytes read
  function Read (Buffer : Lzf.Byte_Array; N : Positive) return Natural is
    Index : Positive;
    Res, Remain : Natural;
  begin
    Index := Buffer'First;
    Remain := N;
    loop
      -- Read Remain bytes until read returns 0 or buffer full
      Res := Sys_Calls.Read (Sys_Calls.Stdin, Buffer(Index)'Address, Remain);
      exit when Res = 0;
      Index := Index + Res;
      Remain := Remain - Res;
      exit when Remain = 0;
    end loop;
    return N - Remain;
  end Read;

  -- Write N bytes
  procedure Write (Buffer : in Lzf.Byte_Array; N : in Positive) is
    Index : Natural;
    Res, Remain : Natural;
  begin
    Index := Buffer'First;
    Remain := N;
    loop
      -- Write Remain bytes until Remain becomes 0
      Res := Sys_Calls.Write (Sys_Calls.Stdout, Buffer(Index)'Address, Remain);
      Index := Index + Res;
      Remain := Remain - Res;
      exit when Remain = 0;
    end loop;
  end Write;

  use type C_Types.Byte;
begin
  Logger.Init ("Azf");

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
  -- Buffer size or header
  if Arg_Dscr.Is_Set (4) and then Arg_Dscr.Is_Set (5) then
    Error ("Headers is exclusive with buffer size");
    return;
  end if;
  if Arg_Dscr.Is_Set (4) then
    Parse_Size (Arg_Dscr.Get_Option (4));
  elsif Arg_Dscr.Is_Set (5) then
    Header_Mode := True;
  end if;
  -- Header is only for lzf
  if Arg_Dscr.Is_Set (5) and then
     (Arg_Dscr.Is_Set (6) or else Arg_Dscr.Is_Set (7)) then
    Error ("Headers is exclusive with Lz4 and Snappy");
    return;
  end if;
  -- Lz4 or snappy
  if Arg_Dscr.Is_Set (6) and then Arg_Dscr.Is_Set (7) then
    Error ("Lz4 and Snappy are mutually exclusive");
    return;
  end if;
  if Arg_Dscr.Is_Set (6) then
    Algo := Lz4_Algo;
  elsif Arg_Dscr.Is_Set (7) then
    Algo := Snappy_Algo;
  else
    Algo := Lzf_Algo;
  end if;

  -- Create buffers
  Inb := new Lzf.Byte_Array(1 .. Buffer_Size * Buffer_Unit);
  Outb := new Lzf.Byte_Array(1 .. Buffer_Size * Buffer_Unit);

  -- Handle direct mode (no header no split)
  if not Header_Mode then
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
      case Algo is
        when Lzf_Algo =>
          Lzf.Compress (Inb(1 .. Inl), Outb.all, Outl);
        when Lz4_Algo =>
          Lz4.Compress (Lz4.Byte_Array (Inb(1 .. Inl)),
                        Lz4.Byte_Array (Outb.all), Outl);
        when Snappy_Algo =>
          Snappy.Compress (Snappy.Byte_Array (Inb(1 .. Inl)),
                           Snappy.Byte_Array (Outb.all), Outl);
      end case;
      Logger.Log_Debug ("Compressed into " & Outl'Img & " bytes");
    else
      case Algo is
        when Lzf_Algo =>
          Lzf.Uncompress (Inb(1..Inl), Outb.all, Outl);
        when Lz4_Algo =>
          Lz4.Uncompress (Lz4.Byte_Array (Inb(1..Inl)),
                          Lz4.Byte_Array (Outb.all), Outl);
        when Snappy_Algo =>
          Snappy.Uncompress (Snappy.Byte_Array (Inb(1..Inl)),
                             Snappy.Byte_Array (Outb.all), Outl);
      end case;
      Logger.Log_Debug ("Uncompressed into " & Outl'Img & " bytes");
    end if;

    -- Write output
    if Outl > 0 then
      Write (Outb.all, Outl);
    end if;
    return;
  end if;

  -- Header mode, lzf only
  if Compress then
    -- Prepare header
    Header(1) := Character'Pos ('Z');
    Header(2) := Character'Pos ('V');
    -- Read, compress and write blocks
    loop
      Inl := Read (Inb.all, Max_Len_Header);
      Logger.Log_Debug ("Read " & Inl'Img & " bytes");
      exit when Inl = 0;
      Lzf.Compress (Inb(1 .. Inl), Outb.all, Outl);
      Logger.Log_Debug ("Compressed into " & Outl'Img & " bytes");
      if Outl > Inl then
        -- Compression lead to longer output, write input
        Header(3) := 0;
        Header(4) := Lzf.Byte (Bit_Ops.Shr (Inl, 8));
        Header(5) := Lzf.Byte (Bit_Ops."And" (Inl, 16#FF#));
        Write (Header, 5);
        Write (Inb.all, Inl);
        Logger.Log_Debug ("Copied");
      else
        -- Write compressed data
        Header(3) := 1;
        Header(4) := Lzf.Byte (Bit_Ops.Shr (Outl, 8));
        Header(5) := Lzf.Byte (Bit_Ops."And" (Outl, 16#FF#));
        Header(6) := Lzf.Byte (Bit_Ops.Shr (Inl, 8));
        Header(7) := Lzf.Byte (Bit_Ops."And" (Inl, 16#FF#));
        Write (Header, 7);
        Write (Outb.all, Outl);
        Logger.Log_Debug ("Written");
      end if;
    end loop;
  else
    loop
      -- Uncompress, read header (at least the not-compressed header)
      Inl := Read (Header, Min_Len_Header);
      -- Empty input?
      exit when Inl = 0;
      Logger.Log_Debug ("Read " & Inl'Img & " bytes");
      if Inl < Min_Len_Header
      or else Header(1) /= Character'Pos ('Z')
      or else Header(2) /= Character'Pos ('V') then
        Error ("Invalid header read");
        return;
      end if;
      if Header(3) = 0 then
        -- Not compressed
        -- Data len
        Outl := Bit_Ops.Shl (Integer (Header(4)), 8) + Integer (Header(5));
        -- Copy the first byte of data
        Outb(1) := Header(Min_Len_Header);
        if Outl /= 1 then
          -- Read remainig data
          Outl := Read (Outb(2 .. Outb'Last), Outl - 1) + 1;
        end if;
        Logger.Log_Debug ("To copy");
      elsif Header(3) = 1 then
        -- Compressed
        Inl := Bit_Ops.Shl (Integer (Header(4)), 8) + Integer (Header(5));
        -- Read last byte of header and data
        Inl := Read (Inb.all, Inl + 1);
        -- Compute size of output
        Expected := Bit_Ops.Shl (Integer (Header(6)), 8) + Integer (Inb(1));
        -- Inl is the last byte to uncompress
        Lzf.Uncompress (Inb(2 .. Inl), Outb.all, Outl);
        if Outl /= Expected then
          Error ("Unexpected uncompressed length ("
                 & Images.Integer_Image (Outl) & "i.o. "
                 & Images.Integer_Image (Expected));
          return;
        end if;
        Logger.Log_Debug ("Uncompressed into " & Outl'Img & " bytes");
      else
        Error ("Invalid header kind read");
        return;
      end if;
      Write (Outb.all, Outl);
    end loop;
  end if;

exception
  when others =>
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end Azf;

