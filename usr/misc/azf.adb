-- Compress or uncompress stdin to stdout
with Basic_Proc, Sys_Calls, Argument, Argument_Parser, C_Types, Bit_Ops,
     As.U, Images, Lzf, Trace.Loggers;
procedure Azf is

  -- Max buffer size in Mega Bytes
  Max_Buffer_Size : constant := 1024;

  Logger : Trace.Loggers.Logger;

  procedure Help is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        &  " [ -s <buffer_size> | -H ] -c | -d | -h");
    Basic_Proc.Put_Line_Error (" -c : Compress stdin to stdout");
    Basic_Proc.Put_Line_Error (" -d : Uncompress stdin to stdout");
    Basic_Proc.Put_Line_Error (" -s : Set buffer size in Mega Bytes (max "
                             & Images.Integer_Image (Max_Buffer_Size) & ")");
    Basic_Proc.Put_Line_Error (" -H : Use headers (and buffers of 64 kB)");
    Basic_Proc.Put_Line_Error (" -h : Display this help");
  end Help;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Help;
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Argument parsing
  Keys : constant Argument_Parser.The_Keys_Type := (
    01 => (False, 'c', As.U.Tus ("compress"),    False),
    02 => (False, 'd', As.U.Tus ("decompress"),  False),
    03 => (True,  's', As.U.Tus ("buffer_size"), False, True, As.U.Tus ("MB")),
    04 => (False, 'H', As.U.Tus ("headers"),     False),
    05 => (False, 'h', As.U.Tus ("help"),        False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Do we compress
  Compress : Boolean;

  -- Header mode
  Header_Mode : Boolean := False;

  -- Buffer size
  Buffer_Size : Positive := 1;

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
      Error ("Buffer size too large");
      return;
    end if;
  exception
    when others =>
      Error ("Invaid buffer size");
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
  if Arg_Dscr.Is_Set (5) then
    Help;
    return;
  end if;
  -- Arg
  if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 0 then
    Error ("Unexpected argument");
    return;
  end if;
  -- (un)compress
  if (Arg_Dscr.Is_Set (1) and then Arg_Dscr.Is_Set (2))
  or else (not Arg_Dscr.Is_Set (1) and then not Arg_Dscr.Is_Set (2)) then
    Error ("Expecting either compress or decompress");
    return;
  end if;
  Compress := Arg_Dscr.Is_Set (1);
  -- Buffer size or header
  if Arg_Dscr.Is_Set (3) and then Arg_Dscr.Is_Set (4) then
    Error ("Headers is exclusive with buffer size");
    return;
  end if;
  if Arg_Dscr.Is_Set (3) then
    Parse_Size (Arg_Dscr.Get_Option (3));
  elsif Arg_Dscr.Is_Set (4) then
    Header_Mode := True;
  end if;

  -- Create buffers
  Inb := new Lzf.Byte_Array(1 .. Buffer_Size * Buffer_Unit);
  Outb := new Lzf.Byte_Array(1 .. Buffer_Size * Buffer_Unit);

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
      Lzf.Compress (Inb(1 .. Inl), Outb.all, Outl);
      Logger.Log_Debug ("Compressed into " & Outl'Img & " bytes");
    else
      Lzf.Uncompress (Inb(1..Inl), Outb.all, Outl);
      Logger.Log_Debug ("Uncompressed into " & Outl'Img & " bytes");
    end if;

    -- Write output
    if Outl > 0 then
      Write (Outb.all, Outl);
    end if;
    return;
  end if;

  -- Header mode
  if Compress then
    -- Prepare header
    Header(1) := Lzf.Byte (Character'Pos ('Z'));
    Header(2) := Lzf.Byte (Character'Pos ('V'));
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
      or else Header(1) /= Lzf.Byte (Character'Pos ('Z'))
      or else Header(2) /= Lzf.Byte (Character'Pos ('V')) then
        Error ("Invalid header");
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
          Outl := Read (Outb(2 .. Outb'Last), Outl - 1);
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
          Error ("Unexpected uncompressed length");
          return;
        end if;
        Logger.Log_Debug ("Uncompressed into " & Outl'Img & " bytes");
      else
        Error ("Invalid header kind");
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

