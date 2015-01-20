-- This package implements the LZF lossless data compression algorithm. LZF is
-- a Lempel-Ziv variant with byte-aligned output, and optimized for speed.

-- Safety/Use Notes:
--  - The byte arrays should be smaller than 1 GB.
--  - For performance reasons, safety checks on expansion are omitted.
--  - Invalid compressed data can cause exceptions (Constraint_Error)

-- General principles:
-- The LZF compressed format knows literal runs and back-references:
--  - Literal run: directly copy bytes from input to output.
--  - Back-reference: copy previous data of the uncompressed stream to the
--    output stream, with specified offset from location in the uncompressed
--    stream and length. The length is at least 3 bytes.
-- The compression algorithm is
--  - Read next byte and the 2 following bytes, these 3 bytes are the "future",
--    hash them
--  - Read the corresponding hash entry and replace the entry by current input
--    index (so, for a hash value, there is always the most recent index)
--  - If the read entry is set, then it is an index. Compare the three bytes at
--    this reference index with the current "future"
--  - If they match, then check how long of the reference matches current text
--    and append a cross reference (with offset from current and length of
--    match)
--  - If no match, then append the current byte as literal.
--  - The first byte of a literal run is a vontrol byte than denotes a literal
--    run (as opposed to a corss reference) and indicates the length of the run,
--    so:
--  - When appending the first literal of a run (first byte of the input or
--    just after a cross reference) skip a byte for this control byte
--  - When appening a corss reference after a literal run, update the control
--    char of this run (set it to the length of the run)
-- The uncompression algorithm is
--  - Read a byte
--  - If this is the control byte of a literal run, then it indicates the
--    length; copy this length of bytes from input to output
--  - If this is a cross-refernce, then it indicates the offset and length;
--    copy this length from *output* at this offset to output.

-- Format of the compressed data:
-- The first byte of the compressed stream is the control byte.
-- -  For literal runs, the highest three bits of the control byte are not set,
--    then the lower bits are the literal run length - 1, and the next bytes
--    are the literals
-- - For back-references, the highest three bits of the control byte are the
--    back-reference length - 2.
--    If all three bits are set, then the back-reference length - 7 is stored in
--    the next byte. The 5 lower bits of the control byte combined with the last
--    byte form the offset for the back-reference (0 for prev byte).

-- Main differences from the original implementation (in C):
-- - The hash table is initialized with -1, which avoids comparisons when
--   the hashing leads to an empty cell. If this raises an issue, the hash
--   can be initialised with 0, or even not initialised at all, but in both
--   cases the test of match "Ref >= 0" must become strict ("Ref > 0").
-- - The most upper byte of the future is always masked when shifting, so that
--   the match always applies to the next 3 bytes, excluding the byte that is in
--   the past.
with Bit_Ops, Trace.Loggers, Hexa_Utils;
use Bit_Ops;
pragma Optimize (Time);
package body Lzf is

  -- Trace logger
  Logger : Trace.Loggers.Logger;
  procedure Log (Msg : in String) is
  pragma Inline (Log);
  begin
    Logger.Log_Debug (Msg);
  end Log;

  -- Image of a byte
  function Image is new Hexa_Utils.Mod_Image (Byte);

  -- The number of entries in the hash table. The size is a trade-off between
  --  hash collisions (reduced compression) and speed (amount that fits in CPU
  --  cache)
  -- The difference between 15 and 14 is very small
  -- Sor small blocks (and 14 is usually a bit faster).
  -- For a low-memory/faster configuration, use Hash_Log = 13
  -- For best compression, use 15 or 16 (or more, up to 22).
  Hash_Log : constant := 16;
  Hash_Size : constant Natural := Shl (1, Hash_Log);
  type Hash_Table_Type is array (0 .. Hash_Size - 1) of Integer;

  -- The maximum number of literals in a chunk (32)
  -- Nb of literals is on 5 bits (the 3 first are 0), so up to 31 literals
  Max_Literal : constant Natural := Shl (1, 5);

  -- The maximum offset allowed for a back-reference (8192)
  -- The offset - 1 is stored in 5 + 8 bits -> 1FFF
  Max_Off : constant Natural := Shl (1, 13);

  -- The maximum back-reference length (264)
  -- 7 + one byte to store length - 3
  Max_Ref : constant Natural := Shl (1, 8) + Shl (1, 3);

  -- Compute the input to Next to compute the future
  -- Return the integer with the first two bytes 0, then the byte at the
  --  index and the byte at index + 1
  function First (A : Byte_Array; I : Integer) return Integer is
  pragma Inline (First);
  begin
    return Shl (Integer(A(A'First + I)), 8) or Integer(A(A'First + I + 1));
  end First;

  -- Update the future with current position
  -- Shift the value 1 byte left, and add the byte at index inPos+2
  function Next (V : Integer; A : Byte_Array; I : Integer) return Integer is
  pragma Inline (Next);
  begin
    return Shl (V and 16#FFFF#, 8) or Integer(A(A'First + I + 2));
  end Next;

  -- Compute the hash value of a future
  function Hash (H : Integer) return Integer is
  pragma Inline (Hash);
  begin
    return (Shr (H, 3 * 8 - Hash_Log) - H * 5) and (Hash_Size - 1);
  end Hash;

  -- Convert an int into a byte
  function To_Byte (I : Integer) return Byte is
  pragma Inline (To_Byte);
  begin
    return Byte (I and 16#FF#);
  end To_Byte;

  -- Compress Input into Output
  -- Outlen is less than 104% of Input length
  -- May raise Too_Big if Output is too small
  procedure Compress (Input : in Byte_Array;
                      Output : out Byte_Array;
                      Outlen : out Natural) is
    -- Indexes in input and output flows
    In_Pos : Integer := 0;
    Out_Pos : Integer := Output'First + 1;
    -- Hash table: indexes are the hash of future and values are indexes
    --  in the input flow
    Hash_Table : Hash_Table_Type := (others => -1);
    -- Current, next and next-next bytes
    Future : Integer;
    -- NUmber of literal in current run
    Literals : Integer := 0;
    -- Next-next byte
    P2 : Byte;
    -- Absolute and relative index, in input, of cross reference
    Ref, Off : Integer;
    -- Do we match the cross reference found at Hash(Future) in Hash_Table
    Match : Boolean;
    -- Length and max_len of a match with cross reference
    Len, Max_Len : Integer;
    use type C_Types.Byte;
  begin
    if not Logger.Is_Init then
      Logger.Init ("Lzf");
    end if;

    if Input'Length = 0 then
      Outlen := 0;
      if Logger.Debug_On then
        Log ("Empty input");
      end if;
      return;
    elsif Input'Length = 1 then
      if Output'Length < 2 then
        raise Too_Big;
      end if;
      Outlen := 2;
      Output(Out_Pos) := 0;
      Output(Out_Pos + 1) := Input(Input'First);
      if Logger.Debug_On then
        Log ("Single byte " & Image (Input(Input'First)));
      end if;
      return;
    elsif Input'Length = 2 then
      if Output'Length < 2 then
        raise Too_Big;
      end if;
      Outlen := 3;
      Output(Out_Pos) := 1;
      Output(Out_Pos + 1) := Input(Input'First);
      Output(Out_Pos + 2) := Input(Input'First + 1);
      if Logger.Debug_On then
        Log ("Two bytes " & Image (Input(Input'First))
           & " and " & Image (Input(Input'First + 1)));
      end if;
      return;
    end if;

    -- Main loop
    Future := First (Input, 0);
    while In_Pos < Input'Length - 2 loop
      P2 := Input(Input'First + In_Pos + 2);
      -- Next: Remove oldest byte, shift Future left, append P2
      Future := Shl (Future and 16#FFFF#, 8) or Integer (P2);
      if Logger.Debug_On then
        Log ("Start loop at index " & In_Pos'Img
           & ", P2 " & Image(P2) & ", future " & Hexa_Utils.Image(Future));
      end if;
      Off := Hash (Future);
      Ref := Hash_Table(Off);
      Hash_Table(Off) := In_Pos;
      if Logger.Debug_On then
        Log ("Hashed index " & In_Pos'Img & ", at offset " & Off'Img
           & ", replacing ref " & Ref'Img);
      end if;

      -- Check if match
      Match := False;
      if Ref < In_Pos
      and then Ref >= 0 then
        Off := In_Pos - Ref - 1;
        if Off < Max_Off
        and then Input(Input'First + Ref + 2) = P2
        and then Input(Input'First + Ref + 1) = To_Byte (Shr (Future, 8))
        and then Input(Input'First + Ref) = To_Byte (Shr (Future, 16)) then
          Match := True;
        end if;
      end if;

      if Match then
        if Logger.Debug_On then
          Log ("Match at offset " & Integer'Image (Off + 1));
        end if;

        if Literals = 0 then
          -- Multiple successive back-references,
          --  so there is no literal run control byte to update
          Out_Pos := Out_Pos - 1;
        else
          -- Set the control byte at the start of the literal run
          --  to store the number of literals
          -- 3 first bits at 0 then Nb on the next 5 bits (so <= 31)
          if Logger.Debug_On then
            Log ("  Store Nb literals " & Integer'Image (Literals - 1)
               & " at pos " & Integer'Image (Out_Pos - Literals - 1));
          end if;
          Output(Out_Pos - Literals - 1) := To_Byte ((Literals - 1));
          Literals := 0;
        end if;

        -- Length of the match
        -- Ensure that there will remain at least 2 more bytes to rebuild
        --  the future from
        Max_Len := Input'Length - In_Pos - 2;
        if Max_Len > Max_Ref then
          Max_Len := Max_Ref;
        end if;
        Len := 3;
        while Len < Max_Len
        and then Input(Input'First + Ref + Len)
               = Input(Input'First + In_Pos + Len) loop
          Len := Len + 1;
        end loop;
        if Logger.Debug_On then
          Log ("  match len " & Len'Img);
        end if;
        Len := Len - 2;

        -- Store match length-2
        if Len < 7 then
          -- Less than 3 bits of length then 5 upper bits of offset
          Output(Out_Pos) := To_Byte (Shr (Off, 8) or Shl (Len, 5));
        else
          -- 3 bits of length then 5 upper bits of offset
          Output(Out_Pos) := To_Byte (Shr (Off, 8) or Shl (7, 5));
          Out_Pos := Out_Pos + 1;
          -- Then Length - 7
          Output(Out_Pos) := To_Byte (Len - 7);
        end if;
        Out_Pos := Out_Pos + 1;
        -- Then 8 lower bits of offset
        Output(Out_Pos) := To_Byte (Off);
        Out_Pos := Out_Pos + 1;
        -- Move one byte forward to allow for a literal run control byte
        Out_Pos := Out_Pos + 1;
        In_Pos := In_Pos + Len;

        if In_Pos < Input'Length - 3 then
          -- Rebuild the future, and store the last 2 bytes to the hashtable.
          -- Storing hashes of the last bytes in back-reference
          --  improves the compression ratio and only reduces speed slightly.
          Future := First (Input, In_Pos);
          Future := Next (Future, Input, In_Pos);
          Hash_Table(Hash (Future)) := In_Pos;
          if Logger.Debug_On then
            Log ("  Future " & Hexa_Utils.Image (Future)
               & " hashed index " & In_Pos'Img
               & " at offset " & Integer'Image (Hash (Future)));
          end if;
          In_Pos := In_Pos + 1;
          Future := Next (Future, Input, In_Pos);
          Hash_Table(Hash (Future)) := In_Pos;
          if Logger.Debug_On then
            Log ("  Future " & Hexa_Utils.Image (Future)
               & " hashed index " & In_Pos'Img
               & " at offset " & Integer'Image (Hash (Future)));
          end if;
          In_Pos := In_Pos + 1;
          if Logger.Debug_On then
            Log ("  Now at index " & In_Pos'Img
               & ", future " & Hexa_Utils.Image(Future));
          end if;
        else
          -- Set correct inpout index
          In_Pos := In_Pos + 2;
        end if;
      else
        -- Not match
        -- Copy one byte from input to output as part of literal
        Output(Out_Pos) := Input(Input'First + In_Pos);
        if Logger.Debug_On then
          Log ("Not Match");
          Log ("  Store literal " & Image (Input(Input'First + In_Pos))
             & " at pos " & Integer'Image (Out_Pos));
        end if;
        In_Pos := In_Pos + 1;
        Out_Pos := Out_Pos + 1;
        Literals := Literals + 1;
        if Literals = Max_Literal then
          if Logger.Debug_On then
            Log ("  Store Nb literals " & Integer'Image (Literals - 1)
               & " at pos " & Integer'Image (Out_Pos - Literals - 1));
          end if;
          Output(Out_Pos - Literals - 1) := To_Byte (Literals - 1);
          Literals := 0;
          -- Move ahead one byte to allow for the literal run control byte
          Out_Pos := Out_Pos + 1;
        end if;
        if Logger.Debug_On then
          Log ("  Nb literals " & Literals'Img);
        end if;
      end if;
    end loop;

    -- Write the remaining few bytes as literals
    while In_Pos < Input'Length loop
      if Logger.Debug_On then
        Log ("Append literal " & Image (Input(Input'First + In_Pos))
           & " at pos " & Integer'Image (Out_Pos));
      end if;
      Output(Out_Pos) := Input(Input'First + In_Pos);
      In_Pos := In_Pos + 1;
      Out_Pos := Out_Pos + 1;
      Literals := Literals + 1;
      if Literals = Max_Literal then
        if Logger.Debug_On then
          Log ("  Store Nb literals " & Integer'Image (Literals - 1)
             & " at pos " & Integer'Image (Out_Pos - Literals - 1));
        end if;
        Output(Out_Pos - Literals - 1) := To_Byte (Literals - 1);
        Literals := 0;
        Out_Pos := Out_Pos + 1;
      end if;
    end loop;

    -- Write the final literal run length to the control byte
    if Literals /= 0 then
      Output(Out_Pos - Literals - 1) := To_Byte (Literals - 1);
      if Logger.Debug_On then
        Log ("  Store Nb literals " & Integer'Image (Literals - 1)
           & " at pos " & Integer'Image (Out_Pos - Literals - 1));
      end if;
    else
      -- One extra byte was inserted to store the length, but there is
      --  no more literal
      Out_Pos := Out_Pos - 1;
    end if;

    Outlen := Out_Pos - Output'First;
  exception
    when Constraint_Error =>
      raise Too_Big;
  end Compress;

  -- Uncompress Input into Output
  -- May raise Too_Big if Output is too small
  -- May raise Invalid if input is not a valid compressed buffer
  procedure Uncompress (Input : in Byte_Array;
                        Output : out Byte_Array;
                        Outlen : out Natural) is
    In_Pos, Out_Pos : Integer;
    Ctrl, Len : Integer;
  begin
    if not Logger.Is_Init then
      Logger.Init ("Lzf");
    end if;

    if Input'Length = 0 then
      Log ("Empty input");
      Outlen := 0;
      return;
    end if;

    In_Pos := Input'First;
    Out_Pos := Output'First;
    if Logger.Debug_On then
      Log ("First index is " & Integer'Image (In_Pos));
    end if;
    loop
      -- Get control char: literals-run or backref
      Ctrl := Integer (Input(In_Pos));
      In_Pos := In_Pos + 1;
      if Ctrl < Max_Literal then
        -- Literal run of length = ctrl + 1,
        -- Copy to output and move forward this many bytes
        if Logger.Debug_On then
          Log ("Got at index " & Integer'Image (In_Pos - 1)
             & " " & Integer'Image (Ctrl + 1) & " litterals");
          for I in In_Pos .. In_Pos + Ctrl loop
             Log ("  Literal " & Hexa_Utils.Image (Integer (Input(I))));
          end loop;
        end if;
        if Out_Pos + Ctrl > Output'Last then
          raise Too_Big;
        end if;
        Output(Out_Pos .. Out_Pos + Ctrl) := Input (In_Pos .. In_Pos + Ctrl);
        In_Pos := In_Pos + Ctrl + 1;
        Out_Pos := Out_Pos + Ctrl + 1;
      else
        -- Back reference: the highest 3 bits are the match length
        Len := Shr (Ctrl, 5);
        -- If the length is maxed, add the next byte to the length
        if Len = 7 then
          Len := Len + Integer(Input(In_Pos));
          In_Pos := In_Pos + 1;
        end if;
        -- Minimum back-reference is 3 bytes,
        --  so 2 was subtracted before storing size
        Len := Len + 2;

        -- Ctrl is now the offset for a back-reference...
        -- the logical AND operation removes the length bits
        Ctrl := Shl (Ctrl and 16#1F#, 8);

        -- The next byte increases the offset
        Ctrl := Ctrl + Integer(Input(In_Pos)) + 1;
        In_Pos := In_Pos + 1;
        if Logger.Debug_On then
          Log ("Got at index " & Integer'Image (In_Pos - 1)
             & " a backref of len " & Integer'Image (Len)
             & " and offset " & Integer'Image(Ctrl));
        end if;

        -- Copy the *back-reference* bytes from the given
        --  location in output to current position
        if Out_Pos + Len - 1 > Output'Last then
          raise Too_Big;
        end if;
        Ctrl := Out_Pos - Ctrl;
        -- Areas may overlap! so we must copy byte per byte
        for I in 0 .. Len - 1 loop
          Output(Out_Pos + I) := Output(Ctrl + I);
        end loop;
        Out_Pos := Out_Pos + Len;
      end if;

      exit when In_Pos > Input'Last;
    end loop;
    Outlen := Out_Pos - Output'First;
  end Uncompress;

end Lzf;

