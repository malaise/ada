-- This package implements the LZF lossless data compression algorithm. LZF is
-- a Lempel-Ziv variant with byte-aligned output, and optimized for speed.
-- Safety/Use Notes:
--  - Each instance should be used by a single thread only.
--  - The data buffers should be smaller than 1 GB.
--  - For performance reasons, safety checks on expansion are omitted.
--  - Invalid compressed data can cause exceptions (Constraint_Error)
-- The LZF compressed format knows literal runs and back-references:
--  - Literal run: directly copy bytes from input to output.
--  - Back-reference: copy previous data to output stream, with specified
--    offset from location and length. The length is at least 3 bytes.
-- The first byte of the compressed stream is the control byte. For literal
-- runs, the highest three bits of the control byte are not set, then the lower
-- bits are the literal run length, and the next bytes are data to copy directly
-- into the output. For back-references, the highest three bits of the control
-- byte are the back-reference length. If all three bits are set, then the
-- back-reference length is stored in the next byte. The lower bits of the
-- control byte combined with the next byte form the offset for the
-- back-reference.
with Bit_Ops;
use Bit_Ops;
package body Lzf is

  -- The number of entries in the hash table. The size is a trade-off between
  --  hash collisions (reduced compression) and speed (amount that fits in CPU
  --  cache)
  Hash_Size : constant Natural := Shl (1, 14);
  type Hash_Table_Type is array (0 .. Hash_Size - 1) of Integer;

  -- The maximum number of literals in a chunk (32)
  Max_Literal : constant Natural := Shl (1, 5);

  -- The maximum offset allowed for a back-reference (8192)
  Max_Off : constant Natural := Shl (1, 13);

  -- The maximum back-reference length (264)
  Max_Ref : constant Natural := Shl (1, 8) + Shl (1, 3);

  -- Return the integer with the first two bytes 0, then the bytes at the
  --  index, then at index+1
  function First (A : Byte_Array; I : Integer) return Integer is
  begin
    return Shl (Integer(A(A'First + I)), 8) or Integer(A(A'First + I + 1));
  end First;

  -- Shift the value 1 byte left, and add the byte at index inPos+2
  function Next (V : Integer; A : Byte_Array; I : Integer) return Integer is
  begin
    return Shl (V, 8) or Integer(A(A'First + I + 2));
  end Next;

  -- Compute the address in the hash table.
  function Hash (H : Integer) return Integer is
  begin
    return Shr (H * 2777, 9) and (Hash_Size - 1);
  end Hash;

  -- Compress Input into Output
  -- Outlen is less than 104% of Input length
  -- May raise Too_Big if Output is too small
  procedure Compress (Input : in Byte_Array;
                      Output : out Byte_Array;
                      Outlen : out Natural) is
    In_Pos : Integer := 0;
    Out_Pos : Integer := Output'First;
    Hash_Table : Hash_Table_Type := (others => 0);
    Future : Integer := First (Input, 0);
    Literals : Integer := 0;
    P2 : Byte;
    Off, Ref : Integer;
    Len, Max_Len : Integer;
    Match : Boolean;
    use type C_Types.Byte;
  begin
    while In_Pos < Input'Length - 4 loop
      P2 := Input(Input'First + In_Pos + 2);
      -- Next
      Future := Shl (Future, 8) + Integer (P2);
      Off := Hash (Future);
      Ref := Hash_Table(Off);
      Hash_Table(Off) := In_Pos;

      -- Check if match
      Match := False;
      if Ref < In_Pos
      and then Ref > 0 then
        Off := In_Pos - Ref - 1;
        if Off < Max_Off
        and then Input(Input'First + Ref + 2) = P2
        and then Input(Input'First + Ref + 1) = Byte (Shr (Future, 8))
        and then Input(Input'First + Ref)     = Byte (Shr (Future, 16)) then
          Match := True;
        end if;
      end if;

      if Match then
        Max_Len := Input'Length - In_Pos - 2;
        if Max_Len > Max_Ref then
          Max_Len := Max_Ref;
        end if;
        if Literals = 0 then
          -- Multiple back-references,
          -- so there is no literal run control byte
          Out_Pos := Out_Pos - 1;
        else
          -- Set the control byte at the start of the literal run
          -- to store the number of literals
          Output(Out_Pos - Literals - 1) := Byte (Literals - 1);
          Literals := 0;
        end if;
        Len := 3;
        while Len < Max_Len
        and then Input(Input'First + Ref + Len)
               = Input(Input'First + In_Pos + Len) loop
          Len := Len + 1;
        end loop;
        Len := Len - 2;

        if Len < 7 then
          Output(Out_Pos) := Byte (Shr (Off, 8) + Shl (Len, 5));
        else
          Output(Out_Pos) := Byte (Shr (Off, 8) + Shl (7, 5));
          Out_Pos := Out_Pos + 1;
          Output(Out_Pos) := Byte (Len - 7);
        end if;
        Out_Pos := Out_Pos + 1;
        Output(Out_Pos) := Byte (Off);
        Out_Pos := Out_Pos + 1;
        -- Move one byte forward to allow for a literal run control byte
        Out_Pos := Out_Pos + 1;
        In_Pos := In_Pos + Len;

        -- Rebuild the future, and store the last bytes to the hashtable.
        -- Storing hashes of the last bytes in back-reference
        --  improves the compression ratio and only reduces speed slightly.
        Future := First (Input, In_Pos);
        Future := Next (Future, Input, In_Pos);
        Hash_Table(Hash (Future)) := In_Pos;
        In_Pos := In_Pos + 1;
        Future := Next (Future, Input, In_Pos);
        Hash_Table(Hash (Future)) := In_Pos;
        In_Pos := In_Pos + 1;
      else
        -- Not match
        -- Copy one byte from input to output as part of literal
        Output(Out_Pos) := Input(Input'First + In_Pos);
        In_Pos := In_Pos + 1;
        Out_Pos := Out_Pos + 1;
        Literals := Literals + 1;
        if Literals = Max_Literal then
          Output(Out_Pos - Literals - 1) := Byte (Literals - 1);
          Literals := 0;
          -- Move ahead one byte to allow for the literal run control byte
          Out_Pos := Out_Pos + 1;
        end if;
      end if;

      -- Write the remaining few bytes as literals
      while In_Pos < Input'Length loop
        Output(Out_Pos) := Input(Input'First + In_Pos);
        In_Pos := In_Pos + 1;
        Out_Pos := Out_Pos + 1;
        Literals := Literals + 1;
        if Literals = Max_Literal then
          Output(Out_Pos - Literals - 1) := Byte (Literals - 1);
          Literals := 0;
          Out_Pos := Out_Pos + 1;
        end if;
      end loop;
      -- Write the final literal run length to the control byte
      Output(Out_Pos - Literals - 1) := Byte (Literals - 1);
      if Literals = 0 then
        Out_Pos := Out_Pos - 1;
      end if;
    end loop;
    Outlen := Out_Pos - Output'First + 1;
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
    In_Pos := Input'First;
    Out_Pos := Output'First;
    loop
      Ctrl := Integer (Input(In_Pos));
      In_Pos := In_Pos + 1;
      if Ctrl < Max_Literal then
        -- Literal run of length = ctrl + 1,
        Ctrl := Ctrl + 1;
        -- Copy to output and move forward this many bytes
        Output(Out_Pos .. Out_Pos + Ctrl - 1) :=
            Input (In_Pos .. In_Pos + Ctrl - 1);
        In_Pos := In_Pos + Ctrl;
        Out_Pos := Out_Pos + Ctrl;
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
        Ctrl := -Shl (Ctrl and 16#1F#, 8) - 1;

        -- The next byte augments/increases the offset
        Ctrl := Ctrl - Integer(Input(In_Pos));
        In_Pos := In_Pos + 1;

        -- Copy the back-reference bytes from the given
        --  location in output to current position
        Ctrl := Ctrl + Out_Pos;
        Output(Out_Pos .. Out_Pos + Len - 1) :=
            Input(In_Pos .. In_Pos+ Len - 1);
        Out_Pos := Out_Pos + Len;
        In_Pos := In_Pos + Len;
      end if;

      exit when Out_Pos >= Output'Last;
    end loop;
    Outlen := Out_Pos - Output'First + 1;
  end Uncompress;

end Lzf;

