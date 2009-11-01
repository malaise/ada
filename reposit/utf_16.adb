with Ada.Strings.Wide_Unbounded;
with Bit_Ops, Unbounded_Arrays;
package body Utf_16 is

  package Unbounded_Unicode is new Unbounded_Arrays (Unicode_Number,
                                                     Unicode_Sequence);


  -- Returns the number of chars of a sequence (coded in the 1st char)
  function Nb_Chars (First_Char : Wide_Character) return Len_Range is
    -- Byte value
    Val : constant Integer := Wide_Character'Pos (First_Char);
  begin
    -- From 0000 to D7FF and from EOOO to FFFF => 1
    -- From D800 to DBFF => 2
    -- From DC00 to DFFF => Error
    if Val < 16#D800# then
      return 1;
    elsif Val < 16#DC00# then
      return 2;
    elsif Val < 16#E000# then
      raise Invalid_Sequence;
    else
      return 1;
    end if;
  end Nb_Chars;

  -- Checks that a Utf-16 sequence is valid
  function Is_Valid (Seq : Sequence) return Boolean is
    Val : Integer;
  begin
     -- Check that sequence is not empty
    if Seq'Length = 0 then
      return False;
    end if;
    -- Check that sequence has the proper Nb of chars
    -- as required by the first char
    if Seq'Length /= Nb_Chars (Seq(Seq'First)) then
      return False;
    end if;
    -- Check that second word (if any) is between DC00 and DFFF
    if Seq'Length = 1 then
      return True;
    end if;
    Val := Wide_Character'Pos (Seq(Seq'First + 1));
    return 16#DC00# <= Val and then Val <= 16#DFFF#;
  exception
    when Invalid_Sequence =>
      return False;
  end Is_Valid;

  -- Checks that a Utf-16 sequence is valid, raise Invalid_Sequence if not
  procedure Check_Valid (Seq : in Sequence) is
  begin
    if not Is_Valid (Seq) then
      raise Invalid_Sequence;
    end if;
  end Check_Valid;

  -- Internal
  -- Decodes the first unicode from the given sequence
  procedure Decode (Seq : in Sequence;
                    Len : out Len_Range;
                    Unicode : out Unicode_Number) is
    Val1, Val2 : Integer;
    use Bit_Ops;
  begin
    Len := Nb_Chars (Seq(Seq'First));

    Val1 := Wide_Character'Pos (Seq(Seq'First));
    if Len = 1 then
      Unicode := Val1;
    end if;

    Val2 := Wide_Character'Pos (Seq(Seq'First + 1));
    if 16#DC00# > Val2 or else Val2 > 16#DFFF# then
      raise Invalid_Sequence;
    end if;
    -- A 20 bits code with 10 highest from the 10 lowest of Val1
    --                and 10 lowest   from the 10 lowest of Val2
    --  +10000
    Unicode := Shl (Val1 and 16#3FF#, 10) or (Val2 and 16#3FF#);
    Unicode := Unicode + 16#10000#;
  end Decode;

  -- Decodes a Utf-16 sequence to Unicode. May raise Invalid_Sequence
  function Decode (Seq : Sequence) return Unicode_Number is
    U : Unicode_Number;
    L : Len_Range;
  begin
    Decode (Seq, L, U);
    if L /= Seq'Length then
      raise Invalid_Sequence;
    end if;
    return U;
  end Decode;

  -- Encodes a Unicode as a Utf-16 sequence
  function Encode (Unicode : Unicode_Number) return Sequence is
    Val, Val1, Val2 : Integer;
    use Bit_Ops;
  begin
    if Unicode < 16#10000# then
      -- One word
      if 16#D800# <= Unicode and then Unicode <= 16#DFFF# then
        raise Excluded_Non_Character;
      end if;
      return "" & Wide_Character'Val (Unicode);
    end if;
    Val := Unicode - 16#10000#;
    -- D800 and 10 highest bits
    -- DC00 and 10 lowest  bits
    Val1 := 16#D800# or Shr (Val and 16#FFC00#, 10);
    Val2 := 16#DC00# or     (Val and 16#003FF#);
    return Wide_Character'Val (Val1) & Wide_Character'Val (Val2);
  end Encode;


  -- Decodes a Utf-16 sequence (of sequences) to Unicode sequence.
  -- May raise Invalid_Sequence
  function Decode (Seq : Sequence) return Unicode_Sequence is
    Index : Positive;
    Len : Positive;
    U : Unicode_Number;
    Res : Unbounded_Unicode.Unbounded_Array;
  begin
    if Seq'Length = 0 then
      return (1 .. 0 => 0);
    end if;
    -- Get each unicode number
    Index := 1;
    loop
      Decode (Seq(Index .. Seq'Last), Len, U);
      Unbounded_Unicode.Append (Res, U);
      Index := Index + Len;
      exit when Index > Seq'Last;
    end loop;
    return Unbounded_Unicode.To_Array (Res);
  end Decode;

  -- Encodes a Unicode sequence as a Utf-16 sequence (of sequecnes)
  function Encode (Unicode : Unicode_Sequence) return Sequence is
    Result : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
  begin
    for I in Unicode'Range loop
      Ada.Strings.Wide_Unbounded.Append (Result, Encode (Unicode(I)));
    end loop;
    return Ada.Strings.Wide_Unbounded.To_Wide_String (Result);
  end Encode;


  -- Decodes a Utf-16 sequence to Wide_Character.
  -- May raise Invalid_Sequence or Not_Wide_Character
  function Decode (Seq : Sequence) return Wide_Character is
    U : constant Unicode_Number := Decode (Seq);
  begin
   return Wide_Character'Val (U);
  exception
    when Constraint_Error =>
      raise Not_Wide_Character;
  end Decode;

  -- Encodes a Unicode as a Utf-16 sequence
  function Encode (Wide_Char : Wide_Character) return Sequence is
  begin
    return Encode (Wide_Character'Pos (Wide_Char));
  end Encode;

  -- Swap a sequence BE <-> LE (big endian <-> little endian)
  procedure Swap (Wide_Char : in out Wide_Character) is
  begin
    Wide_Char := Swap (Wide_Char);
  end Swap;

  function Swap (Wide_Char : Wide_Character) return Wide_Character is
    Val1, Val2 : Integer;
    use Bit_Ops;
  begin
    Val1 := Wide_Character'Pos (Wide_Char);
    Val2 := Shl (Val1 and 16#00FF#, 8)
         or Shr (Val1 and 16#FF00#, 8);
    return Wide_Character'Val (Val2);
  end Swap;

  procedure Swap (Seq : in out Sequence) is
  begin
    for I in Seq'Range loop
      Swap (Seq(I));
    end loop;
  end Swap;

  function Swap (Seq : Sequence) return Sequence is
    Res : Sequence (1 .. Seq'Length) := Seq;
  begin
    Swap (Res);
    return Res;
  end Swap;

  -- Split / merge a UTF-16 sequence into a sequence of bytes (chars)
  function Split (Seq : Sequence) return String is
    Str : String (1 .. Seq'Length * 2);
    J : Positive;
    Val : Integer;
    use Bit_Ops;
  begin
    J := 1;
    for I in Seq'Range loop
      Val := Wide_Character'Pos (Seq(I));
      Str(J + 0) := Character'Val (Shr (Val and 16#FF00#, 8));
      Str(J + 1) := Character'Val (Val and 16#00FF#);
      J := J + 2;
    end loop;
    return Str;
  end Split;

  function Merge (Str : String) return Sequence is
    Res : Sequence (1 .. Str'Length / 2);
    J : Positive;
    Val : Integer;
    use Bit_Ops;
  begin
    if Str'Length mod 2 /= 0 then
      raise Odd_Length;
    end if;
    J := 1;
    for I in Str'Range loop
      if (I - Str'First) mod 2 = 0 then
        Val := Shl (Character'Pos (Str(I)), 8);
      else
        Val := Val or Character'Pos (Str(I));
        Res (J) := Wide_Character'Val (Val);
        J := J + 1;
      end if;
    end loop;
    return Res;
  end Merge;

end Utf_16;

