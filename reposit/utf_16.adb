with Bit_Ops;
package body Utf_16 is

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

  -- Decodes a Utf-16 sequence to Unicode. May raise Invalid_Sequence
  function Decode (Seq : Sequence) return Unicode_Number is
    Val1, Val2 : Integer;
    Result : Unicode_Number;
    use Bit_Ops;
  begin
    Check_Valid (Seq);
    Val1 := Wide_Character'Pos (Seq(Seq'First));
    if Seq'Length = 1 then
      return Val1;
    end if;
    Val2 := Wide_Character'Pos (Seq(Seq'First + 1));
    -- A 20 bits code with 10 highest from the 10 lowest of Val1
    --                and 10 lowest   from the 10 lowest of Val2
    --  +10000
    Result := Shl (Val1 and 16#3FF#, 10) or (Val2 and 16#3FF#);
    Result := Result + 16#10000#;
    return Result;
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
        and Shr (Val1 and 16#FF00#, 8);
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

