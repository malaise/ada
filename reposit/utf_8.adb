with Bit_Ops;
-- Utf_8 encoding/decoding
package body Utf_8 is

  -- Returns the number of chars of a sequence (coded in the 1st char)
  function Nb_Chars (First_Char : Character) return Len_Range is
    use Bit_Ops;
    -- Byte value
    Val : Integer := Character'Pos (First_Char);
  begin
    -- Check bits from highest to lowest
    if    (Val And 2#1000_0000#) = 0 then
      -- Highest bit is 0, Ascii char on 1 byte
      return 1;
    elsif (Val And 2#0100_0000#) = 0 then
      -- 2#10# is forbidden in first char
      raise Invalid_Sequence;
    elsif (Val And 2#0010_0000#) = 0 then
      return 2;
    elsif (Val And 2#0001_0000#) = 0 then
      return 3;
    elsif (Val And 2#0000_1000#) = 0 then
      return 4;
    else
      -- More that four bits set is forbidden
      raise Invalid_Sequence;
    end if;
  end Nb_Chars;

  -- Checks that a Utf-8 sequence is valid
  function Is_Valid (Seq : Sequence) return Boolean is
    use Bit_Ops;
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
    -- Check that all but first bytes start by 2#10#
    for I in Len_Range'Succ(Seq'First) .. Seq'Last loop
      if Integer'(Character'Pos(Seq(I)) And 2#1100_0000#) /= 2#1000_0000# then
        return False;
      end if;
    end loop;
    return True;
  end Is_Valid;

  -- Checks that a Utf-8 sequence is valid, raise Invalid_Sequence if not
  procedure Check_Valid (Seq : in Sequence) is
  begin
    if not Is_Valid (Seq) then
      raise Invalid_Sequence;
    end if;
  end Check_Valid;

  -- Checks that a Utf-8 sequence is safe (valid and not uselessly long...)
  function Is_Safe (Seq : Sequence) return Boolean is
    Unicode : Unicode_Number;
  begin
    if not Is_Valid (Seq) then
      return False;
    end if;
    -- Decode and check vs forbidden values
    Unicode := Decode (Seq);
    if      Unicode = 16#D800#
    or else Unicode = 16#DFFF#
    or else Unicode = 16#FFFE#
    or else Unicode = 16#FFFF# then
      return False;
    end if;
    -- Re-encode and check this leads back to Seq
    if Encode (Unicode) /= Seq then
      return False;
    end if;
    -- All tests OK
    return True;
  end Is_safe;

  -- Checks that a Utf-8 sequence is safe, raise Invalid_Sequence if not
  procedure Check_Safe (Seq : in Sequence) is
  begin
    if not Is_Safe (Seq) then
      raise Invalid_Sequence;
    end if;
  end Check_Safe;

  -- Decodes a Utf-8 sequence. May raise Invalid_Sequence.
  function Decode (Seq : Sequence) return Unicode_Number is

    First : constant Positive := Seq'First;
    function Byte_Of (I : Len_Range) return Natural is
    begin
      return Character'Pos (Seq (First + I - 1));
    end Byte_Of;

    Result : Unicode_Number;
    use Bit_Ops;
  begin
    Check_Valid (Seq);
    if Seq'Length = 1 then
      -- One Byte => Ascii: 0iii_iiii
      Result := Byte_Of (1);
    elsif Seq'Length = 2 then
      -- Seq is 110j_jjjj 10ii_iiii and becomes 0000_0jjj jjii_iiii
      Result :=           Shl (Byte_Of (1) And 2#0001_1111#, 06);
      Result := Result Or     (Byte_Of (2) And 2#0011_1111#);
    elsif Seq'Length = 3 then
      -- Seq is 1110_kkkk 10jj_jjjj 10ii_iiii and becomes kkkk_jjjj jjii_iiii
      Result :=           Shl (Byte_Of (1) And 2#0000_1111#, 12);
      Result := Result Or Shl (Byte_Of (2) And 2#0011_1111#, 06);
      Result := Result Or     (Byte_Of (3) And 2#0011_1111#);
    else -- Seq'Length = 4
      -- Seq is 1111_0lll 10kk_kkkk 10jj_jjjj 10ii_iiii
      --  and becomes 000l_llkk kkkk_jjjj jjii_iiii
      Result :=           Shl (Byte_Of (1) And 2#0000_0111#, 18);
      Result := Result Or Shl (Byte_Of (2) And 2#0011_1111#, 12);
      Result := Result Or Shl (Byte_Of (3) And 2#0011_1111#, 06);
      Result := Result Or     (Byte_Of (4) And 2#0011_1111#);
    end if;
    return Result;
  end Decode;

  -- Encodes a Utf-8 sequence
  function Encode (Unicode : Unicode_Number) return Sequence is
    Nb_Chars : Len_Range;
    Tab : array (Len_Range) of Natural;
    use Bit_Ops;
  begin
    -- Compute sequence length from unicode value
    if    Unicode < 16#000080# then Nb_Chars := 1;
    elsif Unicode < 16#000800# then Nb_Chars := 2;
    elsif Unicode < 16#010000# then Nb_Chars := 3;
    else Nb_Chars := 4;
    end if;
    -- Fill array of values
    if Nb_Chars = 1 then
      -- One Byte => Ascii: 0iii_iiii
      Tab(1) := Unicode;
    elsif Nb_Chars = 2 then
      -- U is 0000_0jjj iiii_iiii and becomes 110j_jjii 10ii_iiii
      Tab(1) := Shr (Unicode And 2#0000_0000_0000_0111_1100_0000#, 06) Or 2#1100_0000#;
      Tab(2) :=     (Unicode And 2#0000_0000_0000_0000_0011_1111#)     Or 2#1000_0000#;
    elsif Nb_Chars = 3 then
      -- U is jjjj_jjjj iiii_iiii and becomes 1110_jjjj 10jj_jjii 10ii_iiii
      Tab(1) := Shr (Unicode And 2#0000_0000_1111_0000_0000_0000#, 12) Or 2#1110_0000#;
      Tab(2) := Shr (Unicode And 2#0000_0000_0000_1111_1100_0000#, 06) Or 2#1000_0000#;
      Tab(3) :=     (Unicode And 2#0000_0000_0000_0000_0011_1111#)     Or 2#1000_0000#;
    else -- Nb_Chars = 4
      -- U is 000k_kkkk jjjj_jjjj iiii_iiii
      -- and becomes 1111_0kkk 10kk_jjjj 10jj_jjii 10ii_iiii
      Tab(1) := Shr (Unicode And 2#0001_1100_0000_0000_0000_0000#, 18) Or 2#1111_0000#;
      Tab(2) := Shr (Unicode And 2#0000_0011_1111_0000_0000_0000#, 12) Or 2#1000_0000#;
      Tab(3) := Shr (Unicode And 2#0000_0000_0000_1111_1100_0000#, 06) Or 2#1000_0000#;
      Tab(4) :=     (Unicode And 2#0000_0000_0000_0000_0011_1111#)     Or 2#1000_0000#;
    end if;
    -- Convert values to chars and return
    declare
      Seq : String (1 .. Nb_Chars);
    begin
      for I in Seq'Range loop
        Seq(I) := Character'Val (Tab(I));
      end loop;
      return Seq;
    end;
  end Encode;

end Utf_8;

