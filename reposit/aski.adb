package body Aski is

  -- Are strict ANSI the characters from 0 (Nul) to 127 (Del) included
  function Is_Strict (C : Character) return Boolean is
  begin
    return C <= Del;
  end Is_Strict;

  function Is_Strict (S : String) return Boolean is
  begin
    for I in S'Range loop
      if not Is_Strict (S(I)) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Strict;

  function Is_Strict (U : Unicode_Number)   return Boolean is
  begin
    return U < Delu;
  end Is_Strict;

  function Is_Strict (S : Unicode_Sequence) return Boolean is
  begin
    for I in S'Range loop
      if not Is_Strict (S(I)) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Strict;

  -- Conversion from strict ANSII to Unicode and reverse
  -- Raise Constraint_Error if a character or unicode is not strict
  function Decode (C : Character) return Unicode_Number is
  begin
    if not Is_Strict (C) then
      raise Constraint_Error;
    end if;
    return Character'Pos (C);
  end Decode;

  function Decode (S : String) return Unicode_Sequence is
    Result : Unicode_Sequence(1 .. S'Length);
  begin
    for I in S'Range loop
      Result(I - S'First + 1) := Decode (S(I));
    end loop;
    return Result;
  end Decode;

  function Encode (U : Unicode_Number) return Character is
  begin
    if not Is_Strict (U) then
      raise Constraint_Error;
    end if;
    return Character'Val (U);
  end Encode;

  function Encode (S : Unicode_Sequence) return String is
    Result : String(1 .. S'Length);
  begin
    for I in S'Range loop
      Result(I - S'First + 1) := Encode (S(I));
    end loop;
    return Result;
  end Encode;

end Aski;

