with As.U, Bit_Ops, My_Math;
package body Hexa_Utils is
  -- Convert an hexadecimal character (0..9 | 'a' .. 'f' | 'A' .. 'F')
  --  into its value (0 .. 15).
  -- Raises Constraint_Error if invalid character.
  function Char_To_Hexa (C : Character) return Hexa_Digit is
  begin
    case C is
      when '0' .. '9' =>
        return Character'Pos(C) - Character'Pos('0');
      when 'a' .. 'f' =>
        return Character'Pos(C) - Character'Pos('a') + 16#0A#;
      when 'A' .. 'F' =>
        return Character'Pos(C) - Character'Pos('A') + 16#0A#;
      when others =>
        raise Constraint_Error;
    end case;
  end Char_To_Hexa;

  -- Convert an hexadecimal digit into the corresponding character
  -- Lower case
  function Hexa_To_Char (H : Hexa_Digit) return Character is
    (if H < 10 then Character'Val (Character'Pos('0') + H)
     else Character'Val (Character'Pos('a') + H - 10));


  -- Image in hexadecimal of an integer
  -- Lower case, no leading space
  function Int_Image (I : Int) return String is
    -- Max of a Int L
    -- If it is positive then L
    -- Else (negative) abs (L) - 1
    function Max_Of (L : Int) return Long_Longs.Ll_Natural is
       -- Ex: 32767 => 32767
      (if L > 0 then Long_Longs.Ll_Integer (L)
       -- Ex: -32768 => 32767
       else abs (Long_Longs.Ll_Integer (L) + 1) );

    -- Max value among Int'First and Int'Last
    Max_First : constant Long_Longs.Ll_Natural := Max_Of (Int'First);
    Max_Last  : constant Long_Longs.Ll_Natural := Max_Of (Int'Last );
    Max       : constant Long_Longs.Ll_Natural
              := (if Max_First >= Max_Last then Max_First else Max_Last);
    -- Len to show Int'First and Int'Last in hexa
    use My_Math;
    Max_Len : constant Long_Longs.Ll_Natural
            := (if Max /= 0 then Trunc (Ln (Real (Max)) / Ln (16.0) ) + 1
                else 1);
    -- Current value and result
    Val : Long_Longs.Ll_Integer := Long_Longs.Ll_Integer (I);
    Res : As.U.Asu_Us;
    use Bit_Ops;
  begin
    -- Extract each hexa digit, from smallest to largest
    -- Will stop at Max_Len for I < 0
    for J in 1 .. Max_Len loop
      Res.Prepend (Hexa_To_Char (Natural (Val and 16#F#)));
      Val := Shr (Val, 4);
      -- Will stop ASAP for I >= 0
      exit when Val = 0;
    end loop;
    return Res.Image;
  end Int_Image;

  -- Image of a modulus
  function Mod_Image (I : Modulus) return String is
    V : Modulus := I;
    Res : As.U.Asu_Us;
  begin
    loop
      Res.Prepend (Hexa_To_Char (Natural (V rem 16)));
      V := V / 16;
      exit when V = 0;
    end loop;
    return Res.Image;
  end Mod_Image;

  -- Image of naturals
  function Nat_Image is new Int_Image (Natural);
  function Image (N : Natural) return String renames Nat_Image;
  function Llnat_Image is new Int_Image (Long_Longs.Ll_Natural);
  function Image (N : Long_Longs.Ll_Natural) return String renames Llnat_Image;
  function Llmod_Image is new Mod_Image (Long_Longs.Llu_Natural);
  function Image (N : Long_Longs.Llu_Natural) return String
                 renames Llmod_Image;

  -- Image in hexadecimal of a Natural, padded with '0' to fit length
  -- Lower case
  -- Raises Constraint_Error if Image(N) > Len
  function Image (N : Natural;
                  Len : Positive; Gap : Character := '0') return String is
    Result : String (1 .. Len) := (others => Gap);
    Imag : constant String := Image (N);
  begin
    Result (Result'Last - Imag'Length + 1 .. Result'Last) := Imag;
    return Result;
  end Image;
  function Image (N : Long_Longs.Ll_Natural;
                  Len : Positive; Gap : Character := '0') return String is
    Result : String (1 .. Len) := (others => Gap);
    Imag : constant String := Image (N);
  begin
    Result (Result'Last - Imag'Length + 1 .. Result'Last) := Imag;
    return Result;
  end Image;
  function Image (N : Long_Longs.Llu_Natural;
                  Len : Positive; Gap : Character := '0') return String is
    Result : String (1 .. Len) := (others => Gap);
    Imag : constant String := Image (N);
  begin
    Result (Result'Last - Imag'Length + 1 .. Result'Last) := Imag;
    return Result;
  end Image;

  -- Value of an hexadecimal string (without 16#...#)
  -- Str must be a valid image with no trailing spaces,
  --  leading spaces are skipped
  -- May raises Constraint_Error if Str is not valid or result is too large
  function Value (Str : String) return Natural is
    Start : Positive;
    Res : Natural := 0;
    Mul : Positive := 1;
  begin
    if Str = "" then
      raise Constraint_Error;
    end if;
    -- Locate first significant char (skip leading spaces)
    for I in Str'Range loop
      if Str(I) /= ' ' then
        Start := I;
        exit;
      end if;
    end loop;
    for I in reverse Str'Range loop
      Res := Res + Char_To_Hexa (Str(I)) * Mul;
      if I /= Start then
        Mul := 16 * Mul;
        exit when Str(I - 1) = ' ';
      end if;
    end loop;
    return Res;
  end Value;
  function Value (Str : String) return Long_Longs.Ll_Natural is
    Start : Positive;
    Res : Long_Longs.Ll_Natural := 0;
    Mul : Long_Longs.Ll_Positive := 1;
  begin
    if Str = "" then
      raise Constraint_Error;
    end if;
    -- Locate first significant char (skip leading spaces)
    for I in Str'Range loop
      if Str(I) /= ' ' then
        Start := I;
        exit;
      end if;
    end loop;
    for I in reverse Str'Range loop
      Res := Res + Long_Longs.Ll_Natural(Char_To_Hexa (Str(I))) * Mul;
      if I /= Start then
        Mul := 16 * Mul;
        exit when Str(I - 1) = ' ';
      end if;
    end loop;
    return Res;
  end Value;
  function Value (Str : String) return Long_Longs.Llu_Natural is
    Start : Positive;
    Res : Long_Longs.Llu_Natural := 0;
    Mul : Long_Longs.Llu_Positive := 1;
    use type Long_Longs.Llu_Natural;
  begin
    if Str = "" then
      raise Constraint_Error;
    end if;
    -- Locate first significant char (skip leading spaces)
    for I in Str'Range loop
      if Str(I) /= ' ' then
        Start := I;
        exit;
      end if;
    end loop;
    for I in reverse Str'Range loop
      Res := Res + Long_Longs.Llu_Natural(Char_To_Hexa (Str(I))) * Mul;
      if I /= Start then
        Mul := 16 * Mul;
        exit when Str(I - 1) = ' ';
      end if;
    end loop;
    return Res;
  end Value;

end Hexa_Utils;

