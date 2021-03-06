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
  package body Int_Image is
    -- Max of a Int L
    -- If it is positive then L
    -- Else (negative) abs (L) - 1
    function Max_Of (L : Int) return Long_Longs.Ll_Natural is
       -- Ex: 32767 => 32767
      (if L > 0 then Long_Longs.Ll_Integer (L)
       -- Ex: -32768 => 32767
       else abs (Long_Longs.Ll_Integer (L) + 1) );

    -- Max value among Int'First and Int'Last
    Max_Set : Boolean := False;
    -- Set Max_Len once, at first call
    Max_Len : Long_Longs.Ll_Natural;
    procedure Set_Max is
      Max_First, Max_Last, Max_Val : Long_Longs.Ll_Natural;
      use My_Math;
    begin
      if Max_Set then
        return;
      end if;
      Max_First := Max_Of (Int'First);
      Max_Last  := Max_Of (Int'Last );
      Max_Val := (if Max_First >= Max_Last then Max_First else Max_Last);
      -- Len to show Int'First and Int'Last in hexa
      Max_Len := (if Max_Val /= 0 then
                    Trunc (Ln (Real (Max_Val)) / Ln (16.0) ) + 1
                  else 1);
      Max_Set := True;
    end Set_Max;

    -- Lower case, no leading space
    function Image (I : Int) return String is
      -- Current value and result
      Val : Long_Longs.Ll_Integer := Long_Longs.Ll_Integer (I);
      Res : As.U.Asu_Us;
      use Bit_Ops;
    begin
      Set_Max;
      -- Extract each hexa digit, from smallest to largest
      -- Will stop at Max_Len for I < 0
      for J in 1 .. Max_Len loop
        Res.Prepend (Hexa_To_Char (Natural (Val and 16#F#)));
        Val := Shr (Val, 4);
        -- Will stop ASAP for I >= 0
        exit when Val = 0;
      end loop;
      return Res.Image;
    end Image;
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
  package Nat_Image is new Int_Image (Natural);
  function Image (N : Natural) return String renames Nat_Image.Image;
  package Llnat_Image is new Int_Image (Long_Longs.Ll_Natural);
  function Image (N : Long_Longs.Ll_Natural) return String
           renames Llnat_Image.Image;
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
  function Mod_Value (Str : String) return Modulus is
    Start : Positive;
    Res : Modulus := 0;
    subtype Pos is Modulus range 1 .. Modulus'Last;
    Mul : Pos := 1;
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
    -- Convert: extract digits from smallest to highest weight
    for I in reverse Str'Range loop
      Res := Res + Modulus (Char_To_Hexa (Str(I))) * Mul;
      if I /= Start then
        Mul := 16 * Mul;
        exit when Str(I - 1) = ' ';
      end if;
    end loop;
    return Res;
  end Mod_Value;
  function Val_Llu_Natual is new Mod_Value (Long_Longs.Llu_Natural);
  function Value (Str : String) return Long_Longs.Llu_Natural
           renames Val_Llu_Natual;

  function Value (Str : String) return Long_Longs.Ll_Natural is
  begin
    return Long_Longs.Ll_Natural (Val_Llu_Natual (Str));
  end Value;

  function Value (Str : String) return Natural is
  begin
    return Natural (Val_Llu_Natual (Str));
  end Value;

end Hexa_Utils;

