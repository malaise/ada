with As.U;
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
  begin
    return (if H < 10 then Character'Val (Character'Pos('0') + H)
            else Character'Val (Character'Pos('a') + H - 10));
  end Hexa_To_Char;

  -- Image in hexadecimal of a Natural
  -- Lower case, no leading space
  function Image (N : Natural) return String is
    V : Natural := N;
    Res : As.U.Asu_Us;
  begin
    loop
      Res.Prepend (Hexa_To_Char (V rem 16));
      V := V / 16;
      exit when V = 0;
    end loop;
    return Res.Image;
  end Image;
  function Image (N : Long_Longs.Ll_Natural) return String is
    V : Long_Longs.Ll_Natural := N;
    Res : As.U.Asu_Us;
  begin
    loop
      Res.Prepend (Hexa_To_Char (Natural (V rem 16)));
      V := V / 16;
      exit when V = 0;
    end loop;
    return Res.Image;
  end Image;

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

end Hexa_Utils;

