-- Various utilities about hexadecimal notation
with Long_Longs;
package Hexa_Utils is

  -- Value of one hexadecimal digit
  subtype Hexa_Digit is Natural range 0 .. 15;

  -- Convert an hexadecimal character ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F')
  --  into its value (0 .. 15).
  -- Raises Constraint_Error if invalid character.
  function Char_To_Hexa (C : Character) return Hexa_Digit;

  -- Convert an hexadecimal digit into the corresponding character
  -- Lower case
  function Hexa_To_Char (H : Hexa_Digit) return Character;

  -- Image in hexadecimal of a Natural
  -- Lower case, no leading space (and no 16#...#)
  function Image (N : Natural) return String;
  function Image (N : Long_Longs.Ll_Natural) return String;

  -- Image in hexadecimal of a Natural, with head padded with gap to fit length
  -- Lower case
  -- Raises Constraint_Error if Image(N) > Len
  function Image (N : Natural;
                  Len : Positive; Gap : Character := '0') return String;
  function Image (N : Long_Longs.Ll_Natural;
                  Len : Positive; Gap : Character := '0') return String;

  -- Value of an hexadecimal string (without 16#...#)
  -- Str must be a valid image with no trailing spaces,
  --  leading spaces are skipped
  -- May raises Constraint_Error if Str is not valid or result is too large
  function Value (Str : String) return Natural;
  function Value (Str : String) return Long_Longs.Ll_Natural;

end Hexa_Utils;

