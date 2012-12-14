-- Various utilities about hexadecimal notation
package Hexa_Utils is

  -- Value of one hexadecimal digit
  subtype Hexa_Digit is Natural range 0 .. 15;

  -- Convert an hexadecimal character ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F')
  --  into its value (0 .. 15).
  -- Raises Constraint_Error if invalid character.
  function Char_To_Hexa (C : Character) return Hexa_Digit;

  -- Value of a string representing the hexadecimal image of a natural
  -- Raises Constraint_Error if invalid character.
  function Value (Str : String) return Natural;

  -- Convert an hexadecimal digit into the corresponding character
  -- Lower case
  function Hexa_To_Char (H : Hexa_Digit) return Character;

  -- Image in hexadecimal of a Natural
  -- Lower case, no leading space
  function Image (N : Natural) return String;

  -- Image in hexadecimal of a Natural, with head padded with gap to fit length
  -- Lower case
  -- Raises Constraint_Error if Image(N) > Len
  function Image (N : Natural; Len : Positive; Gap : Character := '0')
           return String;

end Hexa_Utils;

