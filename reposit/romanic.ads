-- Conversion of romanic numbers (ex: VII) to and from arabic (ex: 7)
package Romanic is

  -- The romanic typo
  -------------------
  -- Longest supported is MMMMDCCCLXXXVIII (4888)
  -- Largest supported is MMMMCMXCIX (4999)
  type Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');
  subtype Index_Range is Positive range 1 .. 16;
  type Number is array (Index_Range range <>) of Digit;

  subtype Arabic is Positive range 1 .. 4999;

  -- Convert a romanic number into arabic
  -- May raise Invalid_Romanic
  function Romanic2Arabic (R : Number) return Arabic;
  Invalid_Romanic : exception;

  -- Convert an arabic number into romanic
  function Arabic2Romanic (A : Arabic) return Number;


  -- Convert Romanic from and to string
  -- May raise Invalid_Romanic
  function Image (R : Number) return String;
  function Value (S : String) return Number;

end Romanic;

