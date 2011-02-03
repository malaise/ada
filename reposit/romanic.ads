-- Conversion of romanic numbers (ex: VII) to anf from arabic (ex: 7)
package Romanic is

  -- The romanic typo
  -------------------
  -- Longest supported is MMMMDCCCLXXXVIII (4888)
  -- Biggest supported is MMMMCMXCIX (4999)

  subtype Arabic_Range is Positive range 1 .. 4999;

  -- Convert a romanic number into arabic
  -- May raise Invalid_Romanic
  function Romanic2Arabic (Romanic : in String) return Arabic_Range;
  Invalid_Romanic : exception;

  -- Convert an arabic number into romanic
  function Arabic2Romanic (Arabic : in Arabic_Range) return String;

end Romanic;

