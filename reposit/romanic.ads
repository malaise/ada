package Romanic is

  -- The romanic typo
  -- Max is MMMCMXCIX (3999)
  Nb_Digits : constant := 7;
  subtype Digits_Range is Positive range 1 .. Nb_Digits;

  type Typo_Rec is record
    Typo : Character;
    Val  : Positive;
  end record;

  Typo_Def_Array : constant array (Digits_Range) of Typo_Rec := (
    ('I',     1),
    ('V',     5),
    ('X',    10),
    ('L',    50),
    ('C',   100),
    ('D',   500),
    ('M', 1_000));

  -- The corresponding arabic value
  subtype Arabic_Range is Positive range 1 .. 3999;

  -- Convert a romanic number into arabic
  -- May raise Invalid_Romanic
  function Romanic2Arabic (Romanic : in String) return Arabic_Range;
  Invalid_Romanic : exception;

  -- Convert an arabic number into romanic
  function Arabic2Romanic (Arabic : in Arabic_Range) return String;

end Romanic;

