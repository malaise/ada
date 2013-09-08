-- Strict ANSI character definition and conversion to/from Unicode
with Ada.Characters.Latin_1;
with Unicode;
package Aski is
  -- Usefull redefinitions
  -- Unicode
  subtype Unicode_Number is Unicode.Unicode_Number;
  subtype Unicode_Sequence is Unicode.Unicode_Sequence;
  -- Constants
  Lf      : constant Character := Ada.Characters.Latin_1.Lf;
  Lf_Char : constant Character := Lf;
  Lf_Str  : constant String    := Lf & "";
  Nul     : constant Character := Ada.Characters.Latin_1.Nul;
  Del     : constant Character := Ada.Characters.Latin_1.Del;
  Delu    : constant Unicode_Number := Character'Pos (Del);


  -- Are strict ANSI the characters from 0 (Nul) to 127 (Del) included
  function Is_Strict (C : Character) return Boolean;
  function Is_Strict (S : String)    return Boolean;
  function Is_Strict (U : Unicode_Number)   return Boolean;
  function Is_Strict (S : Unicode_Sequence) return Boolean;

  -- Conversion from strict ANSII to Unicode and reverse
  -- Raise Constraint_Error if a character or unicode is not strict
  function Decode (C : Character) return Unicode_Number;
  function Decode (S : String)    return Unicode_Sequence;
  function Encode (U : Unicode_Number)   return Character;
  function Encode (S : Unicode_Sequence) return String;

end Aski;

