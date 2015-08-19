-- Strict ANSI character definition and conversion to/from Unicode
with Unicode;
package Aski.Unicode is
  -- Usefull redefinitions
  -- Unicode
  subtype Unicode_Number is Standard.Unicode.Unicode_Number;
  subtype Unicode_Sequence is Standard.Unicode.Unicode_Sequence;

  -- Constants from Ada.Characters.Latin_1
  -- Nul (0)
  Nul_U : constant Unicode_Number := Character'Pos (Nul);

  -- End of transmission
  Eot_U : constant Unicode_Number := Character'Pos (Eot);

  -- Bell
  Bel_U : constant Unicode_Number := Character'Pos (Bel);

  -- Backspace
  Bs_U : constant Unicode_Number := Character'Pos (Bs);

  -- Horizontal tab
  Ht_U : constant Unicode_Number := Character'Pos (Ht);

  -- Line feed
  Lf_U  : constant Unicode_Number := Character'Pos (Lf);

  -- Form feed
  Ff_U  : constant Unicode_Number := Character'Pos (Ff);

  -- Carriage return
  Cr_U  : constant Unicode_Number := Character'Pos (Cr);

  -- Escape
  Esc_U : constant Unicode_Number := Character'Pos (Esc);

  -- Space
  Spc_U : constant Unicode_Number := Character'Pos (Spc);

  -- Delete
  Del_U : constant Unicode_Number := Character'Pos (Del);

  -- Are strict ANSI the characters from 0 (Nul) to 127 (Del) included
  function Is_Strict (U : Unicode_Number)   return Boolean;
  function Is_Strict (S : Unicode_Sequence) return Boolean;

  -- Conversion from strict ANSI to Unicode and reverse
  -- Raise Constraint_Error if a character or unicode is not strict
  function Decode (C : Character) return Unicode_Number;
  function Decode (S : String)    return Unicode_Sequence;
  function Encode (U : Unicode_Number)   return Character;
  function Encode (S : Unicode_Sequence) return String;

end Aski.Unicode;

