-- Strict ANSI character definition and conversion to/from Unicode
with Ada.Characters.Latin_1;
with Unicode;
package Aski is
  -- Usefull redefinitions
  -- Unicode
  subtype Unicode_Number is Unicode.Unicode_Number;
  subtype Unicode_Sequence is Unicode.Unicode_Sequence;

  -- Constants from Ada.Characters.Latin_1
  -- Nul (0)
  Nul   : constant Character := Ada.Characters.Latin_1.Nul;
  Nul_C : constant Character := Nul;
  Nul_S : constant String := Nul & "";
  Nul_U : constant Unicode_Number := Character'Pos (Nul);

  -- End of transmission
  Eot   : constant Character := Ada.Characters.Latin_1.Eot;
  Eot_C : constant Character := Eot;
  Eot_S : constant String := Eot & "";
  Eot_U : constant Unicode_Number := Character'Pos (Eot);

  -- Bell
  Bel   : constant Character := Ada.Characters.Latin_1.Bel;
  Bel_C : constant Character := Bel;
  Bel_S : constant String := Bel & "";
  Bel_U : constant Unicode_Number := Character'Pos (Bel);

  -- Backspace
  Bs   : constant Character := Ada.Characters.Latin_1.Bs;
  Bs_C : constant Character := Bs;
  Bs_S : constant String := Bs & "";
  Bs_U : constant Unicode_Number := Character'Pos (Bs);

  -- Horizontal tab
  Ht   : constant Character := Ada.Characters.Latin_1.Ht;
  Ht_C : constant Character := Ht;
  Ht_S : constant String := Ht & "";
  Ht_U : constant Unicode_Number := Character'Pos (Ht);

  -- Line feed
  Lf    : constant Character := Ada.Characters.Latin_1.Lf;
  Lf_C  : constant Character := Lf;
  Lf_S  : constant String := Lf & "";
  Lf_U  : constant Unicode_Number := Character'Pos (Lf);

  -- Form feed
  Ff    : constant Character := Ada.Characters.Latin_1.Ff;
  Ff_C  : constant Character := Ff;
  Ff_S  : constant String := Ff & "";
  Ff_U  : constant Unicode_Number := Character'Pos (Ff);

  -- Carriage return
  Cr    : constant Character := Ada.Characters.Latin_1.Cr;
  Cr_C  : constant Character := Cr;
  Cr_S  : constant String := Cr & "";
  Cr_U  : constant Unicode_Number := Character'Pos (Cr);

  -- Escape
  Esc   : constant Character := Ada.Characters.Latin_1.Esc;
  Esc_C : constant Character := Esc;
  Esc_S : constant String := Esc & "";
  Esc_U : constant Unicode_Number := Character'Pos (Esc);

  -- Space
  Spc   : constant Character := Ada.Characters.Latin_1.Space;
  Spc_C : constant Character := Spc;
  Spc_S : constant String := Spc & "";
  Spc_U : constant Unicode_Number := Character'Pos (Spc);

  -- Delete
  Del   : constant Character := Ada.Characters.Latin_1.Del;
  Del_C : constant Character := Del;
  Del_S : constant String := Del & "";
  Del_U : constant Unicode_Number := Character'Pos (Del);

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

