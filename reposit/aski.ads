-- Strict ANSI character definition and conversion to/from Unicode
with Ada.Characters.Latin_1;
package Aski is

  -- Constants from Ada.Characters.Latin_1
  -- Nul (0)
  Nul   : constant Character := Ada.Characters.Latin_1.Nul;
  Nul_C : constant Character := Nul;
  Nul_S : constant String := Nul & "";

  -- End of transmission
  Eot   : constant Character := Ada.Characters.Latin_1.Eot;
  Eot_C : constant Character := Eot;
  Eot_S : constant String := Eot & "";

  -- Bell
  Bel   : constant Character := Ada.Characters.Latin_1.Bel;
  Bel_C : constant Character := Bel;
  Bel_S : constant String := Bel & "";

  -- Backspace
  Bs   : constant Character := Ada.Characters.Latin_1.Bs;
  Bs_C : constant Character := Bs;
  Bs_S : constant String := Bs & "";

  -- Horizontal tab
  Ht   : constant Character := Ada.Characters.Latin_1.Ht;
  Ht_C : constant Character := Ht;
  Ht_S : constant String := Ht & "";

  -- Line feed
  Lf    : constant Character := Ada.Characters.Latin_1.Lf;
  Lf_C  : constant Character := Lf;
  Lf_S  : constant String := Lf & "";

  -- Vertical tab
  Vt   : constant Character := Ada.Characters.Latin_1.Vt;
  Vt_C : constant Character := Vt;
  Vt_S : constant String := Vt & "";

  -- Form feed
  Ff    : constant Character := Ada.Characters.Latin_1.Ff;
  Ff_C  : constant Character := Ff;
  Ff_S  : constant String := Ff & "";

  -- Carriage return
  Cr    : constant Character := Ada.Characters.Latin_1.Cr;
  Cr_C  : constant Character := Cr;
  Cr_S  : constant String := Cr & "";

  -- Escape
  Esc   : constant Character := Ada.Characters.Latin_1.Esc;
  Esc_C : constant Character := Esc;
  Esc_S : constant String := Esc & "";

  -- Space
  Spc   : constant Character := Ada.Characters.Latin_1.Space;
  Spc_C : constant Character := Spc;
  Spc_S : constant String := Spc & "";

  -- Delete
  Del   : constant Character := Ada.Characters.Latin_1.Del;
  Del_C : constant Character := Del;
  Del_S : constant String := Del & "";

  -- Are strict ANSI the characters from 0 (Nul) to 127 (Del) included
  function Is_Strict (C : Character) return Boolean;
  function Is_Strict (S : String)    return Boolean;

end Aski;

