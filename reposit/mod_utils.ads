-- Modulus utilities
with Long_Longs;
package Mod_Utils is

  -- Is Val smaller than Crit modulo Last
  -- True if Val < Crit and then Crit - Val < Last/2
  --      or Val > Crit and then Val - Crit > Last/2
  -- Raises Constraint_Error if Val < 0 or Crit < 0 or if Last <= 2
  generic
    type Int is range <>;
  function Int_Smaller (Val, Crit : Int;
                        Last : Int) return Boolean;
  generic
    type Modulus is mod <>;
  function Mod_Smaller (Val, Crit : Modulus;
                        Last : Modulus) return Boolean;
  function Smaller (Val, Crit : Natural;
                    Last : Positive) return Boolean;
  function Smaller (Val, Crit : Long_Longs.Ll_Natural;
                    Last : Long_Longs.Ll_Positive) return Boolean;
  function Smaller (Val, Crit : Long_Longs.Llu_Natural;
                    Last : Long_Longs.Llu_Natural) return Boolean;

  -- Distance between two naturals
  -- if A <= B then B - A else A - B
  -- Raises Constraint_Error if A < 0 or B < 0
  generic
    type Int is range <>;
  function Int_Dist (A, B : Int) return Int;
  generic
    type Modulus is mod <>;
  function Mod_Dist (A, B : Modulus) return Modulus;
  function Dist (A, B : Natural) return Natural;
  function Dist (A, B : Long_Longs.Ll_Natural) return Long_Longs.Ll_Natural;
  function Dist (A, B : Long_Longs.Llu_Natural) return Long_Longs.Llu_Natural;

end Mod_Utils;

