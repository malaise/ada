-- Modulus utilities
-- with Long_Longs;

package body Mod_Utils is

  -- We don't instanciate the generics for integer types in order to benefit
  --  from the subtypes (Natural, Positive) definitions

  -- Is Val smaller than Crit modulo Last
  -- True if Val < Crit and then Crit - Val < Last/2
  --      or Val > Crit and then Val - Crit > Last/2
  -- Raises Constraint_Error if Val < 0 or Crit < 0 or if Last <= 2
  function Int_Smaller (Val, Crit : Int;
                        Last : Int) return Boolean is
    L2 : constant Int := Last / 2;
  begin
    if Val < 0 or else Crit < 0 or else Last <= 2 then
      raise Constraint_Error;
    end if;
    return (if Val < Crit then Crit - Val < L2
            else Val - Crit > L2);
  end Int_Smaller;

  function Mod_Smaller (Val, Crit : Modulus;
                        Last : Modulus) return Boolean is
    L2 : constant Modulus := Last / 2;
  begin
    if Last <= 2 then
      raise Constraint_Error;
    end if;
    return (if Val < Crit then Crit - Val < L2
            else Val - Crit > L2);
  end Mod_Smaller;

  function Smaller (Val, Crit : Natural;
                    Last : Positive) return Boolean is
    L2 : constant Positive := Last / 2;
  begin
    if Last <= 2 then
      raise Constraint_Error;
    end if;
    return (if Val < Crit then Crit - Val < L2
            else Val - Crit > L2);
  end Smaller;

  function Smaller (Val, Crit : Long_Longs.Ll_Natural;
                    Last : Long_Longs.Ll_Positive) return Boolean is
    L2 : constant Long_Longs.Ll_Positive := Last / 2;
  begin
    if Last <= 2 then
      raise Constraint_Error;
    end if;
    return (if Val < Crit then Crit - Val < L2
            else Val - Crit > L2);
  end Smaller;

  function Llu_Smaller is new Mod_Smaller (Long_Longs.Llu_Natural);
  function Smaller (Val, Crit : Long_Longs.Llu_Natural;
                    Last : Long_Longs.Llu_Natural)
           return Boolean renames Llu_Smaller;

  -- Distance between two naturals
  -- if A <= B then B - A else A - B
  -- Raises Constraint_Error if A < 0 or B < 0
  function Int_Dist (A, B : Int) return Int is
  begin
    if A < 0 or else B < 0 then
      raise Constraint_Error;
    end if;
    return (if A <= B then B - A else A - B);
  end Int_Dist;

  function Mod_Dist (A, B : Modulus) return Modulus is
  begin
    return (if A <= B then B - A else A - B);
  end Mod_Dist;

  function Dist (A, B : Natural) return Natural is
  begin
    return (if A <= B then B - A else A - B);
  end Dist;

  function Dist (A, B : Long_Longs.Ll_Natural) return Long_Longs.Ll_Natural is
  begin
    return (if A <= B then B - A else A - B);
  end Dist;

  function Llu_Dist is new Mod_Dist (Long_Longs.Llu_Natural);
  function Dist (A, B : Long_Longs.Llu_Natural)
           return Long_Longs.Llu_Natural renames Llu_Dist;

end Mod_Utils;

