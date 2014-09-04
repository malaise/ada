-- Is Val smaller than Crit modulo Last
-- True if Val < Crit and then Crit - Val < Last/2
--      or Val > Crit and then Val - Crit > Last/2
-- Raises Constraint_Error if Last <= 2
function Modulus_Smaller (Val, Crit : Natural;
                          Last : Positive) return Boolean is
  L2 : constant Positive := Last / 2;
begin
  if Last <= 2 then
    raise Constraint_Error;
  end if;
  return (if Val < Crit then Crit - Val < L2
          else Val - Crit > L2);
end Modulus_Smaller;

