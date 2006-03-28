-- Rounds R at N digits.
-- If N is positive then it applies to the int part
-- else it applies to the frac part.
-- Ex R=990.2 N=2 -> 1000
--    R=5.118 N=-1 -> 5.1
--    R=5.118 N=-2 -> 5.12
with My_Math;
function Round_At (R : My_Math.Real; N : Integer) return My_Math.Real is
  P, T, I, F : My_Math.Real;
  Rounded_Frac : My_Math.Inte;
  use type My_Math.Real, My_Math.Inte;
begin
  P := 10.0 ** N;
  -- Move the "." at position N
  T := R / P;
  -- Separate Int and Frac
  I := My_Math.Int (T);
  F := My_Math.Frac (T);
  -- Round the Frac part, (this leads to -1, 0 or 1)
  Rounded_Frac := My_Math.Round (F);
  -- Add the result to the Int part
  I := I + My_Math.Real(Rounded_Frac);
  -- Restore the original "." position
  T := I * P;
  return T;
end Round_At;

