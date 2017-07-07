with My_Math;
package body Rounds is
  -- Round F at nearest integer
  function Round (F : Float) return Integer is
    (Integer(My_Math.Round (My_Math.Real(F))));

  -- Trunc F at lower integer
  function Trunc (F : Float) return Integer is
    (Integer(My_Math.Trunc (My_Math.Real(F))));

  -- Divide A by B (integers) and return the rounded result
  function Roundiv (A, B : Integer) return Integer is
    (Integer(My_Math.Roundiv (My_Math.Inte(A), My_Math.Inte(B))));

  -- Rounds R at N digits.
  -- If N is positive then it applies to the int part
  -- else it applies to the frac part.
  -- Ex R=990.2 N=2 -> 1000
  --    R=5.118 N=-1 -> 5.1
  --    R=5.118 N=-2 -> 5.12
  function Round_At (F : Float; N : Integer) return Float is
    (Float(My_Math.Round_At (My_Math.Real(F), My_Math.Inte(N))));

end Rounds;

