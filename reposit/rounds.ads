-- Various rounding and tuncating operations
-- See My_Math for details
package Rounds is
  -- Round F at nearest integer
  function Round (F : Float) return Integer;

  -- Trunc F at lower integer
  function Trunc (F : Float) return Integer;

  -- Divide A by B (integers) and return the rounded result
  function Roundiv (A, B : Integer) return Integer;

  -- Rounds F at N digits.
  -- If N is positive then it applies to the int part
  -- else it applies to the frac part.
  -- Ex F=990.2 N=2 -> 1000
  --    F=5.118 N=-1 -> 5.1
  --    F=5.118 N=-2 -> 5.12
  function Round_At (F : Float; N : Integer) return Float;

end Rounds;

