-- Divide A by B (integers) and return the rounded result
with My_Math;
function Roundiv (A, B : Integer) return Integer is
begin
  return Integer(My_Math.Roundiv (My_Math.Inte(A), My_Math.Inte(B)));
end Roundiv;

