-- Trunc A at lower integer
with My_Math;
function Trunc (A : Integer) return Integer is
begin
  return Integer(My_Math.Trunc (My_Math.Real(A)));
end Trunc;

