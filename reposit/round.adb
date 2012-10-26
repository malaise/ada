-- Round A at nearest integer
with My_Math;
function Round (A : Integer) return Integer is
begin
  return Integer(My_Math.Round (My_Math.Real(A)));
end Round;

