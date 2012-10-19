-- Divide A by B (integers) and return the rounded result
package body Roundiv is

  function "/" (A, B : Integer) return Integer is
  begin
    return Integer(Roundiv."/" (My_Math.Inte(A), My_Math.Inte(B)));
  end "/";

  function "/" (A, B : My_Math.Inte) return My_Math.Inte is
    use type My_Math.Real;
  begin
    return My_Math.Round (My_Math.Real(A) / My_Math.Real(B));
  end "/";

end Roundiv;

