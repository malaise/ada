-- Divide A by B (integers) and return the rounded result
with My_Math;
package Roundiv is


  function "/" (A, B : Integer) return Integer;

  function "/" (A, B : My_Math.Inte) return My_Math.Inte;

end Roundiv;

