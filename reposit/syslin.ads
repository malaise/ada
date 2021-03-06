-- A is a square matrix and B is a vector of the same dimension as A
-- Resolution of A.X = B, where X is the vector of unknown values
generic

  type Number is digits <>;

package Syslin is

  -- The 3 sub types of indexes must have the same Length
  --  and be from 1 to Length
  type Vector is array (Positive range <>) of Number;
  type Matrix is array (Positive range <>, Positive range <>) of Number;

  -- The returned vector is indexed from 1 .. Length
  function Gauss (A : Matrix; B : Vector) return Vector;

  -- If the matrix is not a square, or if indexes do not have the same Length
  Dimension_Error   : exception;

  -- If the determinant is nul
  Null_Determinant : exception;

end Syslin;

