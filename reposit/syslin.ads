generic

  type Number is digits <>;

package Syslin is
  -- A is a square matrix and B is a vector of same dimension
  -- Resolution of A.X=B, where X is the vector of unknown values

  -- The 3 sub types of indexes must have the same LENGTH
  --  and be from 1 to LENGTH
  type Vector is array(Positive range <>) of Number;
  type Matrix is array(Positive range <>, Positive range <>) of Number;

  -- The returned vector is indexed from 1 .. LENGTH
  function Gauss (A : Matrix; B : Vector) return Vector;

  -- If the matrix is not a square, or if indexes do not have same LENGTH
  Dimension_Error   : exception; 

  -- If the discriminent is nul
  Discriminent_Error : exception; 

end Syslin; 
