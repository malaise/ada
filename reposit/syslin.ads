generic

  type NUMBER is digits <>;

package SYSLIN is
  -- A is a square matrix and B is a vector of same dimension
  -- Resolution of A.X=B, where X is the vector of unknown values

  -- The 3 sub types of indexes must have the same LENGTH
  --  and be from 1 to LENGTH
  type VECTOR is array(POSITIVE range <>) of NUMBER;
  type MATRIX is array(POSITIVE range <>, POSITIVE range <>) of NUMBER;

  -- The returned vector is indexed from 1 .. LENGTH
  function GAUSS (A : MATRIX; B : VECTOR) return VECTOR;

  -- If the matrix is not a square, or if indexes do not have same LENGTH
  DIMENSION_ERROR   : exception; 

  -- If the discriminent is nul
  DISCRIMINENT_ERROR : exception; 

end SYSLIN; 
