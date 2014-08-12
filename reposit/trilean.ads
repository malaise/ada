-- A boolean wth 3 values
package Trilean is

  -- Definition of values
  -----------------------
  -- True and False are defined in Standard
  pragma Warnings (Off, "redefinition of entity * in Standard");
  type Trilean is (False, True, Other);
  pragma Warnings (On, "redefinition of entity * in Standard");
  Maybe : constant Trilean := Other;

  -- Conversions between Boolean and trilean
  ------------------------------------------
  -- Raises Constraint_Error if Val is Other
  function Tri2Boo (Val : Trilean) return Boolean;

  -- False -> False and True -> True
  function Boo2Tri (Val : Boolean) return Trilean;

  -- Other operations
  -------------------
  -- Image (Mixed_Str)
  function Image (Val : Trilean) return String;

end Trilean;

