-- A boolean wth 3 values
package Trilean is

  pragma Warnings (Off, "redefinition of entity * in Standard");
  type Trilean is (False, True, Other);
  pragma Warnings (On, "redefinition of entity * in Standard");
  Maybe : constant Trilean := Other;

  -- Raises Constraint_Error if the Trilean is Other
  function Tri2Boo (Tri : Trilean) return Boolean;

  -- false -> False and True -> True
  function Boo2Tri (Boo : Boolean) return Trilean;

end Trilean;

