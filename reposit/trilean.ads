package Trilean is

  -- A boolean wth 3 values
  type Trilean is (False, True, Other);
  Maybe : constant Trilean := Other;

  -- Raises Constraint_Error if the Trilean is Other
  function Tri2Boo (Tri : Trilean) return Boolean;

  -- false -> False and True -> True
  function Boo2Tri (Boo : Boolean) return Trilean;

end Trilean;

