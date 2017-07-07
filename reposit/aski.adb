package body Aski is

  -- Are strict ANSI the characters from 0 (Nul) to 127 (Del) included
  function Is_Strict (C : Character) return Boolean is (C <= Del);

  function Is_Strict (S : String) return Boolean is
    (for all C of S => Is_Strict (C));

end Aski;

