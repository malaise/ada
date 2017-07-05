package body Aski is

  -- Are strict ANSI the characters from 0 (Nul) to 127 (Del) included
  function Is_Strict (C : Character) return Boolean is
  begin
    return C <= Del;
  end Is_Strict;

  function Is_Strict (S : String) return Boolean is
  begin
    return (for all C of S => Is_Strict (C));
  end Is_Strict;

end Aski;

