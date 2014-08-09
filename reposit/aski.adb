package body Aski is

  -- Are strict ANSI the characters from 0 (Nul) to 127 (Del) included
  function Is_Strict (C : Character) return Boolean is
  begin
    return C <= Del;
  end Is_Strict;

  function Is_Strict (S : String) return Boolean is
  begin
    for I in S'Range loop
      if not Is_Strict (S(I)) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Strict;

end Aski;

