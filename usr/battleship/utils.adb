with Environ;
package body Utils is


  -- Debug modes
  Dbg_Comm : Boolean := False;
  procedure Init is
  begin
    Dbg_Comm := Environ.Is_Yes ("BATTLESHIP_DEBUG_COMM");
  end Init;

  function Debug_Comm return Boolean is
  begin
    return Dbg_Comm;
  end Debug_Comm;

end Utils;

