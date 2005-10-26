with Ada.Command_Line;
package body Loc_Arg is

  -- returns 1 if the current prog has 1 arg (0 if no arg)
  function Count return Natural is
  begin
    return Ada.Command_Line.Argument_Count;
  end Count;

  -- returns n th parameter of current prog (prog name if n=0)
  function Data (Pos : Natural) return String is
  begin
    if Pos = 0 then
      return Ada.Command_Line.Command_Name;
    else
      return Ada.Command_Line.Argument(Pos);
    end if;
  end Data;

end Loc_Arg;

