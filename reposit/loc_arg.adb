with Ada.Command_Line;
package body Loc_Arg is

  -- Return the number of arguments of current program (0 if no argument)
  function Count return Natural is
  begin
    return Ada.Command_Line.Argument_Count;
  end Count;

  -- Return the Nth argument of current program (program name if Pos = 0)
  function Data (Pos : Natural) return String is
  begin
    if Pos = 0 then
      return Ada.Command_Line.Command_Name;
    else
      return Ada.Command_Line.Argument(Pos);
    end if;
  end Data;

end Loc_Arg;

