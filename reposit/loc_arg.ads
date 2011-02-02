package Loc_Arg is

  -- Return the number of arguments of current program (0 if no argument)
  function Count return Natural;

  -- Return the Nth argument of current program (program name if Pos = 0)
  function Data (Pos : Natural) return String;

end Loc_Arg;

