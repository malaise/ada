package Loc_Arg is

  -- returns 1 if the current prog has 1 arg (0 if no arg)
  function Count return Natural;

  -- returns n th parameter of current prog (prog name if n=0)
  function Data (Pos : Natural) return String;

end Loc_Arg;
