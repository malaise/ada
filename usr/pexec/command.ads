package Command is

  -- Print command line syntax
  procedure Print_Usage;

  -- Parse the command line. Must be called once and only once,
  --  before other calls (or Already_Parsed is raised);
  -- No_Command may be raised here
  -- Picks the first -adcf option, then builds the command_line argument
  procedure Parse (No_Action,      No_Name_Of_Dir,
                   Not_In_Current, First_Level_Only, Leaves_Only,
                   No_Stop_On_Error, Follow_Links : out Boolean);

  -- These 2 must be called after Parse (or Not_Parsed is raised).
  -- They give info about the command_line (Nth_Command may return "").
  function Nbre_Commands return Natural;

  function Nth_Command (N : Positive) return String;

  Already_Parsed, Not_Parsed : exception;
  No_Command : exception;

end Command;

