package Utils is

  -- Exception on CtrlC or SigTerm to abort the game
  Abort_Game : exception;

  -- Debug modes
  procedure Init;
  function Debug_Comm return Boolean;

end Utils;

