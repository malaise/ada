package Action is
  No_Mouse : exception;

  procedure Init;

  procedure End_Action;

  -- True if start again, False if exit
  function Play return Boolean;

end Action;

