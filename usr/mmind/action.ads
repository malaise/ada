package Action is
  No_Mouse : exception;

  procedure Init;

  -- True if start again, False if exit
  function Play return Boolean;

end Action;

