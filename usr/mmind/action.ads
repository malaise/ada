package ACTION is
  NO_MOUSE : exception;

  procedure INIT;

  -- True if start again, False if exit
  function PLAY return BOOLEAN;

end ACTION;
