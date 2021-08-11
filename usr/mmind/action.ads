package Action is
  No_Mouse : exception;

  procedure Init;

  procedure End_Action;

  -- Show code or not (for first run)
  -- Return True if start again, False if exit
  function Play (Show_Code : Boolean) return Boolean;

end Action;

