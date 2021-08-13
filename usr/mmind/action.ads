package Action is
  No_Mouse : exception;

  -- Show code or not (all games)
  procedure Init (Show_Codes : in Boolean);


  procedure End_Action;

  -- Return True if start again, False if exit
  function Play return Boolean;

end Action;

