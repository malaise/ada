package MENU2 is

  procedure MAIN_SCREEN (DATA_CHANGED : in BOOLEAN);

  -- Is the curve stopped (can we exit)
  function CURVED_STOPPED return BOOLEAN;
end MENU2;
