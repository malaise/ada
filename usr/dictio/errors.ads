package Errors is

  -- Exit Codes
  Ok : constant Natural := 0;
  Init_Error : constant Natural := 1;
  Runtime_Error : constant Natural := 2;

  -- Abort execution
  Exit_Error : exception;

end Errors;

