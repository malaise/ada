package Errors is

  -- Exit Codes
  Init_Error : constant Natural := 1;
  Runtime_Error : constant Natural := 2;
  Signal : constant Natural := 3;

  -- Abort execution or error or signal
  Exit_Error, Exit_Signal : exception;

end Errors;

