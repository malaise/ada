package TRACE is

  -- Traces are activated at start-up
  procedure ACTIVATE (ON : in BOOLEAN := TRUE);

  -- Traces the number of call and the message, if activated
  procedure PUT (MESSAGE : in STRING := "");

end TRACE;
