package Trace is

  -- Traces are activated at start-up
  procedure Activate (On : in Boolean := True);

  -- Traces the number of call and the message, if activated
  procedure Put (Message : in String := "");

end Trace;
