-- Log messages in a file named _trace_<pid>
package Trace is

  -- Traces are activated at start-up
  procedure Activate (On : in Boolean := True);

  -- If activated, trace the number of call, possibly the date, and the message
  procedure Put (Message : in String;
                 Date : in Boolean;
                 Flush : in Boolean := False);

end Trace;

