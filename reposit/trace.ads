-- Log messages in a file named _trace_<pid>
package Trace is

  -- Traces are activated at start-up
  procedure Activate (On : in Boolean := True);

  -- Traces the number of call and the message, if activated
  procedure Put (Message : in String; Date : in Boolean;
                 Flush : in Boolean := False);

end Trace;

