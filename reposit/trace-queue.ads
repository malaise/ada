-- Store messages is a queue in memory, then flush in a logger
with Trace.Loggers;
package Trace.Queue is

  -- See the parent package Trace for general informations about the
  --  tracing mechanisms

  -- All the queue loggers of a process log in the same common queue, which is
  --  flushed into the flow of Trace.Loggers,
  --  either on request, on reception of Usr1 signal (see package Event_Mng),
  --  or at process termination

  -- A queue of traces, same operations as Trace.Loggers.Logger
  type Queue_Logger is new Trace.Loggers.Logger with private;

  -- Queue a message of a given severity (or even several severity levels)
  overriding procedure Log (A_Logger : in out Queue_Logger;
                 Severity : in Severities;
                 Message  : in String;
                 Name     : in String := "");
  overriding   procedure Log_Fatal   (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "");
  overriding procedure Log_Error   (A_Logger : in out Queue_Logger;
                       Message  : in String;
                       Name     : in String := "");
  overriding procedure Log_Warning (A_Logger : in out Queue_Logger;
                       Message  : in String;
                       Name     : in String := "");
  overriding procedure Log_Info    (A_Logger : in out Queue_Logger;
                       Message  : in String;
                       Name     : in String := "");
  overriding procedure Log_Debug   (A_Logger : in out Queue_Logger;
                       Message  : in String;
                       Name     : in String := "");

  -- N.B. Set_Flush only affects flushing stderr if Errors_On_Stderr

  -- Flush logs of a logger; in fact flushes the whole queue
  overriding procedure Flush (A_Logger : in out Queue_Logger);

  -- Flush the queue to the flow of loggers
  procedure Flush;

private

  -- Logger, used for operations on the mask and attributes
  type Queue_Logger is new Trace.Loggers.Logger with null record;

end Trace.Queue;

