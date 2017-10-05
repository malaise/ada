-- Store messages is a queue in memory
package Trace.Queue is

  -- See the parent package Trace for general informations about the
  --  tracing mechanisms

  -- All the queue loggers of a process log in the same common queue, which is
  --  dump into the given flow, on request, on reception of Usr signal (see
  --  package Event_Mng), or at process termination
  -- The flow  can be set in the environment variable <Process>_TRACEFILE="file"
  --  where <Process> is the process name (no path, non alphanum characters
  --         replaced by '_')
  --        file is "stdout", "stderr", "async_stdout", "async_stderr",
  --         or any file name (see Output_Flows), possibly with
  --         ${PID}, ${CMD}, ${HOST} or ${DATE}, which are expanded.
  --         Default is stderr.

  -- A queuer of traces
  type Queue_Logger is tagged private;

  -- Initialize the logger, either with a name or anonymous
  --  and set its mask from ENV
  -- If Name is not empty and <proc>_TRACE_<Name> is set (even empty)
  -- or Name is empty and <proc>_TRACE is set, then the mask is set to
  --  Fatal|Error|value
  -- If none is set then the mask is set to the global value got from
  --  <Process>_TRACE_ALL if set, or Fatal|Error by default
  -- Each further call to Init have no effect,
  procedure Init (A_Logger : in out Queue_Logger; Name : in String := "");
  -- Change the name of the logger and reset its mask
  -- Also reset its flush mode
  procedure Reset (A_Logger : in out Queue_Logger; Name : in String);

  -- Return True if logger is init
  function Is_Init (A_Logger : Queue_Logger) return Boolean;
  -- Return name of logger, raise Not_Init if is not init
  Not_Init : exception;
  function Name (A_Logger : Queue_Logger) return String;

  -- Set / get / add / del severities
  -- Raise Not_Init if logger is not init
  procedure Set_Mask (A_Logger : in out Queue_Logger; Mask : in Severities);
  function  Get_Mask (A_Logger : in out Queue_Logger) return Severities;
  procedure Add_Mask (A_Logger : in out Queue_Logger; Mask : in Severities);
  procedure Del_Mask (A_Logger : in out Queue_Logger; Mask : in Severities);

-- Check if a severity is active
  -- Raise Not_Init if logger is not init
  function Is_On (A_Logger : in out Queue_Logger;
                  Severity : in Severities) return Boolean;
  function Fatal_On   (A_Logger : in out Queue_Logger) return Boolean;
  function Error_On   (A_Logger : in out Queue_Logger) return Boolean;
  function Warning_On (A_Logger : in out Queue_Logger) return Boolean;
  function Info_On    (A_Logger : in out Queue_Logger) return Boolean;
  function Debug_On   (A_Logger : in out Queue_Logger) return Boolean;

  -- Log a message of a given severity (or even several severity levels)
  -- Calling it on a logger not initialized implicitly init it with Name
  procedure Log (A_Logger : in out Queue_Logger;
                 Severity : in Severities;
                 Message  : in String;
                 Name     : in String := "");
  procedure Log_Fatal   (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "");
  procedure Log_Error   (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "");
  procedure Log_Warning (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "");
  procedure Log_Info    (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "");
  procedure Log_Debug   (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "");

  -- Flush the whole queue
  procedure Flush;

  -- By default, Errors (Fatal & Error) are directly logged and flushed
  -- on stderr
  Errors_On_Stderr : Boolean := True;
  Flush_Stderr : Boolean := True;

private

  -- Logger
  type Queue_Logger is tagged record
    Inited : Boolean := False;
    Name : As.U.Asu_Us;
    Mask : Severities := 0;
  end record;

end Trace.Queue;

