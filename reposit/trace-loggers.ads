-- Log messages in a flow or file
with Ada.Finalization;
with As.U;
package Trace.Loggers is

  -- All the logers trace in a given flow, set in environment variable
  --  <Process>_TRACEFILE="file"
  --  where <Process> is the process name (no path)
  --        file is "stdout", "stderr", "async_stdout", "async_stderr",
  --        or any file name (see Output_Flows), possibly with
  --        ${PID}, ${CMD}, ${HOST} or ${DATE}, which are expanded.
  --        Default is stderr.

  -- A logger of traces
  type Logger is tagged private;

  -- Initialize the logger, with a name or anonymous
  --  and set its mask from ENV
  -- Setting a name (even empty) activates the logger
  -- If Name is not empty and <proc>_TRACE_<Name> is set (even empty)
  -- or Name is empty and <proc>_TRACE is set, then the mask is set to
  --  Fatal|Error|value
  -- If none is set then the mask is set to the global value got from
  --  <Process>_TRACE_ALL if set, or Fatal|Error by default
  -- Each further call to Init have no effect,
  procedure Init (A_Logger : in out Logger; Name : in String := "");
  -- Set (init) or change the name of the logger
  -- Also reset its flush mode
  procedure Reset (A_Logger : in out Logger; Name : in String);

  -- Return True if logger is init
  function Is_Init (A_Logger : Logger) return Boolean;
  -- Return name of logger, raise Not_Init if is not init
  Not_Init : exception;
  function Name (A_Logger : Logger) return String;

  -- Set / get / add severities
  -- Raise Not_Init if logger is not init
  procedure Set_Mask (A_Logger : in out Logger; Mask : in Severities);
  procedure Add_Mask (A_Logger : in out Logger; Mask : in Severities);
  function  Get_Mask (A_Logger : in out Logger) return Severities;

  -- Check if a severity is active
  -- Raise Not_Init if logger is not init
  function Is_On (A_Logger : in out Logger;
                  Severity : in Severities) return Boolean;
  function Fatal_On   (A_Logger : in out Logger) return Boolean;
  function Error_On   (A_Logger : in out Logger) return Boolean;
  function Warning_On (A_Logger : in out Logger) return Boolean;
  function Info_On    (A_Logger : in out Logger) return Boolean;
  function Debug_On   (A_Logger : in out Logger) return Boolean;

  -- Log a message of a given severity (note that it can have several
  --  severities)
  -- Calling it on a logger not initialized implicitly init it with Name
  procedure Log (A_Logger : in out Logger;
                 Severity : in Severities;
                 Message  : in String;
                 Name     : in String := "");
  procedure Log_Fatal   (A_Logger : in out Logger;
                         Message  : in String;
                         Name     : in String := "");
  procedure Log_Error   (A_Logger : in out Logger;
                         Message  : in String;
                         Name     : in String := "");
  procedure Log_Warning (A_Logger : in out Logger;
                         Message  : in String;
                         Name     : in String := "");
  procedure Log_Info    (A_Logger : in out Logger;
                         Message  : in String;
                         Name     : in String := "");
  procedure Log_Debug   (A_Logger : in out Logger;
                         Message  : in String;
                         Name     : in String := "");

  -- Configure logger to flush each message (True by default)
  -- Set_Flush is independant from logger initialisation
  procedure Set_Flush (A_Logger : in out Logger; Each : in Boolean);

  -- Flush logs of a logger
  procedure Flush (A_Logger : in out Logger);

  -- By default, Errors (Fatal & Error) are also logged on stderr if the
  --  file is not already stderr
  Errors_On_Stderr : Boolean := True;

private

  -- Logger
  type Logger is new Ada.Finalization.Controlled with record
    Init : Boolean := False;
    Name : As.U.Asu_Us;
    Mask : Severities := 0;
    Flush : Boolean := True;
  end record;
  overriding procedure Finalize (A_Logger : in out Logger);

end Trace.Loggers;

