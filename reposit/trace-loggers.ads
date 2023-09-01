-- Log messages in a flow or file
private with Ada.Finalization;
package Trace.Loggers is

  -- See the parent package Trace for general informations about the
  --  tracing mechanisms

  -- All the loggers of a process trace in a given flow, which can be set in
  --  the environment variable <Process>_TRACEFILE="file"
  --  where <Process> is the process name (no path, non alphanum characters
  --         replaced by '_')
  --        file is "stdout", "stderr", "async_stdout", "async_stderr",
  --         or any file name (see Output_Flows), possibly with
  --         ${PID}, ${CMD}, ${HOST} or ${DATE}, which are expanded.
  -- When not asynchronous, the logger uses the global flow.
  -- Default is stderr.
  -- By default, errors (Fatal & Error) are also logged on stderr, if the flow
  --  is not already stderr.

  -- Kind of flow: global or async Stdout / Stderr
  type Flow_Kind_List is (Global, Async_Stdout, Async_Stderr);
  function Get_Flow_Kind return Flow_Kind_List;

  -- A logger of traces
  type Logger is tagged private;

  -- Initialize the logger, either with a name or anonymous
  --  and set its mask from ENV
  -- If Name is not empty and <proc>_TRACE_<Name> is set (even empty)
  -- or Name is empty and <proc>_TRACE is set, then the mask is set to
  --  Fatal|Error|value
  -- If none is set then the mask is set to the global value got from
  --  <Process>_TRACE_ALL if set, or Fatal|Error by default
  -- Each further call to Init have no effect,
  procedure Init (A_Logger : in out Logger; Name : in String := "");
  -- Change the name of the logger and reset its mask
  -- Also reset its flush mode
  procedure Reset (A_Logger : in out Logger; Name : in String);

  -- Return True if logger is init
  function Is_Init (A_Logger : Logger) return Boolean;
  -- Return name of logger, raise Not_Init if is not init
  Not_Init : exception;
  function Name (A_Logger : Logger) return String;

  -- Set / get / add / del severities
  -- Raise Not_Init if logger is not init
  procedure Set_Mask (A_Logger : in out Logger; Mask : in Severities);
  function  Get_Mask (A_Logger : in out Logger) return Severities;
  procedure Add_Mask (A_Logger : in out Logger; Mask : in Severities);
  procedure Del_Mask (A_Logger : in out Logger; Mask : in Severities);

  -- Check if a severity is active
  -- Raise Not_Init if logger is not init
  function Is_On (A_Logger : in out Logger;
                  Severity : in Severities) return Boolean;
  function Fatal_On   (A_Logger : in out Logger) return Boolean;
  function Error_On   (A_Logger : in out Logger) return Boolean;
  function Warning_On (A_Logger : in out Logger) return Boolean;
  function Info_On    (A_Logger : in out Logger) return Boolean;
  function Debug_On   (A_Logger : in out Logger) return Boolean;

  -- Does the logger log on Stderr
  function Flow_Is_Stderr (A_Logger : in out Logger) return Boolean;

  -- Log a message of a given severity (or even several severity levels)
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
  procedure Set_Flush (A_Logger : in out Logger; Activate : in Boolean);
  function Flush_Set (A_Logger : Logger) return Boolean;

  -- Configure logger to also log errors (Fatal & Error) on stderr (True
  --  by default)
  -- Errors_On_Stderr is independant from logger initialisation
  procedure Errors_On_Stderr (A_Logger : in out Logger;
                              Activate : Boolean := True);
  function Are_Errors_On_Stderr (A_Logger : Logger) return Boolean;


  -- Flush logs of a logger
  -- Raise Not_Init if logger is not init
  procedure Flush (A_Logger : in out Logger);

private

  -- Logger
  type Logger is new Ada.Finalization.Controlled with record
    Inited : Boolean := False;
    Name : As.U.Asu_Us;
    Mask : Severities := 0;
    Flush_Flow : Boolean := True;
    Err_On_Stderr : Boolean := True;
  end record;
  overriding procedure Finalize (A_Logger : in out Logger);

end Trace.Loggers;

