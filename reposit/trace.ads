-- Log messages in a file named _trace_<pid>
with Ada.Finalization;
with As.U;
package Trace is

  -- Log traces through a logger
  -- All logers trace in a given flow set in environment variable
  --  <PROCESS>_TRACEFILE="file", default stderr
  --  where <PROCESS> is the process name in UPPERCASE (no path)
  --        file is "stdout", "stderr" or any file name, possibly with
  --          ${PID}, ${CMD}, ${HOST} or ${DATE}, which are expanded
  -- Each logger is anonymous or has a name, and each trace has a severity
  --  (possibly several)
  -- Activate traces by environment variables
  --   <PROCESS>_TRACE[_<LOGGER>]="<severities>"
  --   Where <PROCESS> is the process name in UPPERCASE (no path)
  --         <LOGGER> is the logger name in UPPERCASE, "ALL" for all loggers,
  --         <PROCESS>_TRACE for anonymous loggers
  --         <severities> is a list of severity names of values,
  --           separated by '|', ex: "7|DEBUG|16#30#"
  -- Default severity is Fatal | Error
  -- Any value 0 leads to reset the current severity (further values are ORed)
  --  Ex: "Fatal|0|Error" => "Error"
  -- Parsing error on a severity leads to default severity, except for numeric
  --  values too high, which are discarded.
  -- Fatal | Error are also sent to stderr by default

  -- Trace output is:
  -- "Date Process Logger Severity -> Message"
  -- Where Date     ::= YYyy/Mm/DdTHh:Mm:Ss.mmm
  --       Process  ::= the basename of current process
  --       Logger   ::= the name of the logger, default "-"
  --       Severity ::= FATAL ERROR INFO WARNING DEBUG or a number
  --       Message  ::= the text of the log message

  -- The logger of traces
  type Logger is tagged private;

  -- Initialize the logger, with a name or anonymous
  --  and set its mask from ENV
  -- Setting a name (even empty) activates the logger
  -- If Name is not empty and <proc>_TRACE_<Name> is set
  -- or Name is empty and <proc>_TRACE is set, then the mask is set to
  --  Fatal|Error|value
  -- If none is set then the mask is set to the global value got from
  --  <proc>_TRACE_ALL if set, or Fatal|Error by default
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


  -- A trace severity, one bit for each
  type Severities is new Natural range 0 .. 255;

  -- Predefined severities
  Fatal   : constant Severities := 16#01#;
  Error   : constant Severities := 16#02#;
  Warning : constant Severities := 16#04#;
  Info    : constant Severities := 16#08#;
  Debug   : constant Severities := 16#10#;

  function "And" (L, R : Severities) return Severities;
  function "Or"  (L, R : Severities) return Severities;

  -- Fatal or Error
  function Errors return Severities;
  -- Warnings or Info
  function Infos return Severities;


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

end Trace;

