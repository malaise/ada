-- Log messages in a file or flow
with As.U;
private with Text_Line, Mutexes;
package Trace is

  -- General concepts for Basic_Logger (defined hereafter)
  --  and for Logger (defined in the child packages Trace.Loggers and
  --  Trace.Queue)
  -- Each logger is anonymous or has a name, and each trace has a severity
  --  (possibly several)
  -- Activate the traces by setting the environment variables
  --   <Process>_TRACE[_<Logger>]="<mask>"
  --   Where <Process> is the process name (no path, non alphanum characters
  --          replaced by '_')
  --         <Logger> is the logger name, "ALL" for all loggers,
  --          <Process>_TRACE for anonymous loggers
  --         <mask> is a list of severity names or values,
  --          separated by '|', ex: "7|Debug|16#30#|Warning+"
  -- Default mask is Fatal | Error
  -- A severity name then a '+' means "name and higher"
  -- Any value 0 in the mask leads to reset the current mask (further values
  --  are ORed) --  Ex: "Fatal|0|Error" => "Error"
  -- Parsing error on a severity leads to the default severity, except for
  --  numeric values too high, which are discarded.

  -- Trace output is:
  -- "Date Process Logger Severity -> Message"
  -- Where Date     ::= YYyy-Mm-DdTHh:Mm:Ss.mmm
  --       Process  ::= the basename of the current process
  --       Logger   ::= the name of the logger, default "-"
  --       Severity ::= Fatal, Error, Info, Warning, Debug or a number
  --       Message  ::= the text of the log message

  -- The output of trace is protected by a mutex (but not the other
  --  operations on loggers).

  -- ENVIRONMENT
  ---------------
  -- Init the environement (to be called before init of the loggers)
  --  to a given mask for some loggers and a given log file
  -- BEWARE that some (basic) loggers may initialize during program elaboration,
  --  so before Init_Env is called
  -- Set the severity of the provided loggers to the provided mask
  -- Set the trace file to the provided name (if set);
  procedure Init_Env (Loggers : in As.U.Asu_Array;
                      Mask : in String;
                      File : in String);


  -- SEVERITIES
  -------------
  -- A trace severity, one bit for each
  type Severities is new Natural range 0 .. 255;

  function "And" (L, R : Severities) return Severities;
  function "Or"  (L, R : Severities) return Severities;
  function "Xor" (L, R : Severities) return Severities;

  -- Predefined severities
  Fatal   : constant Severities := 16#01#;
  Error   : constant Severities := 16#02#;
  Warning : constant Severities := 16#04#;
  Info    : constant Severities := 16#08#;
  Debug   : constant Severities := 16#10#;

  -- Fatal or Error
  function Errors return Severities;
  -- Warnings or Info
  function Infos return Severities;


  -- BASIC LOGGER
  ---------------
  -- All the basic loggers of a process trace in a given flow, which can be set
  --  by the environment variable <Process>_TRACEFILE="file"
  --  where <Process> is the process name (no path, non alphanum characters
  --         replaced by '_')
  --        file is "stdout", "stderr", or any file name
  -- Default is stderr

  -- Define the logger, either with a name or anonymous, and it will get its
  --  mask from ENV at first usage
  -- If name is not valid (i.e. not and Ada identifier) then the logger
  --  is disabled (Mask=0)
  -- If Name is not empty and <proc>_TRACE_<Name> is set (even empty)
  -- or Name is empty and <proc>_TRACE is set, then the mask is set to
  --  Fatal|Error|mask
  -- If none is set then the mask is set to the global value got from
  --  <Process>_TRACE_ALL if set, or Fatal|Error by default
  generic
    Name : String;
  package Basic_Logger is

    -- Set / get / add / del severities
    procedure Set_Mask (Mask : in Severities);
    function  Get_Mask return Severities;
    procedure Add_Mask (Mask : in Severities);
    procedure Del_Mask (Mask : in Severities);

    -- Check if a severity is active
    function Is_On (Severity : in Severities) return Boolean;
    function Fatal_On   return Boolean;
    function Error_On   return Boolean;
    function Warning_On return Boolean;
    function Info_On    return Boolean;
    function Debug_On   return Boolean;

    -- Log a message of a given severity (or several severity levels)
    procedure Log (Severity : in Severities;
                   Message  : in String);
    procedure Log_Fatal   (Message  : in String);
    procedure Log_Error   (Message  : in String);
    procedure Log_Warning (Message  : in String);
    procedure Log_Info    (Message  : in String);
    procedure Log_Debug   (Message  : in String);

    -- Configure logger to flush or not each message (True by default)
    procedure Set_Flush (Each : in Boolean);

    -- Flush logs of a logger
    procedure Flush;

  end Basic_Logger;

private
  -- Utilities for child packages
  Lock : Mutexes.Simple_Mutex;

  -- Operations and variables for child packages

  -- Global init, sets the following variables
  procedure Global_Init;
  -- - Process name
  Process : As.U.Asu_Us;
  -- Environment process name, where non-alphanum characters are replaed by '_'
  Env_Proc : As.U.Asu_Us;
  -- - Global severity mask
  Default : constant Severities := Error + Fatal;
  Global_Mask : Severities := Default;
  -- - Stderr output flow
  Stderr : aliased Text_Line.File_Type;

  -- Common format of the output
  function Format (Name     : in String;
                   Severity : in Severities;
                   Message  : in String) return String;

  -- Get mask for a logger
  function Get_Mask (Name : in String) return Severities;

  -- Image of a severity mask (for debug)
  function Image (Severity : Severities) return String;

end Trace;

