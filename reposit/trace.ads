with As.U, Text_Line, Mutex_Manager;
-- Log messages in a file or flow
package Trace is

  -- General concepts for Basic_Logger (defined hereafter)
  --  and for Logger (defined in the child package Trace.Loggers)
  -- Each logger is anonymous or has a name, and each trace has a severity
  --  (possibly several)
  -- Activate the traces by setting the environment variables
  --   <Process>_TRACE[_<Logger>]="<mask>"
  --   Where <PROCESS> is the process name (no path)
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
  -- Where Date     ::= YYyy/Mm/DdTHh:Mm:Ss.mmm
  --       Process  ::= the basename of current process
  --       Logger   ::= the name of the logger, default "-"
  --       Severity ::= Fatal, Error, Info, Warning, Debug or a number
  --       Message  ::= the text of the log message

  -- The output of trace is protected by a mutex (but not the other
  --  operations on loggers).

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
  -- All basic logers trace in stderr

  -- Initialize the logger, either with a name or anonymous,
  --  and set its mask from ENV
  -- If name is not valid (i.e. not and Ada identifier) then the logger
  --  is disabled (Mask=0).
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

    -- Log a message of a given severity (or even several severity levels)
    --  severities)
    procedure Log (Severity : in Severities;
                   Message  : in String);
    procedure Log_Fatal   (Message  : in String);
    procedure Log_Error   (Message  : in String);
    procedure Log_Warning (Message  : in String);
    procedure Log_Info    (Message  : in String);
    procedure Log_Debug   (Message  : in String);

    -- Flush logs of a logger
    procedure Flush;

    -- Configure logger to flush or not each message (True by default)
    procedure Set_Flush (Each : in Boolean);

  end Basic_Logger;

private
  -- Utilities for child packages
  Lock : Mutex_Manager.Simple_Mutex;

  -- Operations and variables for Trace.Loggers

  -- Global basic init, sets the following variables
  procedure Basic_Init;
    -- - Process name
  Process : As.U.Asu_Us;
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

