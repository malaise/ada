-- Store messages is a queue in memory
private with Ada.Finalization;
private with Long_Long_Limited_Pool;
package Trace.Queue is

  -- See the parent package Trace for general informations about the
  --  tracing mechanisms

  -- All the queuers of a process log in the same common queue, which is
  --  dump on stderr on request or at process termination

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

  -- By default, Errors (Fatal & Error) are directly logged on stderr
  Errors_On_Stderr : Boolean := True;

private

  -- Logger
  type Queue_Logger is new Ada.Finalization.Controlled with record
    Inited : Boolean := False;
    Name : As.U.Asu_Us;
    Mask : Severities := 0;
  end record;

  -- Queue
  package Cell_Pools is new Long_Long_Limited_Pool (As.U.Asu_Us,
    Lifo => False, Set => As.U.Set);

  type Pool_Type is new Ada.Finalization.Limited_Controlled with record
    The_Pool : Cell_Pools.Pool_Type;
  end record;
  overriding procedure Finalize (A_Pool : in out Pool_Type);

end Trace.Queue;






