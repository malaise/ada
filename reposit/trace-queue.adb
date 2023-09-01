with Ada.Finalization;
with Long_Long_Limited_Pool, Async_Stdin;
-- Store messages is a queue in memory
package body Trace.Queue is

  -- The pool
  package Cell_Pools is new Long_Long_Limited_Pool (As.U.Asu_Us,
    Lifo => False, Set => As.U.Set);

  type Pool_Type is new Ada.Finalization.Limited_Controlled with record
    The_Pool : Cell_Pools.Pool_Type;
    The_Logger : Trace.Loggers.Logger;
  end record;
  overriding procedure Finalize (A_Pool : in out Pool_Type);
  Pool : Pool_Type;


  -- Log a message of a given severity (or even several severity levels)
  -- Calling it on a logger not initialized implicitly init it with Name
  overriding procedure Log (A_Logger : in out Queue_Logger;
                       Severity : in Severities;
                       Message  : in String;
                       Name     : in String := "") is
    Txt : As.U.Asu_Us;
  begin
    -- Init queue, specific name and no mask
    if not Pool.The_Logger.Is_Init then
      Pool.The_Logger.Init ("Trace_Queues");
      Pool.The_Logger.Set_Mask (Severities'Last);
    end if;
    -- Inir local logger
    Init (A_Logger, Name);
    -- Check severity
    if (Severity and A_Logger.Get_Mask) = 0 then
      return;
    end if;
    Txt := As.U.Tus (Format (A_Logger.Name, Severity, Message));

   -- Put in queue
    Lock.Get;
    Pool.The_Pool.Push (Txt);

    -- Put also on stderr if needed
    if not Flow_Is_Stderr (A_Logger)
    and then Are_Errors_On_Stderr (A_Logger)
    and then (Severity and Errors) /= 0 then
      Stderr.Put_Line (Txt.Image);
      if Flush_Set (A_Logger) then
        Stderr.Flush;
      end if;
    end if;
    Lock.Release;
  exception
    when others =>
      if Lock.Is_Owner then
        Lock.Release;
      end if;
      raise;
  end Log;

  overriding procedure Log_Fatal   (A_Logger : in out Queue_Logger;
                                    Message  : in String;
                                    Name     : in String := "") is
  begin
    Log (A_Logger, Fatal, Message, Name);
  end Log_Fatal;

  overriding procedure Log_Error   (A_Logger : in out Queue_Logger;
                                    Message  : in String;
                                    Name     : in String := "") is
  begin
    Log (A_Logger, Error, Message, Name);
  end Log_Error;

  overriding procedure Log_Warning (A_Logger : in out Queue_Logger;
                                    Message  : in String;
                                    Name     : in String := "") is
  begin
    Log (A_Logger, Warning, Message, Name);
  end Log_Warning;

  overriding procedure Log_Info    (A_Logger : in out Queue_Logger;
                                    Message  : in String;
                                    Name     : in String := "") is
  begin
    Log (A_Logger, Info, Message, Name);
  end Log_Info;

  overriding procedure Log_Debug   (A_Logger : in out Queue_Logger;
                                    Message  : in String;
                                    Name     : in String := "") is
  begin
    Log (A_Logger, Debug, Message, Name);
  end Log_Debug;

  -- Flush the whole queue
  procedure Flush is
  begin
    if not Pool.The_Logger.Is_Init then
      return;
    end if;
    while not Pool.The_Pool.Is_Empty loop
      case Trace.Loggers.Get_Flow_Kind is
        when Trace.Loggers.Async_Stdout =>
          Async_Stdin.Put_Line_Out (Pool.The_Pool.Pop.Image);
        when Trace.Loggers.Async_Stderr =>
          Async_Stdin.Put_Line_Err (Pool.The_Pool.Pop.Image);
        when Trace.Loggers.Global =>
          -- Put, no flush, and not also on Stderr
          Global_Log (Pool.The_Pool.Pop.Image, False, False);
      end case;
    end loop;
    -- Flush
    case Trace.Loggers.Get_Flow_Kind is
      when Trace.Loggers.Async_Stdout =>
        Async_Stdin.Flush_Out;
      when Trace.Loggers.Async_Stderr =>
        Async_Stdin.Flush_Err;
      when Trace.Loggers.Global =>
        Global_Flush;
      end case;
  end Flush;

  -- Flush logs of a logger; in fact flushes the whole queue
  overriding procedure Flush (A_Logger : in out Queue_Logger) is
    pragma Unused (A_Logger);
  begin
    Flush;
  end Flush;

  overriding procedure Finalize (A_Pool : in out Pool_Type) is
    pragma Unreferenced (A_Pool);
  begin
    -- Flush and let The_Pool finalize itself
    Flush;
  end Finalize;

end Trace.Queue;


