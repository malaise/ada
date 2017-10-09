with Ada.Calendar, Ada.Finalization;
with Long_Long_Limited_Pool, Event_Mng,
     Environ, Sys_Calls, Images, Bit_Ops, Socket, Computer, Output_Flows;
-- Store messages is a queue in memory
package body Trace.Queue is

  -- The pool
  package Cell_Pools is new Long_Long_Limited_Pool (As.U.Asu_Us,
    Lifo => False, Set => As.U.Set);

  type Pool_Type is new Ada.Finalization.Limited_Controlled with record
    The_Pool : Cell_Pools.Pool_Type;
    The_Flow : Output_Flows.Output_Flow;
  end record;
  overriding procedure Finalize (A_Pool : in out Pool_Type);
  Pool : Pool_Type;

  -- Global initialisation
  ------------------------
  -- Init once :
  All_Init : Boolean := False;
  -- - Variables PID CMD HOST DATE
  Memory : Computer.Memory_Type;
  -- - Common flow
  Flow_Is_Stderr : Boolean;

  -- Image of our pid
  function Pid_Image is new Images.Int_Image (Sys_Calls.Pid);

  -- A local basic logger
  package Me is new Basic_Logger ("Trace");

  procedure Full_Init is
    File_Name : As.U.Asu_Us;
  begin
    if All_Init then
      return;
    end if;

    -- Do basic init if not yet done
    Basic_Init;

    -- No log until local logger is init
    ------------------------------------
    -- Get process name and set variables

    Memory.Set ("PID", Pid_Image (Sys_Calls.Get_Pid), False, True);
    Memory.Set ("CMD", Process.Image, False, True);
    Memory.Set ("HOST", Socket.Local_Host_Name, False, True);
    Memory.Set ("DATE", Images.Date_Image (Ada.Calendar.Clock,
                                           Format => Images.Iso_Dot),
                False, True);

    -- Get flow name and init flow
    File_Name := As.U.Tus (Environ.Getenv (Env_Proc.Image & "_TRACEFILE"));

    if not File_Name.Is_Null
    and then File_Name.Image /= Output_Flows.Stderr_Name then
      -- Try to open regular file
      begin
        Pool.The_Flow.Set (File_Name.Image);
        Flow_Is_Stderr := False;
      exception
        when others =>
          -- File cannot be created, use stderr
          File_Name.Set_Null;
      end;
    end if;

    if File_Name.Is_Null
    or else File_Name.Image = Output_Flows.Stderr_Name then
      -- Open Stderr
      begin
        Flow_Is_Stderr := True;
        File_Name := As.U.Tus (Output_Flows.Stderr_Name);
        Pool.The_Flow.Set (File_Name.Image, Stderr'Access);
      exception
        when Output_Flows.Already_Error =>
          -- Stderr is already registered, cannot use the File access provided
          Pool.The_Flow.Set (File_Name.Image);
      end;
    end if;

    -- Register Flush as callback for signal SIGUSR1
    Event_Mng.Set_Sig_Usr1_Callback (Flush'Access);

    Me.Log (Debug, "Global init done with mask " & Image (Global_Mask)
                 & " on flow " & File_Name.Image);
    All_Init := True;
  end Full_Init;

  -- Initialize the logger, either with a name or anonymous
  --  and set its mask from ENV
  procedure Init (A_Logger : in out Queue_Logger; Name : in String := "") is
  begin
   -- Init if necessary
    if A_Logger.Inited then
      return;
    end if;
    -- Full init if necessary
    Full_Init;
    -- Init the logger
    A_Logger.Name := As.U.Tus (Name);
    A_Logger.Mask := Get_Mask (Name);
    A_Logger.Inited := True;
    Me.Log (Debug, "Init queue logger name " & Name
                 & " mask " & Image (A_Logger.Mask));
  end Init;

  -- Change the name of the logger and reset its mask
  -- Also reset its flush mode
  procedure Reset (A_Logger : in out Queue_Logger; Name : in String) is
  begin
    A_Logger.Inited := False;
    Me.Log (Debug, "Reset of queue logger " & A_Logger.Name.Image
          & " to " & Name);
    Init (A_Logger, Name);
  end Reset;

  -- Return True if logger is init
  function Is_Init (A_Logger : Queue_Logger) return Boolean is
    (A_Logger.Inited);

  -- INTERNAL: raise Not_Init if not init
  procedure Check_Init (A_Logger : in Queue_Logger) is
  begin
    if not A_Logger.Inited then
      raise Not_Init;
    end if;
  end Check_Init;


  -- Return name of logger, raise Not_Init if is not init
  -- Not_Init : exception;
  function Name (A_Logger : Queue_Logger) return String is
  begin
    Check_Init (A_Logger);
    return A_Logger.Name.Image;
  end Name;

  -- Set / get / add / del severities
  -- Raise Not_Init if logger is not init
  procedure Set_Mask (A_Logger : in out Queue_Logger; Mask : in Severities) is
  begin
    Check_Init (A_Logger);
    A_Logger.Mask := Mask;
    Me.Log (Debug, "Set_Mask " & Image (Mask) &
                   " to logger " & A_Logger.Name.Image);
  end Set_Mask;

  function  Get_Mask (A_Logger : in out Queue_Logger) return Severities is
  begin
     Check_Init (A_Logger);
    return A_Logger.Mask;
  end Get_Mask;

  procedure Add_Mask (A_Logger : in out Queue_Logger; Mask : in Severities) is
  begin
    Check_Init (A_Logger);
    A_Logger.Mask := Severities(Bit_Ops."Or" (Natural(A_Logger.Mask),
                                              Natural(Mask)));
    Me.Log (Debug, "Add_Mask " & Image (Mask)
                 & " to logger " & A_Logger.Name.Image
                 & " -> " & Image (A_Logger.Mask));
  end Add_Mask;

  procedure Del_Mask (A_Logger : in out Queue_Logger; Mask : in Severities) is
  begin
    Check_Init (A_Logger);
    A_Logger.Mask := Severities(Bit_Ops."Xor" (Natural(A_Logger.Mask),
                                              Natural(Mask)));
    Me.Log (Debug, "Del_Mask " & Image (Mask)
                 & " to logger " & A_Logger.Name.Image
                 & " -> " & Image (A_Logger.Mask));
  end Del_Mask;

-- Check if a severity is active
  -- Raise Not_Init if logger is not init
  function Is_On (A_Logger : in out Queue_Logger;
                  Severity : in Severities) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Severity) /= 0;
  end Is_On;

  function Fatal_On   (A_Logger : in out Queue_Logger) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Fatal) /= 0;
  end Fatal_On;

  function Error_On   (A_Logger : in out Queue_Logger) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Error) /= 0;
  end Error_On;

  function Warning_On (A_Logger : in out Queue_Logger) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Warning) /= 0;
  end Warning_On;

  function Info_On    (A_Logger : in out Queue_Logger) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Info) /= 0;
  end Info_On;

  function Debug_On   (A_Logger : in out Queue_Logger) return Boolean is
  begin
     Check_Init (A_Logger);
    return (A_Logger.Mask and Debug) /= 0;
  end Debug_On;

  -- Log a message of a given severity (or even several severity levels)
  -- Calling it on a logger not initialized implicitly init it with Name
  procedure Log (A_Logger : in out Queue_Logger;
                 Severity : in Severities;
                 Message  : in String;
                 Name     : in String := "") is
    Txt : As.U.Asu_Us;
  begin
    -- Init as anonymous if needed
    if not A_Logger.Inited then
      Init (A_Logger, Name);
    end if;
    -- Check severity
    if (Severity and A_Logger.Mask) = 0 then
      return;
    end if;
    Txt := As.U.Tus (Format (A_Logger.Name.Image, Severity, Message));

   -- Put on flow
    Lock.Get;
    if Errors_On_Stderr and then (Severity and Errors) /= 0 then
      Pool.The_Flow.Put_Line (Txt.Image);
    else
      Pool.The_Pool.Push (Txt);
    end if;

    -- Put also on stderr if needed
    if not Flow_Is_Stderr
    and then Errors_On_Stderr
    and then (Severity and Errors) /= 0 then
      Stderr.Put_Line (Txt.Image);
      if Flush_Stderr then
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

  procedure Log_Fatal   (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "") is
  begin
    Log (A_Logger, Fatal, Message, Name);
  end Log_Fatal;

  procedure Log_Error   (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "") is
  begin
    Log (A_Logger, Error, Message, Name);
  end Log_Error;

  procedure Log_Warning (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "") is
  begin
    Log (A_Logger, Warning, Message, Name);
  end Log_Warning;

  procedure Log_Info    (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "") is
  begin
    Log (A_Logger, Info, Message, Name);
  end Log_Info;

  procedure Log_Debug   (A_Logger : in out Queue_Logger;
                         Message  : in String;
                         Name     : in String := "") is
  begin
    Log (A_Logger, Debug, Message, Name);
  end Log_Debug;

  -- Flush the whole queue
  procedure Flush is
  begin
    if not All_Init then
      return;
    end if;
    while not Pool.The_Pool.Is_Empty loop
      Pool.The_Flow.Put_Line (Pool.The_Pool.Pop.Image);
    end loop;
    Pool.The_Flow.Flush;
  end Flush;

  overriding procedure Finalize (A_Pool : in out Pool_Type) is
    pragma Unreferenced (A_Pool);
  begin
    -- Flush and let The_Pool finalize itself
    Flush;
    All_Init := False;
  end Finalize;

end Trace.Queue;


