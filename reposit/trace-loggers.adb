with Ada.Calendar;
with Environ, Sys_Calls, Images, Bit_Ops, Socket, Computer, Output_Flows;
package body Trace.Loggers is


  -- Global initialisation
  ------------------------
  -- Init once :
  All_Init : Boolean := False;
  -- - Variables PID CMD HOST DATE
  Memory : Computer.Memory_Type;
  -- - Common flow
  Flow_Is_Stderr : Boolean;
  Flow : Output_Flows.Output_Flow;

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
    Memory.Set ("DATE", Images.Date_Image (Ada.Calendar.Clock, Iso => True),
                False, True);

    -- Get flow name and init flow
    File_Name := As.U.Tus (Environ.Getenv (Process.Image & "_TRACEFILE"));

    if not File_Name.Is_Null
    and then File_Name.Image /= Output_Flows.Stderr_Name then
      -- Try to open regular file
      begin
        Flow.Set (File_Name.Image);
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
        Flow.Set (File_Name.Image, Stderr'Access);
      exception
        when Output_Flows.Already_Error =>
          -- Stderr is already registered, cannot use the File access provided
          Flow.Set (File_Name.Image);
      end;
    end if;

    Me.Log (Debug, "Global init done with mask " & Image (Global_Mask)
                 & " on flow " & File_Name.Image);
    All_Init := True;
  end Full_Init;

  -- Logger
  ---------

  -- Set logger name, retrieve severity
  -- Set / change the logger name
  procedure Init (A_Logger : in out Logger; Name : in String := "") is
  begin
    -- Init if necessary
    if A_Logger.Init then
      return;
    end if;
    -- Global init if necessary
    Full_Init;
    -- Init the logger
    A_Logger.Name := As.U.Tus (Name);
    A_Logger.Mask := Get_Mask (Name);
    A_Logger.Init := True;
    Me.Log (Debug, "Init logger name " & Name
                 & " mask " & Image (A_Logger.Mask));
  end Init;

  procedure Reset (A_Logger : in out Logger; Name : in String) is
  begin
    A_Logger.Init := False;
    Me.Log (Debug, "Reset of logger " & A_Logger.Name.Image & " to " & Name);
    Init (A_Logger, Name);
    Set_Flush (A_Logger, True);
  end Reset;

  function Is_Init (A_Logger : Logger) return Boolean is
  begin
    return A_Logger.Init;
  end Is_Init;

  -- INTERNAL: raise Not_Init if not init
  procedure Check_Init (A_Logger : in Logger) is
  begin
    if not A_Logger.Init then
      raise Not_Init;
    end if;
  end Check_Init;

  function Name (A_Logger : Logger) return String is
  begin
    Check_Init (A_Logger);
    return A_Logger.Name.Image;
  end Name;

  -- Severities
  ----------
  procedure Set_Mask (A_Logger : in out Logger; Mask : in Severities) is
  begin
    Check_Init (A_Logger);
    A_Logger.Mask := Mask;
    Me.Log (Debug, "Set_Mask " & Image (Mask) &
                   " to logger " & A_Logger.Name.Image);
  end Set_Mask;

  function Get_Mask (A_Logger : in out Logger) return Severities is
  begin
    Check_Init (A_Logger);
    return A_Logger.Mask;
  end Get_Mask;

  procedure Add_Mask (A_Logger : in out Logger; Mask : in Severities) is
  begin
    Check_Init (A_Logger);
    A_Logger.Mask := Severities(Bit_Ops."Or" (Natural(A_Logger.Mask),
                                              Natural(Mask)));
    Me.Log (Debug, "Add_Mask " & Image (Mask)
                 & " to logger " & A_Logger.Name.Image
                 & " -> " & Image (A_Logger.Mask));
  end Add_Mask;

  procedure Del_Mask (A_Logger : in out Logger; Mask : in Severities) is
  begin
    Check_Init (A_Logger);
    A_Logger.Mask := Severities(Bit_Ops."Xor" (Natural(A_Logger.Mask),
                                              Natural(Mask)));
    Me.Log (Debug, "Del_Mask " & Image (Mask)
                 & " to logger " & A_Logger.Name.Image
                 & " -> " & Image (A_Logger.Mask));
  end Del_Mask;


  -- Check if a severity is active
  function Is_On (A_Logger : in out Logger;
                  Severity : in Severities) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Severity) /= 0;
  end Is_On;

  function Fatal_On (A_Logger : in out Logger) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Fatal) /= 0;
  end Fatal_On;

  function Error_On (A_Logger : in out Logger) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Error) /= 0;
  end Error_On;
  function Warning_On (A_Logger : in out Logger) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Warning) /= 0;
  end Warning_On;
  function Info_On (A_Logger : in out Logger) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Info) /= 0;
  end Info_On;
  function Debug_On (A_Logger : in out Logger) return Boolean is
  begin
    Check_Init (A_Logger);
    return (A_Logger.Mask and Debug) /= 0;
  end Debug_On;


  -- Logging
  ----------
  procedure Log (A_Logger : in out Logger;
                 Severity : in Severities;
                 Message  : in String;
                 Name     : in String := "") is
    Txt : As.U.Asu_Us;
  begin
    -- Init as anonymous if needed
    if not A_Logger.Init then
      Init (A_Logger, Name);
    end if;
    -- Check severity
    if (Severity and A_Logger.Mask) = 0 then
      return;
    end if;
    Txt := As.U.Tus (Format (A_Logger.Name.Image, Severity, Message));

    -- Put on flow
    Lock.Get;
    Flow.Put_Line (Txt.Image);
    -- Flush if set on logger
    if A_Logger.Flush then
      Flow.Flush;
    end if;

    -- Put also on stderr if needed
    if not Flow_Is_Stderr
    and then Errors_On_Stderr
    and then (Severity and Errors) /= 0 then
      Stderr.Put_Line (Txt.Image);
      -- Flush if set on logger
      if A_Logger.Flush then
        Stderr.Flush;
      end if;
    end if;
    Lock.Release;
  end Log;

  procedure Log_Fatal   (A_Logger : in out Logger;
                         Message  : in String;
                         Name     : in String := "") is
  begin
    Log (A_Logger, Fatal, Message, Name);
  end Log_Fatal;

  procedure Log_Error   (A_Logger : in out Logger;
                         Message  : in String;
                         Name     : in String := "") is
  begin
    Log (A_Logger, Error, Message, Name);
  end Log_Error;

  procedure Log_Warning (A_Logger : in out Logger;
                         Message  : in String;
                         Name     : in String := "") is
  begin
    Log (A_Logger, Warning, Message, Name);
  end Log_Warning;

  procedure Log_Info    (A_Logger : in out Logger;
                         Message  : in String;
                         Name     : in String := "") is
  begin
    Log (A_Logger, Info, Message, Name);
  end Log_Info;

  procedure Log_Debug   (A_Logger : in out Logger;
                         Message  : in String;
                         Name     : in String := "") is
  begin
    Log (A_Logger, Debug, Message, Name);
  end Log_Debug;


  -- Flushing
  -----------
  -- Configure logger to flush each message
  procedure Set_Flush (A_Logger : in out Logger; Each : in Boolean) is
  begin
    A_Logger.Flush := Each;
  end Set_Flush;

  -- Flush logs of a logger
  procedure Flush (A_Logger : in out Logger) is
    pragma Unreferenced (A_Logger);
  begin
    Flow.Flush;
  end Flush;

  -- Destructor: flush global flow
  --------------
  overriding procedure Finalize (A_Logger : in out Logger) is
    pragma Unreferenced (A_Logger);
  begin
    if Flow.Is_Set then
      Flow.Flush;
    end if;
  end Finalize;

end Trace.Loggers;

