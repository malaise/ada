with Ada.Calendar;
with Sys_Calls, Images, Bit_Ops, Socket, Computer, Async_Stdin;
package body Trace.Loggers is

  -- Global initialisation
  ------------------------
  Logers_Inited : Boolean := False;
  Memory : Computer.Memory_Type;
  -- Kind of flow: global or async Stdout / Stderr
  Flow_Kind : Flow_Kind_List := Global;
  function Get_Flow_Kind return Flow_Kind_List is (Flow_Kind);

  -- Image of our pid
  function Pid_Image is new Images.Int_Image (Sys_Calls.Pid);

  -- A local basic logger
  package Me is new Basic_Logger ("Trace");

  procedure Loggers_Init is
    Flow_Name : As.U.Asu_Us;
  begin

    -- Do global and loggers init once
    if not Logers_Inited then
      Logers_Inited := True;
      Global_Init;

      -- No log until local logger is init
      ------------------------------------
      -- Get process name and set variables

      Memory.Set ("PID", Pid_Image (Sys_Calls.Get_Pid), False, True);
      Memory.Set ("CMD", Process_Name.Image, False, True);
      Memory.Set ("HOST", Socket.Local_Host_Name, False, True);
      Memory.Set ("DATE", Images.Date_Image (Ada.Calendar.Clock), False, True);
    end if;

    -- Get flow name and init flow
    Flow_Name := As.U.Tus (Memory.Eval (Get_Tracefile));

    if Flow_Name.Image = "Asyn_Stdout" then
      Flow_Kind := Async_Stdout;
    elsif Flow_Name.Image = "Asyn_Stderrout" then
      Flow_Kind := Async_Stderr;
    else
      Set_Global_Flow (Flow_Name.Image);
      Flow_Kind := Global;
    end if;

    Me.Log (Debug, "Global init done with mask " & Image (Global_Mask)
                 & " on flow " & Flow_Name.Image);
  end Loggers_Init;

  -- Logger
  ---------

  -- Set logger name, retrieve severity
  -- Set / change the logger name
  procedure Init (A_Logger : in out Logger; Name : in String := "") is
  begin
    -- Init if necessary
    if A_Logger.Inited then
      return;
    end if;
    -- Global init if necessary
    Loggers_Init;
    -- Init the logger
    A_Logger.Name := As.U.Tus (Name);
    A_Logger.Mask := Get_Mask (Name);
    A_Logger.Inited := True;
    Me.Log (Debug, "Init logger name " & Name
                 & " mask " & Image (A_Logger.Mask));
  end Init;

  procedure Reset (A_Logger : in out Logger; Name : in String) is
  begin
    A_Logger.Inited := False;
    Me.Log (Debug, "Reset of logger " & A_Logger.Name.Image & " to " & Name);
    Init (A_Logger, Name);
    Set_Flush (A_Logger, True);
  end Reset;

  function Is_Init (A_Logger : Logger) return Boolean is (A_Logger.Inited);

  -- INTERNAL: raise Not_Init if not init
  procedure Check_Init (A_Logger : in Logger) is
  begin
    if not A_Logger.Inited then
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

  -- Does the logger log on Stderr
  function Flow_Is_Stderr (A_Logger : in out Logger) return Boolean is
  begin
    Check_Init (A_Logger);
    return Flow_Kind = Global and then Global_Is_Stderr;
  end Flow_Is_Stderr;

  -- Logging
  ----------
  procedure Log (A_Logger : in out Logger;
                 Severity : in Severities;
                 Message  : in String;
                 Name     : in String := "") is
    Txt : As.U.Asu_Us;
  begin
    -- Init if needed
    Init (A_Logger, Name);
    -- Check severity
    if (Severity and A_Logger.Mask) = 0 then
      return;
    end if;
    Txt := As.U.Tus (Format (A_Logger.Name.Image, Severity, Message));

    -- Put on flow
    Lock.Get;
    case Flow_Kind is
      when Async_Stdout =>
        -- Put and flush ou Async_Stdout
        Async_Stdin.Put_Line_Out (Txt.Image);
        if A_Logger.Flush_Flow then
          Async_Stdin.Flush_Out;
        end if;
      when Async_Stderr =>
        -- Put and flush ou Async_Stderr
        Async_Stdin.Put_Line_Err (Txt.Image);
        if A_Logger.Flush_Flow then
          Async_Stdin.Flush_Err;
        end if;
      when Global =>
        -- Put and flush on global flow, and put on stderr if need
        Global_Log (Txt.Image,
            A_Logger.Flush_Flow,
            A_Logger.Err_On_Stderr and then (Severity and Errors) /= 0);
    end case;

    -- Also put and flush on stderr if needed
    if Flow_Kind /= Global and then A_Logger.Err_On_Stderr
    and then (Severity and Errors) /= 0 then
      Stderr.Put_Line (Txt.Image);
      -- Flush if set on logger
      if A_Logger.Flush_Flow then
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
  procedure Set_Flush (A_Logger : in out Logger; Activate : in Boolean) is
  begin
    A_Logger.Flush_Flow := Activate;
  end Set_Flush;
  function Flush_Set (A_Logger : Logger) return Boolean is
    (A_Logger.Flush_Flow);

  -- Configure logger to also log errors (Fatal & Error) on stderr
  --  by default)
  -- Errors_On_Stderr is independant from logger initialisation
  procedure Errors_On_Stderr (A_Logger : in out Logger;
                              Activate : Boolean := True) is
  begin
    A_Logger.Err_On_Stderr := Activate;
  end Errors_On_Stderr;
  function Are_Errors_On_Stderr (A_Logger : Logger) return Boolean is
    (A_Logger.Err_On_Stderr);

  -- Flush logs of a logger
  procedure Flush (A_Logger : in out Logger) is
  begin
    Check_Init (A_Logger);
    -- Flush
    case Flow_Kind is
      when Async_Stdout =>
        Async_Stdin.Flush_Out;
      when Async_Stderr =>
        Async_Stdin.Flush_Err;
      when Global =>
        Global_Flush;
    end case;
  end Flush;

  -- Destructor: flush global flow
  --------------
  overriding procedure Finalize (A_Logger : in out Logger) is
  begin
    if A_Logger.Inited then
      Flush (A_Logger);
    end if;
  end Finalize;

end Trace.Loggers;

