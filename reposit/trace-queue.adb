with Bit_Ops, Basic_Proc;
-- Store messages is a queue in memory
package body Trace.Queue is

  -- The pool
  Pool : Pool_Type;

  -- A local basic logger
  package Me is new Basic_Logger ("Trace");

  -- Initialize the logger, either with a name or anonymous
  --  and set its mask from ENV
  procedure Init (A_Logger : in out Queue_Logger; Name : in String := "") is
  begin
   -- Init if necessary
    if A_Logger.Inited then
      return;
    end if;
    -- Global init if necessary
    Basic_Init;
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
    Lock.Get;
    -- Build the text and put or queue it
    Txt := As.U.Tus (Format (A_Logger.Name.Image, Severity, Message));
    if Errors_On_Stderr and then  (Severity and Errors) /= 0 then
      Basic_Proc.Put_Line_Error (Txt.Image);
    else
      Pool.The_Pool.Push (Txt);
    end if;
    Lock.Release;

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
    while not Pool.The_Pool.Is_Empty loop
      Basic_Proc.Put_Line_Error (Pool.The_Pool.Pop.Image);
    end loop;
  end Flush;

  overriding procedure Finalize (A_Pool : in out Pool_Type) is
    pragma Unreferenced (A_Pool);
  begin
    -- Flush and let The_Pool finalize itself
    Flush;
  end Finalize;

end Trace.Queue;


