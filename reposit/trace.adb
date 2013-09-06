with Ada.Calendar;
with Environ, Argument, Sys_Calls,
     Images, Hexa_Utils, Bit_Ops, Upper_Str, Str_Util, Gets,
     Text_Line, Socket, Computer, Parser;
package body Trace is

  -- Utilities
  ------------
  -- Image of our pid
  function Pid_Image is new Images.Int_Image (Sys_Calls.Pid);

  -- Operations on Severities
  function "And" (L, R : Severities) return Severities is
  begin
    return Severities(Bit_Ops."And" (Natural(L), Natural(R)));
  end "And";
  function "Or" (L, R : Severities) return Severities is
  begin
    return Severities(Bit_Ops."Or" (Natural(L), Natural(R)));
  end "Or";

  function Errors return Severities is
  begin
    return Fatal or Error;
  end Errors;
  function Infos return Severities is
  begin
    return Warning or Info;
  end Infos;

  -- Image of a severity mask
  function Image (Severity : Severities) return String is
  begin
    return (case Severity is
        when Fatal   => "Fatal",
        when Error   => "Error",
        when Warning => "Warning",
        when Info    => "Info",
        when Debug   => "Debug",
        when others  => Hexa_Utils.Image (Natural(Severity)));
  end Image;

  -- Parse a severity string
  -- Parsing error on a severity leads to default severity, except for
  --  numeric values too high, which are discarded.
  Default : constant Severities := Error + Fatal;
  function Sep (C : Character) return Boolean is
  begin
    return C = '|';
  end Sep;

  function Parse (Str : String) return Severities is
    Valid : Boolean;
    Result : Severities := Default;
    Iter : Parser.Iterator;

  begin
    -- Optim
    if Str = "" then
      return Default;
    end if;
    Iter.Set (Str, Sep'Access);
  loop
    declare
        Word : constant String
             := Str_Util.Normalize (Upper_Str (Iter.Next_Word));
        N : Natural;
      begin
        exit when Word = "";
        begin
          -- Try to get a natural value
          N := Gets.Get_Int (Word);
          Valid := True;
          if N = 0 then
            -- Reset result
            Result := 0;
          else
            Result := Result or Severities(N);
          end if;
        exception
          when Constraint_Error =>
            Valid := False;
        end;

        if not Valid then
          -- At least INFO, which also ensures 16##
          if Word'Length < 4 then
            return Default;
          end if;
          if Word(1..3) = "16#" and then Word(Word'Last) = '#' then
            -- Discard invalid value, including empty
            N := Hexa_Utils.Value (Word(4 .. Word'Last-1));
            if N = 0 then
              -- Reset result
              Result := 0;
            elsif N < Natural(Severities'Last) then
              -- Skip if too large
              Result := Result or Severities(N);
            end if;
          -- Predefined severities
          elsif Word = "FATAL" then
            Result := Result or Fatal;
          elsif Word = "ERROR" then
             Result := Result or Error;
          elsif Word = "WARNING"
            then Result := Result or Warning;
          elsif Word = "INFO" then
            Result := Result or Info;
          elsif Word = "DEBUG" then
            Result := Result or Debug;
          else
            return Default;
          end if;
        end if;
      end;
    end loop;
    return Result;
  exception
    when others =>
      return Default;
  end Parse;


  -- Global initialisation
  ------------------------
  -- Init once :
  All_Init : Boolean := False;
  -- - Process name
  Process : As.U.Asu_Us;
  -- - Variables PID CMD HOST DATE
  Memory : Computer.Memory_Type;
  -- - Global severity mask
  Global_Mask : Severities := Default;
  -- - Common flow
  Stdout_Name : constant String := "stdout";
  Stderr_Name : constant String := "stderr";
  Flow_Is_Stderr : Boolean;
  File : aliased Text_Line.File_Type;
  Stderr : aliased Text_Line.File_Type;
  Flow : access Text_Line.File_Type;

  -- INTERNAL: Minimal init: global process name, global maks and stderr
  procedure Basic_Init is
  begin
    if Process.Is_Null then
      -- Init process name
      Process := As.U.Tus (Argument.Get_Program_Name);
      -- Open Stderr
      Stderr.Open (Text_Line.Out_File, Sys_Calls.Stderr);
      -- Get global mask
      Global_Mask := Parse (Environ.Getenv (Process.Image & "_TRACE_ALL"));
    end if;
  end Basic_Init;

  -- INTERNAL: Get mask for a process
  function Get_Mask (Name : in String) return Severities is
  begin
    Basic_Init;
    declare
      Env : constant String
          := Process.Image & "_TRACE" & (if Name = "" then ""
                                         else "_" & Name);
    begin
      if Environ.Is_Set (Env) then
        return Parse (Environ.Getenv (Env));
      else
        return Global_Mask;
      end if;
    end;
  end Get_Mask;

  -- INTERNAL: Common format of trace message
  function Format (Name     : in String;
                   Severity : in Severities;
                   Message  : in String) return String is
  begin
    return Images.Date_Image (Ada.Calendar.Clock, Iso => True)
         & " " & Process.Image
         & " " & (if Name = "" then "-" else Name)
         & " " & Image (Severity)
         & " -> " & Message;
  end Format;

  -- Basic_Logger
  package body Basic_Logger is
    Init : Boolean := False;
    Mask : Severities := 0;

    procedure Do_Init is
    begin
      if not Init then
        -- Init once
        Basic_Init;
        Mask := Get_Mask (Name);
        Init := True;
      end if;
    end Do_Init;

    procedure Log (Severity : in Severities;
                   Message  : in String) is
    begin
      Do_Init;
      -- Check severity
      if (Severity and Mask) = 0 then
        return;
      end if;
      -- Put message and flush
      Stderr.Put_Line (Format (Name, Severity, Message));
      Stderr.Flush;
    end Log;

    function Is_On (Severity : in Severities) return Boolean is
    begin
      Do_Init;
      return (Severity and Mask) /= 0;
    end Is_On;
  end Basic_Logger;

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
    if File_Name.Image = Stdout_Name then
      -- Stdout
      File_Name := As.U.Tus (Stdout_Name);
      File.Open (Text_Line.Out_File, Sys_Calls.Stdout);
      Flow_Is_Stderr := False;
    elsif File_Name.Is_Null or else File_Name.Image = Stderr_Name then
      -- Stderr
      File_Name := As.U.Tus (Stderr_Name);
      Flow_Is_Stderr := True;
    else
      -- Given name, expand variables
      begin
        File.Create_All (Memory.Eval (File_Name.Image));
        Flow_Is_Stderr := False;
      exception
        when others =>
          -- File cannot be created, use stderr
          File_Name := As.U.Tus (Stderr_Name);
          Flow.Open (Text_Line.Out_File, Sys_Calls.Stderr);
          Flow_Is_Stderr := True;
      end;
    end if;
    -- Set File
    if Flow_Is_Stderr then
      Flow := Stderr'Access;
    else
      Flow := File'Access;
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

  procedure Add_Mask (A_Logger : in out Logger; Mask : in Severities) is
  begin
    Check_Init (A_Logger);
    A_Logger.Mask := Severities(Bit_Ops."Or" (Natural(A_Logger.Mask),
                                              Natural(Mask)));
    Me.Log (Debug, "Add_Mask " & Image (Mask)
                 & " to logger " & A_Logger.Name.Image
                 & " -> " & Image (A_Logger.Mask));
  end Add_Mask;

  function Get_Mask (A_Logger : in out Logger) return Severities is
  begin
    Check_Init (A_Logger);
    return A_Logger.Mask;
  end Get_Mask;

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
    if Flow.Is_Open then
      Flow.Flush;
    end if;
  end Finalize;

end Trace;

