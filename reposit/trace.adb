with Ada.Calendar, Ada.Exceptions;
with Environ, Argument, Sys_Calls,
     Images, Hexa_Utils, Bit_Ops, Upper_Str, Str_Util, Gets,
     Text_Line, Socket, Computer, Parser;
package body Trace is

  -- Local Logger
  Me : Logger;

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
  Zero    : constant Severities := 0;
  function Sep (C : Character) return Boolean is
  begin
    return C = '|';
  end Sep;
  function Parse (Str : String) return Severities is
    Valid : Boolean;
    Result : Severities := Default;
    Iter : Parser.Iterator;
  begin

    Iter.Set (Str, Sep'Access);
    Log_Debug (Me, "Parsing mask " & Str);
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
            Log_Debug (Me, "- Resetting mask");
          else
            Result := Result or Severities(N);
            Log_Debug (Me, "- Oring" & N'Img);
          end if;
        exception
          when Constraint_Error =>
            Valid := False;
        end;

        if not Valid then
          -- At least INFO, which also ensures 16##
          if Word'Length < 4 then
            return Zero;
          end if;
          if Word(1..3) = "16#" and then Word(Word'Last) = '#' then
            -- Discard invalid value, including empty
            N := Hexa_Utils.Value (Word(4 .. Word'Last-1));
            if N = 0 then
              -- Reset result
              Result := 0;
              Log_Debug (Me, "- Resetting mask");
            elsif N < Natural(Severities'Last) then
              -- Skip if too large
              Result := Result or Severities(N);
              Log_Debug (Me, "- Oring" & N'Img);
            end if;
          -- Predefined severities
          elsif Word = "FATAL" then
            Result := Result or Fatal;
            Log_Debug (Me, "- Oring Fatal");
          elsif Word = "ERROR" then
             Result := Result or Error;
            Log_Debug (Me, "- Oring Error");
          elsif Word = "WARNING"
            then Result := Result or Warning;
            Log_Debug (Me, "- Oring Warning");
          elsif Word = "INFO" then
            Result := Result or Info;
            Log_Debug (Me, "- Oring Info");
          elsif Word = "DEBUG" then
            Result := Result or Debug;
            Log_Debug (Me, "- Oring Debug");
          else
            Log_Error (Me, "Invalid mask " & Word);
            return Default;
          end if;
        end if;
      end;
    end loop;
    Log_Debug (Me, "Returning mask" & Image (Result));
    return Result;
  exception
    when Error:others =>
      Log_Debug (Me, "Exception " & Ada.Exceptions.Exception_Name (Error));
      return Default;
  end Parse;


  -- Global initialisation
  ------------------------
  -- Init once :
  Is_Init : Boolean := False;
  -- - Process name
  Process : As.U.Asu_Us;
  -- - Variables PID CMD HOST DATE
  Memory : Computer.Memory_Type;
  -- - Global severity mask
  Mask : Severities;
  -- - Common flow
  Stdout_Name : constant String := "stdout";
  Stderr_Name : constant String := "stderr";
  Flow_Is_Stderr : Boolean;
  Flow : Text_Line.File_Type;
  Stderr : Text_Line.File_Type;

  -- Global flag of trace activity: True as soon as a logger has a Mask /= 0
  Active  : Boolean := False;

  procedure Init is
    Name, File : As.U.Asu_Us;
  begin
    if Is_Init then
      return;
    end if;

    Process := As.U.Tus (Argument.Get_Program_Name);

    Memory.Set ("PID", Pid_Image (Sys_Calls.Get_Pid), False, True);
    Memory.Set ("CMD", Process.Image, False, True);
    Memory.Set ("HOST", Socket.Local_Host_Name, False, True);
    Memory.Set ("DATE", Images.Date_Image (Ada.Calendar.Clock, Iso => True),
                False, True);

    Name := As.U.Tus (Upper_Str (Process.Image));
    Mask := Parse (Environ.Getenv (Name.Image & "_DEBUG_ALL"));

    File := As.U.Tus (Environ.Getenv (Name.Image & "_DEBUGFILE"));
    if File.Image = Stdout_Name then
      -- Stdout
      File := As.U.Tus (Stdout_Name);
      Flow.Open (Text_Line.Out_File, Sys_Calls.Stdout);
      Flow_Is_Stderr := False;
    elsif File.Is_Null or else File.Image = Stderr_Name then
      -- Stderr
      File := As.U.Tus (Stderr_Name);
      Flow.Open (Text_Line.Out_File, Sys_Calls.Stderr);
      Flow_Is_Stderr := True;
    else
      -- Given name, expand variables
      begin
        Flow.Create_All (Memory.Eval (File.Image));
        Flow_Is_Stderr := False;
      exception
        when others =>
          -- File cannot be created, use stderr
          File := As.U.Tus (Stderr_Name);
          Flow.Open (Text_Line.Out_File, Sys_Calls.Stderr);
          Flow_Is_Stderr := True;
      end;
    end if;
    if not Flow_Is_Stderr then
       Stderr.Open (Text_Line.Out_File, Sys_Calls.Stderr);
    end if;

    Is_Init := True;
    Set_Name (Me, "Trace");
    Log_Info (Me, "Init done with mask " & Image (Mask)
                & " on flow " & File.Image);
  end Init;

  -- Logger
  ---------
  -- Set logger name, retrieve severity
  -- Set / change the logger name
  procedure Set_Name (A_Logger : in out Logger; Name : in String) is
  begin
    -- Init if necessary
    Init;
    -- Store new name  or default
    A_Logger.Name := As.U.Tus (Name);
    -- Get ENV severity Proc_DEBUG[_Name], or global one
    declare
      Env : constant String
          := Upper_Str (Process.Image) & "_DEBUG"
           & Upper_Str (if A_Logger.Name.Is_Null
                        then ""
                        else "_" & A_Logger.Name.Image);
    begin
      if Environ.Is_Set (Env) then
        A_Logger.Mask := Parse (Environ.Getenv (Env));
      else
        A_Logger.Mask := Mask;
      end if;
    end;
    A_Logger.Init := True;
    if A_Logger.Mask /= 0 then
      Active := True;
      Log_Info (Me, "Activating traces");
    end if;
    Log_Info (Me, "Set logger name " & Name & " mask " & Image (A_Logger.Mask));
  end Set_Name;

  procedure Activate (A_Logger : in out Logger) is
  begin
    Log_Info (Me, "Activating logger " & A_Logger.Name.Image);
    Set_Name (A_Logger, A_Logger.Name.Image);
  end Activate;


  -- Severities
  ----------
  procedure Set_Mask (A_Logger : in out Logger; Mask : in Severities) is
  begin
    Init;
    A_Logger.Mask := Mask;
  end Set_Mask;

  procedure Add_Mask (A_Logger : in out Logger; Mask : in Severities) is
  begin
    if not A_Logger.Init then
      Activate (A_Logger);
    end if;
    A_Logger.Mask := Severities(Bit_Ops."Or" (Natural(A_Logger.Mask),
                                              Natural(Mask)));
  end Add_Mask;

  function Get_Mask (A_Logger : in out Logger) return Severities is
  begin
    if not A_Logger.Init then
      Activate (A_Logger);
    end if;
    return A_Logger.Mask;
  end Get_Mask;

  -- Logging
  ----------
  procedure Log (A_Logger : in out Logger;
                 Severity : in Severities;
                 Message  : in String) is
    Txt : As.U.Asu_Us;
  begin
    -- Optim
    if not Active then
      return;
    end if;
    if not A_Logger.Init then
      -- Ensure logger is init (and global init is done)
      Activate (A_Logger);
    end if;
    -- Check severity
    if Bit_Ops."And" (Natural(Severity), Natural(A_Logger.Mask)) = 0 then
      return;
    end if;
    Txt := As.U.Tus (Images.Date_Image (Ada.Calendar.Clock, Iso => True)
           & " " & Process.Image
           & " " & (if A_Logger.Name.Is_Null then "-" else A_Logger.Name.Image)
           & " " & Image (Severity)
           & " -> " & Message);
    Flow.Put_Line (Txt.Image);
    -- Flush if set on logger
    if A_Logger.Flush then
      Flow.Flush;
    end if;

    -- Put on stderr if needed
    if (Severity or Errors) /= 0
    and then not Flow_Is_Stderr
    and then Errors_On_Stderr then
      Stderr.Put_Line (Txt.Image);
    end if;
    -- Flush if set on logger
    if A_Logger.Flush then
      Stderr.Flush;
    end if;
  end Log;

  procedure Log_Fatal   (A_Logger : in out Logger; Message  : in String) is
  begin
    Log (A_Logger, Fatal, Message);
  end Log_Fatal;

  procedure Log_Error   (A_Logger : in out Logger; Message  : in String) is
  begin
    Log (A_Logger, Error, Message);
  end Log_Error;

  procedure Log_Warning (A_Logger : in out Logger; Message  : in String) is
  begin
    Log (A_Logger, Warning, Message);
  end Log_Warning;

  procedure Log_Info    (A_Logger : in out Logger; Message  : in String) is
  begin
    Log (A_Logger, Info, Message);
  end Log_Info;

  procedure Log_Debug   (A_Logger : in out Logger; Message  : in String) is
  begin
    Log (A_Logger, Debug, Message);
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

