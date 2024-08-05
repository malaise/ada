with Ada.Calendar, Ada.Unchecked_Deallocation;
with Environ, Argument, Sys_Calls, Images, Hexa_Utils, Bit_Ops, Upper_Str,
     Str_Util, Gets, Parser, Ada_Words, Directory;
package body Trace is

  -- Process name for getenv
  Env_Proc_Name : As.U.Asu_Us;
  -- Env_Proc_Name seems to be finalized too early during process termination
  -- so we rely on a boolean in order to perform global init only once
  Global_Inited : Boolean := False;

  -- Public utilities
  -------------------
  -- Init the environement
  --  to a given mask for some loggers and a given log file
  procedure Init_Env (Loggers : in As.U.Asu_Array;
                      Mask : in String;
                      File : in String) is
  begin
    Global_Init;
    -- Setenv <proc>_TRACE_<logger> to <mask> for each logger
    for Logger of Loggers loop
      Sys_Calls.Setenv (Process_Name.Image & "_TRACE_" & Logger.Image, Mask);
    end loop;

    -- Setenv <proc>_TRACEFILE to <file> is set
    if File /= "" then
      Sys_Calls.Setenv (Env_Proc_Name.Image & "_TRACEFILE", File);
    end if;
    -- Re evaluate the global flow
    Set_Global_Flow (Get_Tracefile);
  end Init_Env;

  -- Operations on Severities
  function "And" (L, R : Severities) return Severities is
    (Severities(Bit_Ops."And" (Natural(L), Natural(R))));
  function "Or" (L, R : Severities) return Severities is
    (Severities(Bit_Ops."Or" (Natural(L), Natural(R))));
  function "Xor" (L, R : Severities) return Severities is
    (Severities(Bit_Ops."Xor" (Natural(L), Natural(R))));

  function Errors return Severities is (Fatal or Error);
  function Infos return Severities is (Warning or Info);

  -- Image of a severity mask
  function Image (Severity : Severities) return String is
    (case Severity is
        when Fatal   => "Fatal",
        when Error   => "Error",
        when Warning => "Warning",
        when Info    => "Info",
        when Debug   => "Debug",
        when others  => Hexa_Utils.Image (Natural(Severity)));

  -- Parse a severity string
  -- Parsing error on a severity leads to default severity, except for
  --  numeric values too high, which are discarded.
  function Sep (C : Character) return Boolean is (C = '|');

  function Parse (Str : String) return Severities is
    Valid : Boolean;
    Result : Severities := Default_Mask;
    Iter : Parser.Iterator;

  begin
    -- Optim
    if Str = "" then
      return Default_Mask;
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
            return Default_Mask;
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
          elsif Word = "FATAL+" then
            Result := Result or Fatal;
          elsif Word = "ERROR" then
             Result := Result or Error;
          elsif Word = "ERROR+" then
             Result := Result or Error or Fatal;
          elsif Word = "WARNING"
            then Result := Result or Warning;
          elsif Word = "WARNING+"
            then Result := Result or Warning or Error or Fatal;
          elsif Word = "INFO" then
            Result := Result or Info;
          elsif Word = "INFO+" then
            Result := Result or Info or Warning or Error or Fatal;
          elsif Word = "DEBUG" then
            Result := Result or Debug;
          elsif Word = "DEBUG+" then
            Result := Result or Debug or Info or Warning or Error or Fatal;
          else
            return Default_Mask;
          end if;
        end if;
      end;
    end loop;
    return Result;
  exception
    when others =>
      return Default_Mask;
  end Parse;

  -- Private operations
  ---------------------
  -- Private: Init Env_Name if necessary
  procedure Init_Env_Proc_Name is
    C : Character;
  begin
    Process_Name := As.U.Tus (Directory.Basename (Argument.Get_Program_Name));
    Env_Proc_Name := Process_Name;
    -- Same as Process except non alphanum characters replaced by '_'
    for I in 1 .. Env_Proc_Name.Length loop
      C := Env_Proc_Name.Element (I);
      if (C < '0' or else C > '9')
      and then (C < 'A' or else C > 'Z')
      and then (C < 'a' or else C > 'z') then
        Env_Proc_Name.Replace_Element (I, '_');
      end if;
    end loop;
  end Init_Env_Proc_Name;

  -- Global init: global process name, global maks, stderr, global flow
  procedure Global_Init is
  begin
    if not Global_Inited then
      Global_Inited := True;
      -- Init process name and stderr once
      Init_Env_Proc_Name;
      Stderr.Open (Text_Line.Out_File, Sys_Calls.Stderr);
    end if;

    -- Get / update global mask
    Global_Mask := Parse (Environ.Getenv (Env_Proc_Name.Image & "_TRACE_ALL"));
  end Global_Init;

  -- Get tracefile from ENV
  function Get_Tracefile return String is
  begin
    return Environ.Getenv (Env_Proc_Name.Image & "_TRACEFILE");
  end Get_Tracefile;

  -- Global flow properties
  Stdout_Name : constant String := "Stdout";
  Stderr_Name : constant String := "Stderr";
  type File_Access is access all Text_Line.File_Type;
  procedure Free_Flow is new
      Ada.Unchecked_Deallocation (Text_Line.File_Type, File_Access);
  Global_Flow : File_Access;
  Global_Flow_Name : As.U.Asu_Us;

  procedure Set_Global_Flow (New_Flow_Name : in String) is
  begin
    -- See if new flow name
    if Global_Flow /= null and then New_Flow_Name = Global_Flow_Name.Image then
      -- Flow is already set and not changed
      return;
    end if;

    -- Close and deallocate previous flow if not stderr
    if Global_Flow /= Stderr'Access and then Global_Flow /= null then
      if Global_Flow_Name.Image = Stdout_Name then
        Global_Flow.Close;
      else
        Global_Flow.Close_All;
      end if;
      Free_Flow (Global_Flow);
    end if;

    -- Set new flow
    if New_Flow_Name = "" or else New_Flow_Name = Stderr_Name then
      -- Stderr
      Global_Flow := Stderr'Access;
      Global_Flow_Name := As.U.Tus (Stderr_Name);
    else
      begin
        Global_Flow := new Text_Line.File_Type;
        if New_Flow_Name = Stdout_Name then
          -- Stdout
          Global_Flow.Open (Text_Line.Out_File, Sys_Calls.Stdout);
          Global_Flow_Name := As.U.Tus (Stdout_Name);
        else
          -- File (open or create)
          begin
            Global_Flow.Open_All (Text_Line.Out_File, New_Flow_Name,
                                  Text_Line.Start);
          exception
            when Text_Line.Name_Error =>
              Global_Flow.Create_All (New_Flow_Name);
          end;
        end if;
        Global_Flow_Name := As.U.Tus (New_Flow_Name);
      exception
        when others =>
          Global_Flow := Stderr'Access;
          Global_Flow_Name := As.U.Tus (Stderr_Name);
      end;
    end if;

  end Set_Global_Flow;

  -- Private: Get mask for a process
  function Get_Mask (Name : in String) return Severities is
  begin
    Global_Init;
    if Name /= "" and then not Ada_Words.Is_Identifier (Name) then
      return Severities'First;
    end if;
    declare
      Env : constant String
          := Env_Proc_Name.Image & "_TRACE" & (if Name = "" then ""
                                               else "_" & Name);
    begin
      if Environ.Is_Set (Env) then
        return Parse (Environ.Getenv (Env));
      else
        return Global_Mask;
      end if;
    end;
  end Get_Mask;

  -- Private: Global output
  procedure Global_Log (Message : in String;
                        Flush : in Boolean;
                        Log_On_Error : in Boolean) is
  begin
    Global_Flow.Put_Line (Message);
    if Flush then
      Global_Flush;
    end if;

    -- Put also on stderr if needed
    if Log_On_Error and then Global_Flow /= Stderr'Access then
      Stderr.Put_Line (Message);
      -- Flush if set on logger
      if Flush then
        Stderr.Flush;
      end if;
    end if;
  end Global_Log;

  procedure Global_Flush is
  begin
    Global_Flow.Flush;
  end Global_Flush;

 function Global_Is_Stderr return Boolean is (Global_Flow = Stderr'Access);

  -- Private: Common format of trace message
  function Format (Name     : in String;
                   Severity : in Severities;
                   Message  : in String) return String is
    (Images.Date_Image (Ada.Calendar.Clock)
         & " " & Process_Name.Image
         & " " & (if Name = "" then "-" else Name)
         & " " & Image (Severity)
         & " -> " & Message);

  -- Basic_Logger
  package body Basic_Logger is
    Inited : Boolean := False;
    Mask : Severities := 0;
    Flush_Flow : Boolean := True;
    Err_On_Stderr : Boolean := True;

    procedure Basic_Init is
    begin
      if not Inited then
        -- Init once
        Global_Init;
        Set_Global_Flow (Get_Tracefile);
        Mask := Get_Mask (Name);
        Inited := True;
      end if;
    end Basic_Init;

    -- Severities
    -------------
    procedure Set_Mask (Mask : in Severities) is
    begin
      Basic_Init;
      Basic_Logger.Mask := Mask;
    end Set_Mask;
    function Get_Mask return Severities is
    begin
      Basic_Init;
      return Basic_Logger.Mask;
    end Get_Mask;
    procedure Add_Mask (Mask : in Severities) is
    begin
      Basic_Init;
      Basic_Logger.Mask := Basic_Logger.Mask or Mask;
    end Add_Mask;
    procedure Del_Mask (Mask : in Severities) is
    begin
      Basic_Init;
      Basic_Logger.Mask := Basic_Logger.Mask xor Mask;
    end Del_Mask;

    -- Check if a severity is active
    function Is_On (Severity : in Severities) return Boolean is
    begin
      Basic_Init;
      return (Mask and Severity) /= 0;
    end Is_On;
    function Fatal_On return Boolean is
    begin
      Basic_Init;
      return (Mask and Fatal) /= 0;
    end Fatal_On;
    function Error_On return Boolean is
    begin
      Basic_Init;
      return (Mask and Error) /= 0;
    end Error_On;
    function Warning_On return Boolean is
    begin
      Basic_Init;
      return (Mask and Warning) /= 0;
    end Warning_On;
    function Info_On return Boolean is
    begin
      Basic_Init;
      return (Mask and Info) /= 0;
    end Info_On;
    function Debug_On return Boolean is
    begin
      Basic_Init;
      return (Mask and Debug) /= 0;
    end Debug_On;

    -- Does the logger log on Stderr
    function Flow_Is_Stderr return Boolean is (Global_Is_Stderr);

    -- Logging
    ----------
    procedure Log (Severity : in Severities;
                   Message  : in String) is
      Txt : As.U.Asu_Us;
    begin
      Basic_Init;
      -- Check severity
      if (Severity and Mask) = 0 then
        return;
      end if;
      Txt := As.U.Tus (Format (Name, Severity, Message));

      -- Put message and flush
      Lock.Get;
      Global_Log (Txt.Image,
                  Flush_Flow,
                  Err_On_Stderr and then (Severity and Errors) /= 0);

      Lock.Release;
    exception
      when others =>
        if Lock.Is_Owner then
          Lock.Release;
        end if;
        raise;
    end Log;

    procedure Log_Fatal   (Message  : in String) is
    begin
      Log (Fatal, Message);
    end Log_Fatal;

    procedure Log_Error   (Message  : in String) is
    begin
      Log (Error, Message);
    end Log_Error;
    procedure Log_Warning (Message  : in String) is
    begin
      Log (Warning, Message);
    end Log_Warning;
    procedure Log_Info    (Message  : in String) is
    begin
      Log (Info, Message);
    end Log_Info;
    procedure Log_Debug   (Message  : in String) is
    begin
      Log (Debug, Message);
    end Log_Debug;


    -- Flushing
    -----------
    -- Flush logs of a logger
    procedure Flush is
    begin
      Basic_Init;
      Global_Flush;
    end Flush;

    -- Configure logger to flush each message
    procedure Set_Flush (Activate : in Boolean) is
    begin
      Flush_Flow := Activate;
    end Set_Flush;
    function Flush_Set return Boolean is (Flush_Flow);

     -- Configure logger to also log errors (Fatal & Error) on stderr
    procedure Errors_On_Stderr (Activate : Boolean := True) is
    begin
      Err_On_Stderr := Activate;
    end Errors_On_Stderr;
    function Are_Errors_On_Stderr return Boolean is (Err_On_Stderr);

  end Basic_Logger;

end Trace;

