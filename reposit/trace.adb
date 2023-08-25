with Ada.Calendar;
with Environ, Argument, Sys_Calls, Images, Hexa_Utils, Bit_Ops, Upper_Str,
     Str_Util, Gets, Parser, Ada_Words, Directory;
package body Trace is
  -- Private operations
  procedure Init_Env_Name;

  -- Public utilities
  -------------------
  -- Init the environement
  --  to a given mask for some loggers and a given log file
  procedure Init_Env (Loggers : in As.U.Asu_Array;
                      Mask : in String;
                      File : in String) is
  begin
    -- Setenv <proc>_TRACE_<logger> to <mask> for each logger
    for Logger of Loggers loop
      Sys_Calls.Setenv (Env_Proc.Image & "_TRACE_" & Logger.Image, Mask);
    end loop;

    -- Setenv <proc>_TRACEFILE to <file> is set
    if File /= "" then
      Sys_Calls.Setenv (Trace.Env_Proc.Image & "_TRACEFILE", File);
    end if;
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

  -- Private operations
  ---------------------
  -- Private: Init Env_Name if necessary
  procedure Init_Env_Name is
    C : Character;
  begin
    if not Env_Proc.Is_Null then
      return;
    end if;
    Env_Proc := As.U.Tus (Directory.Basename (Argument.Get_Program_Name));
    -- Same as Process except non alphanum characters replaced by '_'
    for I in 1 .. Env_Proc.Length loop
      C := Env_Proc.Element (I);
      if (C < '0' or else C > '9')
      and then (C < 'A' or else C > 'Z')
      and then (C < 'a' or else C > 'z') then
        Env_Proc.Replace_Element (I, '_');
      end if;
    end loop;
  end Init_Env_Name;

  -- Private: Global init: global process name, global maks and stderr
  procedure Global_Init is
  begin
    if Process.Is_Null then
      -- Init process name
      Process := As.U.Tus (Directory.Basename (Argument.Get_Program_Name));
      Init_Env_Name;
      -- Open Stderr
      Stderr.Open (Text_Line.Out_File, Sys_Calls.Stderr);
      -- Get global mask
      Global_Mask := Parse (Environ.Getenv (Env_Proc.Image & "_TRACE_ALL"));
    end if;
  end Global_Init;

  -- Init of a basic logger
  type File_Type_Access is access all Text_Line.File_Type;
  Flow : File_Type_Access;
  procedure Basic_Init is
    Flow_Name : As.U.Asu_Us;
  begin
    Global_Init;
    -- Open flow if necessary
    if Flow /= null then
      return;
    end if;
    Flow_Name := As.U.Tus (Environ.Getenv (Env_Proc.Image & "_TRACEFILE"));
    if Flow_Name.Is_Null or else Flow_Name.Image = "Stderr" then
      -- Stderr
      Flow := Stderr'Access;
    elsif Flow_Name.Image = "Stdout" then
      -- Stdout
      Flow := new Text_Line.File_Type;
      Flow.Open (Text_Line.Out_File, Sys_Calls.Stdout);
    else
      -- File (open or create)
      Flow := new Text_Line.File_Type;
      begin
        begin
          Flow.Open_All (Text_Line.Out_File, Flow_Name.Image);
        exception
          when Text_Line.Name_Error =>
            Flow.Create_All (Flow_Name.Image);
        end;
      exception
        when others =>
          Flow := Stderr'Access;
      end;
    end if;
  end Basic_Init;

  -- Private: Get mask for a process
  function Get_Mask (Name : in String) return Severities is
  begin
    Global_Init;
    if Name /= "" and then not Ada_Words.Is_Identifier (Name) then
      return Severities'First;
    end if;
    declare
      Env : constant String
          := Env_Proc.Image & "_TRACE" & (if Name = "" then ""
                                          else "_" & Name);
    begin
      if Environ.Is_Set (Env) then
        return Parse (Environ.Getenv (Env));
      else
        return Global_Mask;
      end if;
    end;
  end Get_Mask;

  -- Private: Common format of trace message
  function Format (Name     : in String;
                   Severity : in Severities;
                   Message  : in String) return String is
    (Images.Date_Image (Ada.Calendar.Clock)
         & " " & Process.Image
         & " " & (if Name = "" then "-" else Name)
         & " " & Image (Severity)
         & " -> " & Message);

  -- Basic_Logger
  package body Basic_Logger is
    Init : Boolean := False;
    Mask : Severities := 0;
    Flus : Boolean := True;

    procedure Do_Init is
    begin
      if not Init then
        -- Init once
        Basic_Init;
        Mask := Get_Mask (Name);
        Init := True;
      end if;
    end Do_Init;

    -- Severities
    -------------
    procedure Set_Mask (Mask : in Severities) is
    begin
      Do_Init;
      Basic_Logger.Mask := Mask;
    end Set_Mask;
    function Get_Mask return Severities is
    begin
      Do_Init;
      return Basic_Logger.Mask;
    end Get_Mask;
    procedure Add_Mask (Mask : in Severities) is
    begin
      Do_Init;
      Basic_Logger.Mask := Basic_Logger.Mask or Mask;
    end Add_Mask;
    procedure Del_Mask (Mask : in Severities) is
    begin
      Do_Init;
      Basic_Logger.Mask := Basic_Logger.Mask xor Mask;
    end Del_Mask;

    -- Check if a severity is active
    function Is_On (Severity : in Severities) return Boolean is
    begin
      Do_Init;
      return (Mask and Severity) /= 0;
    end Is_On;
    function Fatal_On return Boolean is
    begin
      Do_Init;
      return (Mask and Fatal) /= 0;
    end Fatal_On;
    function Error_On return Boolean is
    begin
      Do_Init;
      return (Mask and Error) /= 0;
    end Error_On;
    function Warning_On return Boolean is
    begin
      Do_Init;
      return (Mask and Warning) /= 0;
    end Warning_On;
    function Info_On return Boolean is
    begin
      Do_Init;
      return (Mask and Info) /= 0;
    end Info_On;
    function Debug_On return Boolean is
    begin
      Do_Init;
      return (Mask and Debug) /= 0;
    end Debug_On;

    -- Logging
    ----------
    procedure Log (Severity : in Severities;
                   Message  : in String) is
      Txt : As.U.Asu_Us;
    begin
      Do_Init;
      -- Check severity
      if (Severity and Mask) = 0 then
        return;
      end if;
      Txt := As.U.Tus (Format (Name, Severity, Message));

      -- Put message and flush
      Lock.Get;
      Flow.Put_Line (Format (Name, Severity, Message));
      if Flus then
        Flow.Flush;
      end if;

     -- Put also on stderr if needed
      if Flow /= Stderr'Access
      and then (Severity and Errors) /= 0 then
        Stderr.Put_Line (Txt.Image);
        -- Flush if set on logger
        if Flus then
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
      Do_Init;
      Flow.Flush;
    end Flush;

    -- Configure logger to flush each message
    procedure Set_Flush (Each : in Boolean) is
    begin
      Do_Init;
      Flus := Each;
    end Set_Flush;

  end Basic_Logger;

end Trace;

