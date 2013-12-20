with Ada.Calendar;
with Environ, Argument, Sys_Calls,
     Images, Hexa_Utils, Bit_Ops, Upper_Str, Str_Util, Gets, Parser;
package body Trace is

  -- Public utilities
  -------------------
  -- Operations on Severities
  function "And" (L, R : Severities) return Severities is
  begin
    return Severities(Bit_Ops."And" (Natural(L), Natural(R)));
  end "And";
  function "Or" (L, R : Severities) return Severities is
  begin
    return Severities(Bit_Ops."Or" (Natural(L), Natural(R)));
  end "Or";
  function "Xor" (L, R : Severities) return Severities is
  begin
    return Severities(Bit_Ops."Xor" (Natural(L), Natural(R)));
  end "Xor";

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


  -- Private operations
  ---------------------
  -- Private: Minimal init: global process name, global maks and stderr
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

  -- Private: Get mask for a process
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

  -- Private: Common format of trace message
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
    ----------
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
    begin
      Do_Init;
      -- Check severity
      if (Severity and Mask) = 0 then
        return;
      end if;
      -- Put message and flush
      Lock.Get;
      Stderr.Put_Line (Format (Name, Severity, Message));
      if Flus then
        Stderr.Flush;
      end if;
      Lock.Release;
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
      Stderr.Flush;
    end Flush;

    -- Configure logger to flush each message
    procedure Set_Flush (Each : in Boolean) is
    begin
      Flus := Each;
    end Set_Flush;

  end Basic_Logger;

end Trace;

