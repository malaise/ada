with Ada.Calendar;
with Long_Longs, Aski, Basic_Proc, Argument, Argument_Parser, As.U, Normal,
     Trace.Loggers, Text_Line, Timers, Chronos.Passive_Timers;
procedure Logrotator is
  -- Current version
  Version : constant String := "1.2";

  -- Trace logger
  Logger : Trace.Loggers.Logger;
  procedure Debug (Message  : in String) is
  begin
    Logger.Log_Debug (Message);
  end Debug;

  -- Result of argument parsing and default values
   -- Argument description
   Keys : constant Argument_Parser.The_Keys_Type := (
   01 => (False, 'h', As.U.Tus ("help"),    False),
   02 => (False, 'v', As.U.Tus ("version"), False),
   03 => (True,  'c', As.U.Tus ("cycle"),  False, True, As.U.Tus ("number")),
   04 => (True,  'p', As.U.Tus ("period"), False, True, As.U.Tus ("number s|h|d")),
   05 => (True,  's', As.U.Tus ("size"),   False, True, As.U.Tus ("number [k|M]")));
  -- Target files root name
  File_Name : As.U.Asu_Us;
  -- Period in seconds, 1d
  Period : Timers.Period_Range := 24.0 * 3600.0;
  -- Cycle : Max number of periods
  Cycle : Positive := 9;
  -- Max size of files
  Max_Size : Long_Longs.Ll_Natural := 0;

  -- Help
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
               & " [ <period> ] [ <cycle> ] [ <size> ] <file_name>");
    for Key of Keys loop
      Basic_Proc.Put_Line_Error (
        "  " & Argument_Parser.Image (Key));
    end loop;
    Basic_Proc.Put_Line_Error (
     "Default is a cycle of 9 times 1 day"
     & " and no file size limitation.");
  end Usage;

  Parse_Error : exception;
  procedure Error (Message : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Message & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Parse_Error;
  end Error;

  -- Parse the arguments
  procedure Parse_Arguments is

     Arg_Dscr : Argument_Parser.Parsed_Dscr;
     No_Key_Index : constant Argument_Parser.The_Keys_Index
                  := Argument_Parser.No_Key_Index;

    -- Temporary string, value and multiplicator
    Arg : As.U.Asu_Us;
    Val : Natural;
    Mult : Character;
  begin
    Arg_Dscr := Argument_Parser.Parse (Keys);
    if not Arg_Dscr.Is_Ok then
      Error (Arg_Dscr.Get_Error);
    end if;

    -- Help and version
    if Arg_Dscr.Is_Set (1) then
      -- Help
      Usage;
      raise Parse_Error;
    end if;
    if Arg_Dscr.Is_Set (2) then
      -- Version
      if Argument.Get_Nbre_Arg /= 1 then
        Error ("Invalid arguments");
      else
        Basic_Proc.Put_Line_Output (Argument.Get_Program_Name & " V" & Version);
        raise Parse_Error;
      end if;
    end if;

    -- Normal mode, need one file name, after options
    if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) /= 1
    or else Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
      Error ("Invalid arguments");
    end if;
    File_Name := As.U.Tus (Arg_Dscr.Get_Option (No_Key_Index));

    -- Cycle
    if Arg_Dscr.Is_Set (03) then
      begin
        Cycle := Positive'Value (Arg_Dscr.Get_Option (03));
      exception
        when others =>
          Error ("Invalid cycle");
      end;
    end if;

    -- Period then s, h or d
    if Arg_Dscr.Is_Set (04) then
      begin
        Arg := As.U.Tus (Arg_Dscr.Get_Option (04));
        if Arg.Length < 2 then
          raise Constraint_Error;
        end if;
        Mult := Arg.Element (Arg.Length);
        Arg.Trail (1);
        Val := Positive'Value (Arg.Image);
        case Mult is
          when 's' => Period := Duration (Val);
          when 'h' => Period := Duration (Val) * 3600.0;
          when 'd' => Period := Ada.Calendar.Day_Duration'Last * Duration (Val);
          when others => raise Constraint_Error;
        end case;
      exception
        when others =>
          Error ("Invalid period " & Arg.Image);
      end;
    end if;

    -- Size then optional k or M
    if Arg_Dscr.Is_Set (05) then
      begin
        Arg := As.U.Tus (Arg_Dscr.Get_Option (05));
        if Arg.Length < 2 then
          Mult := Aski.Nul;
        else
          Mult := Arg.Element (Arg.Length);
          Arg.Trail (1);
        end if;
        Max_Size := Long_Longs.Ll_Positive'Value (Arg.Image);
        case Mult is
          when Aski.Nul => null;
          when 'k' => Max_Size := 1024 * Max_Size;
          when 'M' => Max_Size := 1024 * 2014 * Max_Size;
          when others => raise Constraint_Error;
        end case;
      exception
        when others =>
          Error ("Invalid size " & Arg.Image);
      end;
    end if;

    -- End of argument parsing
    Debug ("File >" & File_Name.Image & "<, period:" & Period'Img
         & "s, cycle:" & Cycle'Img & " , size:" & Max_Size'Img);
  end Parse_Arguments;

  -- Buffer and input output file
  Text : As.U.Asu_Us;
  Input, File : Text_Line.File_Type;
  -- Chronometer
  Timer : Chronos.Passive_Timers.Passive_Timer;
  -- Number of current period cycle
  Cycle_Period : Natural := 0;
  -- Number of current size cycle
  Max_Cycles_Size : constant := 999;
  subtype Cycle_Size_Range is Natural range 000 .. Max_Cycles_Size;
  Cycle_Size : Cycle_Size_Range;
  Size : Long_Longs.Ll_Natural;

  -- Open current input file
  procedure Open_File is
    Name : constant String
         := File_Name.Image & "_"
          & Normal (Cycle_Period, 3, Gap => '0') & "."
          & Normal (Cycle_Size, 3, Gap => '0');
  begin
    Debug ("Creating file " & Name);
    File.Create_All (Name);
    Size := 0;
  exception
    when others =>
      Error ("Cannot create file " & Name);
      raise;
  end Open_File;

  -- Close and open next file, because period expiration
  procedure Next_Period is
  begin
    File.Close_All;
    Cycle_Period := Cycle_Period + 1;
    if Cycle_Period = Cycle then
      Cycle_Period := 0;
    end if;
    Cycle_Size := 0;
    Open_File;
  end Next_Period;

  -- Close and open next file, because file size reached
  procedure Next_Size is
  begin
    File.Close_All;
    if Cycle_Size = Cycle_Size_Range'Last then
      Logger.Log_Warning ("Maximum number of sized files reached for period "
        & Normal (Cycle_Period, 3, Gap => '0') & ".");
      Cycle_Size := Cycle_Size_Range'First;
    else
      Cycle_Size := Cycle_Size + 1;
    end if;
    Open_File;
  end Next_Size;

begin
  Logger.Init ("Logrotator");

  -- Parse keys and options
  Parse_Arguments;

  -- Open stdin
  Input.Open_All (Text_Line.In_File);
  Debug ("Stdin open");

  -- First period
  Timer.Start ( (Delay_Kind => Timers.Delay_Sec,
                 Clock => null,
                 Period => Period,
                 Delay_Seconds => Period));
  Cycle_Period := 0;
  Cycle_Size := 0;
  Open_File;

  -- Main loop
  Periods : loop
    -- Get a buffer as long as some text
    Text := Input.Get;
    exit Periods when Text.Is_Null;

    -- Close/Skip past occurences of period
    while Timer.Has_Expired loop
      Next_Period;
    end loop;

    -- See if file would not be too large
    if Max_Size /= 0
    and then Size /= 0
    and then Size + Long_Longs.Ll_Natural (Text.Length) > Max_Size then
      Next_Size;
    end if;

    -- Put output
    File.Put (Text.Image);
    Size := Size + Long_Longs.Ll_Natural (Text.Length);

  end loop Periods;

  -- Done
  File.Close_All;

exception
  when Parse_Error =>
    null;
end Logrotator;

