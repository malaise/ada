with Ada.Calendar;
with Long_Longs, Aski, Basic_Proc, Argument, Argument_Parser, As.U, Normal,
     Trace.Loggers, Text_Line, Chronos.Passive_Timers, Sys_Calls, Gets, My_Math;
procedure Logrotator is
  -- Current version
  Version : constant String := "2.3";

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
   03 => (True,  'p', As.U.Tus ("period"), False, True, As.U.Tus ("number s|h|d")),
   04 => (True,  'c', As.U.Tus ("cycle"),  False, True, As.U.Tus ("number")),
   05 => (True,  's', As.U.Tus ("size"),   False, True, As.U.Tus ("number [k|M|G]")),
   06 => (True,  'f', As.U.Tus ("files"),  False, True, As.U.Tus ("number")));
  -- Target files root name
  File_Name : As.U.Asu_Us;
  -- Period in seconds, 1d
  Period : Chronos.Passive_Timers.Period_Range := 24.0 * 3600.0;
  -- Cycle : Max number of periods
  Max_Cycles_Max : constant := 999;
  Max_Cycles : Positive := 7;
  -- Max size of files
  Max_Size : Long_Longs.Ll_Natural := 0;
  -- Max number of files
  Max_Files_Max : constant := 999;
  Max_Files : Positive := Max_Files_Max;

  -- Help
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
               & " [ <period> ] [ <cycle> ] [ <size> [ <files> ] ] <file_name>");
    for Key of Keys loop
      Basic_Proc.Put_Line_Error (
        "  " & Argument_Parser.Image (Key));
    end loop;
    Basic_Proc.Put_Line_Error (
     "Default is a cycle of 7 times 1 day"
     & " and no limitation on file size or number.");
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

    -- Period then s, h or d
    if Arg_Dscr.Is_Set (03) then
      declare
        Dur : Duration;
      begin
        Arg := As.U.Tus (Arg_Dscr.Get_Option (03));
        if Arg.Length < 2 then
          raise Constraint_Error;
        end if;
        Mult := Arg.Element (Arg.Length);
        Arg.Trail (1);
        Dur := Duration'Value (Arg.Image);
        case Mult is
          when 's' => Period := Dur;
          when 'h' => Period := Dur * 3600.0;
          when 'd' => Period := Ada.Calendar.Day_Duration'Last * Dur;
          when others => raise Constraint_Error;
        end case;
      exception
        when others =>
          Error ("Invalid period " & Arg.Image);
      end;
    end if;

    -- Cycles
    if Arg_Dscr.Is_Set (04) then
      begin
        Max_Cycles := Positive'Value (Arg_Dscr.Get_Option (04));
        if Max_Cycles > Max_Cycles_Max + 1 then
          raise Constraint_Error;
        end if;
      exception
        when others =>
          Error ("Invalid cycles");
      end;
    end if;

    -- Size then optional k or M or G
    if Arg_Dscr.Is_Set (05) then
      declare
        Got_Size : Float;
        use type My_Math.Real;
      begin
        Arg := As.U.Tus (Arg_Dscr.Get_Option (05));
        if Arg.Length < 2 then
          Mult := Aski.Nul;
        else
          Mult := Arg.Element (Arg.Length);
          if Mult >= '0' and then Mult <= '9' then
            Mult := Aski.Nul;
          else
            Arg.Trail (1);
          end if;
        end if;
        Got_Size := Gets.Get_Int_Float (Arg.Image);
        case Mult is
          when Aski.Nul => Max_Size := 0;
          when 'k' => Max_Size := 1024;
          when 'M' => Max_Size := 1024 * 2014;
          when 'G' => Max_Size := 1024 * 1024 * 2014;
          when others => raise Constraint_Error;
        end case;
        Max_Size := My_Math.Trunc (My_Math.Real (Max_Size)
                                 * My_Math.Real (Got_Size) );
      exception
        when others =>
          Error ("Invalid size " & Arg.Image);
      end;
    end if;

    -- Cycle
    if Arg_Dscr.Is_Set (06) then
      if Max_Size = 0 then
        Error ("Argument <files> requires a <size> to be set");
      end if;
      begin
        Max_Files := Positive'Value (Arg_Dscr.Get_Option (06));
        if Max_Files > Max_Files_Max + 1 then
          raise Constraint_Error;
        end if;
      exception
        when others =>
          Error ("Invalid files");
      end;
    end if;

    -- End of argument parsing
    Debug ("File >" & File_Name.Image & "<, period:" & Period'Img
         & "s, cycles:" & Max_Cycles'Img & " , files size:" & Max_Size'Img
         & "b, file number:" & Max_Files'Img);
  end Parse_Arguments;

  -- Buffer and input output file
  Text : As.U.Asu_Us;
  Input, File : Text_Line.File_Type;
  -- Chronometer
  Timer : Chronos.Passive_Timers.Passive_Timer;
  -- Number of current period cycle
  Cycle_Period : Natural := 0;
  -- Number of current size cycle
  subtype Cycle_Size_Range is Natural range 000 .. Max_Files_Max;
  Cycle_Size : Cycle_Size_Range;
  Size : Long_Longs.Ll_Natural;

  -- Clean all the files for current period
  procedure Clean_All is
    Root : constant String := File_Name.Image & "_"
          & Normal (Cycle_Period, 3, Gap => '0');
    Res : Boolean;
  begin
    Debug ("Cleaning files for " & Root);
    for Y in Cycle_Size_Range range Cycle_Size_Range'First .. Max_Files - 1 loop
      declare
        Name : constant String := Root & "." & Normal (Y, 3, Gap => '0');
      begin
        -- No (more) file
        exit when not Sys_Calls.File_Found (Name);
        Debug ("Cleaning file " & Name);
        Res := Sys_Calls.Unlink (Name);
        if not Res then
          Error ("Cannot clean file " & Name);
        end if;
      end;
    end loop;
  end Clean_All;

  -- Open current input file
  procedure Open_File (New_Period : in Boolean) is
    Name : constant String
         := File_Name.Image & "_"
          & Normal (Cycle_Period, 3, Gap => '0') & "."
          & Normal (Cycle_Size, 3, Gap => '0');
  begin
    if New_Period then
      -- Clean all the files for new current period
      Clean_All;
    end if;
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
    if Cycle_Period = Max_Cycles then
      Logger.Log_Warning ("Maximum number of cycles reached");
      Cycle_Period := 0;
    end if;
    Cycle_Size := 0;
    Open_File (True);
  end Next_Period;

  -- Close and open next file, because file size reached
  procedure Next_Size is
  begin
    File.Close_All;
    Cycle_Size := Cycle_Size + 1;
    if Cycle_Size = Max_Files then
      Logger.Log_Warning ("Maximum number of sized files reached for period "
        & Normal (Cycle_Period, 3, Gap => '0') & ".");
      Cycle_Size := Cycle_Size_Range'First;
    end if;
    Open_File (False);
  end Next_Size;

begin
  Logger.Init ("Logrotator");

  -- Parse keys and options
  Parse_Arguments;

  -- Open stdin
  Input.Open_All (Text_Line.In_File);
  Debug ("Stdin open");

  -- First period
  Timer.Start ( (Delay_Kind => Chronos.Passive_Timers.Delay_Sec,
                 Clock => null,
                 Period => Period,
                 Delay_Seconds => Period));
  Cycle_Period := 0;
  Cycle_Size := 0;
  Open_File (True);

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

