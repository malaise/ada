with Ada.Calendar;
with As.U, As.B, Perpet, Dir_Mng, Normal;
with Pers_Def, Str_Mng;
package body Mesu_Nam is

  function Valid_Date (Date : File_Date_Str) return Boolean is
    Year  : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day  : Ada.Calendar.Day_Number;
  begin
    Year  := Ada.Calendar.Year_Number'Value(Date(1..4));
    Month := Ada.Calendar.Month_Number'Value(Date(5..6));
    Day   := Ada.Calendar.Day_Number'Value(Date(7..8));
    return Day <= Perpet.Nb_Days_Month (Year, Month);
  exception
    when others =>
      return False;
  end Valid_Date;

  function Valid_Pid (Pid : in String) return Boolean is
    Dummy_Pid_Val : Pers_Def.Pid_Range;
  begin
    if Pid'Length /= 3 then
      return False;
    end if;
    Dummy_Pid_Val := Pers_Def.Pid_Range'Value(Pid);
    return True;
  exception
    when others =>
      return False;
  end Valid_Pid;

  -- Check wether fields are valid
  function Valid_File_Def (Date : File_Date_Str := Wild_Date_Str;
                           Time : File_Time_Str := Wild_Time_Str;
                           Pid  : File_Pid_Str  := Wild_Pid_Str;
                           Allow_Wild : Boolean := True)
  return Boolean is
    O_Time : File_Time_Str := Time;
    Valid : Boolean;
  begin
    -- No wild card?
    if not Allow_Wild
        and then (Date = Wild_Date_Str
          or else Time = Wild_Time_Str
          or else Pid = Wild_Pid_Str) then
      return False;
    end if;
    if Date /= Wild_Date_Str and then not Valid_Date (Date) then
      return False;
    elsif Pid /= Wild_Pid_Str and then not Valid_Pid (Pid) then
      return False;
    elsif Time /= Wild_Time_Str then
      Str_Mng.Check_Time (Time, O_Time, Valid);
      return Valid;
    else
      return True;
    end if;
  exception
    when others =>
      return False;
   end Valid_File_Def;

  -- Build a file name (or a template if some " ")
  -- May raise File_Name_Error if some fields have wrong format
  --  or date is not valid
  function Build_File_Name (Date : File_Date_Str := Wild_Date_Str;
                            Time : File_Time_Str := Wild_Time_Str;
                            Pid  : File_Pid_Str  := Wild_Pid_Str)
   return File_Name_Str is
  begin
    if not Valid_File_Def (Date, Time, Pid) then
      raise File_Name_Error;
    end if;
    return Date & Time & "." & Pid;
  end Build_File_Name;

  function Build_File_Name (Mesure : Mesu_Def.Mesure_Rec) return File_Name_Str
  is
    Pid: constant File_Pid_Str := Normal (Integer(Mesure.Pid), 3, Gap => '0');
  begin
    if not Valid_File_Def (Mesure.Date,
                           Mesure.Time,
                           Pid,
                           Allow_Wild => False) then
      raise File_Name_Error;
    end if;
    return Mesure.Date & Mesure.Time & "." & Pid;
  end Build_File_Name;

  -- Check wether fields are valid
  function Valid_File_Name (File_Name : File_Name_Str) return Boolean is
    Date : File_Date_Str;
    Time, O_Time : File_Time_Str;
    Valid : Boolean;
  begin
    if File_Name(13) /= '.' then
      return False;
    end if;

    Date := File_Name (1 .. 8);
    if Date /= "????????" then
      if not Valid_Date (Date) then
        return False;
      end if;
    end if;
    Time := File_Name(9 .. 12);
    if Time /= Wild_Time_Str then
      Str_Mng.Check_Time (Time, O_Time, Valid);
      if not Valid then
        return False;
      end if;
    end if;
    if File_Name(14 .. 16) /= Wild_Pid_Str
    and then not Valid_Pid (File_Name(14 .. 16)) then
      return False;
    end if;
    return True;
  exception
    when others =>
      return False;
  end Valid_File_Name;


  -- Split a file name (or a template)
  -- May raise File_Name_Error if some fields have wrong format
  --  or date is not valid
  procedure Split_File_Name (File_Name : in File_Name_Str;
                             Date      : out File_Date_Str;
                             Time      : out File_Time_Str;
                             Pid       : out File_Pid_Str)  is
  begin
    if not Valid_File_Name (File_Name => File_Name) then
      raise File_Name_Error;
    end if;
    Date := File_Name(1 .. 8);
    Time := File_Name(9 .. 12);
    Pid := File_Name(14 .. 16);
  end Split_File_Name;

  -- Find first file available for given date and pid
  -- 0000 by default, given time if available, or next minute if available
  -- May return Wild_time_Str otherwise
  -- May raise File_Name_Error if date or pid has wild
  function Find_Slot (Date : File_Date_Str;
                      Time : File_Time_Str;
                      Pid  : File_Pid_Str) return File_Time_Str is
    File_Template : File_Name_Str;
    List : Dir_Mng.File_List_Mng.List_Type;
    File : Dir_Mng.File_Entry_Rec;
    Ret_Time : File_Time_Str;

    function Less_Than (L, R : Dir_Mng.File_Entry_Rec) return Boolean is
      use type As.U.Asu_Us;
    begin
      return L.Name < R.Name;
    end Less_Than;
    procedure My_Sort is new Dir_Mng.File_List_Mng.Sort(Less_Than);

    function Equal (Curr, Crit : Dir_Mng.File_Entry_Rec) return Boolean is
      use type As.U.Asu_Us;
    begin
      return Curr.Name = Crit.Name;
    end Equal;

    -- Increase minutes by one
    procedure Next_Minute (Time : in out File_Time_Str) is
      procedure Succ (C : in out Character) is
      begin
        C := Character'Succ (C);
      end Succ;
    begin
      if Time(4) /= '9' then
        Succ (Time(4));
        return;
      end if;
      Time(4) := '0';
      if Time(3) /= '5' then
        Succ (Time(3));
        return;
      end if;
      if Time(1..2) = "23" then
        Time := Wild_Time_Str;
        return;
      end if;
      Time(3) := '0';
      if Time(2) /= '9' then
        Succ (Time(2));
        return;
      end if;
      Succ (Time(1));
    end Next_Minute;

  begin
    -- Check no wild_char in Date or pid
    if      As.B.Locate (As.B.Tbs (Date), Wild_Char & "" ) /= 0
    or else As.B.Locate (As.B.Tbs (Pid),  Wild_Char & "" ) /= 0 then
      raise File_Name_Error;
    end if;
    -- build date????.pid
    File_Template := Build_File_Name (Date => Date, Pid => Pid);
    -- Search list of matching files
    Dir_Mng.List_Dir (List, "", File_Template);

    -- Look for lowest from provided time
    if Time = Wild_Time_Str then
      Ret_Time := "0000";
    else
      Ret_Time := Time;
    end if;
    if List.Is_Empty then
      -- Requested time is available
      return Ret_Time;
    end if;

    -- Check if this file exists
    -- Sort (by time because same date and pid)
    My_Sort (List);
    File.Name := As.U.Tus (Build_File_Name (Date => Date, Time => Ret_Time,
                                            Pid => Pid));
    if not List.Search_Match (Equal'Access, File,
                        From => Dir_Mng.File_List_Mng.Absolute) then
      -- Requested time is available
      return Ret_Time;
    end if;

    -- Requestd time already exists
    -- Look for next minute if available
    loop
      Next_Minute (Ret_Time);
      if Ret_Time = Wild_Time_Str then
        -- Nope
        return Wild_Time_Str;
      end if;
      -- Is slot available
      File.Name := As.U.Tus (Build_File_Name (Date => Date, Time => Ret_Time,
                                              Pid => Pid));
      if not List.Search_Match (Equal'Access, File,
                          From => Dir_Mng.File_List_Mng.Skip_Current) then
        -- Yesss
        return Ret_Time;
      else
        -- Nope
        return Wild_Time_Str;
      end if;
    end loop;
  end Find_Slot;

end Mesu_Nam;

