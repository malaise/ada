package body Perpet is

  type Time_Rec is record
    Year    : Calendar.Year_Number;
    Month   : Calendar.Month_Number;
    Day     : Calendar.Day_Number;
    Seconds : Calendar.Day_Duration;
  end record;

  -- Is a year leap
  function Is_Leap_Year (Year  : Calendar.Year_Number) return Boolean is
  begin
    -- Year is multiple of 4 and not 100, or multiple of 400
    -- the parenthesis tend to optimize:
    --  return FALSE asap in case of year not multiple of 4
    return Year rem 4 = 0 and then
     (Year rem 100 /= 0 or else Year rem 400 = 0);
  end Is_Leap_Year;

  -- Number of days of a month
  function Nb_Days_Month (
   Year  : Calendar.Year_Number;
   Month : Calendar.Month_Number) return Calendar.Day_Number is
    Last_Day_Array : constant array (Calendar.Month_Number) of
     Calendar.Day_Number :=
     (01 => 31, 02 => 28, 03 => 31, 04 => 30, 05 => 31, 06 => 30,
      07 => 31, 08 => 31, 09 => 30, 10 => 31, 11 => 30, 12 => 31);
  begin
    if Month /= 2 then return Last_Day_Array(Month); end if;

    -- February
    if Is_Leap_Year (Year) then
      -- Leap year
      return Last_Day_Array(Month) + 1;
    else
      -- Non leap year
      return Last_Day_Array(Month);
    end if;
  end Nb_Days_Month;

  -- Number of days of a year
  function Nb_Days_Year (Year : Calendar.Year_Number) return Day_Range is
  begin
    if Is_Leap_Year (Year) then
      -- Leap year
      return 366;
    else
      -- Non leap year
      return 365;
    end if;
  end Nb_Days_Year;

  -- Check date validity
  function Is_Valid (
   Year  : Calendar.Year_Number;
   Month : Calendar.Month_Number;
   Day   : Calendar.Day_Number) return Boolean is
  begin
    return Day <= Nb_Days_Month (Year, Month);
  end Is_Valid;

  -- TIME_REC operations
  function Split (Date : Calendar.Time) return Time_Rec is
    Rec : Time_Rec;
  begin

    Calendar.Split (Date => Date,
                    Year    => Rec.Year,
                    Month   => Rec.Month,
                    Day     => Rec.Day,
                    Seconds => Rec.Seconds);
    return Rec;
  end Split;

  function Time_Of (Rec : Time_Rec) return Calendar.Time is
  begin
    return Calendar.Time_Of (Year    => Rec.Year,
                             Month   => Rec.Month,
                             Day     => Rec.Day,
                             Seconds => Rec.Seconds);
  end Time_Of;

  -- Add years & months to a time_rec
  function "+" (Date : Time_Rec; Months : Duration_Rec) return Time_Rec is
    Sum : Integer;
    D : Time_Rec := Date;
  begin
    D.Year := D.Year + Months.Years;
    Sum := D.Month + Months.Months;
    if Sum > 12 then
      D.Month := Sum - 12;
      D.Year := D.Year + 1;
    else
      D.Month := Sum;
    end if;
    -- trunc
    Sum := Nb_Days_Month (D.Year, D.Month);
    if D.Day > Sum then
      D.Day := Sum;
    end if;
    return D;
  exception
    when others => raise Time_Error;
  end "+";

  -- Substract years & months from a time_rec
  function "-" (Date : Time_Rec; Months : Duration_Rec) return Time_Rec is
    Sum : Integer;
    D : Time_Rec := Date;
  begin
    D.Year := D.Year - Months.Years;
    Sum := D.Month - Months.Months;
    if Sum < 1 then
      D.Month := Sum + 12;
      D.Year := D.Year - 1;
    else
      D.Month := Sum;
    end if;
    -- trunc
    Sum := Nb_Days_Month (D.Year, D.Month);
    if D.Day > Sum then
      D.Day := Sum;
    end if;
    return D;
  exception
    when others => raise Time_Error;
  end "-";

  -- Add years & months to a time
  function "+" (Date : Calendar.Time; Months : Duration_Rec)
   return Calendar.Time is
  begin
    return Time_Of (Split(Date) + Months);
  exception
    when others => raise Time_Error;
  end "+";

  -- Substract years & months from a time
  function "-" (Date : Calendar.Time; Months : Duration_Rec)
   return Calendar.Time is
  begin
    return Time_Of (Split(Date) - Months);
  exception
    when others => raise Time_Error;
  end "-";


  -- tries to go to 1st of next month
  -- If not, remaining is set to 0
  procedure Next_Month (
   Date      : in out Time_Rec;
   Remaining : in out Day_Range) is
    Sum : Integer;
  begin
    Sum := Nb_Days_Month (Date.Year, Date.Month);
    if Sum - Date.Day >= Remaining then
      -- Not enough days remaining. Same Month
      Date.Day := Date.Day + Remaining;
      Remaining := 0;
    else
      -- 1st of next month
      Remaining := Remaining - (Sum - Date.Day + 1);
      Date.Day := 1;
      Date := Date + (Years => 0, Months => 1);
    end if;
  end Next_Month;

  -- tries to go to last of previous month
  -- If not, remaining is set to 0
  procedure Prev_Month (
   Date      : in out Time_Rec;
   Remaining : in out Day_Range) is
  begin
    if Date.Day > Remaining then
      -- Not enough days remaining. Same Month
      Date.Day := Date.Day - Remaining;
      Remaining := 0;
    else
      -- last of previous month
      Remaining := Remaining - Date.Day;
      Date := Date - (Years => 0, Months => 1);
      Date.Day := Nb_Days_Month (Date.Year, Date.Month);
    end if;
  end Prev_Month;


  -- Add days to a time
  function "+" (Date : Calendar.Time; Days : Day_Range)
   return Calendar.Time is
    Rec : Time_Rec := Split (Date);
    Remaining : Day_Range := Days;
    Sum : Integer;
  begin
    -- try to go to 1st january next year
    loop
      if Remaining = 0 then
        -- done
        return Time_Of(Rec);
      end if;
      exit when Rec.Month = 1 and then Rec.Day = 1;
      Next_Month (Rec, Remaining);
    end loop;

    -- try to add years
    loop
      Sum := Nb_Days_Year (Rec.Year);
      exit when Remaining < Sum;
      Remaining := Remaining - Sum;
      Rec.Year := Rec.Year + 1;
    end loop;

    -- Complete date
    while Remaining /= 0 loop
      Next_Month (Rec, Remaining);
    end loop;

    return Time_Of(Rec);
  end "+";

  -- Substract days from a time
  function "-" (Date : Calendar.Time; Days : Day_Range)
   return Calendar.Time is
    Rec : Time_Rec := Split (Date);
    Remaining : Day_Range := Days;
    Sum : Integer;
  begin
    -- try to go to 31th december previous year
    loop
      if Remaining = 0 then
        -- done
        return Time_Of(Rec);
      end if;
      exit when Rec.Month = 12 and then Rec.Day = 31;
      Prev_Month (Rec, Remaining);
    end loop;

    -- try to substract years
    loop
      Sum := Nb_Days_Year (Rec.Year);
      exit when Remaining < Sum;
      Remaining := Remaining - Sum;
      Rec.Year := Rec.Year - 1;
    end loop;

    -- Complete date
    while Remaining /= 0 loop
      Prev_Month (Rec, Remaining);
    end loop;

    return Time_Of(Rec);
  end "-";

  -- Nb of days and secs between two dates
  function "-" (Date_1, Date_2 : Calendar.Time)
    return Delta_Rec is
    Delta_Val : Delta_Rec;
    Rec_1, Rec_2 : Time_Rec;
    use Calendar;
  begin
    if Date_1 < Date_2 then
      raise Time_Error;
    end if;
    Delta_Val.Days := 0;
    Delta_Val.Secs := 0.0;
    if Date_1 = Date_2 then
      return Delta_Val;
    end if;
    Rec_1 := Split(Date_1);
    Rec_2 := Split(Date_2);

    if Rec_2.Year = Rec_1.Year
    and then Rec_2.Month = Rec_1.Month
    and then Rec_2.Day = Rec_1.Day then
      -- Same day
      Delta_Val.Secs := Rec_1.Seconds - Rec_2.Seconds;
      return Delta_Val;
    end if;

    -- End of day 2, beginning of day 1
    if Rec_1.Seconds >= Rec_2.Seconds then
      Delta_Val.Days := 1;
      Delta_Val.Secs := Rec_1.Seconds - Rec_2.Seconds;
    else
      Delta_Val.Secs := Rec_1.Seconds + (86_400.0 - Rec_2.Seconds);
    end if;

    if Rec_2.Year = Rec_1.Year
    and then Rec_2.Month = Rec_1.Month then
      -- Same month
      Delta_Val.Days := Delta_Val.Days + (Rec_1.Day - 1) - Rec_2.Day;
      return Delta_Val;
    end if;

    -- End of month 2, beginning of month 1
    Delta_Val.Days := Delta_Val.Days + Nb_Days_Month (Rec_2.Year, Rec_2.Month) - Rec_2.Day;
    Delta_Val.Days := (Delta_Val.Days + Rec_1.Day) - 1;

    if Rec_2.Year = Rec_1.Year then
      for Month in Rec_2.Month + 1 .. Rec_1.Month - 1 loop
        Delta_Val.Days := Delta_Val.Days + Nb_Days_Month (Rec_2.Year, Month);
      end loop;
      return Delta_Val;
    end if;

    -- End of year 2, beginning of year 1
    if Rec_2.Month /= 12 then
      for Month in Rec_2.Month + 1 .. 12 loop
        Delta_Val.Days := Delta_Val.Days + Nb_Days_Month (Rec_2.Year, Month);
      end loop;
    end if;
    if Rec_1.Month /= 1 then
      for Month in 1 .. Rec_1.Month -1 loop
        Delta_Val.Days := Delta_Val.Days + Nb_Days_Month (Rec_1.Year, Month);
      end loop;
    end if;

    -- Add years
    for Year in Rec_2.Year + 1 .. Rec_1.Year - 1 loop
      Delta_Val.Days := Delta_Val.Days + Nb_Days_Year (Year);
    end loop;

    return Delta_Val;

  end "-";

  -- type DAY_OF_WEEK_LIST is (MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, STURDAY, SUNDAY);
  function Get_Day_Of_Week (Date : Calendar.Time) return Day_Of_Week_List is
    Delta_Days : Day_Range;
    Ref_Rec : constant Time_Rec := (
     Year => Calendar.Year_Number'First,
     Month => Calendar.Month_Number'First,
     Day =>  Calendar.Day_Number'First,
     Seconds => 0.0);
    Ref_Date : constant Calendar.Time := Time_Of (Ref_Rec);
    Ref_Day_Of_Week : constant Day_Of_Week_List := Tuesday;
  begin
    Delta_Days := "-" (Date, Ref_Date).Days;
    return Day_Of_Week_List'Val ( (Delta_Days + Day_Of_Week_List'Pos(Ref_Day_Of_Week)) rem 7);
  end Get_Day_Of_Week;

  -- subtype WEEK_OF_YEAR_RANGE is NATURAL range 1 .. 53;
  function Get_Week_Of_Year (Date : Calendar.Time) return Week_Of_Year_Range is
    Rec_0 : Time_Rec;
    Date_0 : Calendar.Time;
    Day_Date_0 : Day_Of_Week_List;
    Week_Of_Week_0 : Week_Of_Year_Range;
    Date_0_Offset : Day_Range;

    Delta_Days : Delta_Rec;

    Max_Nb_Days_A_Week : constant := Day_Of_Week_List'Pos(Day_Of_Week_List'Last) + 1;
    Week_Offset : Natural range 0 .. Week_Of_Year_Range'Last;
    Week_Of_Date : Week_Of_Year_Range;
  begin
    -- 01/01 of the year
    Rec_0 := Split (Date);
    Rec_0.Month := 1;
    Rec_0.Day := 1;
    Rec_0.Seconds := 0.0;
    Date_0 := Time_Of(Rec_0);
    -- Day of week of it
    Day_Date_0 := Get_Day_Of_Week(Date_0);
    -- No of first week is 1 if day_date_0 is Monday .. Thursday
    if Day_Date_0 <= Thursday then
      Week_Of_Week_0 := 1;
    else
      -- 52 or 53? The same as day before day_0
      Week_Of_Week_0 := Get_Week_Of_Year(Date_0 - 1);
    end if;
    -- Number of days of first week in the previous year
    Date_0_Offset := Day_Of_Week_List'Pos(Day_Date_0);

    -- Nb of days between date and 01/01
    Delta_Days := Date - Date_0;
    -- Nb of days between date and Monday of first week
    Delta_Days.Days := Delta_Days.Days + Date_0_Offset;

    -- Week offset from first week
    Week_Offset := Delta_Days.Days / Max_Nb_Days_A_Week;
    -- Week no
    if Week_Offset = 0 then
      -- First week
      Week_Of_Date := Week_Of_Week_0;
    elsif Week_Of_Week_0 = 1 then
      -- Week 1 + offset
      Week_Of_Date := Week_Offset + 1;
    else
      -- 52/53 -> 0, + offset
      Week_Of_Date := Week_Offset;
    end if; 
    return Week_Of_Date;
    
  end Get_Week_Of_Year;


end Perpet;
