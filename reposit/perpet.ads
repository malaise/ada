with Calendar;
package Perpet is

  -- Number of days of a month
  function Nb_Days_Month (
   Year  : Calendar.Year_Number;
   Month : Calendar.Month_Number) return Calendar.Day_Number;

  -- Check date validity
  function Is_Valid (
   Year  : Calendar.Year_Number;
   Month : Calendar.Month_Number;
   Day   : Calendar.Day_Number) return Boolean;

  subtype Day_Range is Natural;

  -- Is a year leap
  function Is_Leap_Year (
   Year  : Calendar.Year_Number) return Boolean;

  -- Number of days of a year
  function Nb_Days_Year (
   Year  : Calendar.Year_Number) return Day_Range;


  subtype Year_Range is Integer range
   0 .. Calendar.Year_Number'Last - Calendar.Year_Number'First;

  subtype Month_Range is Integer range 0 .. Calendar.Month_Number'Last;
  -- Years & Months to add (or substract). Months are truncated :
  --     Jan 31 + 1 month -> Feb 28 (or 29)
  --     Jan 31 + 3 month -> Apr 30
  -- and Mar 31 - 1 month -> Feb 28 (or 29)
  type Duration_Rec is record
    Years  : Year_Range;
    Months : Month_Range;
  end record;

  -- Add years & months to a date
  function "+" (Date : Calendar.Time; Months : Duration_Rec)
   return Calendar.Time;

  -- Substract years & months from a date
  function "-" (Date : Calendar.Time; Months : Duration_Rec)
    return Calendar.Time;

  -- Add days to a date
  function "+" (Date : Calendar.Time; Days : Day_Range)
   return Calendar.Time;

  -- Substract days from a date
  function "-" (Date : Calendar.Time; Days : Day_Range)
    return Calendar.Time;

  type Delta_Rec is record
    Days : Day_Range;
    Secs : Calendar.Day_Duration;
  end record;

  -- Nb of days and secs between two dates
  --  If Date_1 < Date_2, Time_Error will be raised
  function "-" (Date_1, Date_2 : Calendar.Time)
    return Delta_Rec;

  -- Day name of a date
  type Day_Of_Week_List is (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
  function Get_Day_Of_Week (Date : Calendar.Time) return Day_Of_Week_List;

  -- Week number (in year) of a date
  subtype Week_Of_Year_Range is Natural range 1 .. 53;
  function Get_Week_Of_Year (Date : Calendar.Time) return Week_Of_Year_Range;
  
  -- Month name
  type Month_Name_List is (January, February, March, April, May, June, July,
                           August, September, October, November, December);
  function Get_Month_Name (Number : Calendar.Month_Number)
                          return Month_Name_List;


  -- On overflow of year range
  Time_Error : exception renames Calendar.Time_Error;

end Perpet;

