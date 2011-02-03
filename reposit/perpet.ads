-- Perpetual calendar
with Ada.Calendar;
package Perpet is

  -- Number of days of a month
  function Nb_Days_Month (
   Year  : Ada.Calendar.Year_Number;
   Month : Ada.Calendar.Month_Number) return Ada.Calendar.Day_Number;

  -- Check date validity
  function Is_Valid (
   Year  : Ada.Calendar.Year_Number;
   Month : Ada.Calendar.Month_Number;
   Day   : Ada.Calendar.Day_Number) return Boolean;

  -- Nb of days
  subtype Day_Range is Natural;

  -- Is a year leap
  function Is_Leap_Year (
   Year  : Ada.Calendar.Year_Number) return Boolean;

  -- Number of days of a year
  function Nb_Days_Year (
   Year  : Ada.Calendar.Year_Number) return Day_Range;


  subtype Year_Range is Integer range
   0 .. Ada.Calendar.Year_Number'Last - Ada.Calendar.Year_Number'First;

  subtype Month_Range is Integer range 0 .. Ada.Calendar.Month_Number'Last;
  -- Years & Months to add (or substract). Months are truncated:
  --     Jan 31 + 1 month -> Feb 28 (or 29)
  --     Jan 31 + 3 month -> Apr 30
  --     Mar 31 - 1 month -> Feb 28 (or 29)
  type Duration_Rec is record
    Years  : Year_Range;
    Months : Month_Range;
  end record;

  -- Add years & months to a date
  function "+" (Date : Ada.Calendar.Time; Months : Duration_Rec)
   return Ada.Calendar.Time;

  -- Substract years & months from a date
  function "-" (Date : Ada.Calendar.Time; Months : Duration_Rec)
    return Ada.Calendar.Time;

  -- Add days to a date
  function "+" (Date : Ada.Calendar.Time; Days : Day_Range)
   return Ada.Calendar.Time;

  -- Substract days from a date
  function "-" (Date : Ada.Calendar.Time; Days : Day_Range)
    return Ada.Calendar.Time;

  -- Days and seconds
  type Delta_Rec is record
    Days : Day_Range;
    Secs : Ada.Calendar.Day_Duration;
  end record;

  -- Duration to Delta_Rec
  subtype Natural_Duration is Duration range 0.0 .. Duration'Last;
  function To_Delta_Rec (Dur : Natural_Duration) return Delta_Rec;

  -- Delta_rec comparison
  function "<" (Delta_1, Delta_2 : Delta_Rec) return Boolean;

  -- Nb of days and secs between two dates
  --  If Date_1 < Date_2, Time_Error will be raised
  function "-" (Date_1, Date_2 : Ada.Calendar.Time) return Delta_Rec;

  -- Add/Sub a delta to a time
  function "+" (Date : Ada.Calendar.Time; Delta_Date : Delta_Rec)
    return Ada.Calendar.Time;
  function "-" (Date : Ada.Calendar.Time; Delta_Date : Delta_Rec)
    return Ada.Calendar.Time;

  -- Add/Sub a duration to delta, may raise Time_Error if result < 0
  function "+" (Delta_Date : Delta_Rec; Dur : Duration) return Delta_Rec;
  function "-" (Delta_Date : Delta_Rec; Dur : Duration) return Delta_Rec;

  -- Multiply a delta by a factor
  function "*" (Delta_Date : Delta_Rec;
                Factor : Natural_Duration) return Delta_Rec;

  -- Day name of a date
  type Day_Of_Week_List is
     (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
  function Get_Day_Of_Week (Date : Ada.Calendar.Time) return Day_Of_Week_List;

  -- Week number (in year) of a date
  subtype Week_Of_Year_Range is Natural range 1 .. 53;
  function Get_Week_Of_Year (Date : Ada.Calendar.Time)
                             return Week_Of_Year_Range;

  -- Month name
  type Month_Name_List is (January, February, March, April, May, June, July,
                           August, September, October, November, December);
  function Get_Month_Name (Number : Ada.Calendar.Month_Number)
                          return Month_Name_List;


  -- On overflow of year range or when obtaining a negative Delta_Rec
  Time_Error : exception renames Ada.Calendar.Time_Error;

end Perpet;

