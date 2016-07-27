with Ada.Calendar;
package Day_Mng is

  -- Convert Ada.Calendar.Day_Duration
  --  in Hours, Minutes, Seconds and Millisec

  subtype T_Hours    is Natural range 0 ..  23;
  subtype T_Minutes  is Natural range 0 ..  59;
  subtype T_Seconds  is Natural range 0 ..  59;
  subtype T_Millisec is Natural range 0 .. 999;

  -- Split a day duration in hours, minutes, seconds and milliseconds
  procedure Split (Dur : in Ada.Calendar.Day_Duration;
   Hours    : out T_Hours;
   Minutes  : out T_Minutes;
   Seconds  : out T_Seconds;
   Millisec : out T_Millisec);

  -- Pack hours, minutes, seconds and milliseconds into a day duration
  function Pack (
   Hours    : T_Hours;
   Minutes  : T_Minutes;
   Seconds  : T_Seconds;
   Millisec : T_Millisec) return Ada.Calendar.Day_Duration;


  -- Same operations extended to Ada.Calendar.Time
  subtype T_Years  is Ada.Calendar.Year_Number;
  subtype T_Months is Ada.Calendar.Month_Number;
  subtype T_Days   is Ada.Calendar.Day_Number;

  -- Split a time in years, months, days,
  --  hours, minutes, seconds and milliseconds
  procedure Split (Date : in Ada.Calendar.Time;
   Year     : out T_Years;
   Month    : out T_Months;
   Day      : out T_Days;
   Hours    : out T_Hours;
   Minutes  : out T_Minutes;
   Seconds  : out T_Seconds;
   Millisec : out T_Millisec);

  -- Pack years, months, days, hours, minutes, seconds and milliseconds
  --  into a time
  function Pack (
   Year     : T_Years;
   Month    : T_Months;
   Day      : T_Days;
   Hours    : T_Hours;
   Minutes  : T_Minutes;
   Seconds  : T_Seconds;
   Millisec : T_Millisec) return Ada.Calendar.Time;


end Day_Mng;

