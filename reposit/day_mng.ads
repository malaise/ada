with Calendar;
package Day_Mng is

  -- Convert CALENDAR.DAY_DURATION in HOURS, MINUTES, SECONDS and MILLISEC

  subtype T_Hours    is Natural range 0 ..  23;
  subtype T_Minutes  is Natural range 0 ..  59;
  subtype T_Seconds  is Natural range 0 ..  59;
  subtype T_Millisec is Natural range 0 .. 999;

  -- split a day duration in hours, minutes, seconds and milliseconds
  procedure Split (Dur : in Calendar.Day_Duration;
   Hours    : out T_Hours;
   Minutes  : out T_Minutes;
   Seconds  : out T_Seconds;
   Millisec : out T_Millisec);

  -- pack hours, minutes, seconds and milliseconds into a day duration
  function Pack (
   Hours    : T_Hours;
   Minutes  : T_Minutes;
   Seconds  : T_Seconds;
   Millisec : T_Millisec) return Calendar.Day_Duration;

end Day_Mng;

