with CALENDAR;
package DAY_MNG is

  -- Convert CALENDAR.DAY_DURATION in HOURS, MINUTES, SECONDS and MILLISEC

  subtype T_HOURS    is NATURAL range 0 .. 23;
  subtype T_MINUTES  is NATURAL range 0 .. 59;
  subtype T_SECONDS  is NATURAL range 0 .. 59;
  subtype T_MILLISEC is NATURAL range 0 .. 9_999;

  -- split a day duration in hours, minutes, seconds and milliseconds
  procedure SPLIT (DUR : in CALENDAR.DAY_DURATION;
   HOURS    : out T_HOURS;
   MINUTES  : out T_MINUTES;
   SECONDS  : out T_SECONDS;
   MILLISEC : out T_MILLISEC);

  -- pack hours, minutes, seconds and milliseconds into a day duration
  function PACK (
   HOURS    : T_HOURS;
   MINUTES  : T_MINUTES;
   SECONDS  : T_SECONDS;
   MILLISEC : T_MILLISEC) return CALENDAR.DAY_DURATION;
 end DAY_MNG;
