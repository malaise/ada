with MATH;
use MATH;
package body DAY_MNG is

  MIN_IN_HOR : constant := 60;
  SEC_IN_MIN : constant := 60;
  MIL_IN_SEC : constant := 1_000;

  -- split a day duration in hours, minutes, seconds and milliseconds
  procedure SPLIT (DUR : in CALENDAR.DAY_DURATION;
   HOURS    : out T_HOURS;
   MINUTES  : out T_MINUTES;
   SECONDS  : out T_SECONDS;
   MILLISEC : out T_MILLISEC) is
    MSEC : MATH.INTE;
    H    : T_HOURS;
    M    : T_MINUTES;
    S    : T_SECONDS;
  begin
    -- milli seconds in dur
    MSEC := MATH.ROUND (MATH.REAL(DUR) *  MATH.REAL(MIL_IN_SEC));

    -- split milli seconds
    H := T_HOURS(MSEC / (MIN_IN_HOR * SEC_IN_MIN * MIL_IN_SEC) );
    MSEC := MSEC - MATH.INTE(H) * MIN_IN_HOR * SEC_IN_MIN * MIL_IN_SEC;
    M := T_MINUTES(MSEC / (SEC_IN_MIN * MIL_IN_SEC) );
    MSEC := MSEC - MATH.INTE(M) * SEC_IN_MIN * MIL_IN_SEC;
    S := T_SECONDS(MSEC / MIL_IN_SEC);
    MSEC := MSEC - MATH.INTE(S) * MIL_IN_SEC;

    -- out values
    HOURS := H;
    MINUTES := M;
    SECONDS := S;
    MILLISEC := T_MILLISEC(MSEC);

  end SPLIT;


  -- pack hours, minutes, seconds and milliseconds into a day duration
  function PACK (
   HOURS    : T_HOURS;
   MINUTES  : T_MINUTES;
   SECONDS  : T_SECONDS;
   MILLISEC : T_MILLISEC) return CALENDAR.DAY_DURATION is
    DUR : CALENDAR.DAY_DURATION;
  begin
    DUR := CALENDAR.DAY_DURATION (
     CALENDAR.DAY_DURATION(MILLISEC) / MIL_IN_SEC);
    DUR := DUR + CALENDAR.DAY_DURATION (SECONDS);
    DUR := DUR + CALENDAR.DAY_DURATION (MINUTES * SEC_IN_MIN);
    DUR := DUR + CALENDAR.DAY_DURATION (HOURS * SEC_IN_MIN * MIN_IN_HOR);
    return DUR;
  end PACK;

end DAY_MNG;
