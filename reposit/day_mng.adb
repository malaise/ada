with MY_MATH;
package body DAY_MNG is
  use MY_MATH;

  MIN_IN_HOR : constant := 60;
  SEC_IN_MIN : constant := 60;
  MIL_IN_SEC : constant := 1_000;

  -- split a day duration in hours, minutes, seconds and milliseconds
  procedure SPLIT (DUR : in CALENDAR.DAY_DURATION;
   HOURS    : out T_HOURS;
   MINUTES  : out T_MINUTES;
   SECONDS  : out T_SECONDS;
   MILLISEC : out T_MILLISEC) is
    RDUR : MY_MATH.REAL;
    SEC : MY_MATH.REAL;
    H   : T_HOURS;
    M   : T_MINUTES;
    S   : T_SECONDS;
  begin
    -- Round millisecs
    RDUR := MY_MATH.REAL(MY_MATH.ROUND(MY_MATH.REAL(DUR) * MY_MATH.REAL(MIL_IN_SEC)))
          / MY_MATH.REAL(MIL_IN_SEC);

    -- Millisecs
    MILLISEC := T_MILLISEC (MY_MATH.FRAC(RDUR) * MY_MATH.REAL(MIL_IN_SEC));

    -- Seconds in the day
    SEC := MY_MATH.INT (RDUR);

    -- split seconds
    H := T_HOURS(MY_MATH.TRUNC(SEC / MY_MATH.REAL(MIN_IN_HOR * SEC_IN_MIN) ));
    SEC := SEC - MY_MATH.REAL(H) * MY_MATH.REAL(MIN_IN_HOR * SEC_IN_MIN);
    M := T_MINUTES(MY_MATH.TRUNC(SEC / MY_MATH.REAL(SEC_IN_MIN) ));
    SEC := SEC - MY_MATH.REAL(M) * MY_MATH.REAL(SEC_IN_MIN);
    S := T_SECONDS(MY_MATH.TRUNC(SEC) );

    -- out values
    HOURS := H;
    MINUTES := M;
    SECONDS := S;

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

