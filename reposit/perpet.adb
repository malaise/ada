package body PERPET is

  type TIME_REC is record
    YEAR    : CALENDAR.YEAR_NUMBER;
    MONTH   : CALENDAR.MONTH_NUMBER;
    DAY     : CALENDAR.DAY_NUMBER;
    SECONDS : CALENDAR.DAY_DURATION;
  end record;

  -- Is a year leap
  function IS_LEAP_YEAR (YEAR  : CALENDAR.YEAR_NUMBER) return BOOLEAN is
  begin
    -- Year is multiple of 4 and not 100, or multiple of 400
    -- the parenthesis tend to optimize:
    --  return FALSE asap in case of year not multiple of 4
    return YEAR rem 4 = 0 and then
     (YEAR rem 100 /= 0 or else YEAR rem 400 = 0);
  end IS_LEAP_YEAR;

  -- Number of days of a month
  function NB_DAYS_MONTH (
   YEAR  : CALENDAR.YEAR_NUMBER;
   MONTH : CALENDAR.MONTH_NUMBER) return CALENDAR.DAY_NUMBER is
    LAST_DAY_ARRAY : constant array (CALENDAR.MONTH_NUMBER) of
     CALENDAR.DAY_NUMBER :=
     (01 => 31, 02 => 28, 03 => 31, 04 => 30, 05 => 31, 06 => 30,
      07 => 31, 08 => 31, 09 => 30, 10 => 31, 11 => 30, 12 => 31);
  begin
    if MONTH /= 2 then return LAST_DAY_ARRAY(MONTH); end if;

    -- February
    if IS_LEAP_YEAR (YEAR) then
      -- Leap year
      return LAST_DAY_ARRAY(MONTH) + 1;
    else
      -- Non leap year
      return LAST_DAY_ARRAY(MONTH);
    end if;
  end NB_DAYS_MONTH;

  -- Number of days of a year
  function NB_DAYS_YEAR (YEAR : CALENDAR.YEAR_NUMBER) return DAY_RANGE is
  begin
    if IS_LEAP_YEAR (YEAR) then
      -- Leap year
      return 366;
    else
      -- Non leap year
      return 365;
    end if;
  end NB_DAYS_YEAR;

  -- Check date validity
  function IS_VALID (
   YEAR  : CALENDAR.YEAR_NUMBER;
   MONTH : CALENDAR.MONTH_NUMBER;
   DAY   : CALENDAR.DAY_NUMBER) return BOOLEAN is
  begin
    return DAY <= NB_DAYS_MONTH (YEAR, MONTH);
  end IS_VALID;

  -- TIME_REC operations
  function SPLIT (DATE : CALENDAR.TIME) return TIME_REC is
    REC : TIME_REC;
  begin

    CALENDAR.SPLIT (DATE => DATE,
                    YEAR    => REC.YEAR,
                    MONTH   => REC.MONTH,
                    DAY     => REC.DAY,
                    SECONDS => REC.SECONDS);
    return REC;
  end SPLIT;

  function TIME_OF (REC : TIME_REC) return CALENDAR.TIME is
  begin
    return CALENDAR.TIME_OF (YEAR    => REC.YEAR,
                             MONTH   => REC.MONTH,
                             DAY     => REC.DAY,
                             SECONDS => REC.SECONDS);
  end TIME_OF;

  -- Add years & months to a time_rec
  function "+" (DATE : TIME_REC; MONTHS : DURATION_REC) return TIME_REC is
    SUM : INTEGER;
    D : TIME_REC := DATE;
  begin
    D.YEAR := D.YEAR + MONTHS.YEARS;
    SUM := D.MONTH + MONTHS.MONTHS;
    if SUM > 12 then
      D.MONTH := SUM - 12;
      D.YEAR := D.YEAR + 1;
    else
      D.MONTH := SUM;
    end if;
    -- trunc
    SUM := NB_DAYS_MONTH (D.YEAR, D.MONTH);
    if D.DAY > SUM then
      D.DAY := SUM;
    end if;
    return D;
  exception
    when others => raise TIME_ERROR;
  end "+";

  -- Substract years & months from a time_rec
  function "-" (DATE : TIME_REC; MONTHS : DURATION_REC) return TIME_REC is
    SUM : INTEGER;
    D : TIME_REC := DATE;
  begin
    D.YEAR := D.YEAR - MONTHS.YEARS;
    SUM := D.MONTH - MONTHS.MONTHS;
    if SUM < 1 then
      D.MONTH := SUM + 12;
      D.YEAR := D.YEAR - 1;
    else
      D.MONTH := SUM;
    end if;
    -- trunc
    SUM := NB_DAYS_MONTH (D.YEAR, D.MONTH);
    if D.DAY > SUM then
      D.DAY := SUM;
    end if;
    return D;
  exception
    when others => raise TIME_ERROR;
  end "-";

  -- Add years & months to a time
  function "+" (DATE : CALENDAR.TIME; MONTHS : DURATION_REC)
   return CALENDAR.TIME is
  begin
    return TIME_OF (SPLIT(DATE) + MONTHS);
  exception
    when others => raise TIME_ERROR;
  end "+";

  -- Substract years & months from a time
  function "-" (DATE : CALENDAR.TIME; MONTHS : DURATION_REC)
   return CALENDAR.TIME is
  begin
    return TIME_OF (SPLIT(DATE) - MONTHS);
  exception
    when others => raise TIME_ERROR;
  end "-";


  -- tries to go to 1st of next month
  -- If not, remaining is set to 0
  procedure NEXT_MONTH (
   DATE      : in out TIME_REC;
   REMAINING : in out DAY_RANGE) is
    SUM : INTEGER;
  begin
    SUM := NB_DAYS_MONTH (DATE.YEAR, DATE.MONTH);
    if SUM - DATE.DAY >= REMAINING then
      -- Not enough days remaining. Same Month
      DATE.DAY := DATE.DAY + REMAINING;
      REMAINING := 0;
    else
      -- 1st of next month
      REMAINING := REMAINING - (SUM - DATE.DAY + 1);
      DATE.DAY := 1;
      DATE := DATE + (YEARS => 0, MONTHS => 1);
    end if;
  end NEXT_MONTH;

  -- tries to go to last of previous month
  -- If not, remaining is set to 0
  procedure PREV_MONTH (
   DATE      : in out TIME_REC;
   REMAINING : in out DAY_RANGE) is
  begin
    if DATE.DAY > REMAINING then
      -- Not enough days remaining. Same Month
      DATE.DAY := DATE.DAY - REMAINING;
      REMAINING := 0;
    else
      -- last of previous month
      REMAINING := REMAINING - DATE.DAY;
      DATE := DATE - (YEARS => 0, MONTHS => 1);
      DATE.DAY := NB_DAYS_MONTH (DATE.YEAR, DATE.MONTH);
    end if;
  end PREV_MONTH;


  -- Add days to a time
  function "+" (DATE : CALENDAR.TIME; DAYS : DAY_RANGE)
   return CALENDAR.TIME is
    REC : TIME_REC := SPLIT (DATE);
    REMAINING : DAY_RANGE := DAYS;
    SUM : INTEGER;
  begin
    -- try to go to 1st january next year
    loop
      if REMAINING = 0 then
        -- done
        return TIME_OF(REC);
      end if;
      exit when REC.MONTH = 1 and then REC.DAY = 1;
      NEXT_MONTH (REC, REMAINING);
    end loop;

    -- try to add years
    loop
      SUM := NB_DAYS_YEAR (REC.YEAR);
      exit when REMAINING < SUM;
      REMAINING := REMAINING - SUM;
      REC.YEAR := REC.YEAR + 1;
    end loop;

    -- Complete date
    while REMAINING /= 0 loop
      NEXT_MONTH (REC, REMAINING);
    end loop;

    return TIME_OF(REC);
  end "+";

  -- Substract days from a time
  function "-" (DATE : CALENDAR.TIME; DAYS : DAY_RANGE)
   return CALENDAR.TIME is
    REC : TIME_REC := SPLIT (DATE);
    REMAINING : DAY_RANGE := DAYS;
    SUM : INTEGER;
  begin
    -- try to go to 31th december previous year
    loop
      if REMAINING = 0 then
        -- done
        return TIME_OF(REC);
      end if;
      exit when REC.MONTH = 12 and then REC.DAY = 31;
      PREV_MONTH (REC, REMAINING);
    end loop;

    -- try to substract years
    loop
      SUM := NB_DAYS_YEAR (REC.YEAR);
      exit when REMAINING < SUM;
      REMAINING := REMAINING - SUM;
      REC.YEAR := REC.YEAR - 1;
    end loop;

    -- Complete date
    while REMAINING /= 0 loop
      PREV_MONTH (REC, REMAINING);
    end loop;

    return TIME_OF(REC);
  end "-";

  -- Nb of days and secs between two dates
  function "-" (DATE_1, DATE_2 : CALENDAR.TIME)
    return DELTA_REC is
    DELTA_VAL : DELTA_REC;
    REC_1, REC_2 : TIME_REC;
    use CALENDAR;
  begin
    if DATE_1 < DATE_2 then
      raise TIME_ERROR;
    end if;
    DELTA_VAL.DAYS := 0;
    DELTA_VAL.SECS := 0.0;
    if DATE_1 = DATE_2 then
      return DELTA_VAL;
    end if;
    REC_1 := SPLIT(DATE_1);
    REC_2 := SPLIT(DATE_2);

    if REC_2.YEAR = REC_1.YEAR
    and then REC_2.MONTH = REC_1.MONTH
    and then REC_2.DAY = REC_1.DAY then
      -- Same day
      DELTA_VAL.SECS := REC_1.SECONDS - REC_2.SECONDS;
      return DELTA_VAL;
    end if;

    -- End of day 2, beginning of day 1
    if REC_1.SECONDS >= REC_2.SECONDS then
      DELTA_VAL.DAYS := 1;
      DELTA_VAL.SECS := REC_1.SECONDS - REC_2.SECONDS;
    else
      DELTA_VAL.SECS := REC_1.SECONDS + (86_400.0 - REC_2.SECONDS);
    end if;

    if REC_2.YEAR = REC_1.YEAR
    and then REC_2.MONTH = REC_1.MONTH then
      -- Same month
      DELTA_VAL.DAYS := DELTA_VAL.DAYS + (REC_1.DAY - 1) - REC_2.DAY;
      return DELTA_VAL;
    end if;

    -- End of month 2, beginning of month 1
    DELTA_VAL.DAYS := DELTA_VAL.DAYS + NB_DAYS_MONTH (REC_2.YEAR, REC_2.MONTH) - REC_2.DAY;
    DELTA_VAL.DAYS := (DELTA_VAL.DAYS + REC_1.DAY) - 1;

    if REC_2.YEAR = REC_1.YEAR then
      for MONTH in REC_2.MONTH + 1 .. REC_1.MONTH - 1 loop
        DELTA_VAL.DAYS := DELTA_VAL.DAYS + NB_DAYS_MONTH (REC_2.YEAR, MONTH);
      end loop;
      return DELTA_VAL;
    end if;

    -- End of year 2, beginning of year 1
    if REC_2.MONTH /= 12 then
      for MONTH in REC_2.MONTH + 1 .. 12 loop
        DELTA_VAL.DAYS := DELTA_VAL.DAYS + NB_DAYS_MONTH (REC_2.YEAR, MONTH);
      end loop;
    end if;
    if REC_1.MONTH /= 1 then
      for MONTH in 1 .. REC_1.MONTH -1 loop
        DELTA_VAL.DAYS := DELTA_VAL.DAYS + NB_DAYS_MONTH (REC_1.YEAR, MONTH);
      end loop;
    end if;

    -- Add years
    for YEAR in REC_2.YEAR + 1 .. REC_1.YEAR - 1 loop
      DELTA_VAL.DAYS := DELTA_VAL.DAYS + NB_DAYS_YEAR (YEAR);
    end loop;

    return DELTA_VAL;

  end "-";

  -- type DAY_OF_WEEK_LIST is (MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, STURDAY, SUNDAY);
  function GET_DAY_OF_WEEK (DATE : CALENDAR.TIME) return DAY_OF_WEEK_LIST is
    DELTA_DAYS : DAY_RANGE;
    REF_REC : constant TIME_REC := (
     YEAR => CALENDAR.YEAR_NUMBER'FIRST,
     MONTH => CALENDAR.MONTH_NUMBER'FIRST,
     DAY =>  CALENDAR.DAY_NUMBER'FIRST,
     SECONDS => 0.0);
    REF_DATE : constant CALENDAR.TIME := TIME_OF (REF_REC);
    REF_DAY_OF_WEEK : constant DAY_OF_WEEK_LIST := TUESDAY;
  begin
    DELTA_DAYS := "-" (DATE, REF_DATE).DAYS;
    return DAY_OF_WEEK_LIST'VAL ( (DELTA_DAYS + DAY_OF_WEEK_LIST'POS(REF_DAY_OF_WEEK)) rem 7);
  end GET_DAY_OF_WEEK;

  -- subtype WEEK_OF_YEAR_RANGE is NATURAL range 1 .. 53;
  function GET_WEEK_OF_YEAR (DATE : CALENDAR.TIME) return WEEK_OF_YEAR_RANGE is
    REC_0 : TIME_REC;
    DATE_0 : CALENDAR.TIME;
    DAY_DATE_0 : DAY_OF_WEEK_LIST;
    WEEK_OF_WEEK_0 : WEEK_OF_YEAR_RANGE;
    DATE_0_OFFSET : DAY_RANGE;

    DELTA_DAYS : DELTA_REC;

    MAX_NB_DAYS_A_WEEK : constant := DAY_OF_WEEK_LIST'POS(DAY_OF_WEEK_LIST'LAST) + 1;
    WEEK_OFFSET : NATURAL range 0 .. WEEK_OF_YEAR_RANGE'LAST;
    WEEK_OF_DATE : WEEK_OF_YEAR_RANGE;
  begin
    -- 01/01 of the year
    REC_0 := SPLIT (DATE);
    REC_0.MONTH := 1;
    REC_0.DAY := 1;
    REC_0.SECONDS := 0.0;
    DATE_0 := TIME_OF(REC_0);
    -- Day of week of it
    DAY_DATE_0 := GET_DAY_OF_WEEK(DATE_0);
    -- No of first week is 1 if day_date_0 is Monday .. Thursday
    if DAY_DATE_0 <= THURSDAY then
      WEEK_OF_WEEK_0 := 1;
    else
      -- 52 or 53? The same as day before day_0
      WEEK_OF_WEEK_0 := GET_WEEK_OF_YEAR(DATE_0 - 1);
    end if;
    -- Number of days of first week in the previous year
    DATE_0_OFFSET := DAY_OF_WEEK_LIST'POS(DAY_DATE_0);

    -- Nb of days between date and 01/01
    DELTA_DAYS := DATE - DATE_0;
    -- Nb of days between date and Monday of first week
    DELTA_DAYS.DAYS := DELTA_DAYS.DAYS + DATE_0_OFFSET;

    -- Week offset from first week
    WEEK_OFFSET := DELTA_DAYS.DAYS / MAX_NB_DAYS_A_WEEK;
    -- Week no
    if WEEK_OFFSET = 0 then
      -- First week
      WEEK_OF_DATE := WEEK_OF_WEEK_0;
    elsif WEEK_OF_WEEK_0 = 1 then
      -- Week 1 + offset
      WEEK_OF_DATE := WEEK_OFFSET + 1;
    else
      -- 52/53 -> 0, + offset
      WEEK_OF_DATE := WEEK_OFFSET;
    end if; 
    return WEEK_OF_DATE;
    
  end GET_WEEK_OF_YEAR;


end PERPET;
