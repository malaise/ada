with CALENDAR;
package PERPET is

  -- Number of days of a month
  function NB_DAYS_MONTH (
   YEAR  : CALENDAR.YEAR_NUMBER;
   MONTH : CALENDAR.MONTH_NUMBER) return CALENDAR.DAY_NUMBER;

  -- Check date validity
  function IS_VALID (
   YEAR  : CALENDAR.YEAR_NUMBER;
   MONTH : CALENDAR.MONTH_NUMBER;
   DAY   : CALENDAR.DAY_NUMBER) return BOOLEAN;

  subtype DAY_RANGE is NATURAL;

  -- Number of days of a year
  function NB_DAYS_YEAR (
   YEAR  : CALENDAR.YEAR_NUMBER) return DAY_RANGE;


  subtype YEAR_RANGE is INTEGER range
   0 .. CALENDAR.YEAR_NUMBER'LAST - CALENDAR.YEAR_NUMBER'FIRST;

  subtype MONTH_RANGE is INTEGER range 0 .. CALENDAR.MONTH_NUMBER'LAST;
  -- Years & Months to add (or substract). Months are truncated :
  --     Jan 31 + 1 month -> Feb 28 (or 29)
  --     Jan 31 + 3 month -> Apr 30
  -- and Mar 31 - 1 month -> Feb 28 (or 29)
  type DURATION_REC is record
    YEARS  : YEAR_RANGE;
    MONTHS : MONTH_RANGE;
  end record;

  -- Add years & months to a date
  function "+" (DATE : CALENDAR.TIME; MONTHS : DURATION_REC)
   return CALENDAR.TIME;

  -- Substract years & months from a date
  function "-" (DATE : CALENDAR.TIME; MONTHS : DURATION_REC)
    return CALENDAR.TIME;

  -- Add days to a date
  function "+" (DATE : CALENDAR.TIME; DAYS : DAY_RANGE)
   return CALENDAR.TIME;

  -- Substract days from a date
  function "-" (DATE : CALENDAR.TIME; DAYS : DAY_RANGE)
    return CALENDAR.TIME;

  type DELTA_REC is record
    DAYS : DAY_RANGE;
    SECS : CALENDAR.DAY_DURATION;
  end record;

  -- Nb of days and secs between two dates
  --  If DATE_1 < DATE_2, TIME_ERROR will be raised
  function "-" (DATE_1, DATE_2 : CALENDAR.TIME)
    return DELTA_REC;

  type DAY_OF_WEEK_LIST is (MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY);
  function GET_DAY_OF_WEEK (DATE : CALENDAR.TIME) return DAY_OF_WEEK_LIST;

  subtype WEEK_OF_YEAR_RANGE is NATURAL range 1 .. 53;
  function GET_WEEK_OF_YEAR (DATE : CALENDAR.TIME) return WEEK_OF_YEAR_RANGE;

  -- On overflow of year range
  TIME_ERROR : exception renames CALENDAR.TIME_ERROR;

end PERPET;
