package body OPER_DEF is

  function CURRENT_DATE return DATE_REC is
    DATE : DATE_REC;
    SECS : CALENDAR.DAY_DURATION;
  begin
    CALENDAR.SPLIT(CALENDAR.CLOCK, DATE.YEAR, DATE.MONTH, DATE.DAY, SECS);
    return DATE;
  end CURRENT_DATE;

  function LESS_THAN (OPER_1, OPER_2 : OPER_REC) return BOOLEAN is
  begin
    if OPER_1.DATE.YEAR < OPER_2.DATE.YEAR then
      return TRUE;
    elsif OPER_1.DATE.YEAR > OPER_2.DATE.YEAR then
      return FALSE;
    end if;
    if OPER_1.DATE.MONTH < OPER_2.DATE.MONTH then
      return TRUE;
    elsif OPER_1.DATE.MONTH > OPER_2.DATE.MONTH then
      return FALSE;
    end if;
    return OPER_1.DATE.DAY < OPER_2.DATE.DAY;
  end LESS_THAN;

end OPER_DEF;

