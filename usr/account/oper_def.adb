package body OPER_DEF is

  function CURRENT_DATE return DATE_REC is
    DATE : DATE_REC;
    SECS : CALENDAR.DAY_DURATION;
  begin
    CALENDAR.SPLIT(CALENDAR.CLOCK, DATE.YEAR, DATE.MONTH, DATE.DAY, SECS);
    return DATE;
  end CURRENT_DATE;

  -- Sort by date, amount, kind, destination
  -- DO NOT SORT BY STATUS, or a double click on sub list will
  --  mess all up.
  function BEFORE (OPER_1, OPER_2 : OPER_REC) return BOOLEAN is
  begin
    if OPER_1.DATE.YEAR /= OPER_2.DATE.YEAR then
      return OPER_1.DATE.YEAR < OPER_2.DATE.YEAR;
    elsif OPER_1.DATE.MONTH /= OPER_2.DATE.MONTH then
      return OPER_1.DATE.MONTH < OPER_2.DATE.MONTH;
    elsif OPER_1.DATE.DAY /= OPER_2.DATE.DAY then
      return OPER_1.DATE.DAY < OPER_2.DATE.DAY;
    elsif OPER_1.AMOUNT /= OPER_2.AMOUNT then
      return OPER_1.AMOUNT < OPER_2.AMOUNT;
    elsif OPER_1.KIND /= OPER_2.KIND then
      return OPER_1.KIND < OPER_2.KIND;
    else
      return OPER_1.DESTINATION < OPER_2.DESTINATION;
    end if;
  end BEFORE;

end OPER_DEF;

