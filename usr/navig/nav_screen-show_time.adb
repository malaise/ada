-- time displaying
with CALENDAR;
with NORMAL, DAY_MNG;

separate (NAV_SCREEN)
procedure SHOW_TIME is
  YEAR  : CALENDAR.YEAR_NUMBER;
  MONTH : CALENDAR.MONTH_NUMBER;
  DAY   : CALENDAR.DAY_NUMBER;
  DUR   : CALENDAR.DAY_DURATION;
  HOR : DAY_MNG.T_HOURS;
  MIN : DAY_MNG.T_MINUTES;
  SEC : DAY_MNG.T_SECONDS;
  MIL : DAY_MNG.T_MILLISEC;
begin
  -- get date and time
  CALENDAR.SPLIT (CALENDAR.CLOCK, YEAR, MONTH, DAY, DUR);
  -- compute time hours, minutes and seconds
  DAY_MNG.SPLIT (DUR, HOR, MIN, SEC, MIL);
  -- put
  CON_IO.MOVE ( (0, 0), W_TIME);
  CON_IO.PUT (
   S =>
    NORMAL(DAY, 2, TRUE, '0') & "/" & NORMAL(MONTH, 2, TRUE, '0') &
    "/" & NORMAL(YEAR, 4, TRUE, '0') & " " &
    NORMAL(HOR, 2, TRUE, '0') & ":" & NORMAL(MIN, 2, TRUE, '0') & ":" &
    NORMAL(SEC, 2, TRUE, '0'),
   NAME => W_TIME);
end SHOW_TIME;
