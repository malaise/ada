with CALENDAR;
use CALENDAR;
with SOK_DISPLAY;
package body SOK_TIME is

  RUNNING : BOOLEAN := FALSE;
  DUR : DAY_DURATION := 0.0;
  DAY : NATURAL := 0;
  START_CLOCK : CALENDAR.TIME;

  procedure START_TIME is
  begin
    START_CLOCK := CALENDAR.CLOCK - DUR;
    RUNNING := TRUE;
  end START_TIME;

  procedure STOP_TIME is
  begin
    RUNNING := FALSE;
  end STOP_TIME;

  procedure DISP_TIME is
  begin
    if RUNNING then
      -- elapsed time
      declare 
        CURRENT_TIME : constant CALENDAR.TIME := CALENDAR.CLOCK;
      begin
        if CURRENT_TIME > START_CLOCK + DAY_DURATION'LAST then
          DAY := DAY + 1;
          DUR := CURRENT_TIME - (START_CLOCK + DAY_DURATION'LAST);
          START_CLOCK := CALENDAR.CLOCK;
        else
          DUR := CURRENT_TIME - START_CLOCK;
        end if;
      end;
    end if;
    SOK_DISPLAY.PUT_TIME (DAY, DUR);
  end DISP_TIME;

  procedure RESET_TIME is
  begin
    START_CLOCK := CALENDAR.CLOCK;
    DUR := 0.0;
    DAY := 0;
  end RESET_TIME;

  procedure SET_TIME (DAY : NATURAL; DUR : DURATION) is
  begin
    START_CLOCK := CALENDAR.CLOCK - DUR;
    SOK_TIME.DUR := DUR;
    SOK_TIME.DAY := DAY;
  end SET_TIME;

  procedure GET_TIME (DAY : out NATURAL; DUR : out DURATION) is
  begin
    DAY := SOK_TIME.DAY;
    DUR := SOK_TIME.DUR;
  end GET_TIME;

end SOK_TIME;

