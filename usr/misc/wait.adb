with ARGUMENT, TEXT_IO, CALENDAR;
procedure WAIT is

  START : constant CALENDAR.TIME := CALENDAR.CLOCK;

  package DUR_IO is new TEXT_IO.FIXED_IO(DURATION);
  package INT_IO is new TEXT_IO.INTEGER_IO(INTEGER);

  DUR  : DURATION;
  INT  : INTEGER;
  LAST : POSITIVE;

  use CALENDAR;

begin

  if ARGUMENT.GET_NBRE_ARG = 1 then
    begin
      DUR_IO.GET(ARGUMENT.GET_PARAMETER, DUR, LAST);
    exception
      when others =>
        INT_IO.GET(ARGUMENT.GET_PARAMETER, INT, LAST);
        DUR := DURATION(INT);
    end;
  elsif  ARGUMENT.GET_NBRE_ARG = 0 then
    DUR := 1.0;
  else
    raise CONSTRAINT_ERROR;
  end if;
  delay DUR - (CALENDAR.CLOCK - START);

exception
  when others => 
    TEXT_IO.PUT_LINE("Usage : ""wait [seconds]""     (1.0 by default)."); 
end WAIT; 
