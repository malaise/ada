with CALENDAR;
with PERPET, ARGUMENT, DAY_MNG, TEXT_HANDLER, TEXT_IO;
procedure DAY_OF_WEEK is

  package DUR_IO is new TEXT_IO.FIXED_IO (CALENDAR.DAY_DURATION);

  TXT : TEXT_HANDLER.TEXT (10);
  T : CALENDAR.TIME;

  procedure USAGE is
  begin
    TEXT_IO.PUT_LINE("Syntax error. Usage: " & ARGUMENT.GET_PROGRAM_NAME
                 & " dd/mm/yyyy");
  end USAGE;

  function IS_DIGIT (C : CHARACTER) return BOOLEAN is
  begin
    return C >= '0' and then C <= '9';
  end IS_DIGIT;
  function IS_DIGIT (S : STRING) return BOOLEAN is
  begin
    for I in S'RANGE loop
      if not IS_DIGIT(S(I)) then
        return FALSE;
      end if;
    end loop;
    return TRUE;
  end IS_DIGIT;

begin

  if ARGUMENT.GET_NBRE_ARG /= 1 then
    USAGE;
    return;
  end if;

  ARGUMENT.GET_PARAMETER (TXT);
  if TEXT_HANDLER.LENGTH(TXT) /= 10
  or else TEXT_HANDLER.VALUE(TXT)(3) /= '/'
  or else TEXT_HANDLER.VALUE(TXT)(6) /= '/' then
    USAGE;
    return;
  end if;

  if not IS_DIGIT(TEXT_HANDLER.VALUE(TXT)(1 .. 2))
  or else not IS_DIGIT(TEXT_HANDLER.VALUE(TXT)(4 .. 5))
  or else not IS_DIGIT(TEXT_HANDLER.VALUE(TXT)(7 .. 10)) then
    USAGE;
    return;
  end if;

  declare
    YEAR : CALENDAR.YEAR_NUMBER;
    MONTH : CALENDAR.MONTH_NUMBER;
    DAY : CALENDAR.DAY_NUMBER;
    HOUR : DAY_MNG.T_HOURS;
    MINUTE : DAY_MNG.T_MINUTES;
    SECOND : DAY_MNG.T_SECONDS;
    MILLISEC : DAY_MNG.T_MILLISEC;
  begin
    MILLISEC := 0;
    SECOND := 0;
    MINUTE := 0;
    HOUR := 0;
    DAY := CALENDAR.DAY_NUMBER'VALUE(TEXT_HANDLER.VALUE(TXT)(1 .. 2));
    MONTH := CALENDAR.DAY_NUMBER'VALUE(TEXT_HANDLER.VALUE(TXT)(4 .. 5));
    YEAR := CALENDAR.DAY_NUMBER'VALUE(TEXT_HANDLER.VALUE(TXT)(7 .. 10));
    T :=  CALENDAR.TIME_OF (YEAR, MONTH, DAY, DAY_MNG.PACK(HOUR, MINUTE, SECOND, MILLISEC));
  exception
    when others =>
      USAGE;
      return;
  end;

  TEXT_IO.PUT_LINE ("It is a "
       & PERPET.DAY_OF_WEEK_LIST'IMAGE(PERPET.GET_DAY_OF_WEEK(T))
       & "  in week "
       & PERPET.WEEK_OF_YEAR_RANGE'IMAGE(PERPET.GET_WEEK_OF_YEAR(T)));
end DAY_OF_WEEK;
