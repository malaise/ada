with CALENDAR, TEXT_IO;
with PERPET, ARGUMENT, DAY_MNG, TEXT_HANDLER, NORMAL;
procedure DAY_OF_WEEK is

  package DUR_IO is new TEXT_IO.FIXED_IO (CALENDAR.DAY_DURATION);

  TXT : TEXT_HANDLER.TEXT (10);
  T : CALENDAR.TIME;
  YEAR : CALENDAR.YEAR_NUMBER;
  DELTA_DATE_0 : PERPET.DELTA_REC;
  DELTA_DATE_1 : PERPET.DELTA_REC;
  DAY_NO : PERPET.DAY_RANGE;
  TH : STRING (1 .. 2);
  DAYS : TEXT_HANDLER.TEXT(4);

  procedure USAGE is
  begin
    TEXT_IO.PUT_LINE("Syntax error or invalid date.");
    TEXT_IO.PUT_LINE(" Usage: " & ARGUMENT.GET_PROGRAM_NAME
                 & " [ dd/mm/yyyy ]");
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

  if ARGUMENT.GET_NBRE_ARG /= 0  and then ARGUMENT.GET_NBRE_ARG /= 1 then
    USAGE;
    return;
  end if;

  declare
    MONTH : CALENDAR.MONTH_NUMBER;
    DAY : CALENDAR.DAY_NUMBER;
  begin
    if ARGUMENT.GET_NBRE_ARG = 0 then
      -- Current date
      T := CALENDAR.CLOCK;
      declare
        DUMMY_DURATION : CALENDAR.DAY_DURATION;
      begin
        CALENDAR.SPLIT (T, YEAR, MONTH, DAY, DUMMY_DURATION);
      end;
      TEXT_HANDLER.SET(TXT, NORMAL(DAY, 2, GAP => '0') & "/"
                          & NORMAL(MONTH, 2, GAP => '0') & "/"
                          & NORMAL(YEAR, 4, GAP => '0') );
    else
      -- Get date from arg 1
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

      begin
        DAY := CALENDAR.DAY_NUMBER'VALUE(TEXT_HANDLER.VALUE(TXT)(1 .. 2));
        MONTH := CALENDAR.DAY_NUMBER'VALUE(TEXT_HANDLER.VALUE(TXT)(4 .. 5));
        YEAR := CALENDAR.DAY_NUMBER'VALUE(TEXT_HANDLER.VALUE(TXT)(7 .. 10));
      exception
        when others =>
          USAGE;
          return;
      end;
    end if;

    -- Build time of 0h00 of date
    declare
      HOUR : DAY_MNG.T_HOURS := 0;
      MINUTE : DAY_MNG.T_MINUTES := 0;
      SECOND : DAY_MNG.T_SECONDS := 0;
      MILLISEC : DAY_MNG.T_MILLISEC := 0;
    begin
      T :=  CALENDAR.TIME_OF
        (YEAR, MONTH, DAY, DAY_MNG.PACK(HOUR, MINUTE, SECOND, MILLISEC));
    exception
      when others =>
        USAGE;
        return;
    end;
  end;

  -- Compute delta from 01/01 and to 31/12 of year
  declare
    T0 : CALENDAR.TIME;
    T1 : CALENDAR.TIME;
  begin
    T0 := CALENDAR.TIME_OF (YEAR, 1, 1, 0.0);
    DELTA_DATE_0 := PERPET."-" (T, T0);
    
    T1 := CALENDAR.TIME_OF (YEAR, 12, 31, 0.0);
    DELTA_DATE_1 := PERPET."-" (T1, T);
  exception
    when others =>
      TEXT_IO.PUT_LINE("Internal error");
      raise;
  end;


  -- Compute sentence
  DAY_NO := DELTA_DATE_0.DAYS + 1;
  if (DAY_NO rem 100) / 10 /= 1 then
    case DAY_NO rem 10 is
      when 1 =>
        TH := "st";
      when 2 =>
        TH := "nd";
      when 3 =>
        TH := "rd";
      when others =>
        TH := "th";
    end case;
  else
    TH := "th";
  end if;

  if DELTA_DATE_1.DAYS <= 1 then
    TEXT_HANDLER.SET (DAYS, "day");
  else
    TEXT_HANDLER.SET (DAYS, "days");
  end if;

  -- Display result
  TEXT_IO.PUT_LINE (TEXT_HANDLER.VALUE(TXT) & " is a "
       & PERPET.DAY_OF_WEEK_LIST'IMAGE(PERPET.GET_DAY_OF_WEEK(T))
       & ", in week"
       & PERPET.WEEK_OF_YEAR_RANGE'IMAGE(PERPET.GET_WEEK_OF_YEAR(T))
       & ",");
  TEXT_IO.PUT_LINE (" the"
       & PERPET.DAY_RANGE'IMAGE(DAY_NO) 
       & TH
       & " day of the year,"
       & PERPET.DAY_RANGE'IMAGE(DELTA_DATE_1.DAYS)
       & " " & TEXT_HANDLER.VALUE(DAYS)
       & " remaining.");

end DAY_OF_WEEK;

