with CALENDAR, TEXT_IO;
with PERPET, DAY_MNG, MY_IO;
procedure T_DELTA_DATE is

  package DUR_IO is new TEXT_IO.FIXED_IO (CALENDAR.DAY_DURATION);

  T1, T2 : CALENDAR.TIME;
  D : PERPET.DELTA_REC;

  procedure ERROR is
  begin
    MY_IO.PUT(ASCII.BEL);
    TEXT_IO.SKIP_LINE;
    TEXT_IO.SKIP_LINE;
  end ERROR;

  function GET return CALENDAR.TIME is
    YEAR : CALENDAR.YEAR_NUMBER;
    MONTH : CALENDAR.MONTH_NUMBER;
    DAY : CALENDAR.DAY_NUMBER;
    HOUR : DAY_MNG.T_HOURS;
    MINUTE : DAY_MNG.T_MINUTES;
    SECOND : DAY_MNG.T_SECONDS;
    MILLISEC : DAY_MNG.T_MILLISEC;
    use MY_IO;
  begin
    loop
      begin
        loop
          begin
            PUT ("Year -> "); GET (YEAR);
            exit;
          exception
            when others => ERROR;
          end;
        end loop;

        loop
          begin
            PUT ("Month -> "); GET (MONTH);
            exit;
          exception
            when others => ERROR;
          end;
        end loop;

        loop
          begin
            PUT ("Day -> "); GET (DAY);
            exit;
          exception
            when others => ERROR;
          end;
        end loop;

        loop
          begin
            PUT ("Hour -> "); GET (HOUR);
            exit;
          exception
            when others => ERROR;
          end;
        end loop;

        loop
          begin
            PUT ("Minute -> "); GET (MINUTE);
            exit;
          exception
            when others => ERROR;
          end;
        end loop;

        loop
          begin
            PUT ("Second -> "); GET (SECOND);
            exit;
          exception
            when others => ERROR;
          end;
        end loop;

        loop
          begin
            PUT ("Millisec -> "); GET (MILLISEC);
            exit;
          exception
            when others => ERROR;
          end;
        end loop;
        return CALENDAR.TIME_OF (YEAR, MONTH, DAY, DAY_MNG.PACK(HOUR, MINUTE, SECOND, MILLISEC));
      exception
        when CALENDAR.TIME_ERROR => ERROR;
      end;
    end loop;
  end GET;

  procedure PUT (DATE : in CALENDAR.TIME) is
    YEAR : CALENDAR.YEAR_NUMBER;
    MONTH : CALENDAR.MONTH_NUMBER;
    DAY : CALENDAR.DAY_NUMBER;
    SECONDS : CALENDAR.DAY_DURATION;
    use MY_IO;
  begin
    CALENDAR.SPLIT (DATE, YEAR, MONTH, DAY, SECONDS);
    PUT (YEAR); PUT (" ");
    PUT (MONTH); PUT (" ");
    PUT (DAY); PUT (" ");
    MY_IO.NEW_LINE;
  end PUT;

begin
  MY_IO.PUT_LINE ("Date1 :");
  T1 := GET;
  MY_IO.PUT_LINE (PERPET.DAY_OF_WEEK_LIST'IMAGE(PERPET.GET_DAY_OF_WEEK(T1)));
  loop
    begin
      MY_IO.PUT_LINE ("Date2 :");
      T2 := GET;
      MY_IO.PUT_LINE (PERPET.DAY_OF_WEEK_LIST'IMAGE(PERPET.GET_DAY_OF_WEEK(T2)));
      D := PERPET."-"(T1, T2);
      MY_IO.PUT (" Date2 - Date1:");
      MY_IO.PUT (D.DAYS); MY_IO.PUT (" days ");
      DUR_IO.PUT (D.SECS); MY_IO.PUT (" sec");
    
      MY_IO.NEW_LINE (2);
    exception
      when CALENDAR.TIME_ERROR =>
        MY_IO.PUT_LINE ("TIME_ERROR");
    end;
  end loop;
end T_DELTA_DATE;
