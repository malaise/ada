with CALENDAR, TEXT_IO;
with PERPET, MY_IO;
procedure T_PERPET is

  T : CALENDAR.TIME;
  R : PERPET.DURATION_REC;
  D  : PERPET.DAY_RANGE;

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

        return CALENDAR.TIME_OF (YEAR, MONTH, DAY, 0.0);
      exception
        when CALENDAR.TIME_ERROR => ERROR;
      end;
    end loop;
  end GET;

  function GET return PERPET.DURATION_REC is
    DUR : PERPET.DURATION_REC;
    use MY_IO;
  begin
    loop
      begin
        PUT ("Years -> "); GET (DUR.YEARS);
        exit;
      exception
        when others => ERROR;
      end;
    end loop;

    loop
      begin
        PUT ("Months -> "); GET (DUR.MONTHS);
        exit;
      exception
        when others => ERROR;
      end;
    end loop;

    return DUR;
  end GET;


  function GET return PERPET.DAY_RANGE is
    D : PERPET.DAY_RANGE;
    use MY_IO;
  begin
    loop
      begin
        PUT ("Days -> "); GET (D);
        exit;
      exception
        when others => ERROR;
      end;
    end loop;

    return D;
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
  MY_IO.PUT_LINE ("Base :");
  T := GET;
  MY_IO.NEW_LINE;
  loop
    begin
      MY_IO.PUT_LINE ("Delta :");
      R := GET;
      MY_IO.PUT (" Base + Delta:"); PUT (PERPET."+"(T, R));
      MY_IO.PUT (" Base - Delta:"); PUT (PERPET."-"(T, R));
      MY_IO.NEW_LINE;

      MY_IO.PUT_LINE ("Delta :");
      D := GET;
      MY_IO.PUT (" Base + Delta:"); PUT (PERPET."+"(T, D));
      MY_IO.PUT (" Base - Delta:"); PUT (PERPET."-"(T, D));
      MY_IO.NEW_LINE (2);
    exception
      when CALENDAR.TIME_ERROR =>
        MY_IO.PUT_LINE ("TIME_ERROR");
    end;
  end loop;
end T_PERPET;
