with TEXT_IO, CALENDAR;
with DAY_MNG;

procedure T_DAY_MNG is

  package DUR_IO is new TEXT_IO.FIXED_IO (CALENDAR.DAY_DURATION);

  DUR : CALENDAR.DAY_DURATION;
  HOURS    : DAY_MNG.T_HOURS;
  MINUTES  : DAY_MNG.T_MINUTES;
  SECONDS  : DAY_MNG.T_SECONDS;
  MILLISEC : DAY_MNG.T_MILLISEC;

  
begin

  loop

    loop
      begin
        TEXT_IO.PUT ("Enter a duration: ");
        DUR_IO.GET(DUR);
        exit;
      exception
        when others =>
          TEXT_IO.PUT_LINE ("Error.");
      end;
    end loop;

    DAY_MNG.SPLIT (DUR, HOURS, MINUTES, SECONDS, MILLISEC);

    TEXT_IO.PUT_LINE (DAY_MNG.T_HOURS'IMAGE(HOURS) & " h  "
                    & DAY_MNG.T_MINUTES'IMAGE(MINUTES) & " min  "
                    & DAY_MNG.T_SECONDS'IMAGE(SECONDS) & " sec  "
                    & DAY_MNG.T_MILLISEC'IMAGE(MILLISEC) & " msec");

    DUR := DAY_MNG.PACK (HOURS, MINUTES, SECONDS, MILLISEC);

    DUR_IO.PUT(DUR);
    TEXT_IO.NEW_LINE;

  end loop;

end T_DAY_MNG;

