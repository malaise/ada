with TEXT_IO;
with ARGUMENT, NORMAL, MY_IO, GET_FLOAT;
with FL_TIME, FL_GET;

procedure FL is

  TT, T : FL_TIME.TIME_TYPE;
  MAX_HOUR_DIG : constant := 9; -- FL_TIME.HOURS_RANGE'WIDTH;
  WITH_COST : BOOLEAN;
  COST, TMP_COST : FLOAT;
  use FL_TIME;
begin
  if ARGUMENT.GET_NBRE_ARG = 1 and then
     ARGUMENT.GET_PARAMETER = "-c" then
    WITH_COST := TRUE;
  elsif ARGUMENT.GET_NBRE_ARG = 0 then
    WITH_COST := FALSE;
  else
    TEXT_IO.PUT_LINE ("USAGE: " & ARGUMENT.GET_PROGRAM_NAME
                    & " [ -c ]");
    return;
  end if;
    
  TEXT_IO.PUT_LINE ("      HOURS and MINUTES additions.");
  TEXT_IO.PUT_LINE ("      ----------------------------");
  TEXT_IO.PUT_LINE (" Syntax of time is [-]hhhhhhh[.mm] (Return);");
  TEXT_IO.PUT_LINE (" Enter 'C'to clear, 'X' or 'Q' to exit.");
  TEXT_IO.NEW_LINE (2);

  -- Initialise
  TT := (TRUE, 0, 0);
  COST := 0.0;
  TMP_COST := 0.0;

  loop
    -- Display
    TEXT_IO.PUT ("--> ");

    if TT.POSITIV then
      TEXT_IO.PUT (' ');
    else
      TEXT_IO.PUT ('-');
    end if;
    TEXT_IO.PUT (NORMAL(INTEGER(TT.HOURS), MAX_HOUR_DIG + 1));
    TEXT_IO.PUT ('.');
    TEXT_IO.PUT (NORMAL(INTEGER(TT.MINUTES), 2, GAP => '0'));
    if WITH_COST then
       TEXT_IO.PUT ("     This cost: ");
       MY_IO.PUT(TMP_COST, FORE => 4, AFT => 2, EXP => 0);
       TEXT_IO.PUT ("  Total cost: ");
       MY_IO.PUT_LINE(COST, FORE => 5, AFT => 2, EXP => 0);
    else
      TEXT_IO.NEW_LINE;
    end if;
      

    -- Get
    begin
      T := FL_GET.GET_TIME;
    exception
      when FL_GET.ERROR =>
        T := (TRUE, 0, 0);
        TMP_COST := 0.0;
        TEXT_IO.PUT_LINE (ASCII.BEL & "Error.");
      when FL_GET.CLEAR =>
        T := (TRUE, 0, 0);
        TT := (TRUE, 0, 0);
        TMP_COST := 0.0;
        COST := 0.0;
      when FL_GET.QUIT =>
        exit;
    end;

    if WITH_COST and then T /= (TRUE, 0, 0) then
      -- Get hourly cost of aircraft
      declare
        STR : STRING(1 .. 8);
        LEN : NATURAL;
      begin
        TEXT_IO.PUT(">>");
        TEXT_IO.GET_LINE(STR, LEN);
        TMP_COST := GET_FLOAT.GET_FLOAT(STR(1 .. LEN));
      exception
        when others =>
          T := (TRUE, 0, 0);
          TMP_COST := 0.0;
          TEXT_IO.PUT_LINE (ASCII.BEL & "Error.");
      end;
      -- Cost of the flight
      TMP_COST := TMP_COST * FLOAT(T.HOURS)
                + TMP_COST * FLOAT(T.MINUTES) / 60.0;
      if not T.POSITIV then
        TMP_COST := -TMP_COST;
      end if;
    end if;

    -- Add / substract
    declare
      ST : constant FL_TIME.TIME_TYPE := TT;
    begin
      TT := TT + T;
      if TT.HOURS = 0 and then TT.MINUTES = 0 then
        TT.POSITIV := TRUE;
      end if;
    exception
      when TIME_OVERFLOW =>
        TT := ST;
        TEXT_IO.PUT_LINE (ASCII.BEL & "Overflow.");
    end;
    COST := COST + TMP_COST;

  end loop;

end FL;



    
