with ADA.CALENDAR;

with NORMAL;
with ARGUMENT;
with MY_IO;
with DAY_MNG;
with DOS;

with TYPES;
with FILE;
with EURISTIC;

procedure HUNGAR is
  DIM : NATURAL;
  SIGMA : FLOAT;
  LOC_IDEAL_NOTE, IDEAL_NOTE, LOC_NOTE : FLOAT;
  LOC_J : TYPES.INDEX_RANGE;
  NB_ITERATIONS : POSITIVE;
  MAX_ITER_DIGITS : constant := 3;
  START_TIME : ADA.CALENDAR.TIME;

begin
  if ARGUMENT.GET_NBRE_ARG /= 1 then
    MY_IO.PUT_LINE ("Syntax error. Usage : hungar <file_name>");
    return;
  end if;

  START_TIME := ADA.CALENDAR.CLOCK;

  SOLVE:
  declare
    MATTRIX : TYPES.MATTRIX_REC := FILE.READ (ARGUMENT.GET_PARAMETER);
  begin

    DIM := MATTRIX.DIM;

    EURISTIC.SEARCH (MATTRIX, NB_ITERATIONS);
  
    MY_IO.PUT_LINE ("Result:");
    SIGMA := 0.0;
    IDEAL_NOTE := 0.0;
    for I in 1 .. DIM loop
      if TYPES."=" (FILE.GET_KIND, TYPES.REGRET) then
        LOC_IDEAL_NOTE := FLOAT'LAST;
      else
        LOC_IDEAL_NOTE := FLOAT'FIRST;
      end if;
      for J in 1 .. DIM loop
        if TYPES."=" (FILE.GET_KIND, TYPES.REGRET) then
          -- Lowest note of this row
          if FILE.GET_NOTE(I, J) < LOC_IDEAL_NOTE then
            LOC_IDEAL_NOTE := FILE.GET_NOTE(I, J);
          end if;
        else
          -- Highest note of this row
          if FILE.GET_NOTE(I, J) > LOC_IDEAL_NOTE then
            LOC_IDEAL_NOTE := FILE.GET_NOTE(I, J);
          end if;
        end if;
        if MATTRIX.NOTES(I, J) = 1 then
          -- Affectation found
          LOC_J := J;
        end if;
      end loop;

      -- Affectation
      MY_IO.PUT ("row " & NORMAL(I, 3) & " column " & NORMAL(LOC_J, 3));
      LOC_NOTE := FILE.GET_NOTE(I, LOC_J);
      if TYPES."=" (FILE.GET_KIND, TYPES.REGRET) then
        MY_IO.PUT (" cost: ");
      else
        MY_IO.PUT (" note: ");
      end if;
      MY_IO.PUT (LOC_NOTE, 3, 2, 0);
      SIGMA := SIGMA + LOC_NOTE;

      -- Ideal minimum cost
      IDEAL_NOTE := IDEAL_NOTE + LOC_IDEAL_NOTE;
      MY_IO.PUT ("   Ideal: ");
      MY_IO.PUT (LOC_IDEAL_NOTE, 3, 2, 0);

      -- Loss 
      if abs (LOC_IDEAL_NOTE - LOC_NOTE) > FILE.EPSILON then
        MY_IO.PUT (" Loss: ");
        MY_IO.PUT (abs (LOC_IDEAL_NOTE - LOC_NOTE), 3, 2, 0);
      end if;
      MY_IO.NEW_LINE;

    end loop;
    MY_IO.NEW_LINE;

    -- Total
    if TYPES."=" (FILE.GET_KIND, TYPES.REGRET) then
      MY_IO.PUT ("Total cost: ");
      MY_IO.PUT(SIGMA, 6, 2, 0);
      MY_IO.PUT ("  Ideal cost: ");
      MY_IO.PUT(IDEAL_NOTE, 6, 2, 0);
    else
      MY_IO.PUT ("Total note: ");
      MY_IO.PUT(SIGMA, 6, 2, 0);
      MY_IO.PUT ("  Ideal note: ");
      MY_IO.PUT(IDEAL_NOTE, 6, 2, 0);
    end if;
    MY_IO.PUT ("  Total loss: ");
    MY_IO.PUT (abs (IDEAL_NOTE - SIGMA), 6, 2, 0);
    MY_IO.PUT ("  Iter: ");
    if POSITIVE'IMAGE(NB_ITERATIONS)'LENGTH - 1 >= MAX_ITER_DIGITS then
      MY_IO.PUT (POSITIVE'IMAGE(NB_ITERATIONS));
    else
      MY_IO.PUT (NORMAL (NB_ITERATIONS, MAX_ITER_DIGITS));
    end if;
    MY_IO.NEW_LINE;

  end SOLVE;

  COMPUTE_ELAPSE:
  declare
    use type ADA.CALENDAR.TIME;
    DUR : DURATION;
    DAYS : NATURAL;
    HOURS : DAY_MNG.T_HOURS;
    MINUTES : DAY_MNG.T_MINUTES;
    SECONDS : DAY_MNG.T_SECONDS;
    MILLISECS : DAY_MNG.T_MILLISEC;
  begin
    DUR := ADA.CALENDAR.CLOCK - START_TIME;
    DAYS := 0;
    while DUR > ADA.CALENDAR.DAY_DURATION'LAST loop
      DAYS := DAYS + 1;
      DUR := DUR - ADA.CALENDAR.DAY_DURATION'LAST;
    end loop;
    DAY_MNG.SPLIT (DUR, HOURS, MINUTES, SECONDS, MILLISECS);
    DISPLAY_ELAPSE:
    declare
      SOME_TIME_PUT : BOOLEAN := FALSE;
      procedure PUT_TIME (VAL : in NATURAL; MSG : in STRING) is
      begin
        if VAL = 0 and then not SOME_TIME_PUT then
          return;
        end if;
        SOME_TIME_PUT := TRUE;
        MY_IO.PUT (NATURAL'IMAGE(VAL) & " " & MSG);
        if VAL > 1 then
          MY_IO.PUT ("s");
        end if;
      end PUT_TIME;
    begin  
      MY_IO.PUT ("In");
      PUT_TIME (DAYS, "day");
      PUT_TIME (HOURS, "hour");
      PUT_TIME (MINUTES, "minute");
      PUT_TIME (SECONDS, "second");
      if SOME_TIME_PUT then
        MY_IO.PUT (" and");
      end if;
      PUT_TIME (MILLISECS, "millisecond");
      MY_IO.PUT_LINE (".");
    end DISPLAY_ELAPSE;
  end COMPUTE_ELAPSE;
  DOS.SOUND;

exception
  when FILE.READ_ERROR =>
    null;
end HUNGAR;
