with TEXT_IO;

with FL_TIME, FL_GET;

procedure FL is

  package INT_IO is new TEXT_IO.INTEGER_IO (INTEGER);

  TT, T : FL_TIME.TIME_TYPE;
  STR_2 : STRING (1..2);
  MAX_HOUR_DIG : constant := 9; -- FL_TIME.HOURS_RANGE'WIDTH;
  use FL_TIME;
begin
  TEXT_IO.PUT_LINE ("      HOURS and MINUTES additions.");
  TEXT_IO.PUT_LINE ("      ----------------------------");
  TEXT_IO.PUT_LINE (" Syntax of time is [-]hhhhhhh[.mm] (Return);");
  TEXT_IO.PUT_LINE (" Enter 'C'to clear, 'X' or 'Q' to exit.");
  TEXT_IO.NEW_LINE (2);



  TT := (TRUE, 0, 0);
  loop
    TEXT_IO.PUT ("--> ");

    if TT.POSITIV then
      TEXT_IO.PUT (' '); 
    else
      TEXT_IO.PUT ('-'); 
    end if;

    INT_IO.PUT (INTEGER(TT.HOURS), MAX_HOUR_DIG + 1);
    TEXT_IO.PUT ('.');
    INT_IO.PUT (STR_2, INTEGER(TT.MINUTES));
    if STR_2(1) = ' ' then STR_2(1) := '0'; end if;
    TEXT_IO.PUT_LINE (STR_2);

    begin
      T := FL_GET.GET_TIME;
    exception
      when FL_GET.ERROR =>
        T := (TRUE, 0, 0);
        TEXT_IO.PUT_LINE (ASCII.BEL & "Error.");
      when FL_GET.CLEAR =>
        T := (TRUE, 0, 0);
        TT := (TRUE, 0, 0);
      when FL_GET.QUIT =>
        exit;
    end;

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

  end loop;

end FL;



    
