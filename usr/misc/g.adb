with CALENDAR;
with MY_IO;
with U_RAND;
with CLEAR_SCREEN;
procedure G is
  -- generated number
  subtype NUMBER is NATURAL range 0 .. 999_999_999;
  NUM : NUMBER;

  -- result of division of current value
  type DIV_RES is (MINUS_1, ZERO, PLUS_1);
  DIV_ERROR : exception;

  GOT_RES, RES : DIV_RES;
  SUCCESS : BOOLEAN;

  procedure INIT is
    N : POSITIVE;
  begin
    N := POSITIVE (CALENDAR.SECONDS(CALENDAR.CLOCK));
    N := (N mod (U_RAND.SEED_RANGE_2'LAST)) + 1;
    U_RAND.START (NEW_L => N);
  end INIT;

  function RAND return POSITIVE is
    subtype DIGIT is NATURAL range 0 .. 9;
    subtype R_DIGIT is FLOAT range 
     FLOAT (DIGIT'FIRST) .. FLOAT (DIGIT'LAST + 1);
    RET : POSITIVE;
    function TRUNC (R : in R_DIGIT) return DIGIT is
      D : NATURAL;
    begin
      D := NATURAL (R);
      if FLOAT (D) > R then
        D := D - 1;
      end if;
      return D;
    end TRUNC;
    function NEW_DIGIT (ALLOW_0 : in BOOLEAN) return DIGIT is
      R : R_DIGIT;
      D : DIGIT;
    begin
      loop
        R := U_RAND.NEXT * R_DIGIT'LAST;
        D := TRUNC (R);
        exit when ALLOW_0 or else D /= 0;
      end loop;
      return D;
    end NEW_DIGIT;

  begin
    RET := NEW_DIGIT (ALLOW_0 => FALSE);
    for i in 1 .. 8 loop
      RET := RET * 10 + NEW_DIGIT (ALLOW_0 => TRUE);
    end loop;
    return RET;
  end RAND;

  procedure DIV (N : in NUMBER; 
   NEW_N : out NUMBER; RES : out DIV_RES) is
    N0 : NATURAL;
    function TRUNC (R : in FLOAT) return NATURAL is
      N : NATURAL;
    begin
      N := NATURAL (R);
      if FLOAT (N) > R then
        N := N - 1;
      end if;
      return N;
    end TRUNC;
  begin
    N0 := N rem 3;
    if N0 = 0 then
      RES := ZERO;
      NEW_N := N / 3;
    elsif N0 = 1 then
      RES := MINUS_1;
      NEW_N := (N-1) / 3;
    elsif N0 = 2 then
      RES := PLUS_1;
      NEW_N := (N+1) / 3;
    else
      raise PROGRAM_ERROR;
    end if;
    return;
  end DIV;

begin
  CLEAR_SCREEN;
  INIT;

  GAME:
  loop
    NUM := RAND;
    SUCCESS := TRUE;
    MY_IO.PUT ("   ");

    PARTY:
    loop

      GET:
      loop
        MY_IO.PUT ("--> ");
        MY_IO.PUT (NUM, 10); 
        exit PARTY when NUM = 0;
        MY_IO.PUT (
         "  '4' -1   '5' 0   '6' +1   'q' quitter ? ");
        declare
          STR : STRING (1 .. 132);
          LST : NATURAL;
          
        begin
          MY_IO.GET_LINE (STR, LST);
          if LST = 1 then
            if STR(1) = '4' then
              GOT_RES := MINUS_1;
              exit GET;
            elsif STR(1) = '6' then
              GOT_RES := PLUS_1;
              exit GET;
            elsif STR(1) = '5' then
              GOT_RES := ZERO;
              exit GET;
            elsif STR(1) = 'q' or STR(1) = 'Q' then
              CLEAR_SCREEN;
              return;
            end if;
          end if;
        end;
        MY_IO.PUT ("ERR");
      end loop GET;

      DIV (NUM, NUM, RES);
      if RES /= GOT_RES then
        SUCCESS := FALSE;
        CASE RES is
          when ZERO =>
            MY_IO.PUT_LINE (" Erreur, c'etait  0");
          when PLUS_1 =>
            MY_IO.PUT_LINE (" Erreur, c'etait +1");
          when MINUS_1 =>
            MY_IO.PUT_LINE (" Erreur, c'etait -1");
        end case;
      end if;
      case RES is
        when ZERO =>
          MY_IO.PUT (" 0 ");
        when PLUS_1 =>
          MY_IO.PUT ("+1 ");
        when MINUS_1 =>
          MY_IO.PUT ("-1 ");
      end case;
    end loop PARTY;

    MY_IO.PUT ("   ");
    if SUCCESS then MY_IO.PUT_LINE (" Sans faute, BRAVO.");
    else MY_IO.PUT_LINE (" Des erreurs...");
    end if;
    MY_IO.NEW_LINE; 
  end loop GAME;

end G;
