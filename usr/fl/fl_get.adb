with TEXT_IO;

package body FL_GET is

  function PARSE_MINUTES (STR : STRING) return FL_TIME.MINUTES_RANGE is
  begin
    if STR'LENGTH = 1 then
      raise ERROR;
    else
      return FL_TIME.MINUTES_RANGE'VALUE (STR);
    end if;
  end PARSE_MINUTES;

  function GET_TIME return FL_TIME.TIME_TYPE is
    MAX_HOUR_DIG : constant := 9; -- FL_TIME.HOURS_RANGE'WIDTH;
    STR : STRING(1 .. MAX_HOUR_DIG + 2 + 1);
    F, L, P :  NATURAL;
    TIME : FL_TIME.TIME_TYPE := (TRUE, 0, 0);
  begin
    TEXT_IO.PUT ('>');
    TEXT_IO.GET_LINE (STR, L);
    if L = 0 then raise ERROR; end if;
    if L = 1 then
      if STR(1) = '.' then raise ERROR; end if;
      if STR(1) = '-' then raise ERROR; end if;
      if STR(1) = 'x' or else STR(1) = 'X' then raise QUIT;  end if;
      if STR(1) = 'q' or else STR(1) = 'Q' then raise QUIT;  end if;
      if STR(1) = 'c' or else STR(1) = 'C' then raise CLEAR; end if;
    end if;

    if STR(1) = '-' then
      TIME.POSITIV := FALSE;
      F := 2;
    else
      TIME.POSITIV := TRUE;
      F := 1;
    end if;

    P := 0;
    for I in F .. L loop
      case STR(I) is
        when '0'..'9' =>
          null;
        when '.' =>
          if P = 0 then
            P := I;
          else
            raise ERROR;
          end if;
        when others =>
          raise ERROR;
      end case;
    end loop;

    if P = 0 then
      -- no dot : only hours
      TIME.HOURS := FL_TIME.HOURS_RANGE'VALUE (STR(F..L));
      TIME.MINUTES := 0;
    elsif P = L then
      -- dot at last : only hours
      TIME.HOURS := FL_TIME.HOURS_RANGE'VALUE (STR(F..P-1));
      TIME.MINUTES := 0;
    elsif P = F then
      -- dot as first : only minutes
      TIME.HOURS := 0;
      TIME.MINUTES := PARSE_MINUTES (STR(P+1..L));
    else
      -- dot somewhere
      TIME.HOURS := FL_TIME.HOURS_RANGE'VALUE (STR(F..P-1));
      TIME.MINUTES := PARSE_MINUTES (STR(P+1..L));
    end if;

    return TIME;

  exception
    when QUIT | CLEAR => raise;
    when others => raise ERROR;
  end GET_TIME;

end FL_GET;

