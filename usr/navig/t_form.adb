with MY_IO; use MY_IO;
with NAV_TYPES, NAV_FORMAT;
use  NAV_TYPES, NAV_FORMAT;

procedure T_FORM is
  STR_SPEED : STRING (1 .. 6);
  STR_ANGLE : STRING (1 .. 6);
  STR_DRIFT : STRING (1 .. 7);
  RES : FORMAT_RESULT;
  POS : POSITIVE;
  SPEED : T_SPEED;
  ANGLE : T_ANGLE;
  DRIFT : T_DRIFT;
  LST : NATURAL;

  procedure PUT (RES : in FORMAT_RESULT; POS : in POSITIVE) is
  begin
    case RES is
      when SET   => PUT_LINE ("SET");
      when UNSET => PUT_LINE ("UNSET");
      when ERROR => PUT_LINE ("ERROR at pos " & INTEGER'IMAGE(POS) );
    end case;
  end PUT;

begin

  loop
    begin
      PUT ("SPEED ? "); GET_LINE (STR_SPEED, LST);
      STR_SPEED (LST+1 .. STR_SPEED'LAST) := (others => ' ');
      VALUE (STR_SPEED, SPEED, RES, POS);
      PUT (RES, POS);
      if RES = SET then
        PUT ("VALUE : "); PUT_LINE (SPEED);
      end if;
      if RES /= ERROR then
        PUT ("IMAGE : "); PUT_LINE (IMAG(SPEED, RES=SET));
      end if;

      PUT ("ANGLE ? "); GET_LINE (STR_ANGLE, LST);
      STR_ANGLE (LST+1 .. STR_ANGLE'LAST) := (others => ' ');
      VALUE (STR_ANGLE, ANGLE, RES, POS);
      PUT (RES, POS);
      if RES = SET then
        PUT ("VALUE : "); PUT (INTEGER(ANGLE.DEGREES));
        PUT_LINE (INTEGER(ANGLE.MINUTES));
      end if;
      if RES /= ERROR then
        PUT ("IMAGE : "); PUT_LINE (IMAG(ANGLE, RES=SET));
      end if;

      PUT ("DRIFT ? "); GET_LINE (STR_DRIFT, LST);
      STR_DRIFT (LST+1 .. STR_DRIFT'LAST) := (others => ' ');
      VALUE (STR_DRIFT, DRIFT, RES, POS);
      PUT (RES, POS);
      if RES = SET then
        PUT ("VALUE : ");
        if not DRIFT.POSITIV then PUT ('-'); else PUT ('+'); end if;
        PUT (INTEGER(DRIFT.DEGREES));
        PUT_LINE (INTEGER(DRIFT.MINUTES));
      end if;
      if RES /= ERROR then
        PUT ("IMAGE : "); PUT_LINE (IMAG(DRIFT, RES=SET));
      end if;
    exception
      when others => raise;
    end;
  end loop;
end T_FORM;

