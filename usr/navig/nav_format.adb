with MATH;
use MATH;
with NORMAL;
-- to convert got strings in data fields, and fields in string to be put
package body NAV_FORMAT is



  -- from speed to string
  function IMAG (SPEED : NAV_TYPES.T_SPEED; SET : BOOLEAN := TRUE)
   return STRING is
    STR_UNSET : constant STRING := "???.?";
    STR : STRING (1 .. STR_UNSET'LAST);
    I : INTEGER;
  begin
    if SET then
      STR (1 .. 3) := NORMAL (INTEGER (MATH.TRUNC (MATH.REAL(SPEED))), 3, TRUE, ' ');
      STR (4) := '.';
      I := INTEGER (MATH.ROUND (MATH.FRAC(MATH.REAL(SPEED)) * 10.0));
      STR (5 .. 5) := NORMAL (I, 1, FALSE, '0');
      return STR;
    else
      return STR_UNSET;
    end if;
  end IMAG;

  -- from string to angle
  function IMAG (ANGLE : NAV_TYPES.T_ANGLE; SET : BOOLEAN := TRUE)
   return STRING is
    STR_UNSET : constant STRING := "???.??";
    STR : STRING (1 .. STR_UNSET'LAST);
  begin
    if SET then
      STR (1 .. 3) := NORMAL (INTEGER (ANGLE.DEGREES), 3, TRUE, ' ');
      STR (4) := '.';
      STR (5 .. 6) := NORMAL (INTEGER (ANGLE.MINUTES), 2, TRUE, '0');
      return STR;
    else
      return STR_UNSET;
    end if;
  end IMAG;

  -- from string to drift
  function IMAG (DRIFT : NAV_TYPES.T_DRIFT; SET : BOOLEAN := TRUE)
   return STRING is
    STR_UNSET : constant STRING := "????.??";
    STR : STRING (1 .. STR_UNSET'LAST);
  begin
    if SET then
      if DRIFT.POSITIV then
        STR(1) := '+';
      else
        STR(1) := '-';
      end if;
      STR (2 .. 4) := NORMAL (INTEGER (DRIFT.DEGREES), 3, TRUE, ' ');
      STR (5) := '.';
      STR (6 .. 7) := NORMAL (INTEGER (DRIFT.MINUTES), 2, TRUE, '0');
      return STR;
    else
      return STR_UNSET;
    end if;
  end IMAG;

  function IS_DIGIT (C : CHARACTER) return BOOLEAN is
  begin
    return C >= '0' and then C <= '9';
  end IS_DIGIT;


  -- to check generaly the string:
  -- [{' '}] ['+'|'-'] [{' '}] {d} ['.'{d}] [{' '}]
  -- [] : non or once, {} : once or more, | : or, d : digit
  -- returns index of first and last significant character
  -- of ok, pos is index of '.' or 0; if error, it's error position;
  --  if clear, no meaning
  procedure CHECK (STR : in STRING;
   FIRST, LAST : out POSITIVE; SIGN : out BOOLEAN;
   POS : out NATURAL; RES : out FORMAT_RESULT) is
    F, L : POSITIVE;
    FIRST_DIGIT : POSITIVE;
    DIGIT_FOUND : BOOLEAN;
    C : CHARACTER;
    DOT : BOOLEAN;
  begin
    -- parse leading and tailing spaces
    F := STR'FIRST;
    while F <= STR'LAST and then STR(F) = ' ' loop
      F := F + 1;
    end loop;
    L := STR'LAST;
    while L >= STR'FIRST and then STR(L) = ' ' loop
      L := L - 1;
    end loop;
    if STR'LENGTH = 0 or else STR(F) = ' ' then
      -- only spaces : ERR
      POS := STR'FIRST;
      RES := ERROR;
      return;
    end if;

    FIRST := F;
    LAST := L;
    SIGN := FALSE;


    -- first significant char may be + -
    if STR(F) = '+' or else STR(F) = '-' then
      SIGN := TRUE;
      -- go on starting from next significant char
      FIRST_DIGIT := F + 1;
      while FIRST_DIGIT <= STR'LAST and then STR(FIRST_DIGIT) = ' ' loop
        FIRST_DIGIT := FIRST_DIGIT + 1;
      end loop;
    else
      FIRST_DIGIT := F;
    end if;

   -- general syntax ?  or     {d} ['.'{d}]
   DOT := FALSE; -- no dot
   POS := 0;
   for I in FIRST_DIGIT .. L loop
     C := STR(I);
     if C = '?' then
       RES := UNSET;
       RETURN;
     elsif IS_DIGIT(C) then
       DIGIT_FOUND := TRUE;
     elsif C = '.' then
       POS := I;
       if DOT then
         -- a second dot : ERR
         RES := ERROR;
         return;
       else
         DOT := TRUE;
         if not DIGIT_FOUND then
           -- begin with DOT???
           RES := ERROR;
           return;
         end if;
       end if;
     else
       -- unknown char
       POS := I;
       RES := ERROR;
       return;
     end if;
   end loop;

   -- last significant char must be a digit (or ? , but already returned)
   if not IS_DIGIT(C) then
     POS := L;
     RES := ERROR;
     return;
   else
     RES := SET;
     return;
   end if;

  end CHECK;


  -- from string to speed
  procedure VALUE (STR : in STRING; SPEED : out NAV_TYPES.T_SPEED;
   RES : out FORMAT_RESULT; POS : out POSITIVE) is
    F, L : POSITIVE;
    S : BOOLEAN;
    P : NATURAL;
    R : FORMAT_RESULT;
    SPE : FLOAT;
  begin
    POS := 1;
    CHECK (STR, F, L, S, P, R);
    if R = UNSET then
      RES := UNSET;
      return;
    end if;
    if S then
      -- signed speed forbidden: error at first signif char
      P := F;
      R := ERROR;
    end if;

    if R = SET and then P /= 0 and then L - P > 1 then
      -- only 1 digit after dot is allowed
      POS := P + 2;
      RES := ERROR;
      return;
    end if;
    if R = ERROR then
      POS := P;
      RES := R;
      return;
    end if;

    if P = 0 then
      -- no dot
      SPE := FLOAT(INTEGER'VALUE( STR(F..L) ));
    else
      -- dot and 1 digit
      SPE := FLOAT(INTEGER'VALUE( STR(F..P-1) ));
      SPE := SPE + ( FLOAT(INTEGER'VALUE( STR(P+1..L) )) / 10.0);
    end if;
    SPEED := NAV_TYPES.T_SPEED (SPE);
    RES := SET;
  end VALUE;




  -- from string to angle
  procedure VALUE (STR : in STRING; ANGLE : out NAV_TYPES.T_ANGLE;
   RES : out FORMAT_RESULT; POS : out POSITIVE) is
    F, L : POSITIVE;
    S : BOOLEAN;
    P : NATURAL;
    R : FORMAT_RESULT;
  begin
    POS := 1;
    CHECK (STR, F, L, S, P, R);
    if R = UNSET then
      RES := UNSET;
      return;
    end if;
    if S then
      -- signed angle forbidden: error at first signif char
      P := F;
      R := ERROR;
    end if;

    if R = SET and then P /= 0 and then L - P > 2 then
      -- only 1 or 2 digits after dot is allowed
      POS := P + 3;
      RES := ERROR;
      return;
    end if;
    if R = ERROR then
      POS := P;
      RES := R;
      return;
    end if;

    if P = 0 then
      -- no dot
      ANGLE.DEGREES := NAV_TYPES.T_DEGREE'VALUE( STR(F..L) );
      ANGLE.MINUTES := 0;
    elsif P = L-1 then
      -- dot and 1 digit which is in 10 minutes
      declare
        use NAV_TYPES;
      begin
        ANGLE.DEGREES := NAV_TYPES.T_DEGREE'VALUE( STR(F..P-1) );
        ANGLE.MINUTES := NAV_TYPES.T_MINUTE'VALUE( STR(P+1..L) )
         * NAV_TYPES.T_MINUTE'(10);
      end;
    else
      -- dot and 2 digits which are minutes
      ANGLE.DEGREES := NAV_TYPES.T_DEGREE'VALUE( STR(F..P-1) );
      ANGLE.MINUTES := NAV_TYPES.T_MINUTE'VALUE( STR(P+1..L) );
    end if;
    RES := SET;
 end VALUE;

  -- from string to drift
  procedure VALUE (STR : in STRING; DRIFT : out NAV_TYPES.T_DRIFT;
   RES : out FORMAT_RESULT; POS : out POSITIVE) is
    F, L : POSITIVE;
    S : BOOLEAN;
    P : NATURAL;
    R : FORMAT_RESULT;
  begin
    POS := 1;
    CHECK (STR, F, L, S, P, R);
    if R = UNSET then
      RES := UNSET;
      return;
    end if;

    if R = SET and then P /= 0 and then L - P > 2 then
      -- only 1 or 2 digits after dot is allowed
      POS := P + 3;
      RES := ERROR;
      return;
    end if;
    if R = ERROR then
      POS := P;
      RES := R;
      return;
    end if;

    if S then
      -- signed drift at first signif char
      DRIFT.POSITIV := (STR (F) = '+');
      loop
        F := F + 1;
        exit when STR(F) /= ' ';
      end loop;
    else
      DRIFT.POSITIV := TRUE;
    end if;

    if P = 0 then
      -- no dot
      DRIFT.DEGREES := NAV_TYPES.T_DEG_DRIFT'VALUE( STR(F..L) );
      DRIFT.MINUTES := 0;
    elsif P = L-1 then
      -- dot and 1 digit which is in 10 minutes
      declare
        use NAV_TYPES;
      begin
        DRIFT.DEGREES := NAV_TYPES.T_DEG_DRIFT'VALUE( STR(F..P-1) );
        DRIFT.MINUTES := NAV_TYPES.T_MINUTE'VALUE( STR(P+1..L) )
         * NAV_TYPES.T_MINUTE'(10);
       end;
    else
      -- dot and 2 digits which are minutes
      DRIFT.DEGREES := NAV_TYPES.T_DEG_DRIFT'VALUE( STR(F..P-1) );
      DRIFT.MINUTES := NAV_TYPES.T_MINUTE'VALUE( STR(P+1..L) );
    end if;
    RES := SET;
 end VALUE;

end NAV_FORMAT;
