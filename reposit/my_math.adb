
  with ADA.NUMERICS.GENERIC_ELEMENTARY_FUNCTIONS;

with CALENDAR, TEXT_IO;
package body MY_MATH is


  package MATH is new ADA.NUMERICS.GENERIC_ELEMENTARY_FUNCTIONS (REAL);



-- CONSTANTS FOR COMPUTING
--------------------------
  -- greatest number that can be exponented
  LN_MAX               : REAL;
  -- ln (10)
  LN_10                : REAL;
  -- Multiples et sub multiples of pi
  TWO_PI               : constant := 2.0*PI;
  PI_TWO               : constant := PI/2.0;
  PI_FOUR              : constant := PI/4.0;
  PI_HUNDRED_HEIGHTY   : constant := PI/180.0;


  -- Integer part of a real
  function INT (X : REAL) return REAL is

    package REAL_TEXT_IO is
      new TEXT_IO.FLOAT_IO(REAL);

    NEG   : BOOLEAN := FALSE;
    DIG   : constant POSITIVE := 15;  -- digits of real
    EXP   : constant POSITIVE := 4;  -- +123
    TOTAL : constant POSITIVE := 2 + DIG + 1 + EXP + 1;
    -- b-1.<14digits>E+123; (b = 1 extra space)
    subtype TYP_INDEX_STR is POSITIVE range 1 .. TOTAL;
    STR_AUX   : STRING(TYP_INDEX_STR);
    STR_EXP   : STRING(1 .. EXP);
    RESULT    : REAL;
    EXPONENT  : INTEGER;
    INDEX_STR : INTEGER;
  begin
    if X < 0.0 then
      NEG := TRUE;
    end if;

    -- store  x in a string
    REAL_TEXT_IO.PUT(STR_AUX, abs(X), DIG - 1, EXP);

    -- compute exponent
    STR_EXP(1 .. EXP) := STR_AUX(TOTAL - EXP + 1 .. TOTAL);
    EXPONENT := INTEGER'VALUE(STR_EXP);
    if EXPONENT < 0 then

      -- no integer part
      RESULT := 0.0;
    elsif EXPONENT < DIG - 1 then

      -- reset fraction digits to 0
      for INDEX in TOTAL - EXP - DIG + 1 + EXPONENT .. TOTAL - EXP - 1 loop
        STR_AUX(INDEX) := '0';
      end loop;

      -- convert result to real
      REAL_TEXT_IO.GET(STR_AUX, RESULT, INDEX_STR);
    else

      -- no fraction part (number to big)
      RESULT := X;
    end if;

    if NEG then
      return -RESULT;
    else
      return RESULT;
    end if;
  end INT;

  function FRAC (X : REAL) return REAL is
  begin
    return abs(X - INT(X));
  end FRAC;

  -- Real to inte : round or trunc
  function ROUND (X : REAL) return INTE is
    RESULTAT : INTE;
  begin
    if X > 0.0 then
      RESULTAT := TRUNC(X + 0.5);
    else
      RESULTAT := TRUNC(X - 0.5);
    end if;
    return RESULTAT;
  exception
    when others =>
      raise MATH_ERROR;
  end ROUND;

  function TRUNC (X : REAL) return INTE is
    INT : INTE;
  begin
    INT := INTE(X);

    -- adjust +- 1
    if X > 0.0 then

      -- if x > 0 error by exceed
      if REAL(INT) > X then
        INT := INT - 1;
      end if;

    else

      -- if x < 0 error by default
      if REAL(INT) < X then
        INT := INT + 1;
      end if;

    end if;

    return INT;
  exception
    when others =>
      raise MATH_ERROR;
  end TRUNC;

  -- power
  function "**" (NUMBER, EXPONENT : REAL) return REAL is
  begin
   return MATH."**" (NUMBER, EXPONENT);
  exception
    when others =>
      raise MATH_ERROR;
  end "**";

  -- square root
  function SQRT (X : REAL) return REAL is
  begin
    if X < 0.0 then
      raise MATH_ERROR;
    end if;
    return MATH.SQRT (X);
  exception
    when others =>
      raise MATH_ERROR;
  end SQRT;

  -- Based 10 log
  function LOG_10 (X : REAL) return REAL is
  begin
    if X < 0.0 then
      raise MATH_ERROR;
    end if;
    return LN(X)/LN_10;
  exception
    when others =>
      raise MATH_ERROR;
  end LOG_10;

  function EXP (X : REAL := 1.0) return REAL is
  begin
    return E ** X;
  end EXP;

  function LN (X : REAL) return REAL is
  begin
    if X < 0.0 then
      raise MATH_ERROR;
    end if;
    return MATH.LOG (X);
  exception
    when others =>
      raise MATH_ERROR;
  end LN;

  function SIN (X    : REAL;
                MODE : ANGLE_UNIT := RADIAN) return REAL is
    Y      : REAL;
  begin
    if MODE = RADIAN then
      Y := X;
    else
      Y := X * PI_HUNDRED_HEIGHTY;
    end if;
    return MATH.SIN (Y);
  exception
    when others =>
      raise MATH_ERROR;
  end SIN;

  function COS (X    : REAL;
                MODE : ANGLE_UNIT := RADIAN) return REAL is
    Y      : REAL;
  begin
    if MODE = RADIAN then
      Y := X;
    else
      Y := X * PI_HUNDRED_HEIGHTY;
    end if;
    return MATH.COS (Y);
  exception
    when others =>
      raise MATH_ERROR;
  end COS;

  function TG (X    : REAL;
               MODE : ANGLE_UNIT := RADIAN) return REAL is
    Y       : REAL;
  begin
    if MODE = RADIAN then
      Y := X;
    else
      Y := X * PI_HUNDRED_HEIGHTY;
    end if;
    return MATH.TAN (Y);
  exception
    when others =>
      raise MATH_ERROR;
  end TG;

  function ARC_SIN (X    : REAL;
                    MODE : ANGLE_UNIT := RADIAN) return REAL is
    Y : REAL;
  begin
    if abs (X) > 1.0 then
      raise MATH_ERROR;
    end if;
    Y := MATH.ARCSIN (X);
    if MODE = DEGREE then
      Y := Y / PI_HUNDRED_HEIGHTY;
    end if;
    return Y;
  exception
    when others =>
      raise MATH_ERROR;
  end ARC_SIN;

  function ARC_COS (X    : REAL;
                    MODE : ANGLE_UNIT := RADIAN) return REAL is
    Y   : REAL;
  begin
    if abs (X) > 1.0 then
      raise MATH_ERROR;
    end if;
    Y := MATH.ARCCOS (X);
    if MODE = DEGREE then
      Y := Y / PI_HUNDRED_HEIGHTY;
    end if;
    return Y;
  exception
    when others =>
      raise MATH_ERROR;
  end ARC_COS;

  function ARC_TG (X    : REAL;
                   MODE : ANGLE_UNIT := RADIAN) return REAL is
    Y       : REAL;
  begin
    Y := MATH.ARCTAN (X);
    if MODE = DEGREE then
      Y := Y / PI_HUNDRED_HEIGHTY;
    end if;
    return Y;
  exception
    when others =>
      raise MATH_ERROR;
  end ARC_TG;

begin
  LN_MAX := LN(REAL'LARGE);
  LN_10 := LN(10.0);
end MY_MATH;
