-- Operations on the general types for aeronautics navigation
package body NAV_TYPES is

  -- For an angle, reduce its value in a circle (0 .. 359)
  function REDUCTION (DEG : T_COMMON_DEG; MIN : T_MINUTE) return T_ANGLE is
    LOC_DEG : T_COMMON_DEG;
    LOC_MIN : NATURAL;
  begin
    LOC_DEG := DEG;
    LOC_MIN := NATURAL(MIN);
    -- Substract if necessary
    while LOC_DEG > T_DEGREE'LAST loop
      LOC_DEG := LOC_DEG - (T_DEGREE'LAST + 1);
    end loop;
    -- Add if necessary
    if LOC_DEG < 0 then
      if LOC_MIN /= 0 then
        LOC_MIN := NATURAL(T_MINUTE'LAST) + 1 - LOC_MIN;
        LOC_DEG := LOC_DEG - 1;
      end if;
      while LOC_DEG < 0 loop
        LOC_DEG := LOC_DEG + (T_DEGREE'LAST + 1);
      end loop;
    end if;

    return T_ANGLE'(DEGREES => T_DEGREE(LOC_DEG),
     MINUTES => T_MINUTE(LOC_MIN));
  end REDUCTION;

  -- Add two angles to get an angle
  function "+" (A1, A2 : T_ANGLE) return T_ANGLE is
    DEG : T_COMMON_DEG;
    MIN : NATURAL;
  begin
    DEG := 0;
    -- Add minutes
    MIN := NATURAL (A1.MINUTES) + NATURAL (A2.MINUTES);
    if MIN > NATURAL (T_MINUTE'LAST) then
      MIN := MIN - NATURAL (T_MINUTE'LAST) - 1;
      DEG := 1;
    end if;
    -- Add angles
    DEG := DEG + A1.DEGREES + A2.DEGREES;
    return REDUCTION (DEG, T_MINUTE(MIN));
  end "+";

  -- Add two angles to get an angle
  function "+" (A1 : T_ANGLE; A2 : T_DEGREE) return T_ANGLE is
  begin
    return "+" (A1 => A1, A2 => (DEGREES => A2, MINUTES => 0));
  end "+";

  -- Sub 2 angles to get an angle
  function "-" (A1, A2 : T_ANGLE) return T_ANGLE is
    DEG : T_COMMON_DEG;
    MIN : INTEGER;
    V1, V2 : T_ANGLE;
    NEG : BOOLEAN;
    CARRY : BOOLEAN;
  begin
    NEG := A1 < A2;
    if not NEG then
      V1 := A1; V2 := A2;
    else
      V1 := A2; V2 := A1;
    end if;
    -- Sub minutes
    MIN := INTEGER (V1.MINUTES) - NATURAL (V2.MINUTES);
    CARRY := MIN < 0;
    if CARRY then
      MIN := MIN + NATURAL (T_MINUTE'LAST) + 1;
      -- Sub angles
      DEG := V1.DEGREES - V2.DEGREES - 1;
    else
      DEG := V1.DEGREES - V2.DEGREES;
    end if;
    if NEG then
      DEG := - DEG;
    end if;
    return REDUCTION (DEG, T_MINUTE(MIN));
  end "-";

  function ">" (A1, A2 : T_ANGLE) return BOOLEAN is
  begin
    if A1.DEGREES /= A2.DEGREES then
      return A1.DEGREES > A2.DEGREES;
    else
      return A1.MINUTES > A2.MINUTES;
    end if;
  end ">";

  function "<" (A1, A2 : T_ANGLE) return BOOLEAN is
  begin
    if A1.DEGREES /= A2.DEGREES then
      return A1.DEGREES < A2.DEGREES;
    else
      return A1.MINUTES < A2.MINUTES;
    end if;
  end "<";

  function "+" (A : T_ANGLE; D : T_DRIFT) return T_ANGLE is
  begin
    -- Add minutes
    if D.POSITIV then
      return A + T_ANGLE'(D.DEGREES, D.MINUTES);
    else
      return A - T_ANGLE'(D.DEGREES, D.MINUTES);
    end if;
  end "+";

  function "-" (A : T_ANGLE; D : T_DRIFT) return T_ANGLE is
  begin
    if D.POSITIV then
      return A - T_ANGLE'(D.DEGREES, D.MINUTES);
    else
      return A + T_ANGLE'(D.DEGREES, D.MINUTES);
    end if;
  end "-";

  function "-" (A1, A2 : T_ANGLE) return T_DRIFT is
    A3 : T_ANGLE;
  begin
    A3 := A1 - A2;
    if A3.DEGREES in T_DEG_DRIFT then
      return (POSITIV => TRUE,  DEGREES => A3.DEGREES, MINUTES => A3.MINUTES);
    else
      A3 := T_ANGLE'(DEGREES=>0, MINUTES=>0) - A3;
      return (POSITIV => FALSE, DEGREES => A3.DEGREES, MINUTES => A3.MINUTES);
    end if;
  end "-";

end NAV_TYPES;

