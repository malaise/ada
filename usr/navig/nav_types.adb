-- Operations on the general types for aeronautics navigation
package body Nav_Types is

  -- For an angle, reduce its value in a circle (0 .. 359)
  function Reduction (Deg : T_Common_Deg; Min : T_Minute) return T_Angle is
    Loc_Deg : T_Common_Deg;
    Loc_Min : Natural;
  begin
    Loc_Deg := Deg;
    Loc_Min := Natural(Min);
    -- Substract if necessary
    while Loc_Deg > T_Degree'Last loop
      Loc_Deg := Loc_Deg - (T_Degree'Last + 1);
    end loop;
    -- Add if necessary
    if Loc_Deg < 0 then
      if Loc_Min /= 0 then
        Loc_Min := Natural(T_Minute'Last) + 1 - Loc_Min;
        Loc_Deg := Loc_Deg - 1;
      end if;
      while Loc_Deg < 0 loop
        Loc_Deg := Loc_Deg + (T_Degree'Last + 1);
      end loop;
    end if;

    return T_Angle'(Degrees => T_Degree(Loc_Deg),
     Minutes => T_Minute(Loc_Min));
  end Reduction;

  -- Add two angles to get an angle
  function "+" (A1, A2 : T_Angle) return T_Angle is
    Deg : T_Common_Deg;
    Min : Natural;
  begin
    Deg := 0;
    -- Add minutes
    Min := Natural (A1.Minutes) + Natural (A2.Minutes);
    if Min > Natural (T_Minute'Last) then
      Min := Min - Natural (T_Minute'Last) - 1;
      Deg := 1;
    end if;
    -- Add angles
    Deg := Deg + A1.Degrees + A2.Degrees;
    return Reduction (Deg, T_Minute(Min));
  end "+";

  -- Add two angles to get an angle
  function "+" (A1 : T_Angle; A2 : T_Degree) return T_Angle is
  begin
    return "+" (A1 => A1, A2 => (Degrees => A2, Minutes => 0));
  end "+";

  -- Sub 2 angles to get an angle
  function "-" (A1, A2 : T_Angle) return T_Angle is
    Deg : T_Common_Deg;
    Min : Integer;
    V1, V2 : T_Angle;
    Neg : Boolean;
    Carry : Boolean;
  begin
    Neg := A1 < A2;
    if not Neg then
      V1 := A1; V2 := A2;
    else
      V1 := A2; V2 := A1;
    end if;
    -- Sub minutes
    Min := Integer (V1.Minutes) - Natural (V2.Minutes);
    Carry := Min < 0;
    if Carry then
      Min := Min + Natural (T_Minute'Last) + 1;
      -- Sub angles
      Deg := V1.Degrees - V2.Degrees - 1;
    else
      Deg := V1.Degrees - V2.Degrees;
    end if;
    if Neg then
      Deg := - Deg;
    end if;
    return Reduction (Deg, T_Minute(Min));
  end "-";

  function ">" (A1, A2 : T_Angle) return Boolean is
  begin
    if A1.Degrees /= A2.Degrees then
      return A1.Degrees > A2.Degrees;
    else
      return A1.Minutes > A2.Minutes;
    end if;
  end ">";

  function "<" (A1, A2 : T_Angle) return Boolean is
  begin
    if A1.Degrees /= A2.Degrees then
      return A1.Degrees < A2.Degrees;
    else
      return A1.Minutes < A2.Minutes;
    end if;
  end "<";

  function "+" (A : T_Angle; D : T_Drift) return T_Angle is
  begin
    -- Add minutes
    if D.Positiv then
      return A + T_Angle'(D.Degrees, D.Minutes);
    else
      return A - T_Angle'(D.Degrees, D.Minutes);
    end if;
  end "+";

  function "-" (A : T_Angle; D : T_Drift) return T_Angle is
  begin
    if D.Positiv then
      return A - T_Angle'(D.Degrees, D.Minutes);
    else
      return A + T_Angle'(D.Degrees, D.Minutes);
    end if;
  end "-";

  function "-" (A1, A2 : T_Angle) return T_Drift is
    A3 : T_Angle;
  begin
    A3 := A1 - A2;
    if A3.Degrees in T_Deg_Drift then
      return (Positiv => True,  Degrees => A3.Degrees, Minutes => A3.Minutes);
    else
      A3 := T_Angle'(Degrees=>0, Minutes=>0) - A3;
      return (Positiv => False, Degrees => A3.Degrees, Minutes => A3.Minutes);
    end if;
  end "-";

end Nav_Types;

