package CURVE is
  -- What is a point, a point data set
  subtype T_COORDINATE is FLOAT;
  type T_ONE_POINT is record
    X : T_COORDINATE;
    Y : T_COORDINATE;
  end record;
  type T_THE_POINTS is array (POSITIVE range <>) of T_ONE_POINT;

  -- use all screen, all screen same scale,
  -- boundaries set all screen, boundaries set same scale
  type T_SCALE is (CURVE_SCREEN, CURVE_NORMED, FREE_SCREEN, FREE_NORMED);

  type T_BOUNDARIES(SCALE : T_SCALE := CURVE_SCREEN) is record
    -- Always fix x range (X_BOUNDARIES allows to extract then from points)
    X_MIN : T_COORDINATE := 0.0;
    X_MAX : T_COORDINATE := 0.0;
    case SCALE is
      when CURVE_SCREEN | CURVE_NORMED =>
        -- y range computed from points and curve
        null;
      when FREE_SCREEN | FREE_NORMED =>
        -- y range fixed
        Y_MIN : T_COORDINATE := 0.0;
        Y_MAX : T_COORDINATE := 0.0;
    end case;
  end record;

  -- Finds lowest and greatest x according to the set of points
  procedure X_BOUNDARIES (
   POINTS       : in T_THE_POINTS;
   X_MIN, X_MAX : out T_COORDINATE);

  generic
    -- Y = F(x)
    with function F (X : FLOAT) return FLOAT;

  -- Give boundaries and points array : It will draw the curves
  procedure DRAW (BOUNDARIES : in T_BOUNDARIES; POINTS : in T_THE_POINTS);
end CURVE;
