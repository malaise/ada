with My_Math, Null_Procedure;
package Curve is
  -- What is a point, a point data set
  subtype T_Coordinate is My_Math.Real;
  type T_One_Point is record
    X : T_Coordinate;
    Y : T_Coordinate;
  end record;
  type T_The_Points is array (Positive range <>) of T_One_Point;

  -- use all screen, all screen same scale,
  -- boundaries set all screen, boundaries set same scale
  type T_Scale is (Curve_Screen, Curve_Normed, Free_Screen, Free_Normed);

  type T_Boundaries(Scale : T_Scale := Curve_Screen) is record
    -- Always fix x range (X_BOUNDARIES allows to extract then from points)
    X_Min : T_Coordinate := 0.0;
    X_Max : T_Coordinate := 0.0;
    case Scale is
      when Curve_Screen | Curve_Normed =>
        -- y range computed from points and curve
        null;
      when Free_Screen | Free_Normed =>
        -- y range fixed
        Y_Min : T_Coordinate := 0.0;
        Y_Max : T_Coordinate := 0.0;
    end case;
  end record;

  -- Finds lowest and greatest x according to the set of points
  procedure X_Boundaries (
   Points       : in T_The_Points;
   X_Min, X_Max : out T_Coordinate);

  -- Can be (not mandatory) called before a DRAW to make initialisation
  -- of window and test the result
  function Init return Boolean;

  -- Should be called after INIT if DRAW cannot be or is not called
  procedure Destroy;

  generic
    -- Y = F(x)
    with function F (X : My_Math.Real) return My_Math.Real;

    -- Any action to do when a fd_event has been received (see Afpx Fd_event)
    with procedure Fd_Callback is Null_Procedure;

  -- Give boundaries and points array : It will draw the curves
  procedure Draw (Boundaries : in T_Boundaries; Points : in T_The_Points);
end Curve;

