-- Draw a curve (in a Con_io console)
with My_Math;
package Curve is
  -- What is a point, a vector of points
  subtype T_Coordinate is My_Math.Real;
  type T_One_Point is record
    X : T_Coordinate;
    Y : T_Coordinate;
  end record;
  type T_The_Points is array (Positive range <>) of T_One_Point;

  -- Use all the screen, all the screen with same scale,
  --  boundaries set and use all screen, or boundaries set with same scale
  type T_Scale is (Curve_Screen, Curve_Normed, Free_Screen, Free_Normed);

  type T_Boundaries(Scale : T_Scale := Curve_Screen) is record
    -- Always fix x range (function X_Boundaries extracts then from points)
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

  -- Can be (not mandatory) called before a Draw in order to make initialisation
  -- of window and test the result
  function Init return Boolean;

  -- Should be called after Init if Draw cannot be or is not called
  procedure Destroy;

  generic
    -- y = F(x)
    with function F (X : My_Math.Real) return My_Math.Real;

    -- Any action to do when a fd_event is received (see Con_Io.Fd_Event)
    with procedure Fd_Callback is null;
    -- Any action to do on timer event is received (see Con_Io.Timer_Event)
    with procedure Timer_Callback is null;
    -- Any action to do on signal_event is received (see Con_Io.Signal_Event)
    with procedure Signal_Callback is null;

  -- Draw the curves, given boundaries and points array
  procedure Draw (Boundaries : in T_Boundaries; Points : in T_The_Points);
end Curve;

