with My_Math, Curve;
use My_Math;
procedure T_Curve is

  Points : constant Curve.T_The_Points (1 .. 9) :=
   (1 => (X => -4.0, Y =>  0.0),
    2 => (X => -3.0, Y =>  1.0),
    3 => (X => -2.0, Y =>  2.0),
    4 => (X => -1.0, Y =>  3.0),
    5 => (X =>  0.0, Y =>  4.0),
    6 => (X =>  1.0, Y =>  5.0),
    7 => (X =>  2.0, Y =>  6.0),
    8 => (X =>  3.0, Y =>  7.0),
    9 => (X =>  4.0, Y =>  8.0));

  Bounds : Curve.T_Boundaries;

  X_Min, X_Max : Curve.T_Coordinate;


  function F(X : My_Math.Real) return My_Math.Real is
  begin
    return 4.0 * X**3 + 3.0 * X**2 + 2.0 * X + 1.0;
  end F;

  procedure My_Draw is new Curve.Draw(F);

begin

  Curve.X_Boundaries(Points, X_Min, X_Max);
  Bounds := (Scale => Curve.Curve_Screen,
             X_Min => X_Min,
             X_Max => X_Max);
  My_Draw (Bounds, Points);
  Bounds := (Scale => Curve.Free_Normed,
             X_Min => -250.0,
             X_Max => +250.0,
             Y_Min => -250.0,
             Y_Max => +250.0);

  My_Draw (Bounds, Points);

end T_Curve;

