with MY_MATH, CURVE;
use MY_MATH;
procedure T_CURVE is

  POINTS : constant CURVE.T_THE_POINTS (1 .. 9) :=
   (1 => (X => -4.0, Y =>  0.0),
    2 => (X => -3.0, Y =>  1.0),
    3 => (X => -2.0, Y =>  2.0),
    4 => (X => -1.0, Y =>  3.0),
    5 => (X =>  0.0, Y =>  4.0),
    6 => (X =>  1.0, Y =>  5.0),
    7 => (X =>  2.0, Y =>  6.0),
    8 => (X =>  3.0, Y =>  7.0),
    9 => (X =>  4.0, Y =>  8.0));

  BOUNDS : CURVE.T_BOUNDARIES;

  X_MIN, X_MAX : CURVE.T_COORDINATE;


  function F(X : MY_MATH.REAL) return MY_MATH.REAL is
  begin
    return 4.0 * X**3 + 3.0 * X**2 + 2.0 * X + 1.0;
  end F;

  procedure MY_DRAW is new CURVE.DRAW(F);

begin

  CURVE.X_BOUNDARIES(POINTS, X_MIN, X_MAX);
  BOUNDS := (SCALE => CURVE.CURVE_SCREEN,
             X_MIN => X_MIN,
             X_MAX => X_MAX);
  MY_DRAW (BOUNDS, POINTS);
  BOUNDS := (SCALE => CURVE.FREE_NORMED,
             X_MIN => -250.0,
             X_MAX => +250.0,
             Y_MIN => -250.0,
             Y_MAX => +250.0);

  MY_DRAW (BOUNDS, POINTS);

end T_CURVE;

