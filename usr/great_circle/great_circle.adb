with My_Math, Complexes;
package body Great_Circle is

  use type Conv.Rad_Range;

  subtype Real is My_Math.Real;
  use type Real;

  -- For rounding
  Epsilon : constant := 1.0E-5;

  -- Nautical mile in kilometers
  Nautical_Mile : constant := 1.8520;
  -- Earth radius (in nautical miles)
  Earth_Radius : constant Lat_Lon.Distance := 6_371.009 / Nautical_Mile;

  -- Init the logger with a name
  procedure Init_Logger is
  begin
    Logger.Init ("GreatCircle");
  end Init_Logger;

  -- Length of the Chord of an angle, and reverse
  function Chord_Of_Angle (Angle : Conv.Rad_Range; Radius : Lat_Lon.Distance)
                          return Lat_Lon.Distance is
    (2.0 * Radius * Lat_Lon.Distance(My_Math.Sin( Real(Angle / 2.0))));

  function Angle_Of_Chord (Chord : Lat_Lon.Distance; Radius : Lat_Lon.Distance)
                          return Conv.Rad_Coord_Range is
    Tmp : Real;
  begin
    Tmp := Real(Chord / 2.0 / Radius);
    if Tmp > 1.0 and then Tmp - 1.0 < + Epsilon then
      return Conv.Pi;
    else
      return Complexes.Reduct(2.0 * Conv.Rad_Range(My_Math.Arc_Sin(Tmp)));
    end if;
  end Angle_Of_Chord;

  -- Compute the great circle arc (heading and distance)
  --  from point A to point B
  procedure Compute_Route (A, B : in Lat_Lon.Lat_Lon_Rad_Rec;
                           Heading  : out Conv.Geo_Coord_Rec;
                           Distance : out Lat_Lon.Distance) is

    -- The radius for computation (Earth + Altitude);
    Route_Radius : constant Lat_Lon.Distance := Earth_Radius;

    -- Delta lat and lon
    Lat_Lon_Rad_Delta : Lat_Lon.Lat_Lon_Rad_Rec;

    -- Chords on X (at A and B lat) and on Y
    Chord_Xa, Chord_Xb, Chord_Y : Lat_Lon.Distance;

    -- Chord of the result
    Chord_Result : Lat_Lon.Distance;
    -- Angle of result
    Angle_Result : Conv.Rad_Coord_Range;

    -- Heading in radian
    Cos_H : My_Math.Real;
    Heading_Rad_Angle : Conv.Rad_Coord_Range;

  begin
    -- Compute delta of lat and log
    Lat_Lon_Rad_Delta :=
              (X => Complexes.Reduct(B.X - A.X),
               Y => Complexes.Reduct(B.Y - A.Y));

    -- Compute Chords A long, B long and lat
    Chord_Xa := Chord_Of_Angle (Lat_Lon_Rad_Delta.X,
        Route_Radius * Lat_Lon.Distance(My_Math.Cos(Real(A.Y))));
    Chord_Xb := Chord_Of_Angle (Lat_Lon_Rad_Delta.X,
        Route_Radius * Lat_Lon.Distance(My_Math.Cos(Real(B.Y))));
    Chord_Y := Chord_Of_Angle (Lat_Lon_Rad_Delta.Y, Route_Radius);
    -- Compute Chord of result
    Chord_Result := Lat_Lon.Distance(My_Math.Sqrt(Real(
                      Chord_Xa * Chord_Xb + Chord_Y * Chord_Y)));
    -- Compute Angle of result
    Angle_Result :=  Angle_Of_Chord(Chord_Result, Route_Radius);

    -- Compute Arc of result
    Distance := Route_Radius * Angle_Result;

    -- Handle specific cases
    if Distance < Epsilon then
      Heading := Conv.Zero;
      Logger.Log_Debug ("Distance is 0");
      return;
    elsif abs (Angle_Result - Conv.Pi) < Epsilon then
      -- From and To are opposite (antipodes)
      Heading := Conv.Zero;
      Logger.Log_Debug ("Antipodes");
      return;
    end if;

    Logger.Log_Debug ("Delta Lat: " & Lat_Lon_Rad_Delta.Y'Img);
    Logger.Log_Debug ("Delta Lon: " & Lat_Lon_Rad_Delta.X'Img);
    Logger.Log_Debug ("Gamma: " & Angle_Result'Img);

    -- Compute heading
    -- cos H = (cos colatB - cos colatA * cos Angle) / (sin colatA * sin Angle)
    --  cos colatX = sin latX
    Cos_H :=
           (My_Math.Sin(Real(B.Y))
             -   My_Math.Sin(Real(A.Y)) * My_Math.Cos(Real(Angle_Result)))
            / My_Math.Cos(Real(A.Y))
            / My_Math.Sin(Real(Angle_Result)) ;
    Logger.Log_Debug ("Cos H : " & Cos_H'Img);

    -- Round to 0 or 180 if cos between 1+Epsilon and -1-Epsilon
    if abs Cos_H > 1.0 and then abs Cos_H - 1.0 < Epsilon then
      if Cos_H > 1.0 then
        Heading_Rad_Angle := 0.0;
      else
        Heading_Rad_Angle := Conv.Pi;
      end if;
    else
      Heading_Rad_Angle := Complexes.Reduct(Complexes.Radian(
                              My_Math.Arc_Cos(Cos_H)));
      Logger.Log_Debug ("Raw H : " & Heading_Rad_Angle'Img);
      -- Set heading < 0 if B is at west of A
      if Lat_Lon_Rad_Delta.X > Conv.Pi then
        Heading_Rad_Angle := Complexes.Reduct(-Heading_Rad_Angle);
        Logger.Log_Debug ("West correction");
      end if;
    end if;
    Logger.Log_Debug ("H : " & Heading_Rad_Angle'Img);

    -- In degrees
    Heading := Conv.Rad2Geo(Heading_Rad_Angle);

  end Compute_Route;

  -- Apply a great circle arc (heading and distance) to point A
  --  and return destination point B
  function Apply_Route (A : Lat_Lon.Lat_Lon_Rad_Rec;
                        Heading  : Conv.Geo_Coord_Rec;
                        Distance : Lat_Lon.Distance)
           return Lat_Lon.Lat_Lon_Rad_Rec is

    -- Reduct radian
    function Reduct (X : Conv.Rad_Range) return Conv.Rad_Coord_Range is
    begin
      return Complexes.Reduct (X);
    end Reduct;

    -- The radius for computation (Earth + Altitude);
    Route_Radius : constant Lat_Lon.Distance := Earth_Radius;
    -- A colatitude in radian
    A_Coy : constant Conv.Rad_Coord_Range := Reduct (Conv.Pi / 2.0 - A.Y);
    -- Arc of the route
    Arc : constant Conv.Rad_Coord_Range:= Distance / Route_Radius;
    -- Heading in radian
    Head : constant Conv.Rad_Coord_Range := Conv.Geo2Rad(Heading);
    -- Cos of B colatitude:
    -- cos colatB = cos colatA * cos Arc
    --            + sin colatA * sin Arc * cos H
    Cos_B_Coy : constant Real
              := My_Math.Cos(Real(A_Coy)) * My_Math.Cos(Real(Arc))
             +   My_Math.Sin(Real(A_Coy)) * My_Math.Sin(Real(Arc))
               * My_Math.Cos(Real(Head));
    -- B colatitude in radian
    B_Coy : constant Conv.Rad_Coord_Range
          := Conv.Rad_Coord_Range (My_Math.Arc_Cos(Cos_B_Coy));
    -- Delta longitude
    Cos_Delta_X, Delta_X : Real ;


  begin
    Logger.Log_Debug ("A Lon: " & A.X'Img);
    Logger.Log_Debug ("A coLat: " & A_Coy'Img);
    Logger.Log_Debug ("Heading: " & Head'Img);
    Logger.Log_Debug ("Gamma: " & Arc'Img);
    Logger.Log_Debug ("B coLat: " & B_Coy'Img);
    -- cos Delta_X sin B_Coy
    --   = cos Arc * sin A_Coy - sin Arc * cos A_Coy * cos H
    Cos_Delta_X := (My_Math.Cos(Real(Arc)) * My_Math.Sin(Real(A_Coy))
                    -   My_Math.Sin(Real(Arc)) * My_Math.Cos(Real(A_Coy))
                        * My_Math.Cos(Real(Head)) ) / My_Math.Sin(Real(B_Coy));
    if abs Cos_Delta_X > 1.0 and then abs Cos_Delta_X - 1.0 < Epsilon then
      if Cos_Delta_X > 1.0 then
        Delta_X := 0.0;
      else
        Delta_X := Real (Conv.Pi);
      end if;
    else
      Delta_X := My_Math.Arc_Cos(Cos_Delta_X);
    end if;
    Logger.Log_Debug ("Raw Delta Lon: " & Delta_X'Img);
    if Head > Conv.Pi then
      Delta_X := -Delta_X;
      Logger.Log_Debug ("West correction");
    end if;
    Logger.Log_Debug ("Delta Lon: " & Delta_X'Img);

    return B : Lat_Lon.Lat_Lon_Rad_Rec do
      B.X := Reduct (A.X + Reduct (Conv.Rad_Range (Delta_X)));
      B.Y := Reduct (Conv.Pi / 2.0 - B_Coy);
    end return;
  end Apply_Route;

end Great_Circle;

