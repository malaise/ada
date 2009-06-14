with Ada.Text_Io;
with My_Math, C_Nbres;

package body Great_Circle is

  Debug : constant Boolean := False;

  use type Conv.Rad_Range;

  subtype Real is My_Math.Real;

  -- For rounding
  Epsilon : constant := 1.0E-5;

  -- Nautical mile in kilometers
  Nautical_Mile : constant := 1.8520;
  -- Earth radius (in nautical miles)
  Earth_Radius : constant Lat_Lon.Distance := 6_370.0 / Nautical_Mile;

  -- Lenght of the Chord of an angle, and reverse
  function Chord_Of_Angle (Angle : Conv.Rad_Range; Radius : Lat_Lon.Distance)
                          return Lat_Lon.Distance is
  begin
    return 2.0 * Radius * Lat_Lon.Distance(My_Math.Sin(
                          Real(Angle / 2.0)));
  end Chord_Of_Angle;

  function Angle_Of_Chord (Chord : Lat_Lon.Distance; Radius : Lat_Lon.Distance)
                          return Conv.Rad_Coord_Range is
    Tmp : Real;
    use type Real;
  begin
    Tmp := Real(Chord / 2.0 / Radius);
    if Tmp > 1.0 and then Tmp - 1.0 < + Epsilon then
      return Conv.Pi;
    else
      return C_Nbres.Reduct(2.0 * Conv.Rad_Range(My_Math.Arc_Sin(Tmp)));
    end if;
  end Angle_Of_Chord;

  -- Compute heading and distance form point A to point B
  procedure Compute_Route (A, B : in Lat_Lon.Lat_Lon_Geo_Rec;
                           Heading  : out Conv.Geo_Coord_Rec;
                           Distance : out Lat_Lon.Distance) is

    -- The radius for computation (Earth + Altitude);
    Route_Radius : constant Lat_Lon.Distance := Earth_Radius;

    -- Lat lon of A and B in rad
    Lat_Lon_Rad_A, Lat_Lon_Rad_B : Lat_Lon.Lat_Lon_Rad_Rec;
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

  use My_Math;
  begin

    -- Convert args in lat_lon of A and B
    Lat_Lon_Rad_A := Lat_Lon.Geo2Rad(A);
    Lat_Lon_Rad_B := Lat_Lon.Geo2Rad(B);

    -- Compute delta of lat and log
    Lat_Lon_Rad_Delta :=
              (X => C_Nbres.Reduct(Lat_Lon_Rad_B.X - Lat_Lon_Rad_A.X),
               Y => C_Nbres.Reduct(Lat_Lon_Rad_B.Y - Lat_Lon_Rad_A.Y));

    -- Compute Chords A long, B long and lat
    Chord_Xa := Chord_Of_Angle (Lat_Lon_Rad_Delta.X,
        Route_Radius * Lat_Lon.Distance(My_Math.Cos(
                       Real(Lat_Lon_Rad_A.Y))));
    Chord_Xb := Chord_Of_Angle (Lat_Lon_Rad_Delta.X,
        Route_Radius * Lat_Lon.Distance(My_Math.Cos(
                       Real(Lat_Lon_Rad_B.Y))));
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
      if Debug then
        Ada.Text_Io.Put_Line ("Distance is 0");
      end if;
      return;
    elsif abs (Angle_Result - Conv.Pi) < Epsilon then
      -- From and To are opposite (antipodes)
      Heading := Conv.Zero;
      if Debug then
        Ada.Text_Io.Put_Line ("Antipodes");
      end if;
      return;
    end if;

    if Debug then
      Ada.Text_Io.Put_Line ("Delta Lat: " & Lat_Lon_Rad_Delta.Y'Img);
      Ada.Text_Io.Put_Line ("Delta Lon: " & Lat_Lon_Rad_Delta.X'Img);
      Ada.Text_Io.Put_Line ("Gamma : " & Angle_Result'Img);
    end if;

    -- Compute heading
    -- cos H = (cos colatB - cos colatA * cos Angle) / (sin colatA * sin Angle)
    --  cos colatX = sin latX
    Cos_H :=
           ( My_Math.Sin(My_Math.Real(Lat_Lon_Rad_B.Y))
             -   My_Math.Sin(My_Math.Real(Lat_Lon_Rad_A.Y))
               * My_Math.Cos(My_Math.Real(Angle_Result)))
            / My_Math.Cos(My_Math.Real(Lat_Lon_Rad_A.Y))
            / My_Math.Sin(My_Math.Real(Angle_Result)) ;
    if Debug then
      Ada.Text_Io.Put_Line ("Cos H : " & Cos_H'Img);
    end if;

    -- Round to 0 or 180 if cos between 1+Epsilon and -1-Epsilon
    if abs Cos_H > 1.0 and then abs Cos_H - 1.0 < Epsilon then
      if Cos_H > 1.0 then
        Heading_Rad_Angle := 0.0;
      else
        Heading_Rad_Angle := Conv.Pi;
      end if;
    else
      Heading_Rad_Angle := C_Nbres.Reduct(C_Nbres.Radian(
                              My_Math.Arc_Cos(Cos_H)));
      if Debug then
        Ada.Text_Io.Put_Line ("Raw H : " & Heading_Rad_Angle'Img);
      end if;
      -- Set heading < 0 if B is at west of A
      if Lat_Lon_Rad_Delta.X > Conv.Pi then
        Heading_Rad_Angle := C_Nbres.Reduct(-Heading_Rad_Angle);
        if Debug then
          Ada.Text_Io.Put_Line ("West correction");
        end if;
      end if;
    end if;
    if Debug then
      Ada.Text_Io.Put_Line ("H : " & Heading_Rad_Angle'Img);
    end if;

    -- In degrees
    Heading := Conv.Rad2Geo(Heading_Rad_Angle);

  end Compute_Route;

end Great_Circle;

