with Ada.Text_Io;
with Argument, Sys_Calls, My_Math, C_Nbres;
with Conv, Lat_Lon, String_Util;

procedure Great_Circle is

  use type Conv.Rad_Range;
  subtype Distance is Lat_Lon.Distance;

  -- For rounding
  Epsilon : constant := 1.0e-5;

  -- Earth radius
  Earth_Radius : constant Distance := 6_370.0; 

  -- The radius for computation (Earth + Altitude);
  Route_Radius : Distance := Earth_Radius;

  -- Lat lon of A and B in rad
  Lat_Lon_Rad_A, Lat_Lon_Rad_B : Lat_Lon.Lat_Lon_Rad_Rec;
  -- Delta lat and lon
  Lat_Lon_Rad_Delta : Lat_Lon.Lat_Lon_Rad_Rec;

  -- Chords on X (at A and B lat) and on Y
  Chord_Xa, Chord_Xb, Chord_Y : Distance;

  -- Chord of the result
  Chord_Result : Distance;
  -- Angle of result
  Angle_Result : Conv.Rad_Coord_Range;
  -- Arc of result
  Arc_Result : Distance;

  -- Projected chords of X and Y on plan tangent in A
  Proj_Chord_X, Proj_Chord_Y : Distance;

  -- Result angle from X
  Result_Rad_Angle : Conv.Rad_Coord_Range;
  Heading : Conv.Geo_Coord_Rec;

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
      & " add.mm.ss/oddd.mm.ss add.mm.ss/oddd.mm.ss");
    Ada.Text_Io.Put_Line (" where a is N or S and o is E or W.");
    Sys_Calls.Set_Error_Exit_Code;
  end Usage;

  -- Lenght of the Chord of an angle, and reverse
  function Chord_Of_Angle (Angle : Conv.Rad_Range; Radius : Distance)
                          return Distance is
  begin
    return 2.0 * Radius * Distance(My_Math.Sin(My_Math.Real(Angle / 2.0)));
  end Chord_Of_Angle;

  function Angle_Of_Chord (Chord : Distance; Radius : Distance)
                          return Conv.Rad_Coord_Range is
    Tmp : My_Math.Real;
    use type My_Math.Real;
  begin
    Tmp := My_Math.Real(Chord / 2.0 / Radius);
    if Tmp > 1.0 and then Tmp - 1.0 < + Epsilon then
      return Conv.Pi;
    else
      return C_Nbres.Reduct(2.0 * Conv.Rad_Range(My_Math.Arc_Sin(Tmp)));
    end if;
  end Angle_Of_Chord;

  -- Add Pi to result angle if shortest route along meridian is via south pole
  procedure Fix_Angle is
    Signed_A_Y : Conv.Rad_Range := Lat_Lon_Rad_A.Y;
    Signed_B_Y : Conv.Rad_Range := Lat_Lon_Rad_B.Y;
  begin
    -- Set Signed_*_Y < 0 if > Pi
    if Signed_A_Y > Conv.Pi then
      Signed_A_Y := Signed_A_Y - 2.0 * Conv.Pi;
    end if;
    if Signed_B_Y > Conv.Pi then
      Signed_B_Y := Signed_B_Y - 2.0 * Conv.Pi;
    end if;

    -- See if A and B are on the same meridian
    if abs (Lat_Lon_Rad_B.X - Lat_Lon_Rad_A.X) <= Conv.Pi/2.0 then
      -- Yes, add Pi if B is below A on meridian
      if Signed_B_Y < Signed_A_Y then
        Result_Rad_Angle := Result_Rad_Angle + Conv.Pi;
      end if;
    else
      -- A and B are on opposed meridians
      -- Add Pi if B is below -A
      if Signed_B_Y < -Signed_A_Y then
        Result_Rad_Angle := Result_Rad_Angle + Conv.Pi;
      end if;
    end if;
  end Fix_Angle;

begin

  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  -- Convert args in lat_lon of A and B
  begin
    Lat_Lon_Rad_A := Lat_Lon.Geo2Rad(String_Util.Str2Geo(
                                 Argument.Get_Parameter(1)));
    Lat_Lon_Rad_B := Lat_Lon.Geo2Rad(String_Util.Str2Geo(
                                 Argument.Get_Parameter(2)));
  exception
    when others =>
      Usage;
      return;
  end;

  -- Compute delta of lat and log
  declare
    use type C_Nbres.Radian;
  begin
    Lat_Lon_Rad_Delta :=
            (X => C_Nbres.Reduct(Lat_Lon_Rad_B.X - Lat_Lon_Rad_A.X),
             Y => C_Nbres.Reduct(Lat_Lon_Rad_B.Y - Lat_Lon_Rad_A.Y));
  end;

  -- Compute Chords A long, B long and lat
  Chord_Xa := Chord_Of_Angle (Lat_Lon_Rad_Delta.X,
      Route_Radius * Distance(My_Math.Cos(My_Math.Real(Lat_Lon_Rad_A.Y))));
  Chord_Xb := Chord_Of_Angle (Lat_Lon_Rad_Delta.X,
      Route_Radius * Distance(My_Math.Cos(My_Math.Real(Lat_Lon_Rad_B.Y))));
  Chord_Y := Chord_Of_Angle (Lat_Lon_Rad_Delta.Y, Route_Radius);
  -- Compute Chord of result
  Chord_Result := Distance(My_Math.Sqrt(My_Math.Real(
                    Chord_Xa * Chord_Xb + Chord_Y * Chord_Y)));
  -- Compute Angle of result
  Angle_result :=  Angle_Of_Chord(Chord_Result, Route_Radius);
  -- Compute Arc of result
  Arc_Result := Route_Radius * abs Angle_Result;

  Ada.Text_Io.Put_Line ("Distance is " & String_Util.Dist2Str(Arc_Result));

  -- Project both Chords (Xa and Y) on plan tangent in A
  Proj_Chord_X := Chord_Xa * Distance(My_Math.Cos(My_Math.Real(
                                                 Lat_Lon_Rad_Delta.X / 2.0)));
  if abs Proj_Chord_X < Epsilon then
    Proj_Chord_X := 0.0;
  end if;
  Proj_Chord_Y := Chord_Y * Distance(My_Math.Cos(My_Math.Real(
                                                 Lat_Lon_Rad_Delta.Y / 2.0)));

  -- Angle (in tangent plan) from X axis to route
  begin
    Result_Rad_Angle := Conv.Rad_Coord_Range(C_Nbres.Reduct(C_Nbres.Radian(
        My_Math.Arc_Tg(My_Math.Real(Proj_Chord_Y / Proj_Chord_X)))));
    -- Fix arc_tan result if B is at left of A (arc_tan is on -Pi/2 +Pi/2)
    if Lat_Lon_Rad_Delta.X > Conv.Pi then
      Result_Rad_Angle := Conv.Rad_Coord_Range(C_Nbres.Reduct(C_Nbres.Radian(
        Result_Rad_Angle + Conv.Pi)));
    end if;
  exception
    when Constraint_Error =>
      -- X is too small, A and B are on the same or opposed meridians
      -- Angle is Pi/2 or 3Pi/2, depending on shortest route
      Result_Rad_Angle := Conv.Pi / 2.0;
      Fix_Angle;
  end;
  -- Ada.Text_Io.Put_Line ("Horiz angle: " & Result_Rad_Angle'Img);

  -- Angle (in tangent plan) from route to Y axis
  Result_Rad_Angle := Conv.Rad_Coord_Range(
           C_Nbres.Reduct(C_Nbres.Radian(Conv.Pi / 2.0 - Result_Rad_Angle)));
  -- Ada.Text_Io.Put_Line ("Vert angle: " & Result_Rad_Angle'Img);

  -- In degrees
  Heading := Conv.Rad2Geo(Result_Rad_Angle);

  Ada.Text_Io.Put_Line ("Route is " & String_Util.Angle2Str(Heading));

end Great_Circle;

