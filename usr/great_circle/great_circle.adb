with Ada.Text_Io;
with Argument, Sys_Calls, My_Math, C_Nbres;
with Conv, Lat_Lon, String_Util;

procedure Great_Circle is

  use type Conv.Rad_Range;
  subtype Distance is Lat_Lon.Distance;

  -- Earth radius
  R_Earth : constant Distance := 6_370.0; 

  -- The radius for computation (Earth + Altitude);
  Radius : Distance := R_Earth;

  -- Lat lon of A and B in rad
  Lat_Lon_Rad_A, Lat_Lon_Rad_B : Lat_Lon.Lat_Lon_Rad_Rec;
  -- Delta lat and lon
  Lat_Lon_Rad_Delta : Lat_Lon.Lat_Lon_Rad_Rec;

  -- Chords on X and Y
  Chord_X, Chord_Y : Distance;

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
  function Chord_Of_Angle (Angle : Conv.Rad_Range) return Distance is
  begin
    return 2.0 * Radius * Distance(My_Math.Sin(My_Math.Real(Angle / 2.0)));
  end Chord_Of_Angle;

  function Angle_Of_Chord (Chord : Distance) return Conv.Rad_Coord_Range is
  begin
    return C_Nbres.Reduct(2.0 * Conv.Rad_Range(
             My_Math.Arc_Sin(My_Math.Real(Chord / 2.0 / Radius))));
  end Angle_Of_Chord;

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
    use C_Nbres;
  begin
    Lat_Lon_Rad_Delta := (X => Lat_Lon_Rad_B.X - Lat_Lon_Rad_A.X,
                          Y => Lat_Lon_Rad_B.Y - Lat_Lon_Rad_A.Y);
  end;

  -- Compute Chords on lat and long
  Chord_X := Chord_Of_Angle (Lat_Lon_Rad_Delta.X);
  Chord_Y := Chord_Of_Angle (Lat_Lon_Rad_Delta.Y);
  -- Compute Chord of result
  Chord_Result := Distance(My_Math.Sqrt(My_Math.Real(
                    Chord_X * Chord_X + Chord_Y * Chord_Y)));
  -- Compute Angle of result
  Angle_result :=  Angle_Of_Chord(Chord_Result);
  -- Compute Arc of result
  Arc_Result := Radius * abs Angle_Result;

  Ada.Text_Io.Put_Line ("Distance is " & String_Util.Dist2Str(Arc_Result));

  -- Project both Chords on tangent plan
  Proj_Chord_X := Chord_X * Distance(My_Math.Cos(My_Math.Real(
                                                 Lat_Lon_Rad_Delta.X / 2.0)));
  Proj_Chord_Y := Chord_Y * Distance(My_Math.Cos(My_Math.Real(
                                                 Lat_Lon_Rad_Delta.Y / 2.0)));

  -- Angle (in tangent plan) from X axis to route
  begin
    Result_Rad_Angle := Conv.Rad_Coord_Range(C_Nbres.Reduct(C_Nbres.Radian(
        My_Math.Arc_Tg(My_Math.Real(Proj_Chord_Y / Proj_Chord_X)))));
    if Lat_Lon_Rad_Delta.X > Conv.Pi then
      Result_Rad_Angle := Conv.Rad_Coord_Range(C_Nbres.Reduct(C_Nbres.Radian(
        Result_Rad_Angle + Conv.Pi)));
    end if;
  exception
    when Constraint_Error =>
      -- X is too small. Angle is Pi/2 or Pi + Pi/2
      Result_Rad_Angle := Conv.Pi / 2.0;
      if Lat_Lon_Rad_Delta.Y > Conv.Pi then
        Result_Rad_Angle := Result_Rad_Angle + Conv.Pi;
      end if;
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

