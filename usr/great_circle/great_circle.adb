with Ada.Text_Io;
with My_Math, C_Nbres;
with String_Util;

package body Great_Circle is

  Debug : constant Boolean := True;

  use type Conv.Rad_Range;

  subtype Real is My_Math.Real;

  -- For rounding
  Epsilon : constant := 1.0e-5;

  -- Earth radius
  Earth_Radius : constant Lat_Lon.Distance := 6_370.0; 

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

  -- Mathematical spheric coordinates and vector
  type Spheric_Rec is record
    T, P : Real;
  end record;

  function Rad2Spheric (Rad : Lat_Lon.Lat_Lon_Rad_Rec) return Spheric_Rec is
  begin
    return (T => Real(Rad.X), P => Real(C_Nbres.Reduct(Conv.Pi / 2.0 - Rad.Y)));
  end Rad2Spheric;

  type Vector_Rec is record
    A, B, C : Real;
  end record;

  function Vector_Of (A, B : Spheric_Rec) return Vector_Rec is
    V : Vector_Rec;
    Nu : Real;
    use My_Math;
  begin
    Nu := Sin(A.T) * Sin(B.T) * Cos(A.P - B.P) + Cos(A.T) * Cos(B.T);
    V.A := Nu * Sin(B.T) * Cos(B.P) - Sin(A.T) * Cos(A.P);
    V.B := Nu * Sin(B.T) * Sin(B.P) - Sin(A.T) * Sin(A.P);
    V.C := Nu * Cos(B.T) - Cos(A.T);
    return V;
  end Vector_Of;

  function Vector_Len (V : Vector_Rec) return Real is
    use type My_Math.Real;
  begin
    return My_Math.Sqrt (V.A * V.A + V.B * V.B + V.C * V.C);
  end Vector_Len;

  -- Compute heading and distance form point A to point B
  procedure Compute_Route (A, B : in Lat_Lon.Lat_Lon_Geo_Rec;
                           Heading  : out Conv.Geo_Coord_Rec;
                           Distance : out Lat_Lon.Distance) is

    -- The radius for computation (Earth + Altitude);
    Route_Radius : Lat_Lon.Distance := Earth_Radius;

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

    -- Result angle from X
    Result_Rad_Angle : Conv.Rad_Coord_Range;

    -- Add Pi to Result_Rad_Angle if B in south of A
    -- or shortest route along meridian is via south pole
    procedure Fix_Angle is
      Signed_A_Y : Conv.Rad_Range := Lat_Lon_Rad_A.Y;
      Signed_B_Y : Conv.Rad_Range := Lat_Lon_Rad_B.Y;
      Angle : C_Nbres.Radian;
    begin
      -- Set Signed_*_Y < 0 if > Pi
      if Signed_A_Y > Conv.Pi then
        Signed_A_Y := Signed_A_Y - 2.0 * Conv.Pi;
      end if;
      if Signed_B_Y > Conv.Pi then
        Signed_B_Y := Signed_B_Y - 2.0 * Conv.Pi;
      end if;

      -- If same meridian
      Angle :=  C_Nbres.Radian(Result_Rad_Angle);
      if Angle < Epsilon
      or else abs (Angle - Conv.Pi) < Epsilon then
        -- See if A and B are on the same meridian
        if abs Lat_Lon_Rad_Delta.X <= Conv.Pi / 2.0 then
          -- Yes, add Pi if B is below A on meridian
          if Signed_B_Y < Signed_A_Y then
            Angle := Angle + Conv.Pi;
          end if;
        else
          -- A and B are on opposed meridians
          -- Add Pi if B is below -A
          if Signed_B_Y < -Signed_A_Y then
            Angle := Angle + Conv.Pi;
          end if;
        end if;
      else
        -- Si if B is in the south of A
        if Signed_B_Y < Signed_A_Y then
          Angle := Angle + Conv.Pi;
        end if;
      end if;

      Result_Rad_Angle := Conv.Rad_Coord_Range(C_Nbres.Reduct(Angle));
    end Fix_Angle;

  begin

    -- Convert args in lat_lon of A and B
    Lat_Lon_Rad_A := Lat_Lon.Geo2Rad(A);
    Lat_Lon_Rad_B := Lat_Lon.Geo2Rad(B);

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
    Angle_result :=  Angle_Of_Chord(Chord_Result, Route_Radius);
    -- Compute Arc of result
    Distance := Route_Radius * abs Angle_Result;

    if Debug then
      Ada.Text_Io.Put_Line ("Xa = " & String_Util.Dist2Str(Chord_Xa));
      Ada.Text_Io.Put_Line ("Xb = " & String_Util.Dist2Str(Chord_Xb));
      Ada.Text_Io.Put_Line ("Y  = " & String_Util.Dist2Str(Chord_Y));
    end if;


    -- Compute heading
    if Debug then
      -- Get angle between meridian chord and route chord (in secant plan)
      declare
        Dist : Lat_Lon.Distance;
      begin
        Dist := Distance * Distance + Chord_Y * Chord_Y - Chord_Xb * Chord_Xb;
        Dist := Dist / 2.0 / Distance / Chord_Y;
        Result_Rad_Angle := Conv.Rad_Coord_Range(C_Nbres.Reduct(C_Nbres.Radian(
            My_Math.Arc_Cos(Real(Dist)))));
        Fix_Angle;
        Ada.Text_Io.Put_Line ("Nu = " & String_Util.Angle2Str(
            Conv.Rad2Geo(Result_Rad_Angle)));
      end;

      -- Project chords of lat and long in tangent plan
      declare
        P_Chord_Xa, P_Chord_Y : Real;
        use type Real;
      begin
        P_Chord_Xa := Real(Chord_Xa) * My_Math.Cos(
            Real(Lat_Lon_Rad_Delta.X / 2.0));
        P_Chord_Y  :=  Real(Chord_Y) * My_Math.Cos(
            Real(Lat_Lon_Rad_Delta.Y / 2.0));
        begin
          Result_Rad_Angle := Conv.Rad_Coord_Range(C_Nbres.Reduct(C_Nbres.Radian(
               My_Math.Arc_Tg(P_Chord_Xa / P_Chord_Y))));
        exception
          when others =>
            -- Arc tangent infinite: A and B on same parallel
            if Lat_Lon_Rad_Delta.X >= 0.0 then
              -- B is east of A
              Result_Rad_Angle := Conv.Pi / 2.0;
            else
              -- B is west of A
              Result_Rad_Angle := 3.0 * Conv.Pi / 2.0;
            end if;
        end;
       
        -- Fix result if B is at south of A
        -- or if A and B are on the same meridian
        Fix_Angle;
        Ada.Text_Io.Put_Line ("Pr = " & String_Util.Angle2Str(
            Conv.Rad2Geo(Result_Rad_Angle)));
      end;
    end if;

    -- Angle in tangent plan
    declare
      -- Tangents in A to A meridien and to AB
      Tang_A_North, Tang_A_B : Vector_Rec;
      Cos_Angle : Real;
      use type My_Math.Real;
    begin
      Tang_A_North := Vector_Of(Rad2Spheric(Lat_Lon_Rad_A), (P => 0.0, T => 0.0));
      Tang_A_B := Vector_Of(Rad2Spheric(Lat_Lon_Rad_A), Rad2Spheric(Lat_Lon_Rad_A));
      -- Vectorial product
      Cos_Angle := Tang_A_North.A * Tang_A_B.A
                 + Tang_A_North.B * Tang_A_B.B
                 + Tang_A_North.C * Tang_A_B.C;
      Cos_Angle := Cos_Angle / Vector_Len(Tang_A_North) /Vector_Len(Tang_A_B);
      Result_Rad_Angle := Conv.Rad_Coord_Range(C_Nbres.Reduct(C_Nbres.Radian(
               My_Math.Arc_Cos(Cos_Angle))));
    end;
       
    -- Fix result if B is at south of A
    -- or if A and B are on the same meridian
    Fix_Angle;

    -- In degrees
    Heading := Conv.Rad2Geo(Result_Rad_Angle);

  end Compute_Route;

end Great_Circle;

