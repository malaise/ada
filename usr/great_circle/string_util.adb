with Normal, My_Math, Upper_Char;
package body String_Util is

  function Str2Geo (Str : Geo_Str) return Lat_Lon.Lat_Lon_Geo_Rec is
    Geo : Lat_Lon.Lat_Lon_Geo_Rec;
  begin
    -- Check / and .
    if Str(4) /= '.' or else Str(7) /= '.' or else Str(12) /= '/'
    or else Str(17) /= '.' or else Str(20) /= '.' then
      raise Format_Error;
    end if;
    -- Lat N or S
    if Upper_Char(Str(1)) = 'N' then
      Geo.Lat.North := True;
    elsif Upper_Char(Str(1)) = 'S' then
      Geo.Lat.North := False;
    else
      raise Format_Error;
    end if;
    -- Lon W or E
    if Upper_Char(Str(13)) = 'E' then
      Geo.Lon.East := True;
    elsif Upper_Char(Str(13)) = 'W' then
      Geo.Lon.East := False;
    else
      raise Format_Error;
    end if;

    -- Lat
    Geo.Lat.Coord.Deg := Conv.Deg_Range'Value(Str( 2 .. 3));
    Geo.Lat.Coord.Min := Conv.Min_Range'Value(Str( 5 .. 6));
    Geo.Lat.Coord.Sec := Conv.Sec_Range'Value(Str( 8 .. 9));
    Geo.Lat.Coord.Ten := Conv.Sec_Range'Value(Str(10 .. 11)) * 100;
    -- Lon
    Geo.Lon.Coord.Deg := Conv.Deg_Range'Value(Str(14 .. 16));
    Geo.Lon.Coord.Min := Conv.Min_Range'Value(Str(18 .. 19));
    Geo.Lon.Coord.Sec := Conv.Sec_Range'Value(Str(21 .. 22));
    Geo.Lon.Coord.Ten := Conv.Sec_Range'Value(Str(23 .. 24)) * 100;

    -- Check the whole thing
    if not Lat_Lon.Is_Lat_Lon_Ok(Geo) then
      raise Format_Error;
    end if;

    -- Ok
    return Geo;
  exception
    when others =>
      raise Format_Error;
  end Str2Geo;

  -- Round hundredth and propagate
  procedure Round (C : in out Conv.Geo_Coord_Rec) is
    Sec, Min, Deg : Natural;
  begin
    Sec := C.Sec;
    Min := C.Min;
    Deg := C.Deg;
    if C.Ten >= (Conv.Ten_Range'Last + 1) / 2 then
      Sec := Sec + 1;
      if Sec > Conv.Sec_Range'Last then
        Sec := Conv.Sec_Range'First;
        Min := Min + 1;
        if Min > Conv.Min_Range'Last then
          Min := Conv.Min_Range'First;
          Deg := Deg + 1;
          if Deg > Conv.Deg_Range'Last then
            Deg :=  Conv.Deg_Range'First;
          end if;
        end if;
      end if;
    end if;
    C := (Deg, Min, Sec, 0);
  end Round;

  function Geo2Str (Geo : Lat_Lon.Lat_Lon_Geo_Rec) return Geo_Str is
    Rounded : Lat_Lon.Lat_Lon_Geo_Rec;
    Str : Geo_Str;

  begin
    -- Round hundredths
    Rounded := Geo;
    Round (Rounded.Lat.Coord);
    Round (Rounded.Lon.Coord);

    -- Set / and .
    Str(4) := '.'; Str(7) := '.';
    Str(10) := '/';
    Str(15) := '.'; Str(18) := '.';

    -- North or south lat, East or west lon
    if Rounded.Lat.North then
      Str(1) := 'N';
    else
      Str(1) := 'S';
    end if;
    if Rounded.Lon.East then
      Str(13) := 'E';
    else
      Str(13) := 'W';
    end if;

    -- Put the numbers
    Str( 2 ..  3) := Normal (Rounded.Lat.Coord.Deg, 2, Gap => '0');
    Str( 5 ..  6) := Normal (Rounded.Lat.Coord.Min, 2, Gap => '0');
    Str( 8 .. 11) := Normal (Rounded.Lat.Coord.Sec, 4, Gap => '0');
    Str(14 .. 16) := Normal (Rounded.Lon.Coord.Deg, 3, Gap => '0');
    Str(18 .. 19) := Normal (Rounded.Lon.Coord.Min, 2, Gap => '0');
    Str(21 .. 24) := Normal (Rounded.Lon.Coord.Sec, 4, Gap => '0');

    -- Done
    return Str;
  end Geo2Str;

  function Str2Dec (Str : Dec_Str) return Lat_Lon.Lat_Lon_Dec_Rec is
    Dec : Lat_Lon.Lat_Lon_Dec_Rec;
  begin
    -- Check / and .
    if Str(4) /= '.' or else Str(11) /= '/' or else Str(16) /= '.' then
      raise Format_Error;
    end if;
    -- Lat N or S
    if Upper_Char(Str(1)) = 'N' then
      Dec.Lat.North := True;
    elsif Upper_Char(Str(1)) = 'S' then
      Dec.Lat.North := False;
    else
      raise Format_Error;
    end if;
    -- Lon W or E
    if Upper_Char(Str(12)) = 'E' then
      Dec.Lon.East := True;
    elsif Upper_Char(Str(12)) = 'W' then
      Dec.Lon.East := False;
    else
      raise Format_Error;
    end if;

    -- Lat
    Dec.Lat.Coord.Deg := Conv.Deg_Range'Value(Str( 2 ..  3));
    Dec.Lat.Coord.Nan := Conv.Nan_Range'Value(Str( 5 .. 10)) * 1000;
    -- Lon
    Dec.Lon.Coord.Deg := Conv.Deg_Range'Value(Str(13 .. 15));
    Dec.Lon.Coord.Nan := Conv.Nan_Range'Value(Str(17 .. 22)) * 1000;

    -- Chek the whole thing
    if not Lat_Lon.Is_Lat_Lon_Ok(Dec) then
      raise Format_Error;
    end if;

    -- Ok
    return Dec;
  exception
    when others =>
      raise Format_Error;
  end Str2Dec;

  function Dec2Str (Dec : Lat_Lon.Lat_Lon_Dec_Rec) return Dec_Str is
    Str : Dec_Str;
  begin
    -- Set / and .
    Str(4) := '.';
    Str(11) := '/';
    Str(16) := '.';

    -- North or south lat, East or west lon
    if Dec.Lat.North then
      Str(1) := 'N';
    else
      Str(1) := 'S';
    end if;
    if Dec.Lon.East then
      Str(12) := 'E';
    else
      Str(12) := 'W';
    end if;

    -- Put the numbers
    Str( 2 ..  3) := Normal (Dec.Lat.Coord.Deg, 2, Gap => '0');
    Str( 5 .. 11) := Normal (Dec.Lat.Coord.Nan / 10000000, 6, Gap => '0');
    Str(13 .. 13) := Normal (Dec.Lon.Coord.Deg, 3, Gap => '0');
    Str(16 .. 21) := Normal (Dec.Lon.Coord.Nan / 10000000, 6, Gap => '0');

    -- Done
    return Str;
  end Dec2Str;

  function Dist2Str (Dist : Lat_Lon.Distance) return Dist_Str is
    Int : My_Math.Inte;
  begin
    Int := My_Math.Round(My_Math.Real(Dist));
    return Normal(Integer(Int), Dist_Str'Length);
  end Dist2Str;

  function Geoangle2Str (Geo_Angle : Conv.Geo_Coord_Rec) return Geo_Angle_Str is
    Rounded : Conv.Geo_Coord_Rec;
    Str : Geo_Angle_Str;
  begin
    -- Round hundredths
    Rounded := Geo_Angle;
    Round (Rounded);
    -- Set .
    Str(4) := '.'; Str(7) := '.';
    Str(1 ..  3) := Normal (Rounded.Deg, 3, Gap => '0');
    Str(5 ..  6) := Normal (Rounded.Min, 2, Gap => '0');
    Str(8 ..  9) := Normal (Rounded.Sec, 2, Gap => '0');
    return Str;
  end Geoangle2Str;

  function Decangle2Str (Dec_Angle : Conv.Dec_Coord_Rec) return Dec_Angle_Str is
    Str : Dec_Angle_Str;
    I, J : Natural;
    R : My_Math.Real;
    use type My_Math.Real;
  begin
    I := Dec_Angle.Deg;
    R := My_Math.Real (Dec_Angle.Nan) / 100000.0;
    J := Natural (My_Math.Round (R));
    -- Carry
    if J = 10000 then
      J := 0;
      if I = Conv.Deg_Range'Last then
        I := Conv.Deg_Range'First;
      else
        I := I + 1;
      end if;
    end if;
    -- Set .
    Str(4) := '.';
    Str(1 ..  3) := Normal (I, 3, Gap => '0');
    Str(5 ..  8) := Normal (J, 4, Gap => '0');
    return Str;
  end Decangle2Str;


end String_Util;

