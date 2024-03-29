with Normal, Upper_Char;
package body String_Util is

  -- Round Mil millis to seconds and propagate
  -- or Round Mil millis to hundredth and propagate
  procedure Round (C : in out Units.Geo_Coord_Rec;
                   To_Seconds : in Boolean) is
    Sec, Min, Deg : Natural;
    Factor : My_Math.Real;
    Max : Units.Ten_Range;
    use type My_Math.Real;
  begin
    Sec := C.Sec;
    Min := C.Min;
    Deg := C.Deg;
    if To_Seconds then
      Factor := 10000.0;
      Max := 1;
    else
      Factor := 100.0;
      Max := 100;
    end if;
    C.Ten := Units.Ten_Range (My_Math.Round (My_Math.Real (C.Ten) / Factor));
    if C.Ten = Max then
      C.Ten := 0;
      Sec := Sec + 1;
      if Sec > Units.Sec_Range'Last then
        Sec := Units.Sec_Range'First;
        Min := Min + 1;
        if Min > Units.Min_Range'Last then
          Min := Units.Min_Range'First;
          Deg := Deg + 1;
          if Deg > Units.Deg_Range'Last then
            Deg :=  Units.Deg_Range'First;
          end if;
        end if;
      end if;
    end if;
    C := (Deg, Min, Sec, C.Ten * Natural (Factor));
  end Round;

  -- Str <-> Geo
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
    Geo.Lat.Coord.Deg := Units.Deg_Range'Value(Str( 2 .. 3));
    Geo.Lat.Coord.Min := Units.Min_Range'Value(Str( 5 .. 6));
    Geo.Lat.Coord.Sec := Units.Sec_Range'Value(Str( 8 .. 9));
    Geo.Lat.Coord.Ten := Units.Ten_Range'Value(Str(10 .. 11)) * 100;
    -- Lon
    Geo.Lon.Coord.Deg := Units.Deg_Range'Value(Str(14 .. 16));
    Geo.Lon.Coord.Min := Units.Min_Range'Value(Str(18 .. 19));
    Geo.Lon.Coord.Sec := Units.Sec_Range'Value(Str(21 .. 22));
    Geo.Lon.Coord.Ten := Units.Ten_Range'Value(Str(23 .. 24)) * 100;

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

  function Geo2Str (Geo : Lat_Lon.Lat_Lon_Geo_Rec) return Geo_Str is
    Rounded : Lat_Lon.Lat_Lon_Geo_Rec;
    Str : Geo_Str;

  begin
    -- Round hundredths
    Rounded := Geo;
    Round (Rounded.Lat.Coord, False);
    Round (Rounded.Lon.Coord, False);

    -- Set / and .
    Str(4) := '.'; Str(7) := '.';
    Str(12) := '/';
    Str(17) := '.'; Str(20) := '.';

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
    Str( 8 ..  9) := Normal (Rounded.Lat.Coord.Sec, 2, Gap => '0');
    Str(10 .. 11) := Normal (Rounded.Lat.Coord.Ten / 100, 2, Gap => '0');
    Str(14 .. 16) := Normal (Rounded.Lon.Coord.Deg, 3, Gap => '0');
    Str(18 .. 19) := Normal (Rounded.Lon.Coord.Min, 2, Gap => '0');
    Str(21 .. 22) := Normal (Rounded.Lon.Coord.Sec, 2, Gap => '0');
    Str(23 .. 24) := Normal (Rounded.Lon.Coord.Ten / 100, 2, Gap => '0');

    -- Done
    return Str;
  end Geo2Str;

  -- Lstr <-> Geo
  function Lstr2Geo (Lstr : Geo_Lstr) return Lat_Lon.Lat_Lon_Geo_Rec is
    Geo : Lat_Lon.Lat_Lon_Geo_Rec;
  begin
    -- Check / and .
    if Lstr(4) /= '.' or else Lstr(7) /= '.' or else Lstr(14) /= '/'
    or else Lstr(19) /= '.' or else Lstr(22) /= '.' then
      raise Format_Error;
    end if;
    -- Lat N or S
    if Upper_Char(Lstr(1)) = 'N' then
      Geo.Lat.North := True;
    elsif Upper_Char(Lstr(1)) = 'S' then
      Geo.Lat.North := False;
    else
      raise Format_Error;
    end if;
    -- Lon W or E
    if Upper_Char(Lstr(15)) = 'E' then
      Geo.Lon.East := True;
    elsif Upper_Char(Lstr(15)) = 'W' then
      Geo.Lon.East := False;
    else
      raise Format_Error;
    end if;

    -- Lat
    Geo.Lat.Coord.Deg := Units.Deg_Range'Value(Lstr( 2 .. 3));
    Geo.Lat.Coord.Min := Units.Min_Range'Value(Lstr( 5 .. 6));
    Geo.Lat.Coord.Sec := Units.Sec_Range'Value(Lstr( 8 .. 9));
    Geo.Lat.Coord.Ten := Units.Ten_Range'Value(Lstr(10 .. 13));
    -- Lon
    Geo.Lon.Coord.Deg := Units.Deg_Range'Value(Lstr(16 .. 18));
    Geo.Lon.Coord.Min := Units.Min_Range'Value(Lstr(20 .. 21));
    Geo.Lon.Coord.Sec := Units.Sec_Range'Value(Lstr(23 .. 24));
    Geo.Lon.Coord.Ten := Units.Ten_Range'Value(Lstr(25 .. 28));

    -- Check the whole thing
    if not Lat_Lon.Is_Lat_Lon_Ok(Geo) then
      raise Format_Error;
    end if;

    -- Ok
    return Geo;
  exception
    when others =>
      raise Format_Error;
  end Lstr2Geo;

  function Geo2Lstr (Geo : Lat_Lon.Lat_Lon_Geo_Rec) return Geo_Lstr is
    Lstr : Geo_Lstr;

  begin
    -- Set / and .
    Lstr(4) := '.'; Lstr(7) := '.';
    Lstr(14) := '/';
    Lstr(19) := '.'; Lstr(22) := '.';

    -- North or south lat, East or west lon
    if Geo.Lat.North then
      Lstr(1) := 'N';
    else
      Lstr(1) := 'S';
    end if;
    if Geo.Lon.East then
      Lstr(15) := 'E';
    else
      Lstr(15) := 'W';
    end if;

    -- Put the numbers
    Lstr( 2 ..  3) := Normal (Geo.Lat.Coord.Deg, 2, Gap => '0');
    Lstr( 5 ..  6) := Normal (Geo.Lat.Coord.Min, 2, Gap => '0');
    Lstr( 8 ..  9) := Normal (Geo.Lat.Coord.Sec, 2, Gap => '0');
    Lstr(10 .. 13) := Normal (Geo.Lat.Coord.Ten, 4, Gap => '0');
    Lstr(16 .. 18) := Normal (Geo.Lon.Coord.Deg, 3, Gap => '0');
    Lstr(20 .. 21) := Normal (Geo.Lon.Coord.Min, 2, Gap => '0');
    Lstr(23 .. 24) := Normal (Geo.Lon.Coord.Sec, 2, Gap => '0');
    Lstr(25 .. 28) := Normal (Geo.Lon.Coord.Ten, 4, Gap => '0');

    -- Done
    return Lstr;
  end Geo2Lstr;

  -- Str <-> Dec
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
    Dec.Lat.Coord.Deg := Units.Deg_Range'Value(Str( 2 ..  3));
    Dec.Lat.Coord.Nan := Units.Nan_Range'Value(Str( 5 .. 10)) * 1000;
    -- Lon
    Dec.Lon.Coord.Deg := Units.Deg_Range'Value(Str(13 .. 15));
    Dec.Lon.Coord.Nan := Units.Nan_Range'Value(Str(17 .. 22)) * 1000;

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

  -- Round Nan to 10 thousands of deg
  -- or Round nan to micro of deg
  procedure Round (C : in out Units.Dec_Coord_Rec;
                   To_Tenth : in Boolean) is
    Factor : My_Math.Real;
    Max : Units.Nan_Range;
    use type My_Math.Real;
  begin
    if To_Tenth then
      Factor := 100_000.0;
      Max := 1_0000;
    else
      Factor := 1_000.0;
      Max := 1_000_000;
    end if;
    C.Nan := Units.Nan_Range (My_Math.Round (My_Math.Real (C.Nan) / Factor));
    if C.Nan = Max then
      C.Nan := 0;
      if C.Deg = Units.Deg_Range'Last then
        C.Deg :=  Units.Deg_Range'First;
      else
        C.Deg := C.Deg + 1;
      end if;
    end if;
    C := (C.Deg, C.Nan * Natural (Factor));
  end Round;

  function Dec2Str (Dec : Lat_Lon.Lat_Lon_Dec_Rec) return Dec_Str is
    Rounded : Lat_Lon.Lat_Lon_Dec_Rec;
    Str : Dec_Str;

  begin
    Rounded := Dec;
    Round (Rounded.Lat.Coord, False);
    Round (Rounded.Lon.Coord, False);
    -- Set / and .
    Str(4) := '.';
    Str(11) := '/';
    Str(16) := '.';

    -- North or south lat, East or west lon
    if Rounded.Lat.North then
      Str(1) := 'N';
    else
      Str(1) := 'S';
    end if;
    if Rounded.Lon.East then
      Str(12) := 'E';
    else
      Str(12) := 'W';
    end if;

    -- Put the numbers
    Str( 2 ..  3) := Normal (Rounded.Lat.Coord.Deg, 2, Gap => '0');
    Str( 5 .. 10) := Normal (Rounded.Lat.Coord.Nan / 1000, 6, Gap => '0');
    Str(13 .. 15) := Normal (Rounded.Lon.Coord.Deg, 3, Gap => '0');
    Str(17 .. 22) := Normal (Rounded.Lon.Coord.Nan / 1000, 6, Gap => '0');

    -- Done
    return Str;
  end Dec2Str;

  function Dist2Str (Dist : Distance) return Dist_Str is
    Int : My_Math.Inte;
  begin
    Int := My_Math.Round (Dist);
    return Normal (Integer (Int), Dist_Str'Length);
  end Dist2Str;

  function Geoangle2Str (Geo_Angle : Units.Geo_Coord_Rec)
  return Geo_Angle_Str is
    Rounded : Units.Geo_Coord_Rec;
    Str : Geo_Angle_Str;
  begin
    -- Round hundredths
    Rounded := Geo_Angle;
    Round (Rounded, True);
    -- Set .
    Str(4) := '.'; Str(7) := '.';
    Str(1 ..  3) := Normal (Rounded.Deg, 3, Gap => '0');
    Str(5 ..  6) := Normal (Rounded.Min, 2, Gap => '0');
    Str(8 ..  9) := Normal (Rounded.Sec, 2, Gap => '0');
    return Str;
  end Geoangle2Str;

  function Geoangle2Lstr (Geo_Angle : Units.Geo_Coord_Rec)
           return Geo_Angle_Lstr is
    Rounded : Units.Geo_Coord_Rec;
    Lstr : Geo_Angle_Lstr;
  begin
    -- Round hundredths
    Rounded := Geo_Angle;
    Round (Rounded, False);
    -- Set .
    Lstr(4) := '.'; Lstr(7) := '.';
    Lstr( 1 ..  3) := Normal (Rounded.Deg, 3, Gap => '0');
    Lstr( 5 ..  6) := Normal (Rounded.Min, 2, Gap => '0');
    Lstr( 8 ..  9) := Normal (Rounded.Sec, 2, Gap => '0');
    Lstr(10 .. 11) := Normal (Rounded.Ten / 100, 2, Gap => '0');
    return Lstr;
  end Geoangle2Lstr;

  function Lstr2Geoangle (Lstr : Geo_Angle_Lstr) return Units.Geo_Coord_Rec is
    Geo : Units.Geo_Coord_Rec;
  begin
    if Lstr(4) /= '.' or else Lstr(7) /= '.' then
      raise Format_Error;
    end if;
    Geo.Deg := Units.Deg_Range'Value(Lstr( 1 ..  3));
    Geo.Min := Units.Deg_Range'Value(Lstr( 5 ..  6));
    Geo.Sec := Units.Deg_Range'Value(Lstr( 8 ..  9));
    Geo.Ten := Units.Deg_Range'Value(Lstr(10 ..  11)) * 100;
    return Geo;
  end Lstr2Geoangle;

  function Decangle2Str (Dec_Angle : Units.Dec_Coord_Rec) return Dec_Angle_Str is
    Rounded : Units.Dec_Coord_Rec;
    Str : Dec_Angle_Str;
  begin
    -- Round hundredths
    Rounded := Dec_Angle;
    Round (Rounded, True);
    -- Set .
    Str(4) := '.';
    Str(1 ..  3) := Normal (Rounded.Deg, 3, Gap => '0');
    Str(5 ..  8) := Normal (Rounded.Nan / 100000, 4, Gap => '0');
    return Str;
  end Decangle2Str;

end String_Util;

