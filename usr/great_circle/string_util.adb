with Normal, My_Math, Upper_Char;
package body String_Util is

  function Str2Geo (Str : Coord_Str) return Lat_Lon.Lat_Lon_Geo_Rec is
    Geo : Lat_Lon.Lat_Lon_Geo_Rec;
  begin
    -- Check / and .
    if Str(4) /= '.' or else Str(7) /= '.' or else Str(10) /= '/'
    or else Str(15) /= '.' or else Str(18) /= '.' then
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
    if Upper_Char(Str(11)) = 'E' then
      Geo.Lon.East := True;
    elsif Upper_Char(Str(11)) = 'W' then
      Geo.Lon.East := False;
    else
      raise Format_Error;
    end if;

    -- Lat
    Geo.Lat.Coord.Deg := Conv.Deg_Range'Value(Str(2 .. 3));
    Geo.Lat.Coord.Min := Conv.Deg_Range'Value(Str(5 .. 6));
    Geo.Lat.Coord.Sec := Conv.Deg_Range'Value(Str(8 .. 9));
    -- Lon
    Geo.Lon.Coord.Deg := Conv.Deg_Range'Value(Str(12 .. 14));
    Geo.Lon.Coord.Min := Conv.Deg_Range'Value(Str(16 .. 17));
    Geo.Lon.Coord.Sec := Conv.Deg_Range'Value(Str(19 .. 20));
 
    -- Chek the whole thing
    if not Lat_Lon.Is_Lat_Lon_Ok(Geo) then
      raise Format_Error;
    end if;

    -- Ok
    return Geo;
  end Str2Geo;
    

  function Geo2Str (Geo : Lat_Lon.Lat_Lon_Geo_Rec) return Coord_Str is
    Str : Coord_Str;
  begin
    -- Set / and .
    Str(4) := '.'; Str(7) := '.';
    Str(10) := '/';
    Str(15) := '.'; Str(18) := '.';

    -- North or south lat, East or west lon
    if Geo.Lat.North then
      Str(1) := 'N';
    else
      Str(1) := 'S';
    end if;
    if Geo.Lon.East then
      Str(11) := 'E';
    else
      Str(11) := 'W';
    end if;

    -- Put the numbers
    Str( 2 ..  3) := Normal (Geo.Lat.Coord.Deg, 2, Gap => '0');
    Str( 5 ..  6) := Normal (Geo.Lat.Coord.Min, 2, Gap => '0');
    Str( 8 ..  9) := Normal (Geo.Lat.Coord.Sec, 2, Gap => '0');
    Str(12 .. 14) := Normal (Geo.Lon.Coord.Deg, 3, Gap => '0');
    Str(16 .. 17) := Normal (Geo.Lon.Coord.Min, 2, Gap => '0');
    Str(19 .. 20) := Normal (Geo.Lon.Coord.Sec, 2, Gap => '0');

    -- Done
    return Str;
  end Geo2Str;
    

  function Dist2Str (Dist : Lat_Lon.Distance) return Dist_Str is
    Int : My_Math.Inte; 
  begin
    Int := My_Math.Round(My_Math.Real(Dist));
    return Normal(Integer(Int), Dist_Str'Length);
  end Dist2Str;

  function Angle2Str (Angle : Conv.Geo_Coord_Rec) return Angle_Str is
    Str : Angle_Str;
  begin
    -- Set .
    Str(4) := '.'; Str(7) := '.';
    Str(1 ..  3) := Normal (Angle.Deg, 3, Gap => '0');
    Str(5 ..  6) := Normal (Angle.Min, 2, Gap => '0');
    Str(8 ..  9) := Normal (ANgle.Sec, 2, Gap => '0');
    return Str;
  end Angle2Str;

end String_Util;

