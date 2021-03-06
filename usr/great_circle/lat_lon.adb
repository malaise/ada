with My_Math, Complexes, Str_Util;
package body Lat_Lon is

  Max_Lat : constant Units.Deg_Range := 90;
  Max_Lon : constant Units.Deg_Range := 180;

  --  00.00.00 <= Lat.Coord <=  90.00.00
  -- 000.00.00 <= Lon.Coord <= 180.00.00
  function Is_Lat_Lon_Ok (Lat_Lon_Geo : Lat_Lon_Geo_Rec) return Boolean is
  begin
    -- Check lat coord, deg < Max_Lat, or deg = Max_Lat and min=sec=0
    if Lat_Lon_Geo.Lat.Coord.Deg > Max_Lat then
      return False;
    end if;
    if Lat_Lon_Geo.Lat.Coord.Deg = Max_Lat
    and then (Lat_Lon_Geo.Lat.Coord.Min /= 0
      or else Lat_Lon_Geo.Lat.Coord.Sec /= 0
      or else Lat_Lon_Geo.Lat.Coord.Ten /= 0) then
      return False;
    end if;

    -- Check lon coord, deg < Max_Lon, or deg = Max_Lon and min=sec=0
    if Lat_Lon_Geo.Lon.Coord.Deg > Max_Lon then
      return False;
    end if;
    if Lat_Lon_Geo.Lon.Coord.Deg = Max_Lon
    and then (Lat_Lon_Geo.Lon.Coord.Min /= 0
      or else Lat_Lon_Geo.Lon.Coord.Sec /= 0
      or else Lat_Lon_Geo.Lon.Coord.Ten /= 0) then
      return False;
    end if;
    return True;
  end Is_Lat_Lon_Ok;

  function Rad2Geo (Coord : Lat_Lon_Rad_Rec) return Lat_Lon_Geo_Rec is
    Lat_Lon_Geo : Lat_Lon_Geo_Rec;
    use type Complexes.Radian;
  begin
    if Coord.X >= My_Math.Pi then
      -- West longitude
      Lat_Lon_Geo.Lon.East := False;
      Lat_Lon_Geo.Lon.Coord := Units.Rad2Geo(2.0 * My_Math.Pi - Coord.X);
    else
      -- East longitude
      Lat_Lon_Geo.Lon.East := True;
      Lat_Lon_Geo.Lon.Coord := Units.Rad2Geo(Coord.X);
    end if;

    if Coord.Y >= My_Math.Pi then
      -- South latitude
      Lat_Lon_Geo.Lat.North := False;
      Lat_Lon_Geo.Lat.Coord := Units.Rad2Geo(2.0 * My_Math.Pi - Coord.Y);
    else
      -- North latitude
      Lat_Lon_Geo.Lat.North := True;
      Lat_Lon_Geo.Lat.Coord := Units.Rad2Geo(Coord.Y);
    end if;
    return Lat_Lon_Geo;
  end Rad2Geo;

  function Geo2Rad (Coord : Lat_Lon_Geo_Rec) return Lat_Lon_Rad_Rec is
    Lat_Lon_Rad : Lat_Lon_Rad_Rec;
    use type Complexes.Radian;
  begin
    if Coord.Lon.East then
      -- East longitude
      Lat_Lon_Rad.X := Complexes.Reduct(Units.Geo2Rad(Coord.Lon.Coord));
    else
      -- West longitude
      Lat_Lon_Rad.X := Complexes.Reduct(-Units.Geo2Rad(Coord.Lon.Coord));
    end if;
    if Coord.Lat.North then
      -- North latitude
      Lat_Lon_Rad.Y := Complexes.Reduct(Units.Geo2Rad(Coord.Lat.Coord));
    else
      -- South latitude
      Lat_Lon_Rad.Y := Complexes.Reduct(-Units.Geo2Rad(Coord.Lat.Coord));
    end if;
    return Lat_Lon_Rad;
  end Geo2Rad;

  --  00.0000 <= Lat.Coord <=  90.0000
  -- 000.0000 <= Lon.Coord <= 180.0000
  function Is_Lat_Lon_Ok (Lat_Lon_Dec : Lat_Lon_Dec_Rec) return Boolean is
  begin
    -- Check lat coord, deg < Max_Lat, or deg = Max_Lat and min=sec=0
    if Lat_Lon_Dec.Lat.Coord.Deg > Max_Lat then
      return False;
    end if;
    if Lat_Lon_Dec.Lat.Coord.Deg = Max_Lat
    and then Lat_Lon_Dec.Lat.Coord.Nan /= 0 then
      return False;
    end if;

    -- Check lon coord, deg < Max_Lon, or deg = Max_Lon and min=sec=0
    if Lat_Lon_Dec.Lon.Coord.Deg > Max_Lon then
      return False;
    end if;
    if Lat_Lon_Dec.Lon.Coord.Deg = Max_Lon
    and then Lat_Lon_Dec.Lon.Coord.Nan /= 0 then
      return False;
    end if;
    return True;
  end Is_Lat_Lon_Ok;

  function Dec2Geo (Coord : Lat_Lon_Dec_Rec) return Lat_Lon_Geo_Rec is
    ( (Lat => (North => Coord.Lat.North,
               Coord => Units.Dec2Geo (Coord.Lat.Coord)),
       Lon => (East  => Coord.Lon.East,
               Coord => Units.Dec2Geo (Coord.Lon.Coord))));

  function Geo2Dec (Coord : Lat_Lon_Geo_Rec) return Lat_Lon_Dec_Rec is
    ( (Lat => (North => Coord.Lat.North,
               Coord => Units.Geo2Dec (Coord.Lat.Coord)),
       Lon => (East  => Coord.Lon.East,
               Coord => Units.Geo2Dec (Coord.Lon.Coord))) );

  function Rad2Dec (Coord : Lat_Lon_Rad_Rec) return Lat_Lon_Dec_Rec is
    Lat_Lon_Dec : Lat_Lon_Dec_Rec;
    use type Complexes.Radian;
  begin
    if Coord.X >= My_Math.Pi then
      -- West longitude
      Lat_Lon_Dec.Lon.East := False;
      Lat_Lon_Dec.Lon.Coord := Units.Rad2Dec(2.0 * My_Math.Pi - Coord.X);
    else
      -- East longitude
      Lat_Lon_Dec.Lon.East := True;
      Lat_Lon_Dec.Lon.Coord := Units.Rad2Dec(Coord.X);
    end if;

    if Coord.Y >= My_Math.Pi then
      -- South latitude
      Lat_Lon_Dec.Lat.North := False;
      Lat_Lon_Dec.Lat.Coord := Units.Rad2Dec(2.0 * My_Math.Pi - Coord.Y);
    else
      -- North latitude
      Lat_Lon_Dec.Lat.North := True;
      Lat_Lon_Dec.Lat.Coord := Units.Rad2Dec(Coord.Y);
    end if;
    return Lat_Lon_Dec;
  end Rad2Dec;

  function Dec2Rad (Coord : Lat_Lon_Dec_Rec) return Lat_Lon_Rad_Rec is
    Lat_Lon_Rad : Lat_Lon_Rad_Rec;
    use type Complexes.Radian;
  begin
    if Coord.Lon.East then
      -- East longitude
      Lat_Lon_Rad.X := Complexes.Reduct(Units.Dec2Rad(Coord.Lon.Coord));
    else
      -- West longitude
      Lat_Lon_Rad.X := Complexes.Reduct(-Units.Dec2Rad(Coord.Lon.Coord));
    end if;
    if Coord.Lat.North then
      -- North latitude
      Lat_Lon_Rad.Y := Complexes.Reduct(Units.Dec2Rad(Coord.Lat.Coord));
    else
      -- South latitude
      Lat_Lon_Rad.Y := Complexes.Reduct(-Units.Dec2Rad(Coord.Lat.Coord));
    end if;
    return Lat_Lon_Rad;
  end Dec2Rad;

  -- Str is [ <Context>: ] <mapcode>
  function Mapcode2Rad (Str : String) return Lat_Lon.Lat_Lon_Rad_Rec is

    -- Convert a Mapcode real cooordinate (Real fraction of degrees), into
    --   Radian in -180 .. 180
    function To_Radian (R : Mapcodes.Real) return Units.Rad_Coord_Range is
    begin
      -- In 0 .. 2*PI
      return Complexes.To_Radian (Complexes.Degree (R));
    end To_Radian;

    I : Natural;
    Coord : Mapcodes.Coordinate;

  begin
    -- Extract Mapcode and leading optional Context, get Coordinates
    I := Str_Util.Locate (Str, ":");
    if I = 0 then
      -- No context
      Coord := Mapcodes.Decode (Str, "");
    else
      -- Context then mapcode
      Coord :=  Mapcodes.Decode (Str(I + 1 .. Str'Last),
                                 Str(Str'First .. I - 1));
    end if;
    return (X => To_Radian (Coord.Lon),
            Y => To_Radian (Coord.Lat));
  end Mapcode2Rad;

  function To_Degree (R : Units.Rad_Coord_Range) return Mapcodes.Real is
    Res : Mapcodes.Real := Mapcodes.Real (Complexes.To_Degree (R));
    use type Mapcodes.Real;
  begin
    if Res >= 180.0 then
      Res := -(360.0 - Res);
    end if;
  return Res;
  end To_Degree;

  -- Return the international mapcode
  function Rad2Mapcode (Coord : Lat_Lon.Lat_Lon_Rad_Rec;
                        Precision : Precisions := Default_Precision)
           return String is
    C : constant Mapcodes.Coordinate
      := (Lat => To_Degree (Coord.Y),
          Lon => To_Degree (Coord.X));
    Codes : constant Mapcodes.Mapcode_Infos
          := Mapcodes.Encode (C, Shortest => True, Precision => Precision);
  begin
    return Codes(Codes'Last).Mapcode.Image;
  end Rad2Mapcode;

end Lat_Lon;

