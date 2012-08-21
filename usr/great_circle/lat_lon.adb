with C_Nbres;
package body Lat_Lon is

  Max_Lat : constant Conv.Deg_Range := 90;
  Max_Lon : constant Conv.Deg_Range := 180;

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
      or else Lat_Lon_Geo.Lat.Coord.Hun /= 0) then
      return False;
    end if;

    -- Check lon coord, deg < Max_Lon, or deg = Max_Lon and min=sec=0
    if Lat_Lon_Geo.Lon.Coord.Deg > Max_Lon then
      return False;
    end if;
    if Lat_Lon_Geo.Lon.Coord.Deg = Max_Lon
    and then (Lat_Lon_Geo.Lon.Coord.Min /= 0
      or else Lat_Lon_Geo.Lon.Coord.Sec /= 0
      or else Lat_Lon_Geo.Lon.Coord.Hun /= 0) then
      return False;
    end if;
    return True;
  end Is_Lat_Lon_Ok;

  function Rad2Geo (Coord : Lat_Lon_Rad_Rec) return Lat_Lon_Geo_Rec is
    Lat_Lon_Geo : Lat_Lon_Geo_Rec;
    use Conv, C_Nbres;
  begin
    if Coord.X < 0.0 then
      -- West longitude
      Lat_Lon_Geo.Lon.East := False;
      Lat_Lon_Geo.Lon.Coord := Conv.Rad2Geo(-Coord.X);
    else
      -- East longitude
      Lat_Lon_Geo.Lon.East := True;
      Lat_Lon_Geo.Lon.Coord := Conv.Rad2Geo(Coord.X);
    end if;

    if Coord.Y < 0.0 then
      -- South latitude
      Lat_Lon_Geo.Lat.North := False;
      Lat_Lon_Geo.Lat.Coord := Conv.Rad2Geo(-Coord.Y);
    else
      -- North latitude
      Lat_Lon_Geo.Lat.North := True;
      Lat_Lon_Geo.Lat.Coord := Conv.Rad2Geo(Coord.Y);
    end if;
    return Lat_Lon_Geo;
  end Rad2Geo;

  function Geo2Rad (Coord : Lat_Lon_Geo_Rec) return Lat_Lon_Rad_Rec is
    Lat_Lon_Rad : Lat_Lon_Rad_Rec;
    use Conv, C_Nbres;
  begin
    if Coord.Lon.East then
      -- East longitude
      Lat_Lon_Rad.X := C_Nbres.Reduct(Conv.Geo2Rad(Coord.Lon.Coord));
    else
      -- West longitude
      Lat_Lon_Rad.X := C_Nbres.Reduct(-Conv.Geo2Rad(Coord.Lon.Coord));
    end if;
    if Coord.Lat.North then
      -- North latitude
      Lat_Lon_Rad.Y := C_Nbres.Reduct(Conv.Geo2Rad(Coord.Lat.Coord));
    else
      -- South latitude
      Lat_Lon_Rad.Y := C_Nbres.Reduct(-Conv.Geo2Rad(Coord.Lat.Coord));
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
    and then Lat_Lon_Dec.Lat.Coord.Ten /= 0 then
      return False;
    end if;

    -- Check lon coord, deg < Max_Lon, or deg = Max_Lon and min=sec=0
    if Lat_Lon_Dec.Lon.Coord.Deg > Max_Lon then
      return False;
    end if;
    if Lat_Lon_Dec.Lon.Coord.Deg = Max_Lon
    and then Lat_Lon_Dec.Lon.Coord.Ten /= 0 then
      return False;
    end if;
    return True;
  end Is_Lat_Lon_Ok;

  function Dec2Geo (Coord : Lat_Lon_Dec_Rec) return Lat_Lon_Geo_Rec is
  begin
    return (
      Lat => (North => Coord.Lat.North,
              Coord => Conv.Dec2Geo (Coord.Lat.Coord)),
      Lon => (East  => Coord.Lon.East,
              Coord => Conv.Dec2Geo (Coord.Lon.Coord)) );
  end Dec2Geo;

  function Geo2Dec (Coord : Lat_Lon_Geo_Rec) return Lat_Lon_Dec_Rec is
  begin
    return (
      Lat => (North => Coord.Lat.North,
              Coord => Conv.Geo2Dec (Coord.Lat.Coord)),
      Lon => (East  => Coord.Lon.East,
              Coord => Conv.Geo2Dec (Coord.Lon.Coord)) );
  end Geo2Dec;

end Lat_Lon;

