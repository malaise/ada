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

  function Sig2Geo (Coord : Signed_Deg_Rec) return Lat_Lon_Geo_Rec is
    Llat, Llon : Units.Degree;
    Res : Lat_Lon_Geo_Rec;
  begin
    Res.Lat.North := True;
    Res.Lon.East := True;
    -- Latitude from 0 to 90
    Llat := Coord.Lat;
    if Llat < 0.0 then
      Llat := -Llat;
      Res.Lat.North := False;
    end if;
    Res.Lat.Coord := Units.Deg2Geo (Llat);
    -- Longitude from 0 to 180
    Llon := Coord.Lon;
    if Llon < 0.0 then
      Llon := -Llon;
      Res.Lon.East := False;
    end if;
    Res.Lon.Coord := Units.Deg2Geo (Llon);
    return Res;
  end Sig2Geo;

  function Geo2Sig (Coord : Lat_Lon_Geo_Rec) return Signed_Deg_Rec is
    Llat, Llon : Units.Degree;
  begin
    Llat := Units.Geo2Deg (Coord.Lat.Coord);
    Llon := Units.Geo2Deg (Coord.Lon.Coord);
    -- Latitude from -90 to 90
    if not Coord.Lat.North then
      Llat := -Llat;
    end if;
    -- Longitude from -180 to 180
    if not Coord.Lon.East then
      Llon := -Llon;
    end if;
    return (Llat, Llon);
  end Geo2Sig;

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

  -- Mapcode <-> Deg
  function Mapcode2Deg (Str : String) return Signed_Deg_Rec is
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
    return (Lat => Signed_Deg (Coord.Lat),
            Lon => Signed_Deg (Coord.Lon) );
  end Mapcode2Deg;

  -- Return the international mapcode
  function Deg2Mapcode (Coord : Signed_Deg_Rec;
                        Precision : Map_Precisions := Default_Map_Precision)
           return String is
    C : constant Mapcodes.Coordinate := (Lat => Mapcodes.Real (Coord.Lat),
                                         Lon => Mapcodes.Real (Coord.Lon));
    Codes : constant Mapcodes.Mapcode_Infos
          := Mapcodes.Encode (C, Shortest => True, Precision => Precision);
  begin
    return Codes(Codes'Last).Mapcode.Image;
  end Deg2Mapcode;

  -- Mapcode <-> Rad
  function Mapcode2Rad (Str : String) return Lat_Lon_Rad_Rec is
  begin
    return Deg2Rad (Mapcode2Deg (Str));
  end Mapcode2Rad;

  function Rad2Mapcode (Coord : Lat_Lon_Rad_Rec;
                        Precision : Map_Precisions := Default_Map_Precision)
           return String is
  begin
    return Deg2Mapcode (Rad2Deg (Coord), Precision);
  end Rad2Mapcode;

  -- Olc <-> Deg
  function Olc2Deg (Code : Olc.Code_Type) return Signed_Deg_Rec is
    Sw, Ne, Center : Olc.Coordinate;
  begin
    Olc.Decode (Code, Sw, Ne);
    Center := Olc.Center_Of (Sw, Ne);
    return (Lat => Signed_Deg (Center.Lat),
            Lon => Signed_Deg (Center.Lon) );
  end Olc2Deg;

  function Deg2Olc (Coord : Signed_Deg_Rec;
                    Precision : Olc_Precisions := Default_Olc_Precision)
           return Olc.Code_Type is
    C : constant Olc.Coordinate := (Lat => Olc.Real (Coord.Lat),
                                    Lon => Olc.Real (Coord.Lon));
  begin
    return Olc.Encode (C, Precision);
  end Deg2Olc;

  -- Open Loc Code <-> Rad
  function Olc2Rad (Code : Olc.Code_Type) return Lat_Lon_Rad_Rec is
  begin
    return Deg2Rad (Olc2Deg (Code));
  end Olc2Rad;

  function Rad2Olc (Coord : Lat_Lon_Rad_Rec;
                    Precision : Olc_Precisions := Default_Olc_Precision)
           return Olc.Code_Type is
  begin
    return Deg2Olc (Rad2Deg (Coord), Precision);
  end Rad2Olc;

  -- Signed Deg <-> Rad
  function Deg2Rad (Coord : Signed_Deg_Rec) return Lat_Lon_Rad_Rec is
    Llat, Llon : Units.Degree;
  begin
    Llat := Coord.Lat;
    Llon := Coord.Lon;
    if Llat < 0.0 and then Llat > -90.0 then
      Llat := Units.Reduct (Llat);
    end if;
    if Llon < 0.0 and then Llon > -180.0 then
      Llon := Units.Reduct (Llon);
    end if;
    -- Set Lalo
    return Rads : Lat_Lon.Lat_Lon_Rad_Rec do
      Rads.X := Units.Deg2Rad (Llon);
      Rads.Y := Units.Deg2Rad (Llat);
    end return;
  end Deg2Rad;

  function Rad2Deg (Coord : Lat_Lon_Rad_Rec) return Signed_Deg_Rec is
    Llat, Llon : Units.Degree;
  begin
    Llat := Units.Rad2Deg (Coord.Y);
    -- Latitude from -90 to 90
    if Llat > 90.0 then
      Llat := -360.0 + Llat;
    elsif Llat < -90.0 then
      Llat := 360.0 + Llat;
    end if;
    Llon := Units.Rad2Deg (Coord.X);
    -- Longitude from -180 to 180
    if Llon > 180.0 then
      Llon := -360.0 + Llon;
    end if;
    return (Llat, Llon);
  end Rad2Deg;

end Lat_Lon;

