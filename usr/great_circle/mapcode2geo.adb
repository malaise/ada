with Str_Util, Complexes;
with Mapcodes;
with Lat_Lon, Conv;
-- Str is [ <Context>: ] <mapcode>
function Mapcode2Geo (Str : String) return Lat_Lon.Lat_Lon_Geo_Rec is

  use type Conv.Rad_Range;

  -- Convert a Mapcode real cooordinate (Real fraction of degrees), into
  --   Radian in -180 .. 180
  function To_Radian (R : Mapcodes.Real) return Conv.Rad_Range is
    Rad : Conv.Rad_Range;
  begin
    -- In 0 .. 2*PI
    Rad := Complexes.Reduct (Complexes.Radian (R) / 180.0 * Conv.Pi);
    if Rad > Conv.Pi then
      Rad := Rad - 2.0 * Conv.Pi;
    end if;
    return Rad;
  end To_Radian;

  I : Natural;
  Coord : Mapcodes.Coordinate;
  Lalo : Lat_Lon.Lat_Lon_Geo_Rec;
  Rad : Conv.Rad_Range;

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
  -- Latitude
  Rad := To_Radian (Coord.Lat);
  if Rad > 0.0 then
    Lalo.Lat.North := True;
    Lalo.Lat.Coord := Conv.Rad2Geo (Rad);
  else
    Lalo.Lat.North := False;
    Lalo.Lat.Coord := Conv.Rad2Geo (-Rad);
  end if;
  -- Longitude
  Rad := To_Radian (Coord.Lon);
  if Rad > 0.0 then
    Lalo.Lon.East := True;
    Lalo.Lon.Coord := Conv.Rad2Geo (Rad);
  else
    Lalo.Lon.East := False;
    Lalo.Lon.Coord := Conv.Rad2Geo (-Rad);
  end if;
  return Lalo;
end Mapcode2Geo;

