with Str_Util, Complexes;
with Mapcodes;
with Lat_Lon, Conv;
-- Str is [ <Context>: ] <mapcode>
function Mapcode2Geo (Str : String) return Lat_Lon.Lat_Lon_Rad_Rec is

  use type Conv.Rad_Range;

  -- Convert a Mapcode real cooordinate (Real fraction of degrees), into
  --   Radian in -180 .. 180
  function To_Radian (R : Mapcodes.Real) return Conv.Rad_Coord_Range is
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
end Mapcode2Geo;

