with Lat_Lon;

package String_Util is

  -- Ndd.mm.ss/Wddd.mm.dd
  subtype Coord_Str is String(1..20);

  -- Convert Coord_Str to Lat_Lon_Geo_Rec
  -- may raise Format_Error
  Format_Error : exception;
  function Str2Geo (Str : Coord_Str) return Lat_Lon.Lat_Lon_Geo_Rec;

  -- Convert Lat_Lon_Geo_Rec to Coord_Str
  function Geo2Str (Geo : Lat_Lon.Lat_Lon_Geo_Rec) return Coord_Str;

end String_Util;

