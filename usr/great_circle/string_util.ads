with Conv, Lat_Lon;
package String_Util is

  -- Ndd.mm.ss/Wddd.mm.dd
  subtype Coord_Str is String(1..20);

  -- Convert Coord_Str to Lat_Lon_Geo_Rec
  -- may raise Format_Error
  Format_Error : exception;
  function Str2Geo (Str : Coord_Str) return Lat_Lon.Lat_Lon_Geo_Rec;

  -- Convert Lat_Lon_Geo_Rec to Coord_Str
  function Geo2Str (Geo : Lat_Lon.Lat_Lon_Geo_Rec) return Coord_Str;

  -- Distance is 5 digits
  subtype Dist_Str is String (1..5);
  function Dist2Str (Dist : Lat_Lon.Distance) return Dist_Str;

  -- Angle is ddd.mm.dd
  subtype Angle_Str is  String(1..9);
  function Angle2Str (Angle : Conv.Geo_Coord_Rec) return Angle_Str;

end String_Util;

