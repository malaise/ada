with Conv, Lat_Lon;
package String_Util is

  -- Ndd.mm.ss/Wddd.mm.dd
  subtype Geo_Str is String(1..20);

  -- Convert Geo_Str to Lat_Lon_Geo_Rec
  -- may raise Format_Error
  Format_Error : exception;
  function Str2Geo (Str : Geo_Str) return Lat_Lon.Lat_Lon_Geo_Rec;

  -- Convert Lat_Lon_Geo_Rec to Geo_Str
  function Geo2Str (Geo : Lat_Lon.Lat_Lon_Geo_Rec) return Geo_Str;

  -- Geo_Angle is ddd.mm.dd
  subtype Geo_Angle_Str is  String(1..9);
  function Geoangle2Str (Geo_Angle : Conv.Geo_Coord_Rec) return Geo_Angle_Str;


  -- Ndd.ijkl/Wddd.ijkl
  subtype Dec_Str is String (1..18);

 -- Convert Dec_Str to Lat_Lon_Geo_Rec
  -- may raise Format_Error
  function Str2Dec (Str : Dec_Str) return Lat_Lon.Lat_Lon_Dec_Rec;

  -- Convert Lat_Lon_Geo_Rec to Dec_Str
  function Dec2Str (Dec : Lat_Lon.Lat_Lon_Dec_Rec) return Dec_Str;

  -- Dec_Angle is ddd.ijkl
  subtype Dec_Angle_Str is  String(1..8);
  function Decangle2Str (Dec_Angle : Conv.Dec_Coord_Rec) return Dec_Angle_Str;


  -- Distance is 5 digits
  subtype Dist_Str is String (1..5);
  function Dist2Str (Dist : Lat_Lon.Distance) return Dist_Str;

end String_Util;

