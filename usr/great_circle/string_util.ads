with My_Math;
with Units, Lat_Lon;
package String_Util is

  -- Ndd.mm.ssss/Wddd.mm.ssss
  subtype Geo_Str is String(1..24);
  -- Convert Geo_Str to Lat_Lon_Geo_Rec
  -- may raise Format_Error
  Format_Error : exception;
  function Str2Geo (Str : Geo_Str) return Lat_Lon.Lat_Lon_Geo_Rec;
  -- Convert Lat_Lon_Geo_Rec to Geo_Str
  function Geo2Str (Geo : Lat_Lon.Lat_Lon_Geo_Rec) return Geo_Str;

  -- Ndd.mm.ssssss/Wddd.mm.ssssss
  subtype Geo_Lstr is String(1..28);
  -- Convert Geo_Lstr to Lat_Lon_Geo_Rec
  -- may raise Format_Error
  function Lstr2Geo (Lstr : Geo_Lstr) return Lat_Lon.Lat_Lon_Geo_Rec;
  -- Convert Lat_Lon_Geo_Rec to Geo_Lstr
  function Geo2Lstr (Geo : Lat_Lon.Lat_Lon_Geo_Rec) return Geo_Lstr;

  -- Geo_Angle is ddd.mm.ss or ddd.mm.ssss
  subtype Geo_Angle_Str is  String(1..9);
  function Geoangle2Str (Geo_Angle : Units.Geo_Coord_Rec) return Geo_Angle_Str;
  subtype Geo_Angle_Lstr is  String(1..11);
  function Geoangle2Lstr (Geo_Angle : Units.Geo_Coord_Rec)
           return Geo_Angle_Lstr;
  function Lstr2Geoangle (Lstr : Geo_Angle_Lstr) return Units.Geo_Coord_Rec;


  -- Ndd.ijklmn/Wddd.ijklmn
  subtype Dec_Str is String (1..22);
 -- Convert Dec_Str to Lat_Lon_Geo_Rec
  -- may raise Format_Error
  function Str2Dec (Str : Dec_Str) return Lat_Lon.Lat_Lon_Dec_Rec;
  -- Convert Lat_Lon_Geo_Rec to Dec_Str
  function Dec2Str (Dec : Lat_Lon.Lat_Lon_Dec_Rec) return Dec_Str;

  -- Dec_Angle is ddd.ijkl
  subtype Dec_Angle_Str is  String(1..8);
  function Decangle2Str (Dec_Angle : Units.Dec_Coord_Rec) return Dec_Angle_Str;

  -- Distance is 5 digits
  subtype Distance is My_Math.Real;
  subtype Dist_Str is String (1..5);
  function Dist2Str (Dist : Distance) return Dist_Str;

end String_Util;

