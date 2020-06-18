with Trace.Loggers;
with Conv, Lat_Lon, String_Util;

package Great_Circle is

  -- Init logger before using it (otherwise it will be anonymous)
  procedure Init_Logger;
  Logger : Trace.Loggers.Logger;

  subtype Distance is String_Util.Distance;

  -- Compute the great circle arc (heading and distance in Nm)
  --  from point A to point B
  procedure Compute_Route (A, B : in Lat_Lon.Lat_Lon_Rad_Rec;
                           Head  : out Conv.Rad_Coord_Range;
                           Dist : out String_Util.Distance);

  -- Apply a great circle arc (heading and distance in Nm) to point A
  --  and return destination point B
  function Apply_Route (A : Lat_Lon.Lat_Lon_Rad_Rec;
                        Head  : Conv.Rad_Coord_Range;
                        Dist : String_Util.Distance)
           return Lat_Lon.Lat_Lon_Rad_Rec;

end Great_Circle;

