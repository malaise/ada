with Trace.Loggers;
with Conv, Lat_Lon;

package Great_Circle is

  -- Init logger befor using it (otherwise it will be anonymous)
  procedure Init_Logger;
  Logger : Trace.Loggers.Logger;

  -- Compute the great circle arc (heading and distance in Nm)
  --  from point A to point B
  procedure Compute_Route (A, B : in Lat_Lon.Lat_Lon_Rad_Rec;
                           Heading  : out Conv.Rad_Coord_Range;
                           Distance : out Lat_Lon.Distance);

  -- Apply a great circle arc (heading and distance in Nm) to point A
  --  and return destination point B
  function Apply_Route (A : Lat_Lon.Lat_Lon_Rad_Rec;
                        Heading  : Conv.Rad_Coord_Range;
                        Distance : Lat_Lon.Distance)
           return Lat_Lon.Lat_Lon_Rad_Rec;

end Great_Circle;

