with Trace;
with Conv, Lat_Lon;

package Great_Circle is

  Logger : Trace.Logger;

  -- Compute heading and distance form point A to point B
  procedure Compute_Route (A, B : in Lat_Lon.Lat_Lon_Geo_Rec;
                           Heading  : out Conv.Geo_Coord_Rec;
                           Distance : out Lat_Lon.Distance);

end Great_Circle;

