-- Convert lat/long, radians, mapcodes
with Mapcodes;
with Units;
package Lat_Lon is

  type Lat_Geo_Rec is record
    North : Boolean;
    Coord : Units.Geo_Coord_Rec;
  end record;

  type Lon_Geo_Rec is record
    East : Boolean;
    Coord : Units.Geo_Coord_Rec;
  end record;

  type Lat_Lon_Geo_Rec is record
    Lat : Lat_Geo_Rec;
    Lon : Lon_Geo_Rec;
  end record;

  type Lat_Lon_Rad_Rec is record
    -- X is the longitude
    X : Units.Rad_Coord_Range;
    -- Y is the latitude
    Y : Units.Rad_Coord_Range;
  end record;

  --  00.00.00 <= Lat.Coord <=  90.00.00
  -- 000.00.00 <= Lon.Coord <= 180.00.00
  function Is_Lat_Lon_Ok (Lat_Lon_Geo : Lat_Lon_Geo_Rec) return Boolean;

  function Rad2Geo (Coord : Lat_Lon_Rad_Rec) return Lat_Lon_Geo_Rec;
  function Geo2Rad (Coord : Lat_Lon_Geo_Rec) return Lat_Lon_Rad_Rec;

  type Lat_Dec_Rec is record
    North : Boolean;
    Coord : Units.Dec_Coord_Rec;
  end record;

  type Lon_Dec_Rec is record
    East : Boolean;
    Coord : Units.Dec_Coord_Rec;
  end record;

  type Lat_Lon_Dec_Rec is record
    Lat : Lat_Dec_Rec;
    Lon : Lon_Dec_Rec;
  end record;

  --  00.0000 <= Lat.Coord <=  90.0000
  -- 000.0000 <= Lon.Coord <= 180.0000
  function Is_Lat_Lon_Ok (Lat_Lon_Dec : Lat_Lon_Dec_Rec) return Boolean;

  function Dec2Geo (Coord : Lat_Lon_Dec_Rec) return Lat_Lon_Geo_Rec;
  function Geo2Dec (Coord : Lat_Lon_Geo_Rec) return Lat_Lon_Dec_Rec;

  function Rad2Dec (Coord : Lat_Lon_Rad_Rec) return Lat_Lon_Dec_Rec;
  function Dec2Rad (Coord : Lat_Lon_Dec_Rec) return Lat_Lon_Rad_Rec;

  -- Mapcode <-> Rad
  -- Str is [ <Context>: ] <mapcode>
  function Mapcode2Rad (Str : String) return Lat_Lon.Lat_Lon_Rad_Rec;
  -- Return the international mapcode
  subtype Precisions is Mapcodes.Precisions;
  Default_Precision : constant Precisions := 2;
  function Rad2Mapcode (Coord : Lat_Lon.Lat_Lon_Rad_Rec;
                        Precision : Precisions := Default_Precision)
           return String;

end Lat_Lon;

