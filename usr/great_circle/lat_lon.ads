-- Convert lat/long, radians, mapcodes
with Mapcodes, Olc, Geohash36, Geohash;
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

  -- Positive fractions of degree
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

  -- Signed degrees (-180 .. 180)
  use type Units.Degree;
  subtype Signed_Deg is Units.Degree range -180.0 .. 180.0;
  type Signed_Deg_Rec is record
    Lat : Signed_Deg;
    Lon : Signed_Deg;
  end record;

  --  00.00.00 <= Lat.Coord <=  90.00.00
  -- 000.00.00 <= Lon.Coord <= 180.00.00
  function Is_Lat_Lon_Ok (Lat_Lon_Geo : Lat_Lon_Geo_Rec) return Boolean;
  function Is_Lat_Lon_Ok (Lat_Lon_Dec : Lat_Lon_Dec_Rec) return Boolean;

  function Rad2Geo (Coord : Lat_Lon_Rad_Rec) return Lat_Lon_Geo_Rec;
  function Geo2Rad (Coord : Lat_Lon_Geo_Rec) return Lat_Lon_Rad_Rec;

  function Dec2Geo (Coord : Lat_Lon_Dec_Rec) return Lat_Lon_Geo_Rec;
  function Geo2Dec (Coord : Lat_Lon_Geo_Rec) return Lat_Lon_Dec_Rec;

  function Sig2Geo (Coord : Signed_Deg_Rec) return Lat_Lon_Geo_Rec;
  function Geo2Sig (Coord : Lat_Lon_Geo_Rec) return Signed_Deg_Rec;

  function Rad2Dec (Coord : Lat_Lon_Rad_Rec) return Lat_Lon_Dec_Rec;
  function Dec2Rad (Coord : Lat_Lon_Dec_Rec) return Lat_Lon_Rad_Rec;

  -- Mapcodes
  -- Input Str is [ <Context>: ] <mapcode>, output is the international mapcode
  subtype Map_Precisions is Mapcodes.Precisions;
  Default_Map_Precision : constant Map_Precisions := 2;
  -- Mapcode <-> Deg
  function Mapcode2Deg (Str : String) return Signed_Deg_Rec;
  function Deg2Mapcode (Coord : Signed_Deg_Rec;
                        Precision : Map_Precisions := Default_Map_Precision)
           return String;
  -- Mapcode <-> Rad
  function Mapcode2Rad (Str : String) return Lat_Lon_Rad_Rec;
  function Rad2Mapcode (Coord : Lat_Lon_Rad_Rec;
                        Precision : Map_Precisions := Default_Map_Precision)
           return String;

  -- Open Loc Code
  subtype Olc_Precisions is Olc.Precision_Range;
  Default_Olc_Precision : constant Olc_Precisions := Olc.Default_Precision;
  -- Open Loc Code <-> Deg
  function Olc2Deg (Code : Olc.Code_Type) return Signed_Deg_Rec;
  function Deg2Olc (Coord : Signed_Deg_Rec;
                    Precision : Olc_Precisions := Default_Olc_Precision)
           return Olc.Code_Type;
  -- Open Loc Code <-> Rad
  function Olc2Rad (Code : Olc.Code_Type) return Lat_Lon_Rad_Rec;
  function Rad2Olc (Coord : Lat_Lon_Rad_Rec;
                    Precision : Olc_Precisions := Default_Olc_Precision)
           return Olc.Code_Type;

  -- Geohash36
  subtype Gh36_Precisions is Geohash36.Precision_Range;
  Default_Gh36_Precision : constant Gh36_Precisions
                         := Geohash36.Default_Precision;
  -- Geohash36 <-> Deg
  function Gh362Deg (Code : Geohash36.Code_Type) return Signed_Deg_Rec;
  function Deg2Gh36 (Coord : Signed_Deg_Rec;
                     Precision : Gh36_Precisions := Default_Gh36_Precision)
           return Geohash36.Code_Type;
  -- Geohash36 <-> Rad
  function Gh362Rad (Code : Geohash36.Code_Type) return Lat_Lon_Rad_Rec;
  function Rad2Gh36 (Coord : Lat_Lon_Rad_Rec;
                     Precision : Gh36_Precisions := Default_Gh36_Precision)
           return Geohash36.Code_Type;

  -- Geohash
  subtype Gh_Precisions is Geohash.Precision_Range;
  Default_Gh_Precision : constant Gh_Precisions := Geohash.Default_Precision;
  -- Geohash <-> Deg
  function Gh2Deg (Code : Geohash.Code_Type) return Signed_Deg_Rec;
  function Deg2Gh (Coord : Signed_Deg_Rec;
                   Precision : Gh_Precisions := Default_Gh_Precision)
           return Geohash36.Code_Type;
  -- Geohash <-> Rad
  function Gh2Rad (Code : Geohash.Code_Type) return Lat_Lon_Rad_Rec;
  function Rad2Gh (Coord : Lat_Lon_Rad_Rec;
                   Precision : Gh_Precisions := Default_Gh_Precision)
           return Geohash36.Code_Type;
  -- Signed Deg <-> Rad
  function Deg2Rad (Coord : Signed_Deg_Rec) return Lat_Lon_Rad_Rec;
  function Rad2Deg (Coord : Lat_Lon_Rad_Rec) return Signed_Deg_Rec;

end Lat_Lon;

