-- Definition of DegMinSec, DegFrac, Radians, Degrees and conversions
with My_Math, Complexes;
package Units is

  subtype Deg_Range is Natural range 0 .. 359;
  subtype Min_Range is Natural range 0 .. 59;
  subtype Sec_Range is Natural range 0 .. 59;
  subtype Ten_Range is Natural range 0 .. 9999;
  subtype Nan_Range is Natural range 0 .. 999999999;

  -- Degrees, Minutes, Seconds and 10 milli
  type Geo_Coord_Rec is record
    Deg : Deg_Range;
    Min : Min_Range;
    Sec : Sec_Range;
    Ten : Ten_Range;
  end record;

  Zero : constant Geo_Coord_Rec := (0, 0, 0, 0);

  -- Degrees, Nanodegrees
  type Dec_Coord_Rec is record
    Deg : Deg_Range;
    Nan : Nan_Range;
  end record;

  -- Degrees and Radians
  subtype Degree is Complexes.Degree;
  subtype Radian is Complexes.Radian;

  -- 0 .. 360 and 0 .. 2*Pi
  subtype Deg_Coord_Range is Complexes.Reducted_Degree;
  subtype Rad_Coord_Range is Complexes.Reducted_Radian;
  Pi : constant Rad_Coord_Range := Rad_Coord_Range (My_Math.Pi);
  function Reduct (A : Degree) return Deg_Coord_Range renames Complexes.Reduct;
  function Reduct (A : Radian) return Rad_Coord_Range renames Complexes.Reduct;


  -- Conversion DMS <-> Rad
  function Rad2Geo (Coord : Rad_Coord_Range) return Geo_Coord_Rec;
  function Geo2Rad (Coord : Geo_Coord_Rec) return Rad_Coord_Range;

  -- Conversion Dec <-> DMS
  function Dec2Geo (Coord : Dec_Coord_Rec) return Geo_Coord_Rec;
  function Geo2Dec (Coord : Geo_Coord_Rec) return Dec_Coord_Rec;

  -- Conversion Dec <-> Rad
  function Dec2Rad (Coord : Dec_Coord_Rec) return Rad_Coord_Range;
  function Rad2Dec (Coord : Rad_Coord_Range) return Dec_Coord_Rec;

  -- Conversion Deg <-> Rad
  function Deg2Rad (Coord : Deg_Coord_Range) return Rad_Coord_Range;
  function Rad2Deg (Coord : Rad_Coord_Range) return Deg_Coord_Range;

  -- Conversion Deg <-> DMS
  function Deg2Geo (Coord : Deg_Coord_Range) return Geo_Coord_Rec;
  function Geo2Deg (Coord : Geo_Coord_Rec) return Deg_Coord_Range;

end Units;

