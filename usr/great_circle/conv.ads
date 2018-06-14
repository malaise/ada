-- Converts DegMinSec to Radians and vice versa
with My_Math, Complexes;
package Conv is

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
    Ten : Nan_Range;
  end record;

  Zero : constant Geo_Coord_Rec := (0, 0, 0, 0);

  -- Degrees, Nanodegrees
  type Dec_Coord_Rec is record
    Deg : Deg_Range;
    Nan : Nan_Range;
  end record;

  -- Radians
  subtype Rad_Range is Complexes.Radian;

  subtype Rad_Coord_Range is Complexes.Reducted_Radian;
  Pi : constant Rad_Coord_Range := Rad_Coord_Range(My_Math.Pi);


  -- Conversion DMS <-> Rad
  function Rad2Geo (Coord : Rad_Coord_Range) return Geo_Coord_Rec;

  function Geo2Rad (Coord : Geo_Coord_Rec) return Rad_Coord_Range;


  -- Conversion Dec <-> DMS
  function Dec2Geo (Coord : Dec_Coord_Rec) return Geo_Coord_Rec;

  function Geo2Dec (Coord : Geo_Coord_Rec) return Dec_Coord_Rec;


  -- Conversion Dec <-> Rad
  function Dec2Rad (Coord : Dec_Coord_Rec) return Rad_Coord_Range;

  function Rad2Dec (Coord : Rad_Coord_Range) return Dec_Coord_Rec;


  -- Conversion real (-180 .. 180) <-> Rad
  function Real2Rad (R : My_Math.Real) return Rad_Coord_Range;

  function Rad2Real (Coord : Rad_Coord_Range) return My_Math.Real;


  -- Conversion real (-180 .. 180) <-> DMS
  function Real2Geo (R : My_Math.Real) return Geo_Coord_Rec;

  function Geo2Real (Coord : Geo_Coord_Rec) return My_Math.Real;

end Conv;

