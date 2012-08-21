-- Converts DegMinSec to Radians and vice versa
with My_Math, C_Nbres;
package Conv is

  subtype Deg_Range is Natural range 0 .. 359;
  subtype Min_Range is Natural range 0 .. 59;
  subtype Sec_Range is Natural range 0 .. 59;
  subtype Hun_Range is Natural range 0 .. 99;
  subtype Ten_Range is Natural range 0 .. 9999;

  -- Degrees, Minutes, Seconds and Hundreds of seconds
  type Geo_Coord_Rec is record
    Deg : Deg_Range;
    Min : Min_Range;
    Sec : Sec_Range;
    Hun : Hun_Range;
  end record;

  Zero : constant Geo_Coord_Rec := (0, 0, 0, 0);

  -- Degrees, Minutes and 10 thousands
  type Dec_Coord_Rec is record
    Deg : Deg_Range;
    Ten : Ten_Range;
  end record;

  -- Radians
  subtype Rad_Range is C_Nbres.Radian;
  Pi : constant Rad_Range := Rad_Range(My_Math.Pi);

  subtype Rad_Coord_Range is C_Nbres.Reducted_Radian;


  -- Conversion DMS <-> Rad
  function Rad2Geo (Coord : Rad_Coord_Range) return Geo_Coord_Rec;

  function Geo2Rad (Coord : Geo_Coord_Rec) return Rad_Coord_Range;


  -- Conversion Dec <-> DMS
  function Dec2Geo (Coord : Dec_Coord_Rec) return Geo_Coord_Rec;

  function Geo2Dec (Coord : Geo_Coord_Rec) return Dec_Coord_Rec;

end Conv;

