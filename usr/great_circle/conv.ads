with My_Math, C_Nbres;
package Conv is

  subtype Deg_Range is Natural range 0 .. 359;
  subtype Min_Range is Natural range 0 .. 59;
  subtype Sec_Range is Natural range 0 .. 59;

  type Geo_Coord_Rec is record
    Deg : Deg_Range;
    Min : Min_Range;
    Sec : Sec_Range;
  end record;

  subtype Rad_Range is C_Nbres.Radian;
  Pi : constant Rad_Range := Rad_Range(My_Math.Pi);

  subtype Rad_Coord_Range is C_Nbres.Reducted_Radian;

  function Rad2Geo (Coord : Rad_Coord_Range) return Geo_Coord_Rec;

  function Geo2Rad (Coord : Geo_Coord_Rec) return Rad_Coord_Range;

end Conv;

