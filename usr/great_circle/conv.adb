package body Conv is
  use My_Math;

  function Rad2Geo (Coord : Rad_Coord_Range) return Geo_Coord_Rec is
    Deg : C_Nbres.Reducted_Degree;
    Sr : My_Math.Real;
    Si : Natural;
    R : Geo_Coord_Rec;
  begin
    -- Convert to degrees (fraction)
    Deg := C_Nbres.To_Degree(Coord);

    R.Deg := Deg_Range(My_Math.Trunc(My_Math.Real(Deg)));

    -- Full seconds
    Sr := My_Math.Frac(My_Math.Real(Deg));
    Sr := Sr * 60.0 * 60.0;
    Si := Natural(My_Math.Trunc(Sr));

    -- Minutes
    R.Min := Si / 60;
    -- Seconds
    R.Sec := Si rem 60;
    return R;
  end Rad2Geo;

  function Geo2Rad (Coord : Geo_Coord_Rec) return Rad_Coord_Range is
    Deg : C_Nbres.Reducted_Degree;
    use C_Nbres;
  begin
    Deg := C_Nbres.Reducted_Degree(Coord.Deg);
    Deg := Deg + C_Nbres.Reducted_Degree(Coord.Min) / 60.0;
    Deg := Deg + C_Nbres.Reducted_Degree(Coord.Sec) / 60.0 / 60.0;
    return C_Nbres.To_Radian(Deg);
  end Geo2Rad;

end Conv;

