package body Conv is
  use type My_Math.Real;

  function Rad2Geo (Coord : Rad_Coord_Range) return Geo_Coord_Rec is
    Deg : Complexes.Reducted_Degree;
    Tr, Sr : My_Math.Real;
    Ti, Si : Natural;
    R : Geo_Coord_Rec;
  begin
    -- Convert to degrees (fraction)
    Deg := Complexes.To_Degree(Coord);

    R.Deg := Deg_Range(My_Math.Trunc(My_Math.Real(Deg)));

    -- Full seconds and hundredths
    Sr := My_Math.Frac(My_Math.Real(Deg));
    Sr := Sr * 60.0 * 60.0;
    Tr := My_Math.Frac(Sr);
    Ti := Natural(My_Math.Round (Tr * 10000.0));
    Si := Natural(My_Math.Trunc(Sr));

    -- Check if hundredths was rounded to 10000
    if Ti = 10000 then
      Ti := 0;
      Si := Si + 1;
    end if;
    R.Ten := Ti;

    if Si = 60 * 60 then
      -- Rounded to next degree
      if R.Deg = Deg_Range'Last then
        R.Deg := Deg_Range'First;
      else
        R.Deg := R.Deg + 1;
      end if;
      R.Min := 0;
      R.Sec := 0;
    else
      -- Minutes
      R.Min := Si / 60;
      -- Seconds
      R.Sec := Si rem 60;
    end if;

    return R;
  end Rad2Geo;

  function Geo2Rad (Coord : Geo_Coord_Rec) return Rad_Coord_Range is
    Deg : Complexes.Reducted_Degree;
    use type Complexes.Degree;
  begin
    Deg := Complexes.Reducted_Degree(Coord.Deg);
    Deg := Deg + Complexes.Degree(Coord.Min) / 60.0;
    Deg := Deg + Complexes.Degree(Coord.Sec) / 60.0 / 60.0;
    Deg := Deg + Complexes.Degree(Coord.Ten) / 60.0 / 60.0 / 10000.0;
    return Complexes.To_Radian(Deg);
  end Geo2Rad;

  function Dec2Geo (Coord : Dec_Coord_Rec) return Geo_Coord_Rec is
    Deg : Complexes.Degree;
    Rad : Complexes.Reducted_Radian;
    use type Complexes.Degree, Complexes.Reducted_Radian;
  begin
    Deg := Complexes.Degree(Coord.Deg)
         + Complexes.Degree(Coord.Nan)
           / (Complexes.Degree(Nan_Range'Last) + 1.0);
    Rad := Complexes.To_Radian (Deg);
    return Rad2Geo (Rad);
  end Dec2Geo;

  function Geo2Dec (Coord : Geo_Coord_Rec) return Dec_Coord_Rec is
    Rad : Rad_Range;
    Deg : Complexes.Degree;
    R : My_Math.Real;
    I : My_Math.Inte;
    Dec : Dec_Coord_Rec;
  begin
    Rad := Geo2Rad (Coord);
    Deg := Complexes.To_Degree (Rad);
    R := My_Math.Round_At (My_Math.Real (Deg), -9);
    Dec.Deg := Deg_Range (My_Math.Trunc (R));

    I := My_Math.Round (My_Math.Frac(R) * My_Math.Real (Nan_Range'Last + 1));
    if I > My_Math.Inte(Nan_Range'Last) then
      if Dec.Deg = Deg_Range'Last then
        Dec.Deg := Deg_Range'First;
      else
        Dec.Deg := Dec.Deg + 1;
      end if;
      Dec.Nan := 0;
    else
      Dec.Nan := Nan_Range(I);
    end if;
    return Dec;
  end Geo2Dec;

  -- Conversion Dec <-> Rad
  function Rad2Dec (Coord : Rad_Coord_Range) return Dec_Coord_Rec is
    Deg : Complexes.Degree;
    R : My_Math.Real;
    I : My_Math.Inte;
    Dec : Dec_Coord_Rec;
  begin
    Deg := Complexes.To_Degree (Coord);
    R := My_Math.Round_At (My_Math.Real (Deg), -9);
    Dec.Deg := Deg_Range (My_Math.Trunc (R));

    I := My_Math.Round (My_Math.Frac(R) * My_Math.Real (Nan_Range'Last + 1));
    if I > My_Math.Inte(Nan_Range'Last) then
      if Dec.Deg = Deg_Range'Last then
        Dec.Deg := Deg_Range'First;
      else
        Dec.Deg := Dec.Deg + 1;
      end if;
      Dec.Nan := 0;
    else
      Dec.Nan := Nan_Range(I);
    end if;
    return Dec;
  end Rad2Dec;

  function Dec2Rad (Coord : Dec_Coord_Rec) return Rad_Coord_Range is
    R : My_Math.Real;
  begin
    R := My_Math.Real (Coord.Deg)
       + My_Math.Real (Coord.Nan) / My_Math.Real (Nan_Range'Last + 1);
    return Complexes.To_Radian (Complexes.Degree (R));
  end Dec2Rad;

  function Real2Rad (R : My_Math.Real) return Rad_Coord_Range is
  begin
    return Complexes.To_Radian (Complexes.Degree (R));
  end Real2Rad;

  function Rad2Real (Coord : Rad_Coord_Range) return My_Math.Real is
  begin
    return My_Math.Real (Complexes.To_Degree (Coord));
  end Rad2Real;

  function Real2Geo (R : My_Math.Real) return Geo_Coord_Rec is
    Deg : Complexes.Degree;
    Rad : Complexes.Reducted_Radian;
    use type Complexes.Degree, Complexes.Reducted_Radian;
  begin
    Deg := Complexes.Degree(R);
    Rad := Complexes.To_Radian (Deg);
    return Rad2Geo (Rad);
  end Real2Geo;

  function Geo2Real (Coord : Geo_Coord_Rec) return My_Math.Real is
    Rad : Rad_Range;
  begin
    Rad := Geo2Rad (Coord);
    return My_Math.Real (Complexes.To_Degree (Rad));
  end Geo2Real;

end Conv;

