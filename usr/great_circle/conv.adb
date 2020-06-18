package body Conv is
  use type My_Math.Real;

  function Rad2Geo (Coord : Rad_Coord_Range) return Geo_Coord_Rec is
  begin
    return Deg2Geo (Complexes.To_Degree(Coord));
  end Rad2Geo;

  function Geo2Rad (Coord : Geo_Coord_Rec) return Rad_Coord_Range is
  begin
    return Complexes.To_Radian (Geo2Deg (Coord));
  end Geo2Rad;

  function Dec2Geo (Coord : Dec_Coord_Rec) return Geo_Coord_Rec is
    Deg : Complexes.Degree;
    Rad : Complexes.Reducted_Radian;
    use type Complexes.Degree;
  begin
    Deg := Complexes.Degree (Coord.Deg)
         + Complexes.Degree (Coord.Nan)
           / (Complexes.Degree (Nan_Range'Last) + 1.0);
    Rad := Complexes.To_Radian (Deg);
    return Rad2Geo (Rad);
  end Dec2Geo;

  function Geo2Dec (Coord : Geo_Coord_Rec) return Dec_Coord_Rec is
    Rad : Rad_Coord_Range;
    Deg : Degree;
    R : My_Math.Real;
    I : My_Math.Inte;
    Dec : Dec_Coord_Rec;
  begin
    Rad := Geo2Rad (Coord);
    Deg := Complexes.To_Degree (Rad);
    R := My_Math.Round_At (My_Math.Real (Deg), -9);
    Dec.Deg := Deg_Range (My_Math.Trunc (R));

    I := My_Math.Round (My_Math.Frac(R) * My_Math.Real (Nan_Range'Last + 1));
    if I > My_Math.Inte (Nan_Range'Last) then
      if Dec.Deg = Deg_Range'Last then
        Dec.Deg := Deg_Range'First;
      else
        Dec.Deg := Dec.Deg + 1;
      end if;
      Dec.Nan := 0;
    else
      Dec.Nan := Nan_Range (I);
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

    I := My_Math.Round (My_Math.Frac (R) * My_Math.Real (Nan_Range'Last + 1));
    if I > My_Math.Inte(Nan_Range'Last) then
      if Dec.Deg = Deg_Range'Last then
        Dec.Deg := Deg_Range'First;
      else
        Dec.Deg := Dec.Deg + 1;
      end if;
      Dec.Nan := 0;
    else
      Dec.Nan := Nan_Range (I);
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

  function Deg2Rad (Coord : Deg_Coord_Range) return Rad_Coord_Range is
  begin
    return Complexes.To_Radian (Coord);
  end Deg2Rad;

  function Rad2Deg (Coord : Rad_Coord_Range) return Deg_Coord_Range is
  begin
    return Complexes.To_Degree (Coord);
  end Rad2Deg;

  function Deg2Geo (Coord : Deg_Coord_Range) return Geo_Coord_Rec is
    Tr, Sr : My_Math.Real;
    Ti, Si : Natural;
    Geo : Geo_Coord_Rec;
  begin

    Geo.Deg := Deg_Range (My_Math.Trunc (My_Math.Real (Coord)));

    -- Full seconds and hundredths
    Sr := My_Math.Frac (My_Math.Real (Coord));
    Sr := Sr * 60.0 * 60.0;
    Tr := My_Math.Frac(Sr);
    Ti := Natural (My_Math.Round (Tr * 10000.0));
    Si := Natural (My_Math.Trunc (Sr));

    -- Check if hundredths was rounded to 10000
    if Ti = 10000 then
      Ti := 0;
      Si := Si + 1;
    end if;
    Geo.Ten := Ti;

    if Si = 60 * 60 then
      -- Rounded to next degree
      if Geo.Deg = Deg_Range'Last then
        Geo.Deg := Deg_Range'First;
      else
        Geo.Deg := Geo.Deg + 1;
      end if;
      Geo.Min := 0;
      Geo.Sec := 0;
    else
      -- Minutes
      Geo.Min := Si / 60;
      -- Seconds
      Geo.Sec := Si rem 60;
    end if;

    return Geo;
  end Deg2Geo;

  function Geo2Deg (Coord : Geo_Coord_Rec) return Deg_Coord_Range is
    Deg : Deg_Coord_Range;
    use type Complexes.Degree;
  begin
    Deg := Deg_Coord_Range (Coord.Deg);
    Deg := Deg + Complexes.Degree (Coord.Min) / 60.0;
    Deg := Deg + Complexes.Degree (Coord.Sec) / 60.0 / 60.0;
    Deg := Deg + Complexes.Degree (Coord.Ten) / 60.0 / 60.0 / 10000.0;
    return Deg;
  end Geo2Deg;

end Conv;

