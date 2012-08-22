with Round_At;
package body Conv is
  use My_Math;

  function Rad2Geo (Coord : Rad_Coord_Range) return Geo_Coord_Rec is
    Deg : C_Nbres.Reducted_Degree;
    Hr, Sr : My_Math.Real;
    Hi, Si : Natural;
    R : Geo_Coord_Rec;
  begin
    -- Convert to degrees (fraction)
    Deg := C_Nbres.To_Degree(Coord);

    R.Deg := Deg_Range(My_Math.Trunc(My_Math.Real(Deg)));

    -- Full seconds and hundredths
    Sr := My_Math.Frac(My_Math.Real(Deg));
    Sr := Sr * 60.0 * 60.0;
    Hr := My_Math.Frac(Sr);
    Hi := Natural(My_Math.Round (Hr * 100.0));
    Si := Natural(My_Math.Trunc(Sr));

    -- Check if hundredths was rounded to 100
    if Hi = 100 then
      Hi := 0;
      Si := Si + 1;
    end if;
    R.Hun := Hi;

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
    Deg : C_Nbres.Reducted_Degree;
    use type C_Nbres.Degree;
  begin
    Deg := C_Nbres.Reducted_Degree(Coord.Deg);
    Deg := Deg + C_Nbres.Reducted_Degree(Coord.Min) / 60.0;
    Deg := Deg + C_Nbres.Reducted_Degree(Coord.Sec) / 60.0 / 60.0;
    Deg := Deg + C_Nbres.Reducted_Degree(Coord.Hun) / 60.0 / 60.0 / 100.0;
    return C_Nbres.To_Radian(Deg);
  end Geo2Rad;

  function Dec2Geo (Coord : Dec_Coord_Rec) return Geo_Coord_Rec is
    Deg : C_Nbres.Degree;
    Rad : C_Nbres.Reducted_Radian;
    use type C_Nbres.Degree, C_Nbres.Reducted_Radian;
  begin
    Deg := C_Nbres.Degree(Coord.Deg)
         + C_Nbres.Degree(Coord.Ten) / (C_Nbres.Degree(Ten_Range'Last) + 1.0);
    Rad := C_Nbres.To_Radian (Deg);
    return Rad2Geo (Rad);
  end Dec2Geo;

  function Geo2Dec (Coord : Geo_Coord_Rec) return Dec_Coord_Rec is
    Rad : Rad_Range;
    Deg : C_Nbres.Degree;
    R : My_Math.Real;
    I : My_Math.Inte;
    Dec : Dec_Coord_Rec;
  begin
    Rad := Geo2Rad (Coord);
    Deg := C_Nbres.To_Degree (Rad);
    R := Round_At (My_Math.Real (Deg), -4);
    Dec.Deg := Deg_Range (My_Math.Trunc (R));

    I := My_Math.Round (My_Math.Frac(R) * My_Math.Real (Ten_Range'Last + 1));
    if I > My_Math.Inte(Ten_Range'Last) then
      if Dec.Deg = Deg_Range'Last then
        Dec.Deg := Deg_Range'First;
      else
        Dec.Deg := Dec.Deg + 1;
      end if;
      Dec.Ten := 0;
    else
      Dec.Ten := Ten_Range(I);
    end if;
    return Dec;

  end Geo2Dec;

end Conv;

