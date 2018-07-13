with Basic_Proc, Argument, Gets, My_Math, Normalization;
with Conv, Lat_Lon, Great_Circle;
procedure Hp_Gc is
  Lat1, Lon1, Lat2, Lon2 : My_Math.Real;
  A, B : Lat_Lon.Lat_Lon_Rad_Rec;
  H : Conv.Geo_Coord_Rec;
  D : Lat_Lon.Distance;
  R : My_Math.Real;
  I, J : Integer;
  Frac_Len : constant := 9;
  use type My_Math.Real;

  function Set_Lalo (Lat, Lon : My_Math.Real) return Lat_Lon.Lat_Lon_Rad_Rec is
    Llat, Llon : My_Math.Real;
  begin
    -- Normalize
    Llat := Lat;
    Llon := Lon;
    while Llat >  90.0  loop Llat := Llat - 90.0;  end loop;
    while Llat < -90.0  loop Llat := Llat + 90.0;  end loop;
    while Llon >  180.0 loop Llon := Llon - 180.0; end loop;
    while Llon < -180.0 loop Llon := Llon + 180.0; end loop;
    -- Set Lalo
    return Lalo : Lat_Lon.Lat_Lon_Rad_Rec do
      Lalo.X := Conv.Real2Rad (Llon);
      Lalo.Y := Conv.Real2Rad (Llat);
      Great_Circle.Logger.Log_Debug ("Got point OK:" & Lalo.X'Img & Lalo.Y'Img);
    end return;
  end Set_Lalo;

begin
  -- Get arguments
  if Argument.Get_Nbre_Arg = 2 then
    -- 2 mapcodes
    begin
      A := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 1));
      B := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 2));
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("ERROR. Invalid argument");
        Basic_Proc.Set_Error_Exit_Code;
        return;
    end;
  elsif Argument.Get_Nbre_Arg = 4 then
    -- 4 arguments: 2 lat lon
    begin
      Lat1 := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 1));
      Lon1 := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 2));
      Lat2 := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 3));
      Lon2 := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 4));
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("ERROR. Invalid argument");
        Basic_Proc.Set_Error_Exit_Code;
        return;
    end;
    A := Set_Lalo (Lat1, Lon1);
    B := Set_Lalo (Lat2, Lon2);
  else
    Basic_Proc.Put_Line_Error ("ERROR. Usage: "
         & Argument.Get_Program_Name
         & " <lat1> <lon1> <lat2> <lon2>");
    Basic_Proc.Put_Line_Error ("  or:         "
         & Argument.Get_Program_Name & " [<context>:]<mapcode> [<context>:]<mapcode>");
    Basic_Proc.Put_Line_Error ("<lat> and <long> in degrees (decimals).");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Great circle
  Great_Circle.Compute_Route (A, B, H, D);

  -- Display result, Heading, then dist in Nm, Km, m [, mm ]
  R := My_Math.Round_At (Conv.Geo2Real (H), -Frac_Len);
  Basic_Proc.Put_Output ("H: "
      & Normalization.Normal_Fixed (R, Frac_Len + 5, 4, '0'));
  I := Integer (My_Math.Round (My_Math.Real (D) * 1.852));
  Basic_Proc.Put_Output (", D:"
      & My_Math.Inte'Image (My_Math.Round (My_Math.Real (D))) & "Nm"
      & I'Img & "km");
  if I < 1000 then
    J := Integer (My_Math.Round (My_Math.Real (D) * 1852.0));
    Basic_Proc.Put_Output (J'Img & "m");
    if J < 1000 then
      J := Integer (My_Math.Round (My_Math.Real (D) * 1852000.0));
      Basic_Proc.Put_Output (J'Img & "mm");
    end if;
  end if;
  Basic_Proc.Put_Line_Output (".");

end Hp_Gc;

