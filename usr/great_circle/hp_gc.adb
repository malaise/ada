-- High precision great circle
-- Takes as input either two mapcodes or two lat-long (in degres with decimals)
-- Outputs Heading (in degrees with 9 decimals) and distance (down to the mm)
with Basic_Proc, Argument, Gets, My_Math, Normalization, As.U.Utils,
     Str_Util.Regex;
with Conv, Lat_Lon, Great_Circle;
procedure Hp_Gc is
  Compute : Boolean;
  Lat1, Lon1, Lat2, Lon2 : My_Math.Real;
  A, B : Lat_Lon.Lat_Lon_Rad_Rec;
  H : Conv.Geo_Coord_Rec;
  D : Lat_Lon.Distance;
  Str, Unit : As.U.Asu_Us;
  R : My_Math.Real;
  I, J : Integer;
  Frac_Len : constant := 9;
  Km_In_Nm : constant := 1.852;
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
  Great_Circle.Init_Logger;
  -- Get arguments: -c mapcode mapcode or -c lat lon lat lon
  --                -a mapcode heading distance or -a lat lon heading distance
  begin
    if (Argument.Get_Nbre_Arg = 3
        or else Argument.Get_Nbre_Arg = 5)
    and then Argument.Get_Parameter (Occurence => 1) = "-c" then
      Compute := True;
      if Argument.Get_Nbre_Arg = 3 then
        -- 2 mapcodes
        A := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 2));
        B := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 3));
      else
        -- 4 arguments: 2 lat lon
        Lat1 := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 2));
        Lon1 := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 3));
        Lat2 := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 4));
        Lon2 := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 5));
        A := Set_Lalo (Lat1, Lon1);
        B := Set_Lalo (Lat2, Lon2);
      end if;
    elsif (Argument.Get_Nbre_Arg = 4
           or else Argument.Get_Nbre_Arg = 5)
    and then Argument.Get_Parameter (Occurence => 1) = "-a" then
      Compute := False;
      if Argument.Get_Nbre_Arg = 4 then
        -- 1 mapcode, 1 heading and 1 distance
        A := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 2));
        I := 3;
      else
        Lat1 := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 2));
        Lon1 := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 3));
        A := Set_Lalo (Lat1, Lon1);
        I := 4;
      end if;
      R := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => I));
      H := Conv.Real2Geo (R);
      -- Distance is positive or real, with unit "Nm", "km", "m" or "mm"
      Argument.Get_Parameter (Str, Occurence => I + 1);
      -- Split to extraft unit
      declare
        Words : constant As.U.Utils.Asu_Array
              := Str_Util.Regex.Split (Str.Image,
                                       "([0-9]+)(\.[0-9]+)?([Nkm]?m)", 3);
      begin
        if Words'Length <= 1 then
          raise Constraint_Error;
        end if;
        Unit := Words(Words'Last);
      end;
      -- Get val and parse unit
      Str.Trail (Unit.Length);
      R := Gets.Get_Int_Real (Str.Image);
      if Unit.Image = "Nm" then
        -- Nm is the target unit
        null;
      elsif Unit.Image = "km" then
        R := R / Km_In_Nm;
      elsif Unit.Image = "m" then
        R := R / Km_In_Nm / 1000.0;
      elsif Unit.Image = "mm" then
        R := R / Km_In_Nm / 1000.0 / 1000.0;
      else
        raise Constraint_Error;
      end if;
      D := Lat_Lon.Distance (R);
    else
      Basic_Proc.Put_Line_Error ("ERROR. Usage: "
          & Argument.Get_Program_Name & " <compute_route> | <apply_route>");
      Basic_Proc.Put_Line_Error (
          "<compute_route> ::= -c <point> <point> | -c <map_code> <map_code>");
      Basic_Proc.Put_Line_Error (
          "<apply_route>   ::= -a <point> <route> | -r <map_code> <route>");
      Basic_Proc.Put_Line_Error (
          "<point>         ::= <lat1> <lon1> <lat2> <lon2>");
      Basic_Proc.Put_Line_Error (
          "<map_code>      ::= <lat1> <lon1> <lat2> <lon2>");
      Basic_Proc.Put_Line_Error (
          "<route>         ::= <heading> <length><unit>");
      Basic_Proc.Put_Line_Error (
          "<unit>          ::= Nm | km | m | mm");
      Basic_Proc.Set_Error_Exit_Code;
      return;
    end if;
  exception
    when others =>
      Basic_Proc.Put_Line_Error ("ERROR. Invalid argument");
      Basic_Proc.Set_Error_Exit_Code;
      return;
  end;

  if Compute then
    -- Compute and display great circle
    Great_Circle.Compute_Route (A, B, H, D);

    -- Display result, Heading, then dist in Nm, Km, m [, mm ]
    R := My_Math.Round_At (Conv.Geo2Real (H), -Frac_Len);
    Basic_Proc.Put_Output ("H: "
        & Normalization.Normal_Fixed (R, Frac_Len + 5, 4, '0'));
    I := Integer (My_Math.Round (My_Math.Real (D) * Km_In_Nm));
    Basic_Proc.Put_Output (", D:"
        & My_Math.Inte'Image (My_Math.Round (My_Math.Real (D))) & "Nm"
        & I'Img & "km");
    if I < 1000 then
      J := Integer (My_Math.Round (My_Math.Real (D) * Km_In_Nm * 1_000.0));
      Basic_Proc.Put_Output (J'Img & "m");
      if J < 1000 then
        J := Integer (My_Math.Round (My_Math.Real (D) * Km_In_Nm * 1_000.0
                                     * 1_000.0));
        Basic_Proc.Put_Output (J'Img & "mm");
      end if;
    end if;
    Basic_Proc.Put_Line_Output (".");
  else
    -- Apply great circle and display target
    B := Great_Circle.Apply_Route (A, H, D);
    -- Lat and lon of B
    R := Conv.Rad2Real (B.Y);
    Basic_Proc.Put_Output (
      Normalization.Normal_Fixed (R, Frac_Len + 5, 4, '0') & " ");
    R := Conv.Rad2Real (B.X);
    Basic_Proc.Put_Line_Output (
      Normalization.Normal_Fixed (R, Frac_Len + 5, 4, '0'));
  end if;

end Hp_Gc;

