-- High precision great circle
-- Takes as input either two mapcodes or two lat-long (in degres with decimals)
-- Outputs Heading (in degrees with 9 decimals) and distance (down to the mm)
with Basic_Proc, Argument, Gets, My_Math, Normalization, As.U.Utils,
     Str_Util.Regex;
with Conv, Lat_Lon, Great_Circle, String_Util;
procedure Hp_Gc is
  type Mode_List is (Compute, Apply_Lalo, Apply_Mapcode);
  Mode : Mode_List;
  Lat1, Lon1, Lat2, Lon2 : Conv.Degree;
  A, B : Lat_Lon.Lat_Lon_Rad_Rec;
  H : Conv.Rad_Coord_Range;
  D : String_Util.Distance;
  Str, Unit : As.U.Asu_Us;
  R : My_Math.Real;
  I : Integer;
  Frac_Len : constant := 9;
  Km_In_Nm : constant := 1.852;
  use type My_Math.Real;

  function Set_Lalo (Lat, Lon : Conv.Degree) return Lat_Lon.Lat_Lon_Rad_Rec is
    Llat, Llon : Conv.Degree;
    use type Conv.Degree;
  begin
    Llat := Lat;
    Llon := Lon;
    if Llat < 0.0 and then Llat > -90.0 then
      Llat := Conv.Reduct (Llat);
    end if;
    if Llon < 0.0 and then Llon > -180.0 then
      Llon := Conv.Reduct (Llon);
    end if;
    -- Set Lalo
    return Lalo : Lat_Lon.Lat_Lon_Rad_Rec do
      Lalo.X := Conv.Deg2Rad (Llon);
      Lalo.Y := Conv.Deg2Rad (Llat);
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
      Mode := Compute;
      if Argument.Get_Nbre_Arg = 3 then
        -- 2 mapcodes
        A := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 2));
        B := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 3));
      else
        -- 4 arguments: 2 lat lon
        Lat1 := Conv.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 2)));
        Lon1 := Conv.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 3)));
        Lat2 := Conv.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 4)));
        Lon2 := Conv.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 5)));
        A := Set_Lalo (Lat1, Lon1);
        B := Set_Lalo (Lat2, Lon2);
      end if;
    elsif (Argument.Get_Nbre_Arg = 4
           or else Argument.Get_Nbre_Arg = 5)
    and then Argument.Get_Parameter (Occurence => 1) = "-a" then
      if Argument.Get_Nbre_Arg = 4 then
        -- 1 mapcode, 1 heading and 1 distance
        A := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 2));
        I := 3;
        Mode := Apply_Mapcode;
      else
        Lat1 := Conv.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 2)));
        Lon1 := Conv.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 3)));
        A := Set_Lalo (Lat1, Lon1);
        I := 4;
        Mode := Apply_Lalo;
      end if;
      R := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => I));
      H := Conv.Deg2Rad (Conv.Degree (R));
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
      D := String_Util.Distance (R);
    else
      Basic_Proc.Put_Line_Error ("ERROR. Usage: "
          & Argument.Get_Program_Name & " <compute_route> | <apply_route>");
      Basic_Proc.Put_Line_Error (
          "<compute_route> ::= -c <point> <point> | -c <map_code> <map_code>");
      Basic_Proc.Put_Line_Error (
          "<apply_route>   ::= -a <point> <route> | -r <map_code> <route>");
      Basic_Proc.Put_Line_Error (
          "<point>         ::= <lat> <lon>>");
      Basic_Proc.Put_Line_Error (
          "<map_code>      ::= [ <context>: ] <mapcode>");
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

  if Mode = Compute then
    -- Compute and display great circle
    Great_Circle.Compute_Route (A, B, H, D);

    -- Display result, Heading, then dist in Nm, km [, m]  [, mm ]
    -- Nm with 5.6 digits, km with 5.6 digits,
    --  m with 3.3 digits and mm with 3 digits
    R := My_Math.Round_At (My_Math.Real (Conv.Rad2Deg (H)), -Frac_Len);
    Basic_Proc.Put_Output ("H: "
        & Normalization.Normal_Fixed (R, Frac_Len + 5, 4, '0'));
    Basic_Proc.Put_Output (", D:"
        & Normalization.Normal_Fixed (D, 13, 6, '0') & "Nm");
    R := D * Km_In_Nm;
    Basic_Proc.Put_Output (Normalization.Normal_Fixed (R, 13, 6, '0') & "km");
    R := R * 1000.0;
    if R < 1000.0 then
      Basic_Proc.Put_Output (Normalization.Normal_Fixed (R, 8, 4, '0') & "m");
      R := R * 1000.0;
      if R < 1000.0 then
        I := Integer (My_Math.Round (R));
        Basic_Proc.Put_Output (I'Img & "mm");
      end if;
    end if;
    Basic_Proc.Put_Line_Output (".");
  else
    -- Apply great circle and display target
    B := Great_Circle.Apply_Route (A, H, D);
    -- Lat and lon of B
    if Mode = Apply_Lalo then
      -- Display Lat Long of destination
      R := My_Math.Real (Conv.Rad2Deg (B.Y));
      -- Latitude from -90 to 90
      if R > 180.0 then
        R := -360.0 + R;
      end if;
      Basic_Proc.Put_Output (
        Normalization.Normal_Fixed (R, Frac_Len + 5, 4, '0') & " ");
      R := My_Math.Real (Conv.Rad2Deg (B.X));
      -- Longitude from -180 to 180
      if R > 180.0 then
        R := -360.0 + R;
      end if;
      Basic_Proc.Put_Line_Output (
        Normalization.Normal_Fixed (R, Frac_Len + 5, 4, '0'));
   else
     -- Display Mapcode of destination
     Basic_Proc.Put_Line_Output (Lat_Lon.Rad2Mapcode (B));
   end if;
  end if;

end Hp_Gc;

