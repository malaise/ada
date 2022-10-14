-- High precision great circle
-- Takes as input either two mapcodes or or two open location codes,
--   or two lat-long (in degres with decimals)
--   Outputs heading (in degrees with 9 decimals) and distance (down to the mm)
-- Takes as input either a mapcode or an open location code or a lat-long,
--   and a heading and distance
--   Outputs the mapcode or open location code or lat-long of the destination
with Olc;
with Basic_Proc, Argument, Gets, My_Math, Normalization, As.U.Utils,
     Str_Util.Regex;
with Units, Lat_Lon, Great_Circle, String_Util;
procedure Hp_Gc is
  type Mode_List is (Compute, Apply_Lalo, Apply_Mapcode, Apply_Olc);
  Mode : Mode_List;
  Lat1, Lon1, Lat2, Lon2 : Units.Degree;
  A, B : Lat_Lon.Lat_Lon_Rad_Rec;
  H : Units.Rad_Coord_Range;
  D : String_Util.Distance;
  Str, Unit : As.U.Asu_Us;
  R : My_Math.Real;
  Degs : Lat_Lon.Signed_Deg_Rec;
  I : Integer;
  Map_Precision : Lat_Lon.Map_Precisions;
  Olc_Precision : Lat_Lon.Olc_Precisions;
  Frac_Len : constant := 9;
  Km_In_Nm : constant := 1.852;
  use type My_Math.Real;

  -- Is input a mapcode or Olc, or raise Constraint_Error
  function Is_Mapcode (Str : String) return Boolean is
  begin
    if Str_Util.Locate (Str, ".") /= 0 then
      return True;
    elsif Str_Util.Locate (Str, "+") /= 0 then
      return False;
    else
      raise Constraint_Error;
    end if;
  end Is_Mapcode;

begin
  Great_Circle.Init_Logger;
  -- Get arguments:
  --  -c mapcode mapcode or -c olc olc or -c lat lon lat lon
  --  -a mapcode heading distance or -a olc heading distance
  --    or -a lat lon heading distance
  --    with -a mapcode or -a olc, then possibly -p precision
  begin
    if (Argument.Get_Nbre_Arg = 3
        or else Argument.Get_Nbre_Arg = 5)
    and then Argument.Get_Parameter (Occurence => 1) = "-c" then
      Mode := Compute;
      if Argument.Get_Nbre_Arg = 3 then
       if Is_Mapcode (Argument.Get_Parameter (Occurence => 2)) then
          -- 2 mapcodes
          A := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 2));
          B := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 3));
        else
          -- 2 open location codes
          A := Lat_Lon.Olc2Rad (Argument.Get_Parameter (Occurence => 2));
          B := Lat_Lon.Olc2Rad (Argument.Get_Parameter (Occurence => 3));
        end if;
      else
        -- 4 arguments: 2 lat lon
        Lat1 := Units.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 2)));
        Lon1 := Units.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 3)));
        Lat2 := Units.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 4)));
        Lon2 := Units.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 5)));
        A := Lat_Lon.Deg2Rad ( (Lat1, Lon1) );
        B := Lat_Lon.Deg2Rad ( (Lat2, Lon2) );
      end if;
    elsif    Argument.Get_Nbre_Arg = 4
    or else Argument.Get_Nbre_Arg = 5 then

      if Argument.Get_Nbre_Arg = 4 then
        if Is_Mapcode (Argument.Get_Parameter (Occurence => 2)) then
          -- 1 mapcode, 1 heading and 1 distance
          declare
            Mapcode : constant String
                    := Argument.Get_Parameter (Occurence => 2);
            -- Locate first "-" after territory
            Col : constant Natural := Str_Util.Locate (Mapcode, ":");
            Index : constant Natural := Str_Util.Locate (Mapcode, "-", Col);
          begin
            A := Lat_Lon.Mapcode2Rad (Mapcode);
            if Index = 0 then
              Map_Precision := 0;
            else
              Map_Precision := Mapcode'Length - Index;
            end if;
          end;
          Mode := Apply_Mapcode;
        else
          -- 1 olc, 1 heading and 1 distance
          declare
            Code : constant String := Argument.Get_Parameter (Occurence => 2);
          begin
            A := Lat_Lon.Mapcode2Rad (Code);
            Olc_Precision := Olc.Precision_Of (Code);
          end;
          Mode := Apply_Olc;
        end if;
        I := 3;
      elsif Argument.Get_Nbre_Arg = 5 then
        -- 1 lat long, 1 heading and 1 distance
        Lat1 := Units.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 2)));
        Lon1 := Units.Degree (Gets.Get_Int_Real (
            Argument.Get_Parameter (Occurence => 3)));
        A := Lat_Lon.Deg2Rad ( (Lat1, Lon1) );
        I := 4;
        Mode := Apply_Lalo;
      end if;

      -- The route
      R := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => I));
      H := Units.Deg2Rad (Units.Degree (R));
      -- Distance is positive or real, with unit "Nm", "km", "m" or "mm"
      Argument.Get_Parameter (Str, Occurence => I + 1);
      -- Split to extra unit
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
      raise Constraint_Error;
    end if;
  exception
    when others =>
      Basic_Proc.Put_Line_Error (
          "ERROR: Invalid argument. Usage: ");
      Basic_Proc.Put_Line_Error (
          Argument.Get_Program_Name & " <compute_route> | <apply_route>");
      Basic_Proc.Put_Line_Error (
          "<compute_route> ::= -c <point> <point> | -c <map_code> <map_code> | -c <olc> <olc>");
      Basic_Proc.Put_Line_Error (
          "<apply_route>   ::= -a <point> <route> | -a <map_code> <route> | -a <olc> <route>");
      Basic_Proc.Put_Line_Error (
          "<point>         ::= <lat> <lon>>");
      Basic_Proc.Put_Line_Error (
          "<map_code>      ::= [ <context>: ] <mapcode>");
      Basic_Proc.Put_Line_Error (
          "<route>         ::= <heading> <length><unit>");
      Basic_Proc.Put_Line_Error (
          "<unit>          ::= Nm | km | m | mm");
      Basic_Proc.Set_Error_Exit_Code;
      Basic_Proc.Set_Error_Exit_Code;
      return;
  end;

  if Mode = Compute then
    -- Compute and display great circle
    Great_Circle.Compute_Route (A, B, H, D);

    -- Display result, Heading, then dist in Nm, km [, m]  [, mm ]
    -- Nm with 5.6 digits, km with 5.6 digits,
    --  m with 3.3 digits and mm with 3 digits
    R := My_Math.Round_At (My_Math.Real (Units.Rad2Deg (H)), -Frac_Len);
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
      Degs := Lat_Lon.Rad2Deg (B);
      Basic_Proc.Put_Output (
        Normalization.Normal_Fixed (
          My_Math.Real (Degs.Lat), Frac_Len + 5, 4, '0') & " ");
      Basic_Proc.Put_Line_Output (
        Normalization.Normal_Fixed (
          My_Math.Real (Degs.Lon), Frac_Len + 5, 4, '0'));
    elsif Mode = Apply_Mapcode then
      -- Display Mapcode of destination
      Basic_Proc.Put_Line_Output (Lat_Lon.Rad2Mapcode (B, Map_Precision));
    else
      -- Display open location code of destination
      Basic_Proc.Put_Line_Output (Lat_Lon.Rad2Olc (B, Olc_Precision));
    end if;
  end if;

end Hp_Gc;

