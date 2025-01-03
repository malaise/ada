-- High precision great circle
-- Takes as input either
--   * two mapcodes
--   * or two open location codes,
--   * or two geohash36 codes
--   * or two geohash codes
--   * or two lat/lon (in degres with decimals)
--   Outputs heading (in degrees with 9 decimals) and distance (down to the mm)
-- Takes as input one code as above and  and a heading and distance
--   Outputs the code the destination
with Olc;
with Basic_Proc, Argument, Gets, My_Math, Normalization, As.U.Utils,
     Str_Util.Regex, Reg_Exp, Int_Img;
with Units, Lat_Lon, Great_Circle, String_Util;
procedure Hp_Gc is
  package Olc_Lib renames Olc;
  type Mode_List is (Compute, Apply);
  type Kind_List is (Sexa, Deci, Mapcode, Olc, Gh36, Gh);
  Mode : Mode_List;
  Kind : Kind_List;
  Lat1, Lon1, Lat2, Lon2 : Units.Degree;
  A, B : Lat_Lon.Lat_Lon_Rad_Rec;
  H : Units.Rad_Coord_Range;
  D : String_Util.Distance;
  Str, Unit : As.U.Asu_Us;
  R : My_Math.Real;
  Degs : Lat_Lon.Signed_Deg_Rec;
  I : Positive;
  Mm : Natural;
  Map_Precision : Lat_Lon.Map_Precisions;
  Olc_Precision : Lat_Lon.Olc_Precisions;
  Gh36_Precision : Lat_Lon.Gh36_Precisions;
  Gh_Precision : Lat_Lon.Gh_Precisions;
  Frac_Len : constant := 9;
  Km_In_Nm : constant := 1.852;
  use type My_Math.Real;

  -- Is input a mapcode,  Olc, Gh36 or Gh, or raise Constraint_Error
  Gh36_Pat : constant String :=  "[23456789bBCdDFgGhHjJKlLMnNPqQrRtTVWX]+";
  Gh_Pat   : constant String :=  "[0123456789bcdefghjkmnpqrstuvwxyz]+";
  function Kind_Of (Str : String) return Kind_List is
  begin
    if Str_Util.Locate (Str, "/") /= 0 then
      if      Str_Util.Locate (Str, "N") /= 0
      or else Str_Util.Locate (Str, "S") /= 0 then
        return Sexa;
      else
        return Deci;
      end if;
    elsif Str_Util.Locate (Str, ".") /= 0 then
      return Mapcode;
    elsif Str_Util.Locate (Str, "+") /= 0 then
      return Olc;
    elsif Str_Util.Locate (Str, "@GH36") = Str'Last - 4 then
      return Gh36;
    elsif Str_Util.Locate (Str, "@GH") = Str'Last - 2 then
      return Gh;
    elsif Reg_Exp.Match(Gh36_Pat, Str, True)
    and then Reg_Exp.Match(Gh_Pat, Str, True) then
      -- Ambiguous
      Basic_Proc.Put_Line_Error ( "ERROR: Ambiguous argument");
      raise Constraint_Error;
    elsif Reg_Exp.Match(Gh36_Pat, Str, True) then
      return Gh36;
    elsif Reg_Exp.Match(Gh_Pat, Str, True) then
      return Gh;
   else
      raise Constraint_Error;
    end if;
  end Kind_Of;

  -- Strip the potential tail (@...) of a Geohash36 or Geohash code
  function Strip (Code : String) return String is
    Index : constant Natural := Str_Util.Locate (Code, "@");
  begin
    if Index = 0 then
      return Code;
    else
      return Code (Code'First .. Index - 1);
    end if;
  end Strip;

begin
  Great_Circle.Init_Logger;
  -- Get arguments:
  --  -c mapcode mapcode or -c olc olc or -c lat/lon lat/lon
  --  -a mapcode heading distance or -a olc heading distance
  --    or -a lat lon heading distance
  --    with -a mapcode or -a olc, then possibly -p precision
  begin
    if Argument.Get_Nbre_Arg = 3
    and then Argument.Get_Parameter (Occurence => 1) = "-c" then
      Mode := Compute;
      Kind := Kind_Of (Argument.Get_Parameter (Occurence => 2));
      case Kind is
        when Sexa =>
          -- 2 arguments lat/lon in sexadecimal
          A := Lat_Lon.Geo2Rad (String_Util.Lstr2Geo (
                                  Argument.Get_Parameter(2)));
          B := Lat_Lon.Geo2Rad (String_Util.Lstr2Geo (
                                  Argument.Get_Parameter(3)));
        when Deci =>
          -- 2 arguments lat/lon in decimal
          Str := As.U.Tus (Argument.Get_Parameter (Occurence => 2) );
          I := Str.Locate ("/");
          Lat1 := Units.Degree (Gets.Get_Int_Real (Str.Slice (1, I - 1)));
          Lon1 := Units.Degree (Gets.Get_Int_Real (
                                 Str.Slice (I + 1, Str.Length)));
          Str := As.U.Tus (Argument.Get_Parameter (Occurence => 3) );
          I := Str.Locate ("/");
          Lat2 := Units.Degree (Gets.Get_Int_Real (Str.Slice (1, I - 1)));
          Lon2 := Units.Degree (Gets.Get_Int_Real (
                                 Str.Slice (I + 1, Str.Length)));
          A := Lat_Lon.Deg2Rad ( (Lat1, Lon1) );
          B := Lat_Lon.Deg2Rad ( (Lat2, Lon2) );
        when Mapcode =>
          -- 2 mapcodes
          A := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 2));
          B := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter (Occurence => 3));
        when Olc =>
          -- 2 open location codes
          A := Lat_Lon.Olc2Rad (Argument.Get_Parameter (Occurence => 2));
          B := Lat_Lon.Olc2Rad (Argument.Get_Parameter (Occurence => 3));
        when Gh36 =>
          -- 2 Geohash36 codes
          A := Lat_Lon.Gh362Rad (
            Strip (Argument.Get_Parameter (Occurence => 2)));
          B := Lat_Lon.Gh362Rad (
            Strip (Argument.Get_Parameter (Occurence => 3)));
        when Gh =>
          -- 2 Geohash codes
          A := Lat_Lon.Gh2Rad (
            Strip (Argument.Get_Parameter (Occurence => 2)));
          B := Lat_Lon.Gh2Rad (
            Strip (Argument.Get_Parameter (Occurence => 3)));
      end case;
    elsif Argument.Get_Nbre_Arg = 4
    and then Argument.Get_Parameter (Occurence => 1) = "-a" then
      Mode := Apply;
      Kind := Kind_Of (Argument.Get_Parameter (Occurence => 2));
      case Kind is
        when Sexa =>
          -- 1 lat/long in sexadecimal, 1 heading and 1 distance
          A := Lat_Lon.Geo2Rad (String_Util.Lstr2Geo (
                                  Argument.Get_Parameter(2)));
        when Deci =>
          -- 1 lat/long in decimal, 1 heading and 1 distance
          Str := As.U.Tus (Argument.Get_Parameter (Occurence => 2) );
          I := Str.Locate ("/");
          Lat1 := Units.Degree (Gets.Get_Int_Real (Str.Slice (1, I - 1)));
          Lon1 := Units.Degree (Gets.Get_Int_Real (
                                 Str.Slice (I + 1, Str.Length)));
          A := Lat_Lon.Deg2Rad ( (Lat1, Lon1) );
        when Mapcode =>
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
          Kind := Mapcode;
        when Olc =>
          -- 1 olc, 1 heading and 1 distance
          declare
            Code : constant String := Argument.Get_Parameter (Occurence => 2);
          begin
            A := Lat_Lon.Mapcode2Rad (Code);
            Olc_Precision := Olc_Lib.Precision_Of (Code);
          end;
          Kind := Olc;
        when Gh36 =>
          -- 1 Geohash36 code, 1 heading and 1 distance
          declare
            Code : constant String
                 := Strip (Argument.Get_Parameter (Occurence => 2));
          begin
            A := Lat_Lon.Gh362Rad (Code);
            Gh36_Precision := Code'Length;
          end;
          Kind := Gh36;
        when Gh =>
          -- 1 Geohash code, 1 heading and 1 distance
          declare
            Code : constant String
                 := Strip (Argument.Get_Parameter (Occurence => 2));
          begin
            A := Lat_Lon.Gh2Rad (Code);
            Gh_Precision := Code'Length;
          end;
          Kind := Gh36;
      end case;

      -- The route heading
      if Kind = Sexa then
        H := Units.Geo2Rad (String_Util.Lstr2Geoangle (
                                Argument.Get_Parameter (Occurence => 3)));
      else
        R := Gets.Get_Int_Real (Argument.Get_Parameter (Occurence => 3));
        H := Units.Deg2Rad (Units.Degree (R));
      end if;
      -- The route length
      -- Distance is positive or real, with unit "Nm", "km", "m" or "mm"
      Argument.Get_Parameter (Str, Occurence => 4);
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
          "ERROR: Invalid argument.");
      Basic_Proc.Put_Line_Error (
          "Usage: " & Argument.Get_Program_Name
        & " <compute_route> | <apply_route>");
      Basic_Proc.Put_Line_Error (
          "<compute_route> ::= -c <point> <point> | -c <code> <code>");
      Basic_Proc.Put_Line_Error (
          "<apply_route>   ::= -a <point> <route> | -a <code> <route>");
      Basic_Proc.Put_Line_Error (
          "<point>         ::= <sexa> | <deci>");
      Basic_Proc.Put_Line_Error (
          "<sexa>     ::= add.mm.ssssss/oddd.mm.ssssss");
      Basic_Proc.Put_Line_Error (
          "  where a is N or S and o is E or W.");
      Basic_Proc.Put_Line_Error (
          "<deci>         ::= D.d/D.d");
      Basic_Proc.Put_Line_Error (
          "  where D is positivie or negative, and d up to 9 digits");
      Basic_Proc.Put_Line_Error (
          "<code>      ::= <map_code> | <olc> | <geohash36> | <geohash>");
      Basic_Proc.Put_Line_Error (
          "<map_code>  ::= [ <context>: ] <mapcode>");
      Basic_Proc.Put_Line_Error (
          "<olc>       ::= { <letter_or_digit> } + [ { <letter_or_digit> } ]");
      Basic_Proc.Put_Line_Error (
          "<geohash36> ::= { <letter_or_digit> } [ @GH36 ]");
      Basic_Proc.Put_Line_Error (
          "<geohash>   ::= { <letter_or_digit> } [ @GH ]");
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

    -- Display result: Heading, then dist
    Basic_Proc.Put_Output ("H: ");
    if Kind = Sexa then
      Basic_Proc.Put_Output (
        String_Util.Geoangle2Lstr (Units.Rad2Geo (H)));

    else
      R := My_Math.Round_At (My_Math.Real (Units.Rad2Deg (H)), -Frac_Len);
      Basic_Proc.Put_Output (
        Normalization.Normal_Fixed (R, Frac_Len + 5, 4, '0'));
    end if;
    -- Dist in Nm, km [, m]  [, mm ]
    -- Nm with 5.6 digits, km with 5.6 digits,
    --  m with 3.3 digits and mm with 3 digits
    Basic_Proc.Put_Output (", D:"
        & Normalization.Normal_Fixed (D, 13, 6, '0') & "Nm");
    R := D * Km_In_Nm;
    Basic_Proc.Put_Output (Normalization.Normal_Fixed (R, 13, 6, '0') & "km");
    R := R * 1000.0;
    if R < 1000.0 then
      Basic_Proc.Put_Output (Normalization.Normal_Fixed (R, 8, 4, '0') & "m");
      R := R * 1000.0;
      if R < 1000.0 then
        Mm := Integer (My_Math.Round (R));
        Basic_Proc.Put_Output (" " & Int_Img (Mm) & "mm");
      end if;
    end if;
    Basic_Proc.Put_Line_Output (".");
  else
    -- Apply great circle and display target
    B := Great_Circle.Apply_Route (A, H, D);
    case Kind is
      when Sexa =>
        -- Display Lat Lon in sexadecimal of destination
        Basic_Proc.Put_Line_Output (String_Util.Geo2Lstr (Lat_Lon.Rad2Geo (B)));
      when Deci =>
        -- Display Lat Lon in decimal of destination
        Degs := Lat_Lon.Rad2Deg (B);
        Basic_Proc.Put_Output (
          Str_Util.Strip (
            Normalization.Normal_Fixed (
              My_Math.Real (Degs.Lat), Frac_Len + 5, 4, '0'), Str_Util.Head)
          & "/");
        Basic_Proc.Put_Line_Output (
          Str_Util.Strip (
            Normalization.Normal_Fixed (
              My_Math.Real (Degs.Lon), Frac_Len + 5, 4, '0'), Str_Util.Head));
      when Mapcode =>
        -- Display Mapcode of destination
        Basic_Proc.Put_Line_Output (Lat_Lon.Rad2Mapcode (B, Map_Precision));
      when Olc =>
        -- Display open location code of destination
        Basic_Proc.Put_Line_Output (Lat_Lon.Rad2Olc (B, Olc_Precision));
      when Gh36 =>
        -- Display Geohash36 code of destination
        Basic_Proc.Put_Line_Output (Lat_Lon.Rad2Gh36 (B, Gh36_Precision));
      when Gh =>
        -- Display Geohash code of destination
        Basic_Proc.Put_Line_Output (Lat_Lon.Rad2Gh (B, Gh_Precision));
      end case;
  end if;

end Hp_Gc;

