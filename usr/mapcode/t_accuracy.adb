with Argument, Basic_Proc, Rnd, Str_Util, As.U, Gets, My_Math, Normalization,
     Trace.Loggers;
with Mapcodes;
procedure T_Accuracy is

  use type Mapcodes.Real;

  -- Usage
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " [ <mapcode> | <lat_lon> ]");
    Basic_Proc.Put_Line_Error ("  <mapcode> ::= [ <context>:]<code>");
    Basic_Proc.Put_Line_Error ("  <lat_lon> ::= <latitude> <longitude>");
    Basic_Proc.Put_Line_Error ("Default: random lat_lon");
  end Usage;

  -- Error
  Give_Up : exception;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Give_Up;
  end Error;

  -- Get a lat or lon from arg
  function Get_Arg (Occ : Positive) return Mapcodes.Real is
  begin
    return Mapcodes.Real (Gets.Get_Llint_Real (
        Argument.Get_Parameter (Occurence => Occ)));
  end Get_Arg;

  -- Image of a real
  function Image (F : Mapcodes.Real) return String is
    R : Mapcodes.Real;
    Frac_Len : constant := 9;
  begin
    R := Mapcodes.Real (My_Math.Round_At (My_Math.Real (F), -Frac_Len));
    return Normalization.Normal_Fixed (My_Math.Real (R), Frac_Len + 5, 4, '0');
  end Image;

  -- Search the change of mapcode
  function Search (Cod, Ctx : String;
                   Precision : Mapcodes.Precisions;
                   Ref : Mapcodes.Coordinate;
                   Incr_Lat, Incr_Lon : Mapcodes.Real) return Mapcodes.Real is
    Cur_Coord, Prev_Coord : Mapcodes.Coordinate;
    Found : Boolean;
  begin
    Cur_Coord := Ref;
    loop
      -- Incr Lat or Lon
      Prev_Coord := Cur_Coord;
      Cur_Coord.Lat := Cur_Coord.Lat + Incr_Lat;
      Cur_Coord.Lon := Cur_Coord.Lon + Incr_Lon;
      -- Get the mapcodes
      declare
        Codes : constant Mapcodes.Mapcode_Infos
              := Mapcodes.Encode (Cur_Coord, Ctx, False, Precision);
      begin
        Found := False;
        for C in Codes'Range loop
          if Codes(C).Mapcode.Image = Cod then
            -- Until the mapcode changes
            Found := True;
            exit;
          end if;
        end loop;
      end;
      exit when not Found;
    end loop;
    return ( (if Incr_Lat /= 0.0 then Prev_Coord.Lat else Prev_Coord.Lon) );
  end Search;

  -- Expected precisions in meters
  Expected : constant array (Mapcodes.Precisions) of Mapcodes.Real
           := (0 => 7.49, 1 => 1.39, 2 => 0.251);

  -- Step for detecting change of mapcode
  Step : constant Mapcodes.Real := 1.0E-7;

  -- Random generator
  Gen : constant access Rnd.Generator := Rnd.Gen;
  -- Input and reference coordinates
  In_Coord, Ref_Coord : Mapcodes.Coordinate;
  -- Mapcode
  Ctx, Cod : As.U.Asu_Us;
  -- Separator index
  Index : Natural;
  -- Trace logger
  Logger : Trace.Loggers.Logger;

begin
  Logger.Init ("Accuracy");

  case Argument.Get_Nbre_Arg is
    when 0 =>
      -- If no arg then random lat, lon
      Gen.Randomize;
      In_Coord.Lat := Mapcodes.Real (Gen.Float_Random ( -90.0,  90.0));
      In_Coord.Lon := Mapcodes.Real (Gen.Float_Random (-180.0, 180.0));
    when 1 =>
      -- If one argument then a [<ctx>:]<mapcode>, get its lat, lon
      Argument.Get_Parameter (Cod);
      Index := Str_Util.Locate (Cod.Image, ":");
      if Index /= 0 then
        Ctx := Cod.Uslice (1, Index - 1);
        Cod.Delete (1, Index);
      end if;
      In_Coord := Mapcodes.Decode (Cod.Image, Ctx.Image);
    when 2 =>
      -- If two args then get lat, lon
      begin
        In_Coord.Lat := Get_Arg (1);
        In_Coord.Lon := Get_Arg (2);
      exception
        when others =>
          Error ("Invalid arguments");
      end;
    when others =>
      Error ("Invalid arguments");
  end case;

  Basic_Proc.Put_Line_Output (Image (In_Coord.Lat) & " "
                            & Image (In_Coord.Lon));
  Logger.Log_Debug ("Input coord " & Image (In_Coord.Lat) & " "
                                   & Image (In_Coord.Lon));

  -- For each precision
  for Precision in Mapcodes.Precisions loop
    -- Get all the mapcodes with this precision
    declare
       -- Mapcodes for these input coordinates
       Codes : constant Mapcodes.Mapcode_Infos
             := Mapcodes.Encode (In_Coord, Precision => Precision);
        -- Max and min coordinates
        Max, Min : Mapcodes.Coordinate;
        -- Distance
        Dist, T : Mapcodes.Real;
        Expect : constant Mapcodes.Real := Expected(Precision);
    begin
      Logger.Log_Debug ("Precision" & Precision'Img
                      & ", expecting " & Image (Expect));
      -- For each mapocode
      for C in Codes'Range loop
        -- Get the reference lat, lon
        Ref_Coord := Mapcodes.Decode (Codes(C).Mapcode.Image,
                                      Codes(C).Territory_Alpha_Code.Image);
        Logger.Log_Debug ("  Code " & Codes(C).Territory_Alpha_Code.Image
                        & ":" & Codes(C).Mapcode.Image);
        -- Sarch max and min lat, max and min lon
        Max.Lat := Search (Codes(C).Mapcode.Image,
                           Codes(C).Territory_Alpha_Code.Image, Precision,
                           Ref_Coord,
                           Step, 0.0);
        Min.Lat := Search (Codes(C).Mapcode.Image,
                           Codes(C).Territory_Alpha_Code.Image, Precision,
                           Ref_Coord,
                           -Step, 0.0);
        Max.Lon := Search (Codes(C).Mapcode.Image,
                           Codes(C).Territory_Alpha_Code.Image, Precision,
                           Ref_Coord,
                           0.0, Step);
        Min.Lon := Search (Codes(C).Mapcode.Image,
                           Codes(C).Territory_Alpha_Code.Image, Precision,
                           Ref_Coord,
                           0.0, -Step);
        -- Compute distance maxlat, maxlon, minlat, minlon
        -- Sqrt (delta_lat**2 + delta_lon**2)
        T := Max.Lat - Min.Lat;
        Dist := T * T;
        T := Max.Lon - Min.Lon;
        Dist := Dist + T * T;
        Dist := Mapcodes.Real (My_Math.Sqrt (My_Math.Real (Dist)));
        -- In meters: 1 deg = 60 Nm
        Dist := Dist * 60.0 * 1852.0;
        Logger.Log_Debug ("  Dist " & Image (Dist));
        -- Check that it is within precision
        if Dist > Expect then
          Basic_Proc.Put_Line_Error (
              "Mapcode " & Codes(C).Territory_Alpha_Code.Image
            & ":" & Codes(C).Mapcode.Image
            & " leads to incorrect precision " & Image (Dist)
            & " instead of " & Image (Expect));
        else
          Basic_Proc.Put_Line_Output (
              "Mapcode " & Codes(C).Territory_Alpha_Code.Image
            & ":" & Codes(C).Mapcode.Image
            & " OK");
        end if;
      end loop;
    end;
  end loop;
exception
  when Give_Up =>
    null;
end T_Accuracy;

