with Argument, Basic_Proc, Rnd, Str_Util, As.U, Gets, My_Math, Normalization,
     Trace.Loggers;
with Mapcodes;
procedure T_Accuracy is

  use type Mapcodes.Real;

  -- Trace logger
  Logger : Trace.Loggers.Logger;

  -- Emit warnings
  Warning : Boolean := False;

  -- Usage
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " [ -w | --warnings ] [ <mapcode> | <lat_lon> ]");
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

  -- Search a mapcode within an array
  function Search (Code : String; Codes : Mapcodes.Mapcode_Infos;
                   Log : Boolean) return Boolean is
  begin
    for C in Codes'Range loop
      if Log then
        Logger.Log_Debug ("    Search " & Code
                        & " v.s. " & Codes(C).Mapcode.Image);
      end if;
      if Codes(C).Mapcode.Image = Code then
        return True;
      end if;
    end loop;
    return False;
  end Search;

  -- Search the boundary change of mapcode
  function Bounds (Cod, Ctx : String;
                   Precision : Mapcodes.Precisions;
                   Ref : Mapcodes.Coordinate;
                   Incr_Lat, Incr_Lon : Mapcodes.Real) return Mapcodes.Real is
    Cur_Coord, Prev_Coord : Mapcodes.Coordinate;
  begin
    Cur_Coord := Ref;
    while abs Cur_Coord.Lat <= 90.0 and then abs Cur_Coord.Lon < 180.0 loop
      -- Incr Lat or Lon
      Prev_Coord := Cur_Coord;
      Cur_Coord.Lat := Cur_Coord.Lat + Incr_Lat;
      Cur_Coord.Lon := Cur_Coord.Lon + Incr_Lon;
      -- Get the mapcodes
      declare
        Codes : constant Mapcodes.Mapcode_Infos
              := Mapcodes.Encode (Cur_Coord, Ctx, False, Precision);
      begin
        -- Until the mapcode changes
        exit when not Search (Cod, Codes, False);
      end;
    end loop;
    return (if Incr_Lat /= 0.0 then Prev_Coord.Lat else Prev_Coord.Lon);
  end Bounds;

  -- Check that a lat lon generates a mapcode
  procedure Check_In (Lat, Lon : Mapcodes.Real;
                      Cod, Ctx : String;
                      Precision : Mapcodes.Precisions) is
    Codes : constant Mapcodes.Mapcode_Infos
          := Mapcodes.Encode ( (Lat, Lon), Ctx, False, Precision);
  begin
    if not Search (Cod, Codes, True) then
      if Warning then
        Basic_Proc.Put_Line_Error ("WARNING: " & Image (Lat)
          & " " & Image (Lon)
          & " does no generate mapcode " & Ctx & ":" & Cod);
        Basic_Proc.Set_Exit_Code (2);
      end if;
    end if;
  end Check_In;

  -- Expected precisions in meters from center of cell to any point in cell
  Expected : constant array (Mapcodes.Precisions) of Mapcodes.Real
           := (0 => 7.49, 1 => 1.39, 2 => 0.251);

  -- Step for detecting change of mapcode
  Step : constant Mapcodes.Real := 1.0E-7;

  -- Random generator
  Gen : Rnd.Generator renames Rnd.Gen.all;
  -- Input and reference coordinates
  In_Coord, Ref_Coord : Mapcodes.Coordinate;
  -- Mapcode
  Ctx, Cod : As.U.Asu_Us;
  -- Separator index
  Index : Natural;

  Narg, Sarg : Natural;

begin
  Logger.Init ("Accuracy");

  Narg := Argument.Get_Nbre_Arg;
  Sarg := 1;
  -- Help
  if Narg > 0 and then (Argument.Get_Parameter (Sarg) = "--help"
                        or else Argument.Get_Parameter (Sarg) = "-h")  then
    Usage;
    return;
  end if;
  -- Optional Warning flag
  if Narg > 0 and then (Argument.Get_Parameter (Sarg) = "--warnings"
                        or else Argument.Get_Parameter (Sarg) = "-w")  then
    Warning := True;
    Narg := Narg - 1;
    Sarg := Sarg + 1;
  end if;

  case Narg is
    when 0 =>
      -- If no arg then random lat, lon
      Gen.Randomize;
      In_Coord.Lat := Mapcodes.Real (Gen.Float_Random ( -90.0,  90.0));
      In_Coord.Lon := Mapcodes.Real (Gen.Float_Random (-180.0, 180.0));
    when 1 =>
      -- If one argument then a [<ctx>:]<mapcode>, get its lat, lon
      Argument.Get_Parameter (Cod, Occurence => Sarg);
      Index := Str_Util.Locate (Cod.Image, ":");
      if Index /= 0 then
        Ctx := Cod.Uslice (1, Index - 1);
        Cod.Delete (1, Index);
      end if;
      In_Coord := Mapcodes.Decode (Cod.Image, Ctx.Image);
    when 2 =>
      -- If two args then get lat, lon
      begin
        In_Coord.Lat := Get_Arg (Sarg);
        In_Coord.Lon := Get_Arg (Sarg + 1);
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
      -- Distances and Tempo coord
      Dist, Tmp, Cor : Mapcodes.Real;
      -- Expected max result
      Expect : constant Mapcodes.Real := 2.2 * Expected(Precision);
      use type My_Math.Real;
    begin
      Logger.Log_Debug ("Precision" & Precision'Img
                      & ", expecting " & Image (Expect));
      -- For each mapocode
      for C in Codes'Range loop
        -- Get the reference lat, lon
        Ref_Coord := Mapcodes.Decode (Codes(C).Mapcode.Image,
                                      Codes(C).Territory_Alpha_Code.Image);
        Logger.Log_Debug ("  Code " & Codes(C).Territory_Alpha_Code.Image
                        & ":" & Codes(C).Mapcode.Image
                        & " -> " & Image (Ref_Coord.Lat)
                        & " " & Image (Ref_Coord.Lon));
        -- Search max and min in lat, and max and min in lon
        Max.Lat := Bounds (Codes(C).Mapcode.Image,
                           Codes(C).Territory_Alpha_Code.Image, Precision,
                           Ref_Coord,
                           Step, 0.0);
        Min.Lat := Bounds (Codes(C).Mapcode.Image,
                           Codes(C).Territory_Alpha_Code.Image, Precision,
                           Ref_Coord,
                           -Step, 0.0);
        Max.Lon := Bounds (Codes(C).Mapcode.Image,
                           Codes(C).Territory_Alpha_Code.Image, Precision,
                           Ref_Coord,
                           0.0, Step);
        Min.Lon := Bounds (Codes(C).Mapcode.Image,
                           Codes(C).Territory_Alpha_Code.Image, Precision,
                           Ref_Coord,
                           0.0, -Step);
        -- Check that corners generate the same mapcode
        Check_In (Max.Lat, Max.Lon, Codes(C).Mapcode.Image,
                  Codes(C).Territory_Alpha_Code.Image, Precision);
        Check_In (Max.Lat, Min.Lon, Codes(C).Mapcode.Image,
                  Codes(C).Territory_Alpha_Code.Image, Precision);
        Check_In (Min.Lat, Max.Lon, Codes(C).Mapcode.Image,
                  Codes(C).Territory_Alpha_Code.Image, Precision);
        Check_In (Min.Lat, Min.Lon, Codes(C).Mapcode.Image,
                  Codes(C).Territory_Alpha_Code.Image, Precision);
        -- Compute distance maxlat, maxlon, minlat, minlon
        -- Correct delta_lon by Cos (sum of lat / 2)
        Tmp := Max.Lat + Min.Lat;
        Cor := Mapcodes.Real (My_Math.Cos (My_Math.Real (Tmp) * My_Math.Pi / 360.0));
        -- Sqrt (delta_lat**2 + delta_lon**2)
        Tmp := Max.Lat - Min.Lat;
        Dist := Tmp * Tmp;
        Tmp := (Max.Lon - Min.Lon) * Cor;
        Dist := Dist + Tmp * Tmp;
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
          Basic_Proc.Put_Line_Error (
              "  Boundaries are " & Image (Min.Lat) & " " & Image (Min.Lon)
            & " and " & Image (Max.Lat) & " " & Image (Max.Lon));
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
    Basic_Proc.Set_Error_Exit_Code;
end T_Accuracy;

