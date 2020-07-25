with Ada.Text_Io;
with As.U.Utils, Str_Util.Regex,
     Gets, Directory, Environ, Sys_Calls.File_Access, Text_Line;
with Conv, Lat_Lon, String_Util, Great_Circle;
separate (Intercept)
package body Fpl is
  -- Patchig a FPL file: Patch policy, None is file is empty
  File_Name : As.U.Asu_Us;
  File : Text_Line.File_Type;
  Fpl_Data : As.U.Utils.Asu_Ua.Unb_Array;
  -- Number of points and the line in file where it is defined
  Numenr : Positive;
  Numenr_Line : Natural;
  -- Ades characteristics
  Ades_Alt, Ades_Lat, Ades_Lon : My_Math.Real;
  -- Declination at Ades
  Declination : My_Math.Real;

  -- Ilage or a real
  package Real_Io is new Ada.Text_Io.Float_Io (My_Math.Real);
  function Image (R : My_Math.Real) return String is
    Str : String (1 .. 12);
  begin
    Real_Io.Put (Str, R, 6, 0);
    return Str_Util.Strip (Str, Str_Util.Head);
  end Image;

  procedure Parse_Args is
    -- Argument
    Arg : As.U.Asu_Us;
    Char : Character;
    -- File descriptor and rights
    Stat : Sys_Calls.File_Stat_Rec;
    Can_Read, Can_Write, Can_Exec : Boolean;
    -- Line read and end indicator
    Line, Last_Line : As.U.Asu_Us;
    Done : Boolean;
    Argument_Error, Access_Error, Format_Error : exception;
    use type My_Math.Real, Sys_Calls.File_Kind_List;
  begin
    if Argument.Get_Nbre_Arg = Next_Arg + 4 then
      Argument.Get_Parameter (Arg, Occurence => Next_Arg + 2);
      if    Arg.Image = "-a" or else Arg.Image = "--append" then
        Policy := First;
      elsif Arg.Image = "-A" or else Arg.Image = "--append_alternate" then
        Policy := Alternate;
      else
        raise Argument_Error;
      end if;
    else
        raise Argument_Error;
    end if;
    -- Get declination, sign or E or W, then value
    Argument.Get_Parameter (Arg, Occurence => Next_Arg + 3);
    if Arg.Length < 2 then
      raise Argument_Error;
    end if;
    Char := Arg.Element (1);
    if Char = 'E' then
      Char := '+';
    elsif Char = 'W' then
      Char := '-';
    end if;
    Arg.Delete (1, 1);
    begin
      Declination := Gets.Get_Real (Arg.Image);
    exception
      when others =>
        raise Argument_Error;
    end;
    if Char = '-' then
      Declination := -Declination;
    elsif Char /= '+' then
      raise Argument_Error;
    end if;
    Logger.Log_Debug ("Got declination " & Image (Declination));

    -- Get file name
    Argument.Get_Parameter (File_Name, Occurence => Next_Arg + 4);
    Logger.Log_Debug ("Got file " & File_Name.Image);
    -- Check file access
    Stat := Sys_Calls.File_Stat (File_Name.Image);
    if Stat.Kind /= Sys_Calls.File then
      Logger.Log_Debug ("Cannot access file " & File_Name.Image);
      raise Access_Error;
    end if;
    Sys_Calls.File_Access.Has_Access (Stat, Can_Read, Can_Write, Can_Exec);
    if not Can_Read or else not Can_Write or else Can_Exec then
      Logger.Log_Debug ("Invalid rights for file " & File_Name.Image);
      raise Access_Error;
    end if;
    -- Load File
    File.Open_All (Text_Line.In_File, File_Name.Image);
    Numenr_Line := 0;
    loop
      Last_Line := Line;
      Line := File.Get;
      Done := Text_Line.End_Reached (Line.Image);
      Text_Line.Trim (Line);
      Logger.Log_Debug ("  Read line " & Line.Image);
      -- Find the line "NUMENR x", x being the number of points
      --  (including ADEP and ADES), store it without x
      if Line.Length >=8 and then Line.Head (7).Image = "NUMENR " then
        Numenr_Line := Fpl_Data.Length + 1;
        begin
          Numenr := Positive'Value (Line.Slice (8, Line.Length));
          if Numenr = 1 then
            raise Constraint_Error;
          end if;
          Line.Trail (Line.Length - 7);
          Logger.Log_Debug ("  Got Numenr: " & Images.Integer_Image (Numenr)
                          & " at line " & Images.Integer_Image (Numenr_Line));
        exception
          when others =>
            raise Format_Error;
        end;
      end if;
      if not Line.Is_Null then
        Fpl_Data.Append (Line);
      end if;
      exit when Done;
    end loop;
    File.Close_All;
    if Numenr_Line = 0 then
      raise Format_Error;
    end if;
    -- Parse Alt, Lat and Long of ADES
    Logger.Log_Debug ("  Got ADES: " & Last_Line.Image);
    declare
      Words : constant As.U.Utils.Asu_Array
            := Str_Util.Regex.Split_Sep (Last_Line.Image, " ");
    begin
      if Words'Length /= 6
      or else Words(1).Image /= "1" or else Words(3).Image /= "ADES" then
        raise Format_Error;
      end if;
      Ades_Alt := Gets.Get_Real (Words(4).Image);
      Logger.Log_Debug ("ADES Alt " & Image (Ades_Alt));
      Ades_Lat := Gets.Get_Real (Words(5).Image);
      Logger.Log_Debug ("ADES Lat " & Image (Ades_Lat));
      Ades_Lon := Gets.Get_Real (Words(6).Image);
      Logger.Log_Debug ("ADES Lon " & Image (Ades_Lon));
    end;
  exception
    when Argument_Error =>
      Error ("Syntax error");
    when Access_Error =>
      Error ("Error accessing file " & File_Name.Image);
    when Format_Error =>
      Error ("Invalid data in file " & File_Name.Image);
    when others =>
      Error ("Error processing file " & File_Name.Image);
  end Parse_Args;

  -- Append an approach point at one but last position
  -- "28 APPx DRCT <alt> <angle> <distance>"
  App_Num : Natural := 0;
  procedure Append_App (Alt : in Positive; Ang : in Angle; Dst : in Distance) is
    use My_Math;
    use type Conv.Deg_Coord_Range;
    -- 1 Nm is 1 minute of angle => convert to fraction of degrees
    Arc : constant Real := Real (Dst) / 60.0;

    -- Spherical trigo (default)
    A : constant Lat_Lon.Lat_Lon_Rad_Rec
      := (X => Conv.Deg2Rad (Conv.Reduct (Conv.Degree (Ades_Lon))),
          Y => Conv.Deg2Rad (Conv.Reduct (Conv.Degree (Ades_Lat))));
    H : constant Conv.Rad_Coord_Range
      := Conv.Deg2Rad (Conv.Reduct (Conv.Degree (Ang)
                                  + Conv.Degree (Declination)));
    D : constant String_Util.Distance := String_Util.Distance (Dst);
    B : Lat_Lon.Lat_Lon_Rad_Rec;

    -- Result
    Lat, Lon : My_Math.Real;
    Line : As.U.Asu_Us;

    procedure Normalize is
    begin
      -- Normalize Point Lat (-90 .. 90)
      if Lat > 90.0 then
        Lat := 180.0 - Lat;
        Lon := Lon + 180.0;
      elsif Lat < -90.0 then
        Lat := -(180.0 + Lat);
        Lon := Lon + 180.0;
      end if;
      -- Normalize Lon (-180 .. 180)
      if Lon >= 360.0 then
        Lon := Lon - 360.0;
      elsif Lon <= -360.0 then
        Lon := Lon + 360.0;
      end if;
      if Lon > 180.0 then
        Lon := -360.0 + Lon;
      elsif Lon <= -180.0 then
        Lon := 360.0 + Lon;
      end if;
    end Normalize;

  begin
    -- Add point
    Numenr := Numenr + 1;
    App_Num := App_Num + 1;
    Line.Set ("28 APP" & Normalization.Normal_Int (App_Num, 1, True, '0')
              & " DRCT ");
    -- Ades_Lat + Alt
    Line.Append (Image (My_Math.Real'(Ades_Alt + My_Math.Real (Alt))));
    Logger.Log_Debug ("Adding Dst:" & Dst'Img & ", Angle:"  & Ang'Img);


    -- Spherical trigo,
    B := Great_Circle.Apply_Route (A, H, D);
    Lat := My_Math.Real (Conv.Rad2Deg (B.Y));
    Lon := My_Math.Real (Conv.Rad2Deg (B.X));
    Normalize;
    Logger.Log_Debug ("  Spherical trigo => " & Image (Lat)
                    & " " & Image (Lon));

    -- Flat trigo, on option, overwritting spherical trigo
    if Environ.Is_Yes ("INTERCEPT_FLAT_TRIGO") then
      Lat := Arc * Sin (90.0 - (Real (Ang) + Declination), Degree);
      Lon := Arc * Cos (90.0 - (Real (Ang) + Declination), Degree)
                            / Cos (Ades_Lat, Degree);
      Lat := Ades_Lat + Lat;
      Lon := Ades_Lon + Lon;
      Normalize;
      Logger.Log_Debug ("  Flat trigo => " & Image (Lat) & " " & Image (Lon));
    end if;

    -- Write
    Line.Append (" " & Image (Lat) & " " & Image (Lon));
    Logger.Log_Debug ("Appending line: " & Line.Image);
    Fpl_Data.Insert (Fpl_Data.Length, Line);
  end Append_App;

  -- Save Fpl file
  Append_Suffix : constant String := "-App";
  procedure Save is
    Path, Prefix, Suffix : As.U.Asu_Us;
    Line : As.U.Asu_Us;
  begin
    -- Update Numenr
    Line := Fpl_Data.Element (Numenr_Line);
    Line.Append (Images.Integer_Image (Numenr));
    Fpl_Data.Replace_Element (Numenr_Line, Line);
    -- Build new name
    Path.Set (Directory.Dirname (File_Name.Image));
    Prefix.Set (Directory.File_Prefix (File_Name.Image));
    Suffix.Set (Directory.File_Suffix (File_Name.Image));
    -- Delete "."
    Suffix.Delete (1, 1);
    Path.Set (Directory.Build_File_Name (
        Path.Image, Prefix.Image & Append_Suffix, Suffix.Image));
    Logger.Log_Debug ("Saving file: " & Path.Image);
    -- Save
    File.Create_All (Path.Image);
    for I in 1 .. Fpl_Data.Length loop
      File.Put_Line (Fpl_Data.Element (I).Image);
    end loop;
    File.Close_All;
  end Save;

end Fpl;

