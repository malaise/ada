with Ada.Characters.Latin_1;
with As.B, Argument, Basic_Proc, Con_Io, Afpx, Str_Util, Language, Reg_Exp;
with Conv, Lat_Lon, String_Util, Great_Circle, Afpx_Xref;

procedure Gc is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " [ add.mm.ssss/oddd.mm.ssss add.mm.ssss/oddd.mm.ssss ]");
    Basic_Proc.Put_Line_Error ("   or: " & Argument.Get_Program_Name
      & " [ add.ijklmn/oddd.ijklmn add.ijklmn/oddd.ijklmn ]");
    Basic_Proc.Put_Line_Error ("   or: " & Argument.Get_Program_Name
      & " [ [<context>:]<mapcode> [<context>:]<mapcode> ]");
    Basic_Proc.Put_Line_Error (" where a is N or S and o is E or W.");
  end Usage;

  Use_Afpx : Boolean;

  Decode_Ok : Boolean;
  A, B : Lat_Lon.Lat_Lon_Rad_Rec;
  Heading  : Conv.Geo_Coord_Rec;
  Distance : Lat_Lon.Distance;

  Get_Handle : Afpx.Get_Handle_Rec;
  Result : Afpx.Result_Rec;

  Sexa_Pattern : constant String :=
      "[NnSs][0-9]{2}\.[0-9]{2}\.[0-9]{4}/[EeWw][0-9]{3}\.[0-9]{2}\.[0-9]{4}";
  Deci_Pattern : constant String :=
      "[NnSs][0-9]{2}\.[0-9]{6}/[EeWw][0-9]{3}\.[0-9]{6}";

  Mode_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Mode;
  subtype A_Flds is Afpx.Field_Range
                    range Afpx_Xref.Main.A_First .. Afpx_Xref.Main.A_Last;
  subtype B_Flds is Afpx.Field_Range
                    range Afpx_Xref.Main.B_First .. Afpx_Xref.Main.B_Last;
  subtype Code_Flds is Afpx.Field_Range
                    range Afpx_Xref.Main.A_Ctx .. Afpx_Xref.Main.B_Code;
  Heading_Ab_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Heading;
  Distance_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Distance;
  Heading_Ba_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Revert;
  Switch_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Switch;
  Clear_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Clear;
  Compute_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Compute;
  Exit_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Quit;

  type Mode_List is (Sexa_Mode, Deci_Mode, Code_Mode);
  Mode : Mode_List := Sexa_Mode;
  Need_Clean : Boolean := False;

  Deg : constant String := "°"; --## rule line off Char

  use type Afpx.Field_Range, Afpx.Event_List, Afpx.Keyboard_Key_List;

  procedure Clear_Result is
  begin
    Afpx.Clear_Field (Heading_Ab_Field);
    Afpx.Clear_Field (Distance_Field);
    Afpx.Clear_Field (Heading_Ba_Field);
  end Clear_Result;

  Degree_Sign : constant Wide_Character
              := Language.Char_To_Wide (Ada.Characters.Latin_1.Degree_Sign);

  procedure Reset_Field (Field : in Afpx.Absolute_Field_Range) is
    Char : Wide_Character;
  begin
    Afpx.Reset_Field (Field);
    if Mode /= Deci_Mode then
      return;
    end if;
    if Afpx.Get_Field_Width (Field) <= 2 then
      Char := Afpx.Decode_Wide_Field (Field, 0)(1);
      -- "o" -> ".", "'" -> " " and """[/]" becomes "o[/]"
      if Char = Language.String_To_Wide(Deg)(1) then
        Afpx.Encode_Wide_Field (Field, (0, 0), ".");
      elsif Char = ''' then
        Afpx.Encode_Wide_Field (Field, (0, 0), " ");
      elsif Char = '"' then
        Afpx.Encode_Wide_Field (Field, (0, 0), Degree_Sign & "");
      end if;
    end if;
  end Reset_Field;

  procedure Reset is
  begin
    -- Deactivate / clear fields
    if Mode = Code_Mode then
      for Field in A_Flds loop
        Afpx.Set_Field_Activation (Field, False);
      end loop;
      for Field in B_Flds loop
        Afpx.Set_Field_Activation (Field, False);
      end loop;
      for Field in Code_Flds loop
        Reset_Field (Field);
      end loop;
    else
      for Field in A_Flds loop
        Reset_Field (Field);
      end loop;
      for Field in B_Flds loop
        Reset_Field (Field);
      end loop;
      for Field in Code_Flds loop
        Afpx.Set_Field_Activation (Field, False);
      end loop;
    end if;
    Clear_Result;
    -- Update Mode text and Switch button
    case Mode is
      when Sexa_Mode =>
        Afpx.Encode_Field (Mode_Field, (0, 0), "Sexigesimal mode");
        Afpx.Encode_Field (Switch_Field, (1, 8), "Deci");
      when Deci_Mode =>
        Afpx.Encode_Field (Mode_Field, (0, 0), "Decimal mode    ");
        Afpx.Encode_Field (Switch_Field, (1, 8), "Code");
      when Code_Mode =>
        Afpx.Encode_Field (Mode_Field, (0, 0), "Mapcode mode    ");
        Afpx.Encode_Field (Switch_Field, (1, 8), "Sexi");
    end case;
    if Mode /= Code_Mode then
      Get_Handle.Cursor_Field := A_Flds'First;
    else
      Get_Handle.Cursor_Field := Code_Flds'First;
    end if;
    Get_Handle.Cursor_Col := 0;
    Get_Handle.Insert := False;
  end Reset;

  -- Clear result fields during input
  function Next_Field_Cb (Unused_Cursor_Field : Afpx.Field_Range;
                          Unused_New_Field : Boolean;
                          Cursor_Col : Con_Io.Col_Range;
                          Unused_Offset : Con_Io.Col_Range;
                          Enter_Field_Cause : Afpx.Enter_Field_Cause_List;
                          Str : Afpx.Unicode_Sequence) return Con_Io.Col_Range is
    Last : Con_Io.Col_Range;
    use type Afpx.Enter_Field_Cause_List;
  begin
    if Need_Clean then
      Clear_Result;
      Need_Clean := False;
    end if;
    if Enter_Field_Cause = Afpx.Left then
      return Afpx.Last_Index (Str, False);
    elsif Enter_Field_Cause = Afpx.Mouse then
      Last := Afpx.Last_Index (Str, True);
      if Cursor_Col <= Last then
        return Cursor_Col;
      else
        return Last;
      end if;
    else
      return 0;
    end if;
  end Next_Field_Cb;

  procedure Decode_Point (First_Fld, Last_Fld : in Afpx.Field_Range;
                          Point : out Lat_Lon.Lat_Lon_Rad_Rec;
                          Ok : out Boolean;
                          Cursor : in out Afpx.Field_Range) is
    -- Two '"' added and two 'o' instead of '.' in Afpx screen
    Point_Txt : As.B.Asb_Bs(String_Util.Geo_Str'Length+4);

    -- Replace trailing spaces by '0' for decimal numeric fields (len = 4)
    function Pad_Field (Field : Afpx.Field_Range) return String is
      Init : constant String := Afpx.Decode_Field(Field, 0, False);
      Str : String := Init;
    begin
      if Str'Length = 4 then
        for I in reverse Str'Range loop
          exit when Str(I) /= ' ';
          Str(I) := '0';
        end loop;
      end if;
      if Str /= Init then
        Afpx.Encode_Field(Field, (0, 0), Str);
      end if;
      return Str;
    end Pad_Field;

  begin
    Point_Txt.Set_Null;
    for Field in First_Fld .. Last_Fld loop
      Point_Txt.Append (Pad_Field(Field));
    end loop;
    Great_Circle.Logger.Log_Debug ("Decoded point: " & Point_Txt.Image);
    if Mode = Sexa_Mode then
      -- Replace Nddomm'ssss"/Edddomm'ssss" by Ndd.mm.ssss/Eddd.mm.ssss
      Point_Txt.Set (Str_Util.Substit (Point_Txt.Image, Deg, "."));
      Point_Txt.Set (Str_Util.Substit (Point_Txt.Image, "'", "."));
      Point_Txt.Set (Str_Util.Substit (Point_Txt.Image, """", ""));
      Great_Circle.Logger.Log_Debug ("Parsed point: " & Point_Txt.Image);
      Point := Lat_Lon.Geo2Rad (String_Util.Str2Geo(Point_Txt.Image));
    else -- Deci_Mode
      -- Replace Ndd.ij klmno/Eddd.ij klmno by Ndd.ijklmn/Eddd.ijklmn
      Point_Txt.Set (Str_Util.Substit (Point_Txt.Image, Deg, ""));
      Point_Txt.Set (Str_Util.Substit (Point_Txt.Image, " ", ""));
      Great_Circle.Logger.Log_Debug ("Parsed point: " & Point_Txt.Image);
      Point := Lat_Lon.Dec2Rad (String_Util.Str2Dec(Point_Txt.Image));
    end if;
    Great_Circle.Logger.Log_Debug ("Got point OK");
    Ok := True;
  exception
    when others =>
      Great_Circle.Logger.Log_Debug ("Decode point Exception");
      Ok := False;
      Cursor := First_Fld;
  end Decode_Point;

  -- Decode a mapcode
  procedure Decode_Mapcode (First_Fld, Last_Fld : in Afpx.Field_Range;
                            Point : out Lat_Lon.Lat_Lon_Rad_Rec;
                            Ok : out Boolean;
                            Cursor : in out Afpx.Field_Range) is
    -- 6 for context, ":" and 12 for mapcode
    Mapcode_Txt : As.B.Asb_Bs(19);
  begin
    Mapcode_Txt.Set_Null;
    for Field in First_Fld .. Last_Fld loop
      Mapcode_Txt.Append (Str_Util.Strip (
          Afpx.Decode_Field(Field, 0, False), Str_Util.Both));
    end loop;
    Great_Circle.Logger.Log_Debug ("Parsed mapcode: " & Mapcode_Txt.Image);
    Point := Lat_Lon.Mapcode2Rad (Mapcode_Txt.Image);
    Great_Circle.Logger.Log_Debug ("Got point OK");
    Ok := True;
  exception
    when others =>
      Great_Circle.Logger.Log_Debug ("Decode mapcode Exception");
      Ok := False;
      Cursor := First_Fld;
  end Decode_Mapcode;

  -- Decode points or mapcodes, set A and B. Return OK
  function Decode return Boolean is
    Ok : Boolean;
  begin
    Get_Handle.Cursor_Col := 0;
    Get_Handle.Insert := False;
    Clear_Result;
    if Mode /= Code_Mode then
      Get_Handle.Cursor_Field := A_Flds'First;
      Decode_Point (A_Flds'First, A_Flds'Last, A, Ok,
                    Get_Handle.Cursor_Field);
      if Ok then
        Decode_Point (B_Flds'First, B_Flds'Last, B, Ok,
                      Get_Handle.Cursor_Field);
      end if;
    else
      Get_Handle.Cursor_Field := Code_Flds'First;
      Decode_Mapcode (Code_Flds'First, Code_Flds'First + 2, A, Ok,
                      Get_Handle.Cursor_Field);
      if Ok then
        Decode_Mapcode (Code_Flds'First + 3, Code_Flds'Last, B, Ok,
                        Get_Handle.Cursor_Field);
      end if;
    end if;
    if Ok then
      Great_Circle.Logger.Log_Debug ("Got point A:" & A.X'Img & A.Y'Img);
      Great_Circle.Logger.Log_Debug ("Got point B:" & B.X'Img & B.Y'Img);
    end if;
    return Ok;
  end Decode;

  -- Encode a point in degree of decimal
  procedure Encode_Point (First_Fld, Last_Fld : in Afpx.Field_Range;
                          Point : in Lat_Lon.Lat_Lon_Rad_Rec) is
    Str : constant String := (
        if Mode = Sexa_Mode then String_Util.Geo2Str (Lat_Lon.Rad2Geo (Point))
        else                     String_Util.Dec2Str (Lat_Lon.Rad2Dec (Point)));
    Index : Positive := Str'First;
    Len : Positive;
  begin
    for Field in First_Fld .. Last_Fld loop
      Len := Afpx.Get_Field_Width (Field);
      if Afpx.Is_Get_Kind (Field) then
        Afpx.Encode_Field (Field, (0, 0), Str(Index .. Index + Len - 1));
      end if;
      -- Index of next field,
      if Afpx.Is_Get_Kind (Field) then
        Index := Index + Len;
      elsif Mode = Sexa_Mode then
        -- Skip the last "" (in '"/')
        Index := Index + 1;
      elsif Mode = Deci_Mode then
        -- Skip the space in the middle of deci part
        --  and the Deg at the end ("o/")
        for Char of String'(Afpx.Decode_Field (Field, 0, False)) loop
          if Char = '.' or else Char = '/' then
            Index := Index + 1;
          end if;
        end loop;
      end if;
    end loop;
    null;
  end Encode_Point;

  -- Encode a mapcode
  procedure Encode_Mapcode (Unused_First_Fld, Last_Fld : in Afpx.Field_Range;
                            Point : in Lat_Lon.Lat_Lon_Rad_Rec) is
    Code : constant String := Lat_Lon.Rad2Mapcode (Point);
  begin
    Afpx.Encode_Field (Last_Fld, (0, 0), Code);
  end Encode_Mapcode;

  -- Encode A and B as points or mapcodes
  procedure Encode is
  begin
    if Mode /= Code_Mode then
      Encode_Point (A_Flds'First, A_Flds'Last, A);
      Encode_Point (B_Flds'First, B_Flds'Last, B);
    else
      Encode_Mapcode (Code_Flds'First,     Code_Flds'First + 2, A);
      Encode_Mapcode (Code_Flds'First + 3, Code_Flds'Last,      B);
    end if;
  end Encode;

  -- Encode Heading (in degrees, with special degree char, if mode is not
  --  decimal)
  procedure Encode_Heading (F : in Afpx.Field_Range;
                            H : in Conv.Geo_Coord_Rec) is
  begin
    if Mode /= Deci_Mode then
      -- Sexa or code
      declare
        Str : constant String := String_Util.Geoangle2Str(H);
        -- Will append " and set o and ' instead of 2 first .
        Wstr : Wide_String (1 .. Str'Length + 1);
      begin
        Wstr := Language.String_To_Wide (Str) & '"';
        Wstr(4) := Language.Char_To_Wide (
                     Ada.Characters.Latin_1.Degree_Sign);
        Wstr(7) := ''';
        Afpx.Encode_Wide_Field (F, (0, 0), Wstr);
      end;
    else
      -- Deci
      declare
        Str : constant String := String_Util.Decangle2Str(Conv.Geo2Dec(H));
        -- Will append o
        Wstr : Wide_String (1 .. Str'Length + 1);
      begin
        Wstr := Language.String_To_Wide (Str) & Degree_Sign;
        Afpx.Encode_Wide_Field (F, (0, 0), Wstr);
      end;
      Great_Circle.Logger.Log_Debug ("Heading encoded");
    end if;
  end Encode_Heading;

begin

  if Argument.Get_Nbre_Arg = 0 then
    Use_Afpx := True;
  elsif Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-x" then
    Use_Afpx := True;
  elsif Argument.Get_Nbre_Arg = 2 then
    Use_Afpx := False;
  else
    Basic_Proc.Set_Error_Exit_Code;
    Usage;
    return;
  end if;

  -- Convert args in lat_lon of A and B
  if not Use_Afpx then
    -- See if Ndd.mm.ss or Ndd.ijkl
    if Reg_Exp.Match (Sexa_Pattern, Argument.Get_Parameter(1), True)
    and then Reg_Exp.Match (Sexa_Pattern, Argument.Get_Parameter(2), True) then
      Mode := Sexa_Mode;
    elsif Reg_Exp.Match (Deci_Pattern, Argument.Get_Parameter(1), True)
    and then Reg_Exp.Match (Deci_Pattern, Argument.Get_Parameter(2), True) then
      Mode := Deci_Mode;
    else
      Mode := Code_Mode;
    end if;
    Great_Circle.Logger.Log_Debug ("Mode: " & Mode'Img);
    begin
      -- Parse arguments
      if Mode = Sexa_Mode then
        A := Lat_Lon.Geo2Rad (String_Util.Str2Geo(Argument.Get_Parameter(1)));
        B := Lat_Lon.Geo2Rad (String_Util.Str2Geo(Argument.Get_Parameter(2)));
      elsif Mode = Deci_Mode then
        A := Lat_Lon.Dec2Rad (String_Util.Str2Dec(Argument.Get_Parameter(1)));
        B := Lat_Lon.Dec2Rad (String_Util.Str2Dec(Argument.Get_Parameter(2)));
      else
        -- Coordinates of mapcodes
        A := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter(1));
        B := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter(2));
      end if;
      Great_Circle.Logger.Log_Debug ("Got point A:" & A.X'Img & A.Y'Img);
      Great_Circle.Logger.Log_Debug ("Got point B:" & B.X'Img & B.Y'Img);
      -- Compute
      Great_Circle.Compute_Route(A, B, Heading, Distance);
      -- Put result
      if Mode /= Deci_Mode then
        -- Sexa or Code
        Basic_Proc.Put_Output ("Route: "
            & String_Util.Geoangle2Str(Heading));
      else
        -- Deci
        Basic_Proc.Put_Output ("Route: "
            & String_Util.Decangle2Str(Conv.Geo2Dec(Heading)));
      end if;
      Basic_Proc.Put_Line_Output ("   Distance(Nm): "
                        & String_Util.Dist2Str(Distance));
    exception
      when others =>
        Basic_Proc.Set_Error_Exit_Code;
        Usage;
        return;
    end;
  else
    Afpx.Use_Descriptor (Afpx_Xref.Main.Dscr_Num);
    Get_Handle := (others => <>);
    Reset;
    loop
      Afpx.Put_Then_Get (Get_Handle, Result, False, Next_Field_Cb'Access);

      if (Result.Event = Afpx.Keyboard
          and then Result.Keyboard_Key = Afpx.Break_Key)
      or else (Result.Event = Afpx.Mouse_Button
               and then Result.Field_No = Exit_Field) then
        -- Exit
        exit;
      elsif (Result.Event = Afpx.Keyboard
             and then Result.Keyboard_Key = Afpx.Escape_Key)
      or else (Result.Event = Afpx.Mouse_Button
               and then Result.Field_No = Clear_Field) then
        -- Reset
        Reset;
      elsif (Result.Event = Afpx.Keyboard
          and then Result.Keyboard_Key = Afpx.Return_Key)
      or else (Result.Event = Afpx.Mouse_Button
               and then Result.Field_No = Compute_Field) then
        -- Decode input
        if Decode then
          -- Compute
          Great_Circle.Compute_Route(A => A, B => B,
                                     Heading => Heading,
                                     Distance => Distance);
          Encode_Heading (Heading_Ab_Field, Heading);
          Afpx.Encode_Field (Distance_Field, (0, 0),
                             String_Util.Dist2Str(Distance));
          Great_Circle.Logger.Log_Debug ("Distance encoded");
          -- Compute reverse heading
          Great_Circle.Compute_Route(A => B, B => A,
                                     Heading => Heading,
                                     Distance => Distance);
          Encode_Heading (Heading_Ba_Field, Heading);
          -- Clean the result fields at next cursor change field
          Need_Clean := True;
        end if;
      elsif Result.Event = Afpx.Mouse_Button
      and then Result.Field_No = Switch_Field then
        Decode_Ok := Decode;
        -- Switch
        Mode := (if Mode /= Mode_List'Last then Mode_List'Succ (Mode)
                 else Mode_List'First);
        Reset;
        if Decode_Ok then
          -- Encode points / mapcodes
          Encode;
        end if;
      end if;

    end loop;

  end if;

end Gc;

