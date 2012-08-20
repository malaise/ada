with Ada.Characters.Latin_1;
with As.B, Argument, Basic_Proc, Con_Io, Afpx, String_Mng, Language;
with Conv, Lat_Lon, String_Util, Great_Circle, Afpx_Xref;

procedure Gc is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " [ add.mm.ss/oddd.mm.ss add.mm.ss/oddd.mm.ss ]");
    Basic_Proc.Put_Line_Error (" where a is N or S and o is E or W.");
  end Usage;

  Debug : constant Boolean := False;

  Use_Afpx : Boolean;

  A, B : Lat_Lon.Lat_Lon_Geo_Rec;
  Heading  : Conv.Geo_Coord_Rec;
  Distance : Lat_Lon.Distance;

  Cursor_Field : Afpx.Field_Range;
  Cursor_Col : Con_Io.Col_Range;
  Insert : Boolean;
  Result : Afpx.Result_Rec;

  subtype A_Flds is Afpx.Field_Range
                    range Afpx_Xref.Main.A_First .. Afpx_Xref.Main.A_Last;
  subtype B_Flds is Afpx.Field_Range
                    range Afpx_Xref.Main.B_First .. Afpx_Xref.Main.B_Last;
  Heading_Ab_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Heading;
  Distance_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Distance;
  Heading_Ba_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Revert;
  Compute_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Compute;
  Exit_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Quit;

  Decode_Ok : Boolean;
  Need_Clean : Boolean := False;

  Redisplay : Boolean;

  use type Afpx.Field_Range, Afpx.Event_List, Afpx.Keyboard_Key_List;

  procedure Clear_Result is
  begin
    Afpx.Clear_Field (Heading_Ab_Field);
    Afpx.Clear_Field (Distance_Field);
    Afpx.Clear_Field (Heading_Ba_Field);
  end Clear_Result;

  -- Clear result fields during input
  function Next_Field_Cb (Cursor_Field : Afpx.Field_Range;
                          New_Field : Boolean;
                          Cursor_Col : Con_Io.Col_Range;
                          Enter_Field_Cause : Afpx.Enter_Field_Cause_List;
                          Str : Afpx.Unicode_Sequence) return Con_Io.Col_Range is
    pragma Unreferenced (Cursor_Field, New_Field, Cursor_Col);
    use type Afpx.Enter_Field_Cause_List;
  begin
    if Need_Clean then
      Clear_Result;
      Need_Clean := False;
    end if;
    if Enter_Field_Cause = Afpx.Left then
      return Afpx.Last_Index (Str, False);
    else
      return 0;
    end if;
  end Next_Field_Cb;

  procedure Decode_Point (First_Fld, Last_Fld : in Afpx.Field_Range;
                          Point : out Lat_Lon.Lat_Lon_Geo_Rec;
                          Ok : out Boolean;
                          Cursor : in out Afpx.Field_Range) is
    -- Two '"' added in Afpx screen
    Point_Txt : As.B.Asb_Bs(String_Util.Coord_Str'Length+2);
  begin
    Point_Txt.Set_Null;
    for Field in First_Fld .. Last_Fld loop
      Point_Txt.Append (Afpx.Decode_Field(Field, 0));
    end loop;
    if Debug then
      Basic_Proc.Put_Line_Error ("Decoded point: " & Point_Txt.Image);
    end if;
    -- Replace Ndd°mm'ss"/Eddd°mm'ss" by Ndd.mm.ss/Eddd.mm.ss
    -- "°" has already been replaced by " " in Afpx.Decode_Field
    Point_Txt.Set (String_Mng.Replace (Point_Txt.Image, " ", "."));
    Point_Txt.Set (String_Mng.Replace (Point_Txt.Image, "'", "."));
    Point_Txt.Set (String_Mng.Replace (Point_Txt.Image, """", ""));
    if Debug then
      Basic_Proc.Put_Line_Error ("Parsed point: " & Point_Txt.Image);
    end if;
    Point := String_Util.Str2Geo(Point_Txt.Image);
    if Debug then
      Basic_Proc.Put_Line_Error ("Got point OK: " & Point_Txt.Image);
    end if;
    Ok := True;
  exception
    when others =>
      if Debug then
        Basic_Proc.Put_Line_Error ("Decode point Exception");
      end if;
      Ok := False;
      Cursor := First_Fld;
  end Decode_Point;

  procedure Encode_Heading (F : in Afpx.Field_Range;
                            H : in Conv.Geo_Coord_Rec) is
    Str : constant String := String_Util.Angle2Str(H);
    -- Will append " and set ° and ' instead of 2 first .
    Wstr : Wide_String (1 .. Str'Length + 1);
  begin
    Wstr := Language.String_To_Wide (Str) & '"';
    Wstr(4) := Language.Char_To_Wide (
                 Ada.Characters.Latin_1.Degree_Sign);
    Wstr(7) := ''';
    Afpx.Encode_Wide_Field (F, (0, 0), Wstr);
 end Encode_Heading;

begin

  if Argument.Get_Nbre_Arg = 0 then
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
    begin
      -- Parse arguments
      A := String_Util.Str2Geo(Argument.Get_Parameter(1));
      B := String_Util.Str2Geo(Argument.Get_Parameter(2));
      -- Compute
      Great_Circle.Compute_Route(A, B, Heading, Distance);
      -- Put result
      Basic_Proc.Put_Output ("Route: " & String_Util.Angle2Str(Heading));
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
    -- First Get field
    Redisplay := True;
    Cursor_Field := Afpx.Next_Cursor_Field(0);
    Cursor_Col := 0;
    Insert := False;
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                         Result, Redisplay, False,
                         Next_Field_Cb'Access);

      -- Exit
      exit when (Result.Event = Afpx.Keyboard
                 and then Result.Keyboard_Key = Afpx.Break_Key)
      or else   (Result.Event = Afpx.Mouse_Button
                 and then Result.Field_No = Exit_Field);

      -- Reset
      if Result.Event = Afpx.Keyboard
      and then Result.Keyboard_Key = Afpx.Escape_Key then
        for Field in A_Flds loop
          Afpx.Reset_Field (Field);
        end loop;
        for Field in B_Flds loop
          Afpx.Reset_Field (Field);
        end loop;
        Clear_Result;
        Cursor_Field := A_Flds'First;
        Cursor_Col := 0;
        Insert := False;
      end if;

      -- Compute
      if (Result.Event = Afpx.Keyboard
          and then Result.Keyboard_Key = Afpx.Return_Key)
      or else (Result.Event = Afpx.Mouse_Button
               and then Result.Field_No = Compute_Field) then
        Cursor_Field := A_Flds'First;
        Cursor_Col := 0;
        Insert := False;
        Clear_Result;
        Decode_Point (A_Flds'First, A_Flds'Last, A, Decode_Ok, Cursor_Field);
        if Decode_Ok then
          Decode_Point (B_Flds'First, B_Flds'Last, B, Decode_Ok, Cursor_Field);
        end if;
        if Decode_Ok then
          Great_Circle.Compute_Route(A => A, B => B,
                                     Heading => Heading,
                                     Distance => Distance);
          Encode_Heading (Heading_Ab_Field, Heading);
          Afpx.Encode_Field (Distance_Field, (0, 0),
                             String_Util.Dist2Str(Distance));
          Great_Circle.Compute_Route(A => B, B => A,
                                     Heading => Heading,
                                     Distance => Distance);
          Encode_Heading (Heading_Ba_Field, Heading);
          -- Clean the result fields at next cursor change field
          Need_Clean := True;
        end if;
      end if;

      -- Refresh
      Redisplay := Result.Event = Afpx.Refresh;

    end loop;

  end if;

end Gc;

