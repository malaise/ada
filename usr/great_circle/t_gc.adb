with Ada.Text_Io;
with Argument, Basic_Proc, Text_Handler, Con_Io, Afpx;
with Conv, Lat_Lon, String_Util, Great_Circle;

procedure T_Gc is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
      & " add.mm.ss/oddd.mm.ss add.mm.ss/oddd.mm.ss");
    Ada.Text_Io.Put_Line (" where a is N or S and o is E or W.");
    Basic_Proc.Set_Error_Exit_Code;
  end Usage;

  Use_Afpx : Boolean;

  A, B : Lat_Lon.Lat_Lon_Geo_Rec;
  Heading  : Conv.Geo_Coord_Rec;
  Distance : Lat_Lon.Distance;

  Cursor_Field : Afpx.Field_Range;
  Cursor_Col : Con_Io.Col_Range;
  Result : Afpx.Result_Rec;

  subtype A_Flds is Afpx.Field_Range range 06 .. 18;
  subtype B_Flds is Afpx.Field_Range range 19 .. 31;
  Heading_Field  : constant Afpx.Field_Range := 32;
  Distance_Field  : constant Afpx.Field_Range := 33;
  Compute_Field  : constant Afpx.Field_Range := 34;
  Exit_Field  : constant Afpx.Field_Range := 35;

  Decode_Ok : Boolean;
  Need_Clean : Boolean := False;

  Redisplay : Boolean;

  use type Afpx.Field_Range, Afpx.Event_List, Afpx.Keyboard_Key_List;

  -- Clear result fields during input
  function Next_Field_Cb (Cursor_Field : Afpx.Field_Range;
                          Enter_Field_Cause : Afpx.Enter_Field_Cause_List;
                          Str : String) return Con_Io.Col_Range is
    use type Afpx.Enter_Field_Cause_List;
  begin
    if Need_Clean then
      Afpx.Clear_Field (Heading_Field);
      Afpx.Clear_Field (Distance_Field);
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
    Point_Txt : Text_Handler.Text(String_Util.Coord_Str'Length);
  begin
    Text_Handler.Empty(Point_Txt);
    for Field in First_Fld .. Last_Fld loop
      Text_Handler.Append(Point_Txt, Afpx.Decode_Field(Field, 0));
    end loop;
    Point := String_Util.Str2Geo(Text_Handler.Value(Point_Txt));
    Ok := True;
  exception
    when others =>
      Ok := False;
      Cursor := First_Fld;
  end Decode_Point;

  procedure Clear_Result is
  begin
    Afpx.Clear_Field (Heading_Field);
    Afpx.Clear_Field (Distance_Field);
  end Clear_Result;

begin

  if Argument.Get_Nbre_Arg = 0 then
    Use_Afpx := True;
  elsif Argument.Get_Nbre_Arg = 2 then
    Use_Afpx := False;
  else
    Usage;
    return;
  end if;

  -- Convert args in lat_lon of A and B
  if not Use_Afpx then
    begin
      A := String_Util.Str2Geo(Argument.Get_Parameter(1));
      B := String_Util.Str2Geo(Argument.Get_Parameter(2));
    exception
      when others =>
        Usage;
        return;
    end;

    Great_Circle.Compute_Route(A, B, Heading, Distance);

    Ada.Text_Io.Put_Line ("Route is " & String_Util.Angle2Str(Heading));
    Ada.Text_Io.Put_Line ("Distance is " & String_Util.Dist2Str(Distance));
  else
    Afpx.Use_Descriptor (1);
    -- First Get field
    Redisplay := True;
    Cursor_Field := Afpx.Next_Cursor_Field(0);
    Cursor_Col := 0;
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Result, Redisplay,
                         Next_Field_Cb'Unrestricted_Access);

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
      end if;

      -- Compute
      if (Result.Event = Afpx.Keyboard
          and then Result.Keyboard_Key = Afpx.Return_Key)
      or else (Result.Event = Afpx.Mouse_Button
               and then Result.Field_No = Compute_Field) then
        Cursor_Field := A_Flds'First;
        Cursor_Col := 0;
        Clear_Result;
        Decode_Point (A_Flds'First, A_Flds'Last, A, Decode_Ok, Cursor_Field);
        if Decode_Ok then
          Decode_Point (B_Flds'First, B_Flds'Last, B, Decode_Ok, Cursor_Field);
        end if;
        if Decode_Ok then
          Great_Circle.Compute_Route(A, B, Heading, Distance);
          Afpx.Encode_Field (Heading_Field, (0, 0),
                             String_Util.Angle2Str(Heading));
          Afpx.Encode_Field (Distance_Field, (0, 0),
                             String_Util.Dist2Str(Distance));
          -- Clean the result fields at next cursor change field
          Need_Clean := True;
        end if;
      end if;

      -- Refresh
      Redisplay := Result.Event = Afpx.Wakeup_Event
           or else Result.Event = Afpx.Refresh;

    end loop;

  end if;

end T_Gc;

