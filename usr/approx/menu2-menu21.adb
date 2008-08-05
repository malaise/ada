with Con_Io, Afpx, Curve;
with Points, Screen, Dialog, Point_Str;
separate(Menu2)
package body Menu21 is

  type Restore_List is (None, Partial);
  Cursor_Field : Afpx.Field_Range;

  The_Bounds_Set : Boolean := False;
  The_Bounds : Curve.T_Boundaries;

  procedure Reset_Bounds is
  begin
    The_Bounds_Set := False;
  end Reset_Bounds;

  function Bounds_Set return Boolean is
  begin
    return The_Bounds_Set;
  end Bounds_Set;

  procedure Get_Bounds (Set : out Boolean; Bounds : out Curve.T_Boundaries) is
  begin
    Set := The_Bounds_Set;
    Bounds := The_Bounds;
  end Get_Bounds;


  procedure Error (Msg : in Screen.S_Error_List) is
  begin
    Screen.Error(Msg);
    -- Restore screen
    Afpx.Use_Descriptor(4, False);
    Screen.Init_For_Main21 (Cursor_Field);
  end Error;

  procedure Put_Bounds is
    use Curve;
  begin
    -- Allow clear if bounds set
    Afpx.Set_Field_Activation (Screen.Exit_Button_Fld, The_Bounds_Set);
    Afpx.Clear_Field (33);
    Afpx.Clear_Field (34);
    if not The_Bounds_Set then
      Afpx.Encode_Field (33, (0,0), "Not set");
      return;
    end if;
    case The_Bounds.Scale is
      when Curve.Curve_Screen =>
        Afpx.Encode_Field (33, (0, 0), "Computed & fit screen");
      when Curve.Curve_Normed =>
        Afpx.Encode_Field (33, (0, 0), "Computed & normed");
      when Curve.Free_Screen =>
        Afpx.Encode_Field (33, (0, 0), "Defined & fit screen");
      when Curve.Free_Normed =>
        Afpx.Encode_Field (33, (0, 0), "Defined & normed");
    end case;
    Afpx.Encode_Field (34, (0, 0), "Xmin: "
         & Point_Str.Coordinate_Image(The_Bounds.X_Min));
    Afpx.Encode_Field (34, (1, 0), "Xmax: "
         & Point_Str.Coordinate_Image(The_Bounds.X_Max));
    if      The_Bounds.Scale = Curve.Free_Screen
    or else The_Bounds.Scale = Curve.Free_Normed then
      Afpx.Encode_Field (34, (2, 0), "Ymin: "
           & Point_Str.Coordinate_Image(The_Bounds.Y_Min));
      Afpx.Encode_Field (34, (3, 0), "Ymax: "
           & Point_Str.Coordinate_Image(The_Bounds.Y_Max));
    end if;
  end Put_Bounds;

  -- Compute_X should be set only when scale is curve*
  procedure Set_Bounds (Scale : in Curve.T_Scale;
                        Compute_X : in Boolean := False) is
    Loc_Bounds : Curve.T_Boundaries(Scale);
    Set : Boolean;
    use Curve;
  begin
    if Compute_X and then
      (        Scale = Curve.Free_Screen
       or else Scale = Curve.Free_Normed) then
      raise Program_Error;
    end if;
    Screen.Put_Title (Screen.Boundaries);
    if Compute_X then
      Curve.X_Boundaries(Points.P_The_Points,
                         Loc_Bounds.X_Min, Loc_Bounds.X_Max);
    else
      Set := False;
      Dialog.Read_Coordinate(Screen.I_Xmin, Set, Loc_Bounds.X_Min);
      if not Set then
        return;
      end if;
      Set := False;
      Dialog.Read_Coordinate(Screen.I_Xmax, Set, Loc_Bounds.X_Max);
      if not Set then
        return;
      end if;
      if      Scale = Curve.Free_Screen
      or else Scale = Curve.Free_Normed then
      Set := False;
        Dialog.Read_Coordinate(Screen.I_Ymin, Set, Loc_Bounds.Y_Min);
        if not Set then
          return;
        end if;
        Set := False;
        Dialog.Read_Coordinate(Screen.I_Ymax, Set, Loc_Bounds.Y_Max);
        if not Set then
          return;
        end if;
      end if;
    end if;
    The_Bounds := Loc_Bounds;
    The_Bounds_Set := True;
  end Set_Bounds;


  procedure Main_Screen is
    Cursor_Col : Con_Io.Col_Range;
    Insert : Boolean;
    Redisplay : Boolean;
    Ptg_Result : Afpx.Result_Rec;
    Restore : Restore_List;
    Activate_No_Curve : Boolean;

    use Afpx;

  begin
    Afpx.Use_Descriptor(4);

    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;
    Restore := Partial;

    loop
      -- Activate or not according to curve activity
      Activate_No_Curve := Menu2.Curved_Stopped;
      case Restore is
        when None =>
          null;
        when Partial =>
          Afpx.Use_Descriptor(4, False);
          Screen.Init_For_Main21 (Cursor_Field);
          Screen.Put_File;
          Put_Bounds;
      end case;
      -- Clear
      Screen.Put_Title (Screen.Boundaries, not Activate_No_Curve);
      Afpx.Set_Field_Activation (Screen.Exit_Button_Fld, Activate_No_Curve);
      Afpx.Set_Field_Activation (20, Activate_No_Curve);
      Afpx.Set_Field_Activation (21, Activate_No_Curve);
      Afpx.Set_Field_Activation (22, Activate_No_Curve);
      Afpx.Set_Field_Activation (24, Activate_No_Curve);
      Afpx.Set_Field_Activation (25, Activate_No_Curve);
      Afpx.Set_Field_Activation (26, Activate_No_Curve);
      Afpx.Set_Field_Activation (28, Activate_No_Curve);
      Afpx.Set_Field_Activation (29, Activate_No_Curve);
      Afpx.Set_Field_Activation (30, Activate_No_Curve);

      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                         Ptg_Result, Redisplay);
      Redisplay := False;
      Restore := None;
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              return;
            when Afpx.Escape_Key =>
              return;
            when Afpx.Break_Key =>
              raise Screen.Exit_Requested;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Screen.List_Scroll_Fld_Range'First ..
                 Screen.List_Scroll_Fld_Range'Last =>
              Screen.Scroll(Ptg_Result.Field_No);
            when Screen.Ok_Button_Fld =>
              -- Back
              return;
            when Screen.Exit_Button_Fld =>
              -- Clear
              The_Bounds_Set := False;
              Put_Bounds;
            when 21 =>
              -- Computed fit screen
              Set_Bounds (Curve.Curve_Screen, Compute_X => True);
              Restore := Partial;
            when 22 =>
              -- Computed normed
              Set_Bounds (Curve.Curve_Normed, Compute_X => True);
              Restore := Partial;
            when 25 =>
              -- X set fit screen : Get Xmin & Xmax
              Set_Bounds (Curve.Curve_Screen);
              Restore := Partial;
            when 26 =>
              -- X set normed : Get Xmin & Xmax
              Set_Bounds (Curve.Curve_Normed);
              Restore := Partial;
            when 29 =>
              -- Defined fit screen : Get Xmin, Xmax Ymin & Ymax
              Set_Bounds (Curve.Free_Screen);
              Restore := Partial;
            when 30 =>
              -- Defined normed : Get Xmin, Xmax Ymin & Ymax
              Set_Bounds (Curve.Free_Normed);
              Restore := Partial;
            when others =>
              null;
          end case;
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Wakeup_Event =>
          null;
        when Afpx.Refresh =>
          Redisplay := True;
      end case;
    end loop;

  end Main_Screen;

end Menu21;

