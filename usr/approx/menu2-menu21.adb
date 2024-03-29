with Point_Str;
separate(Menu2)
package body Menu21 is

  type Restore_List is (None, Partial);
  Get_Handle : Afpx.Get_Handle_Rec;

  The_Bounds_Set : Boolean := False;
  The_Bounds : Curve.T_Boundaries;

  procedure Reset_Bounds is
  begin
    The_Bounds_Set := False;
  end Reset_Bounds;

  function Bounds_Set return Boolean is (The_Bounds_Set);

  procedure Get_Bounds (Set : out Boolean; Bounds : out Curve.T_Boundaries) is
  begin
    Set := The_Bounds_Set;
    Bounds := The_Bounds;
  end Get_Bounds;

  procedure Put_Bounds is
    use Curve;
  begin
    -- Allow clear if bounds set
    Afpx.Set_Field_Activation (Screen.Exit_Button_Fld, The_Bounds_Set);
    Afpx.Clear_Field (Afpx_Xref.Bounds.Mode);
    Afpx.Clear_Field (Afpx_Xref.Bounds.Vals);
    if not The_Bounds_Set then
      Afpx.Encode_Field (Afpx_Xref.Bounds.Mode, (0,0), "Not set");
      return;
    end if;
    case The_Bounds.Scale is
      when Curve.Curve_Screen =>
        Afpx.Encode_Field (Afpx_Xref.Bounds.Mode, (0, 0),
                           "Computed & fit screen");
      when Curve.Curve_Normed =>
        Afpx.Encode_Field (Afpx_Xref.Bounds.Mode, (0, 0),
                           "Computed & normed");
      when Curve.Free_Screen =>
        Afpx.Encode_Field (Afpx_Xref.Bounds.Mode, (0, 0),
                           "Defined & fit screen");
      when Curve.Free_Normed =>
        Afpx.Encode_Field (Afpx_Xref.Bounds.Mode, (0, 0),
                           "Defined & normed");
    end case;
    Afpx.Encode_Field (Afpx_Xref.Bounds.Vals, (0, 0), "Xmin: "
         & Point_Str.Coordinate_Image(The_Bounds.X_Min));
    Afpx.Encode_Field (Afpx_Xref.Bounds.Vals, (1, 0), "Xmax: "
         & Point_Str.Coordinate_Image(The_Bounds.X_Max));
    if      The_Bounds.Scale = Curve.Free_Screen
    or else The_Bounds.Scale = Curve.Free_Normed then
      Afpx.Encode_Field (Afpx_Xref.Bounds.Vals, (2, 0), "Ymin: "
           & Point_Str.Coordinate_Image(The_Bounds.Y_Min));
      Afpx.Encode_Field (Afpx_Xref.Bounds.Vals, (3, 0), "Ymax: "
           & Point_Str.Coordinate_Image(The_Bounds.Y_Max));
    end if;
  end Put_Bounds;

  -- Compute_X should be set only when scale is curve*
  procedure Set_Bounds (Scale : in Curve.T_Scale;
                        Compute_X : in Boolean := False) is
    Loc_Bounds : Curve.T_Boundaries(Scale);
    The_Y_Bounds_Set : Boolean;
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

      if The_Bounds_Set then
        Loc_Bounds.X_Min := The_Bounds.X_Min;
        Set := True;
      else
        Set := False;
      end if;
      Dialog.Read_Coordinate(Screen.I_Xmin, Set, Loc_Bounds.X_Min);
      if not Set then
        return;
      end if;

      if The_Bounds_Set then
        Loc_Bounds.X_Max := The_Bounds.X_Max;
        Set := True;
      else
        Set := False;
      end if;
      Dialog.Read_Coordinate(Screen.I_Xmax, Set, Loc_Bounds.X_Max);
      if not Set then
        return;
      end if;

      if      Scale = Curve.Free_Screen
      or else Scale = Curve.Free_Normed then
        The_Y_Bounds_Set := The_Bounds_Set
                    and then (The_Bounds.Scale = Curve.Free_Screen
                      or else The_Bounds.Scale = Curve.Free_Normed);

        if The_Y_Bounds_Set then
          Loc_Bounds.Y_Min := The_Bounds.Y_Min;
          Set := True;
        else
          Set := False;
        end if;
        Dialog.Read_Coordinate(Screen.I_Ymin, Set, Loc_Bounds.Y_Min);
        if not Set then
          return;
        end if;

        if The_Y_Bounds_Set then
          Loc_Bounds.Y_Max := The_Bounds.Y_Max;
          Set := True;
        else
          Set := False;
        end if;
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
    Ptg_Result : Afpx.Result_Rec;
    Restore : Restore_List;
    Activate_No_Curve : Boolean;

    use Afpx;

  begin
    Afpx.Use_Descriptor(Afpx_Xref.Bounds.Dscr_Num);

    Get_Handle := (others => <>);
    Restore := Partial;

    loop
      -- Activate or not according to curve activity
      Activate_No_Curve := Menu2.Curve_Stopped;
      case Restore is
        when None =>
          null;
        when Partial =>
          Afpx.Use_Descriptor(Afpx_Xref.Bounds.Dscr_Num, False);
          Get_Handle.Cursor_Field := Screen.Init_For_Main21;
          Screen.Put_File;
          Put_Bounds;
      end case;
      -- Clear
      Screen.Put_Title (Screen.Boundaries, not Activate_No_Curve);
      Afpx.Set_Field_Activation (Screen.Exit_Button_Fld, Activate_No_Curve);
      Afpx.Set_Field_Activation (Afpx_Xref.Bounds.Comp,
                                 Activate_No_Curve);
      Afpx.Set_Field_Activation (Afpx_Xref.Bounds.Comp_Fit_Scr,
                                 Activate_No_Curve);
      Afpx.Set_Field_Activation (Afpx_Xref.Bounds.Comp_Normed,
                                 Activate_No_Curve);
      Afpx.Set_Field_Activation (Afpx_Xref.Bounds.Xd_Yc,
                                 Activate_No_Curve);
      Afpx.Set_Field_Activation (Afpx_Xref.Bounds.Xd_Yc_Fit_Scr,
                                 Activate_No_Curve);
      Afpx.Set_Field_Activation (Afpx_Xref.Bounds.Xd_Yc_Normed,
                                 Activate_No_Curve);
      Afpx.Set_Field_Activation (Afpx_Xref.Bounds.Xyd,
                                 Activate_No_Curve);
      Afpx.Set_Field_Activation (Afpx_Xref.Bounds.Xyd_Fit_Scr,
                                 Activate_No_Curve);
      Afpx.Set_Field_Activation (Afpx_Xref.Bounds.Xyd_Normed,
                                 Activate_No_Curve);

      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
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
            when Screen.List_Scroll_Fld_Range'First =>
              Screen.Scroll(Ptg_Result.Field_No);
            when Screen.Ok_Button_Fld =>
              -- Back
              return;
            when Screen.Exit_Button_Fld =>
              -- Clear
              The_Bounds_Set := False;
              Put_Bounds;
            when Afpx_Xref.Bounds.Comp_Fit_Scr =>
              -- Computed fit screen
              Set_Bounds (Curve.Curve_Screen, Compute_X => True);
              Restore := Partial;
            when Afpx_Xref.Bounds.Comp_Normed =>
              -- Computed normed
              Set_Bounds (Curve.Curve_Normed, Compute_X => True);
              Restore := Partial;
            when Afpx_Xref.Bounds.Xd_Yc_Fit_Scr =>
              -- X set fit screen : Get Xmin & Xmax
              Set_Bounds (Curve.Curve_Screen);
              Restore := Partial;
            when Afpx_Xref.Bounds.Xd_Yc_Normed =>
              -- X set normed : Get Xmin & Xmax
              Set_Bounds (Curve.Curve_Normed);
              Restore := Partial;
            when Afpx_Xref.Bounds.Xyd_Fit_Scr =>
              -- Defined fit screen : Get Xmin, Xmax Ymin & Ymax
              Set_Bounds (Curve.Free_Screen);
              Restore := Partial;
            when Afpx_Xref.Bounds.Xyd_Normed =>
              -- Defined normed : Get Xmin, Xmax Ymin & Ymax
              Set_Bounds (Curve.Free_Normed);
              Restore := Partial;
            when others =>
              null;
          end case;
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Refresh =>
          null;
      end case;
    end loop;

  end Main_Screen;

end Menu21;

