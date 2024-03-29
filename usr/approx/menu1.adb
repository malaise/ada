with As.U, Afpx, Directory, Select_File, Normal;
with Points, Screen, Set_Points_List, Dialog, Point_Str, Menu2, Afpx_Xref;
package body Menu1 is

  type Restore_List is (None, Partial, Full);
  Get_Handle : Afpx.Get_Handle_Rec;
  File_Name_Txt : As.U.Asu_Us;

  -- Update of point status during file selection
  procedure Put_Point_Status is
    -- Width of nb_point
    Height : Afpx.Height_Range;
    Width  : Afpx.Width_Range;
  begin
    Afpx.Get_Field_Size(Afpx_Xref.Selection.Nb_Points, Height, Width);
    Afpx.Encode_Field(Afpx_Xref.Selection.Nb_Points, (0, 0),
                      Normal(Points.P_Nb, Width));
    if Points.P_Saved then
      Afpx.Clear_Field(Afpx_Xref.Selection.Not_Saved);
    else
      Afpx.Reset_Field(Afpx_Xref.Selection.Not_Saved);
    end if;
  end Put_Point_Status;

  package Msf is new Select_File(Afpx_Xref.Selection.Dscr_Num,
                                            Put_Point_Status);


  procedure Encode_File_In_Get (File_Name : in String) is
  begin
    Afpx.Set_Field_Activation (Screen.Get_Fld, True);
    Afpx.Set_Field_Protection (Screen.Get_Fld, True);
    Afpx.Encode_Field (Screen.Get_Fld, (0, 0),
       Screen.Procuste(File_Name, Screen.Get_Get_Width));
  end Encode_File_In_Get;


  procedure Error (Msg : in Screen.S_Error_List) is
  begin
    Screen.Error(Msg);
    -- Restore screen
    Afpx.Use_Descriptor(Afpx_Xref.Points.Dscr_Num, False);
    Get_Handle.Cursor_Field := Screen.Init_For_Main1;
  end Error;

  function Exit_Prog return Boolean is
  begin
    loop
      begin
        Screen.Put_Title(Screen.Exit_Approx);
        if Dialog.Confirm_Lost then
          -- The end
          Afpx.Release_Descriptor;
          return True;
        end if;
        return False;
      exception
        when Screen.Exit_Requested =>
          -- Go on asking
          null;
      end;
    end loop;
  end Exit_Prog;

  -- Read a data file
  function Read_File (File_Name : in File.F_T_File_Name) return Boolean is
    -- Clear points when read failes
    procedure Clear is
    begin
      Points.P_Clear;
      Set_Points_List;
      File_Name_Txt.Set_Null;
    end Clear;
  begin
    Screen.Put_Title (Screen.Read_Points);
    Encode_File_In_Get (File_Name);
    if File.F_Exists (File_Name) then
      Screen.Put_Title (Screen.Read_Points);
      File_Name_Txt := As.U.Tus (File_Name);
      begin
        -- Get data in points and list
        Points.P_Store (File.F_Read (File_Name));
        Points.P_Saved;
        Set_Points_List;
        Screen.Put_Title (Screen.Data);
        return True;
      exception
        -- Error reading. Prev data is lost :-(
        when File.F_Access_Error =>
          Clear;
          Error (Screen.E_Access_Error);
        when File.F_Io_Error =>
          Clear;
          Error (Screen.E_Io_Error);
        when File.F_Format_Error =>
          Clear;
          Error(Screen.E_Format_Error);
      end;
    else
      -- Error but prev data is kept
      Error (Screen.E_File_Not_Found);
    end if;
    return False;
  end Read_File;

  procedure Load_Save (Load : in Boolean; Restore : out Restore_List) is
    Tmp_File_Name : As.U.Asu_Us;
  begin
    Restore := None;
    -- Title
    if Load then
      Screen.Put_Title(Screen.Read_Points);
    else
      Screen.Put_Title(Screen.Write_Points);
    end if;
    if not Load and then Points.P_Empty then
      -- Error when saving no data
      Error (Screen.E_No_Data);
      Restore := Partial;
      return;
    elsif Load and then not Points.P_Saved then
      -- Confirm loss when loading and unsaved points
      Restore := Partial;
      if not Dialog.Confirm_Lost then
        return;
      end if;
    end if;

    -- Select file
    Restore := Full;
    declare
      Kind : Directory.File_Kind_List;
      use type Directory.File_Kind_List;
    begin
      if not Load then
        Tmp_File_Name := File_Name_Txt;
      end if;
      begin
        Tmp_File_Name := As.U.Tus (Msf.Get_File (Tmp_File_Name.Image,
                                                 Load, True));
      exception
        when Msf.Exit_Requested =>
          Tmp_File_Name.Set_Null;
      end;
      if Tmp_File_Name.Is_Null then
        -- Cancelled
       return;
      end if;
      Kind := Directory.File_Kind (Tmp_File_Name.Image);
      if Kind = Directory.Link then
        -- Follow link recursively
        begin
          Directory.Read_Link (Tmp_File_Name.Image, Tmp_File_Name);
        exception
          when others =>
            Error (Screen.E_Io_Error);
        end;
        Kind := Directory.File_Kind (Tmp_File_Name.Image);
      end if;
    end;

    -- Restore (for errors)
    Afpx.Use_Descriptor(Afpx_Xref.Points.Dscr_Num);
    Set_Points_List;
    Get_Handle.Cursor_Field := Screen.Init_For_Main1;
    Screen.Put_File (File_Name_Txt.Image);
    Encode_File_In_Get (Tmp_File_Name.Image);
    Restore := Partial;
    -- load or save
    if Load then
      if Read_File (Tmp_File_Name.Image) then
        -- Done,
        File_Name_Txt := Tmp_File_Name;
        -- Else kept or lost
      end if;
    else
      if File.F_Exists(Tmp_File_Name.Image)
      and then not Screen.Confirm(Screen.C_File_Exists, True) then
        return;
      end if;
      begin
        File.F_Write(Tmp_File_Name.Image, Points.P_The_Points);
        Points.P_Saved;
        File_Name_Txt := Tmp_File_Name;
      exception
        when File.F_Access_Error =>
          Error (Screen.E_Access_Error);
        when others =>
          Error (Screen.E_Io_Error);
      end;
    end if;
  end Load_Save;

  procedure Read_Point (Set : in out Boolean; Point : in out Points.P_T_One_Point) is
    Lp : Points.P_T_One_Point;
    Ok : Boolean;
  begin
    Ok := Set;
    Lp := Point;
    Dialog.Read_Coordinate(Screen.I_X, Ok, Lp.X);
    if not Ok then
      Set := False;
      return;
    end if;
    Ok := Set;
    Dialog.Read_Coordinate(Screen.I_Y, Ok, Lp.Y);
    if not Ok then
      Set := False;
      return;
    end if;
    Point := Lp;
    Set := True;
  end Read_Point;

  procedure Main_Screen (Init_File_Name : in File.F_T_File_Name) is
    Ptg_Result : Afpx.Result_Rec;
    Restore : Restore_List;

    function Handle_Key return Boolean is
    begin
      case Ptg_Result.Keyboard_Key is
        when Afpx.Return_Key =>
          return False;
        when Afpx.Escape_Key =>
          if Exit_Prog then
            -- The end
            return True;
          else
            Restore := Partial;
            return False;
          end if;
        when Afpx.Break_Key =>
          if Exit_Prog then
            -- The end
            return True;
          else
            Restore := Partial;
            return False;
          end if;
      end case;
    end Handle_Key;

    Saved_Index : Afpx.Line_List_Mng.Ll_Natural;
    Point_Set : Boolean;
    Point_Index : Positive;
    Data_Changed : Boolean;
    A_Point : Points.P_T_One_Point;
    use type Afpx.Absolute_Field_Range, Afpx.Line_List_Mng.Ll_Natural;

    -- Delete / modify a point
    procedure Edit_Point is
    begin
      Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
      Afpx.Set_Field_Activation (Afpx_Xref.Points.Center, False);
      -- Get index then point
      Point_Index := Positive (Afpx.Line_List.Get_Position);
      A_Point := Points.P_One_Point(Point_Index);
      if Ptg_Result.Field_No = Afpx_Xref.Points.Delete then
        -- Delete a point
        Screen.Put_Title(Screen.Suppress_1);
        Afpx.Encode_Field (Screen.Get_Fld, (0, 0),
            Point_Str.Encode_Rec(A_Point).Str(1 .. Screen.Get_Get_Width));
        Afpx.Set_Field_Activation(Screen.Get_Fld, True);
        if Screen.Confirm(Screen.C_Delete_Point, True) then
          Points.P_Upd_Point (Points.Remove, Point_Index, A_Point);
          Saved_Index := Afpx.Line_List.Get_Position;
          if Saved_Index = Afpx.Line_List.List_Length then
            Saved_Index := Saved_Index - 1;
          end if;
          Set_Points_List;
          Data_Changed := True;
        end if;
      else
        -- Modify
        Screen.Put_Title(Screen.Modify_1);
        Point_Set := True;
        Read_Point(Point_Set, A_Point);
        if Point_Set then
          Points.P_Upd_Point (Points.Modify, Point_Index, A_Point);
          Saved_Index := Afpx.Line_List.Get_Position;
          Set_Points_List;
          Data_Changed := True;
        end if;
      end if;
      Afpx.Set_Field_Protection (Afpx.List_Field_No, False);
      Afpx.Set_Field_Activation (Afpx_Xref.Points.Center, True);
      Restore := Partial;
    end Edit_Point;

  begin
    Afpx.Use_Descriptor(Afpx_Xref.Points.Dscr_Num);
    Get_Handle.Cursor_Field := Screen.Init_For_Main1;
    File_Name_Txt.Set_Null;
    Screen.Put_File ("");

    -- Get field width

    -- File?
    if Init_File_Name /= "" then
      if Read_File (Init_File_Name) then
        File_Name_Txt := As.U.Tus (Init_File_Name);
      end if;
      Restore := Partial;
    else
      Restore := None;
    end if;

    -- Update Nb of points and save_status
    Screen.Put_Point_Status;

    Get_Handle.Cursor_Col := 0;
    Get_Handle.Insert := False;
    Data_Changed := True;
    Saved_Index := 0;
    loop
      begin
        case Restore is
          when None =>
            null;
          when Partial =>
            Afpx.Use_Descriptor(Afpx_Xref.Points.Dscr_Num, False);
            if Saved_Index /= 0 then
              Afpx.Line_List.Move_At (Saved_Index);
              Afpx.Update_List (Afpx.Center_Selected);
            end if;
            Get_Handle.Cursor_Field := Screen.Init_For_Main1;
            Screen.Put_File (File_Name_Txt.Image);
          when Full =>
            Afpx.Use_Descriptor(Afpx_Xref.Points.Dscr_Num);
            Set_Points_List;
            if Saved_Index /= 0 then
              Afpx.Line_List.Move_At (Saved_Index);
              Afpx.Update_List (Afpx.Center_Selected);
            end if;
            Get_Handle.Cursor_Field := Screen.Init_For_Main1;
            Screen.Put_File (File_Name_Txt.Image);
        end case;

        -- Delete/modify/approximation/sort
        if Points.P_Nb = 0 then
          Afpx.Set_Field_Activation (Afpx_Xref.Points.Delete, False);
          Afpx.Set_Field_Activation (Afpx_Xref.Points.Modify, False);
          Afpx.Set_Field_Activation (Afpx_Xref.Points.Approx, False);
          Afpx.Set_Field_Activation (Afpx_Xref.Points.Sort, False);
        else
          Afpx.Set_Field_Activation (Afpx_Xref.Points.Delete, True);
          Afpx.Set_Field_Activation (Afpx_Xref.Points.Modify, True);
          Afpx.Set_Field_Activation (Afpx_Xref.Points.Approx, True);
          Afpx.Set_Field_Activation (Afpx_Xref.Points.Sort, True);
        end if;

        Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
        Restore := None;
        Saved_Index := 0;
        case Ptg_Result.Event is
          when Afpx.Keyboard =>
            exit when Handle_Key;
          when Afpx.Mouse_Button =>
            case Ptg_Result.Field_No is
              when Screen.List_Scroll_Fld_Range =>
                Screen.Scroll(Ptg_Result.Field_No);
              when Screen.Exit_Button_Fld =>
                -- The end
                exit when Exit_Prog;
                Restore := Partial;
              when Afpx_Xref.Points.Load | Afpx_Xref.Points.Save =>
                Load_Save(Ptg_Result.Field_No = Afpx_Xref.Points.Load,
                          Restore);
                Data_Changed := True;
              when Afpx_Xref.Points.New_Points =>
                -- New points
                Screen.Put_Title(Screen.New_Points);
                if Dialog.Confirm_Lost then
                  Points.P_Clear;
                  Set_Points_List;
                  -- Update file_name, nb of points and save_status
                  File_Name_Txt.Set_Null;
                end if;
                Data_Changed := True;
                Restore := Partial;
              when Afpx_Xref.Points.Add =>
                -- Add point
                Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
                Afpx.Set_Field_Activation (Afpx_Xref.Points.Center, False);
                Screen.Put_Title(Screen.Add_1);
                if not Afpx.Line_List.Is_Empty then
                  Saved_Index := Afpx.Line_List.Get_Position;
                end if;
                loop
                  Point_Set := False;
                  Read_Point(Point_Set, A_Point);
                  exit when not Point_Set;
                  Points.P_Upd_Point (Points.Add, 1, A_Point);
                  Set_Points_List;
                  Data_Changed := True;
                  Saved_Index := Afpx.Line_List.List_Length;
                end loop;
                Afpx.Set_Field_Protection (Afpx.List_Field_No, False);
                Afpx.Set_Field_Activation (Afpx_Xref.Points.Center, True);
                Restore := Partial;
              when Afpx_Xref.Points.Delete
                 | Afpx_Xref.Points.Modify
                 | Afpx.List_Field_No =>
                Edit_Point;
              when Afpx_Xref.Points.Approx =>
                -- Approximation
                Screen.Store_File;
                Saved_Index := Afpx.Line_List.Get_Position;
                Menu2.Main_Screen(Data_Changed);
                Restore := Full;
                Data_Changed := False;
              when Afpx_Xref.Points.Sort =>
                -- Sort
                Points.P_Sort;
                Set_Points_List;
                Data_Changed := True;
              when others =>
                null;
            end case;
          when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
             | Afpx.Refresh =>
            null;
        end case;
      exception
        when Screen.Exit_Requested =>
          exit when Exit_Prog;
          Restore := Partial;
      end;
    end loop;

  end Main_Screen;

end Menu1;

