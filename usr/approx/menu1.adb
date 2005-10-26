with Con_Io, Afpx, Directory, Text_Handler, Select_File, Normal;
with Points, Screen, Set_Points_List, Dialog, Point_Str, Menu2;
package body Menu1 is

  type Restore_List is (None, Partial, Full);
  Cursor_Field : Afpx.Field_Range;
  File_Name_Txt : Text_Handler.Text (Directory.Max_Dir_Name_Len);

  procedure Put_Point_Status is
    -- Width of nb_point
    Height : Afpx.Height_Range;
    Width  : Afpx.Width_Range;
  begin
    Afpx.Get_Field_Size(17, Height, Width);
    Afpx.Encode_Field(18, (0, 0), Normal(Points.P_Nb, Width));
    if Points.P_Saved then
      Afpx.Clear_Field(19);
    else
      Afpx.Reset_Field(19);
    end if;
  end Put_Point_Status;

  function My_Select_File is new Select_File(Put_Point_Status);

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
    Afpx.Use_Descriptor(1, False);
    Screen.Init_For_Main1 (Cursor_Field);
  end Error;

  function Exit_Prog return Boolean is
  begin
    Screen.Put_Title(Screen.Exit_Approx);
    if Dialog.Confirm_Lost then
      -- The end
      Con_Io.Destroy;
      return True;
    end if;
    return False;
  end Exit_Prog;

  -- Read a data file
  function Read_File (File_Name : in File.F_T_File_Name) return Boolean is
  begin
    Screen.Put_Title (Screen.Read_Points);
    Encode_File_In_Get (File_Name);
    if File.F_Exists(File_Name) then
      Screen.Put_Title (Screen.Read_Points);
      Text_Handler.Set (File_Name_Txt, File_Name);
      begin
        -- Get data in points and list
        Points.P_Store (File.F_Read(File_Name));
        Points.P_Saved;
        Set_Points_List;
        Screen.Put_Title (Screen.Data);
        return True;
      exception
        when File.F_Access_Error | File.F_Io_Error =>
          -- Error reading. Prev data is lost :-(
          Points.P_Clear;
          Set_Points_List;
          Text_Handler.Empty (File_Name_Txt);
          Error(Screen.E_Io_Error);
      end;
    else
      -- Error but prev data is kept
      Error(Screen.E_File_Not_Found);
    end if;
    return False;
  end Read_File;

  procedure Load_Save (Load : in Boolean; Restore : out Restore_List) is
    Tmp_File_Name : Text_Handler.Text (Directory.Max_Dir_Name_Len);
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
      Rights : Natural;
      Modif_Time : Directory.Time_T;
      Fsize : Directory.Size_T;
      use Directory;
    begin
      Text_Handler.Set (Tmp_File_Name,
                      My_Select_File(2, Text_Handler.Value(File_Name_Txt),
                                     Load));
      if Text_Handler.Empty (Tmp_File_Name) then
        -- Cancelled
       return;
      end if;
      Directory.File_Stat (Text_Handler.Value(Tmp_File_Name),
                           Kind, Rights, Modif_Time, Fsize);
      if Kind = Directory.Link then
        -- Follow link recursively
        Directory.Read_Link (Text_Handler.Value(Tmp_File_Name),
                             Tmp_File_Name);
        Directory.File_Stat (Text_Handler.Value(Tmp_File_Name),
                             Kind, Rights, Modif_Time, Fsize);
      end if;
    end;



    -- Restore (for errors)
    Afpx.Use_Descriptor(1);
    Set_Points_List;
    Screen.Init_For_Main1 (Cursor_Field);
    Screen.Put_File (Text_Handler.Value(File_Name_Txt));
    Encode_File_In_Get (Text_Handler.Value(Tmp_File_Name));
    Restore := Partial;
    -- load or save
    if Load then
      if Read_File (Text_Handler.Value(Tmp_File_Name)) then
        -- Done,
        Text_Handler.Set (File_Name_Txt, Tmp_File_Name);
        -- Else kept or lost
      end if;
    else
      if File.F_Exists(Text_Handler.Value(Tmp_File_Name))
      and then not Screen.Confirm(Screen.C_File_Exists, True) then
        return;
      end if;
      begin
        File.F_Write(Text_Handler.Value(Tmp_File_Name), Points.P_The_Points);
        Points.P_Saved;
        Text_Handler.Set (File_Name_Txt, Tmp_File_Name);
      exception
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
    Cursor_Col : Con_Io.Col_Range;
    Redisplay : Boolean;
    Ptg_Result : Afpx.Result_Rec;
    Restore : Restore_List;
    A_Point : Points.P_T_One_Point;
    Point_Set : Boolean;
    Point_Index : Positive;
    Data_Changed : Boolean;
    Saved_Index : Natural;

    use Afpx;

  begin
    Afpx.Use_Descriptor(1);
    Screen.Init_For_Main1 (Cursor_Field);
    Text_Handler.Empty (File_Name_Txt);
    Screen.Put_File ("");

    -- Get field width

    -- File?
    if Init_File_Name /= "" then
      if Read_File (Init_File_Name) then
        Text_Handler.Set (File_Name_Txt, Init_File_Name);
      end if;
      Restore := Partial;
    else
      Restore := None;
    end if;

    -- Update Nb of points and save_status
    Screen.Put_Point_Status;

    Cursor_Col := 0;
    Redisplay := False;
    Data_Changed := True;
    Saved_Index := 0;
    loop
      case Restore is
        when None =>
          null;
        when Partial =>
          Afpx.Use_Descriptor(1, False);
          if Saved_Index /= 0 then
            Afpx.Line_List_Mng.Move_To (Afpx.Line_List, Afpx.Line_List_Mng.Next,
                                        Saved_Index - 1, False);
            Afpx.Update_List (Afpx.Center);
          end if;
          Screen.Init_For_Main1 (Cursor_Field);
          Screen.Put_File (Text_Handler.Value(File_Name_Txt));
        when Full =>
          Afpx.Use_Descriptor(1);
          Set_Points_List;
          if Saved_Index /= 0 then
            Afpx.Line_List_Mng.Move_To (Afpx.Line_List, Afpx.Line_List_Mng.Next,
                                        Saved_Index - 1, False);
            Afpx.Update_List (Afpx.Center);
          end if;
          Screen.Init_For_Main1 (Cursor_Field);
          Screen.Put_File (Text_Handler.Value(File_Name_Txt));
      end case;

      -- Delete/modify/approximation/sort
      if Points.P_Nb = 0 then
        Afpx.Set_Field_Activation (26, False);
        Afpx.Set_Field_Activation (27, False);
        Afpx.Set_Field_Activation (29, False);
        Afpx.Set_Field_Activation (31, False);
      else
        Afpx.Set_Field_Activation (26, True);
        Afpx.Set_Field_Activation (27, True);
        Afpx.Set_Field_Activation (29, True);
        Afpx.Set_Field_Activation (31, True);
      end if;

      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Ptg_Result, Redisplay);
      Redisplay := False;
      Restore := None;
      Saved_Index := 0;
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              null;
            when Afpx.Escape_Key =>
              if Exit_Prog then
                -- The end
                return;
              else
                Restore := Partial;
              end if;
            when Afpx.Break_Key =>
              null;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Screen.List_Scroll_Fld_Range'First ..
                 Screen.List_Scroll_Fld_Range'Last =>
              Screen.Scroll(Ptg_Result.Field_No);
            when Screen.Exit_Button_Fld =>
              if Exit_Prog then
                -- The end
                return;
              else
                Restore := Partial;
              end if;
            when 21 | 22 =>
              Load_Save(Ptg_Result.Field_No = 21, Restore);
              Data_Changed := True;
            when 23 =>
              -- New points
              Screen.Put_Title(Screen.New_Points);
              if Dialog.Confirm_Lost then
                Points.P_Clear;
                Set_Points_List;
                Text_Handler.Empty(File_Name_Txt);
                -- Update file_name, nb of points and save_status
                Text_Handler.Empty (File_Name_Txt);
              end if;
              Data_Changed := True;
              Restore := Partial;
            when 25 =>
              -- Add point
              Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
              Screen.Put_Title(Screen.Add_1);
              loop
                Point_Set := False;
                Read_Point(Point_Set, A_Point);
                exit when not Point_Set;
                Points.P_Upd_Point (Points.Add, 1, A_Point);
                Set_Points_List;
                Data_Changed := True;
              end loop;
              Afpx.Set_Field_Protection (Afpx.List_Field_No, False);
              Saved_Index := Afpx.Line_List_Mng.List_Length (Afpx.Line_List);
              Restore := Partial;
            when 26 | 27 | Afpx.List_Field_No =>
              -- Delete / modify a point
              Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
              -- Get index then point
              Point_Index := Afpx.Line_List_Mng.Get_Position (Afpx.Line_List);
              A_Point := Points.P_One_Point(Point_Index);
              if Ptg_Result.Field_No = 26 then
                -- Delete a point
                Screen.Put_Title(Screen.Suppress_1);
                Afpx.Encode_Field (Screen.Get_Fld, (0, 0),
                    Point_Str.Encode_Rec(A_Point).Str(1 .. Screen.Get_Get_Width));
                Afpx.Set_Field_Activation(Screen.Get_Fld, True);
                if Screen.Confirm(Screen.C_Delete_Point, True) then
                  Points.P_Upd_Point (Points.Remove, Point_Index, A_Point);
                  Saved_Index := Afpx.Line_List_Mng.Get_Position (Afpx.Line_List);
                  if Saved_Index = Afpx.Line_List_Mng.List_Length (Afpx.Line_List) then
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
                  Saved_Index := Afpx.Line_List_Mng.Get_Position (Afpx.Line_List);
                  Set_Points_List;
                  Data_Changed := True;
                end if;
              end if;
              Afpx.Set_Field_Protection (Afpx.List_Field_No, False);
              Restore := Partial;
            when 29 =>
              -- approximation
              Screen.Store_File;
              Saved_Index := Afpx.Line_List_Mng.Get_Position (Afpx.Line_List);
              Menu2.Main_Screen(Data_Changed);
              Restore := Full;
              Data_Changed := False;
            when 31 =>
              -- Sort
              Points.P_Sort;
              Set_Points_List;
              Data_Changed := True;
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

end Menu1;
