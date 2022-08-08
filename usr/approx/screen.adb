with As.U, Con_Io, Normal;
with Points, Resol, Menu2;
package body Screen is

  -- Common fields
  Title_Fld : constant Afpx.Field_Range := Afpx_Xref.Points.Title;
  File_Fld : constant Afpx.Field_Range := Afpx_Xref.Points.File;
  Nb_Point_Fld : constant Afpx.Field_Range := Afpx_Xref.Points.Points;
  State_Point_Fld : constant Afpx.Field_Range := Afpx_Xref.Points.State;
  Info_Fld : constant Afpx.Field_Range := Afpx_Xref.Points.Info;

  Degree_Fld : constant Afpx.Field_Range := Afpx_Xref.Compute.Degree;

  Stored_File_Name : As.U.Asu_Us;

  -- Return width of Get field
  function Get_Get_Width return Afpx.Width_Range is
    (Afpx.Get_Field_Width (Get_Fld));


  procedure Put_Title (S_Action : in S_Action_List; Option : in Boolean := False) is

    procedure Encode_Title (Msg : in String) is
    begin
      Afpx.Clear_Field (Title_Fld);
      Afpx.Encode_Field (Title_Fld, (0, 0), Msg);
    end Encode_Title;

  begin
    case S_Action is
      when Data         => Encode_Title("Data management");
      when Read_Points  => Encode_Title("Load a file of points");
      when Write_Points => Encode_Title("Save points in a file");
      when New_Points   => Encode_Title("Clear current points");
      when Modify_1     => Encode_Title("Modify a point");
      when Add_1        => Encode_Title("Add a new point");
      when Suppress_1   => Encode_Title("Delete a point");
      when Approximate  => Encode_Title("Data approximation");
      when Sort_Points  => Encode_Title("Sort points");
      when Get_Degree   => Encode_Title("Set the degree");
      when Polynom      =>
        if Option then
          Encode_Title("Compute polynom");
        else
          Encode_Title("View polynom");
        end if;
      when Y_F_X        => Encode_Title("Compute Y from X");
      when Scales       => Encode_Title("Set scales type");
      when Boundaries   =>
        if Option then
          Encode_Title("View scales boundaries");
        else
          Encode_Title("Set scales boundaries");
        end if;
      when Draw_Curve   => Encode_Title("Draw curve");
      when Show_Curve   => Encode_Title("Show curve");
      when Exit_Approx  => Encode_Title("Exit approx");
    end case;
  end Put_Title;

  -- Truncate head of string:  "> " & truncated head
  -- Or or padds with spaces
  function Procuste (Str : String; Len : Positive) return String is
    Res : String (1 .. Len);
  begin
    if Str'Length <= Len then
      Res (1 .. Str'Length) := Str;
      Res (Str'Length + 1 .. Len) := (others => ' ');
    else
      Res (1 .. 2) := "> ";
      Res (3 .. Len) := Str (Str'Last - Len + 3 .. Str'Last);
    end if;
    return Res;
  end Procuste;


  -- Put file name
  procedure Put_File (File_Name : in File.F_T_File_Name) is
    Height : Afpx.Height_Range;
    Width  : Afpx.Width_Range;
  begin
    Afpx.Get_Field_Size(File_Fld, Height, Width);
    Afpx.Encode_Field(File_Fld, (0, 0), Procuste(File_Name, Width));
  end Put_File;

  -- Scroll the list
  procedure Scroll (Fld_No : in List_Scroll_Fld_Range) is
  begin
    case Fld_No is
      when 11 => Afpx.Update_List(Afpx.Top);
      when 12 => Afpx.Update_List(Afpx.Page_Up);
      when 13 => Afpx.Update_List(Afpx.Up);
      when 14 => Afpx.Update_List(Afpx.Center_Selected);
      when 15 => Afpx.Update_List(Afpx.Down);
      when 16 => Afpx.Update_List(Afpx.Page_Down);
      when 17 => Afpx.Update_List(Afpx.Bottom);
    end case;
  end Scroll;

  -- Clear all menu dependant fields
  procedure Clear_Menu (Subtitle : in Boolean := False) is
    use type Afpx.Absolute_Field_Range;
  begin
    -- Inhibit all menu dependant fields
    for I in Menu_Fld_Range loop
      if not Subtitle or else
        (I /= Menu_Fld_Range'First and then
         I /= Menu_Fld_Range'Succ(Menu_Fld_Range'First) ) then
        Afpx.Set_Field_Activation (I, False);
      end if;
    end loop;
  end Clear_Menu;

  -- Encode in info field
  procedure Encode_Info (Msg : in String) is
  begin
    Afpx.Clear_Field (Info_Fld);
    Afpx.Encode_Field (Info_Fld, (0, 0), Msg);
  end Encode_Info;

  -- Ptg on Ok (and Cancel) buttons
  function S_Confirm (Wait_Curve : in Boolean := False) return Boolean is
    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result : Afpx.Result_Rec;
    Get_Prot : Boolean;
    Get_Act : Boolean;
    Res : Boolean;
  begin
    -- Protect get field
    Get_Prot := Afpx.Get_Field_Protection (Get_Fld);
    if not Get_Prot then
      Afpx.Set_Field_Protection (Get_Fld, True);
    end if;
    Afpx.Set_Field_Colors(Get_Fld,
                          Background => Afpx.Get_Descriptor_Background);
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              Res := True;
              exit;
            when Afpx.Escape_Key =>
              Res := False;
              exit;
            when Afpx.Break_Key =>
              raise Exit_Requested;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when List_Scroll_Fld_Range =>
              Scroll(Ptg_Result.Field_No);
            when Ok_Button_Fld =>
              Res := True;
              exit;
            when Cancel_Button_Fld =>
              Res := False;
              exit;
            when others =>
              null;
          end case;
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Refresh =>
          null;
      end case;
      exit when Wait_Curve and then Menu2.Curve_Stopped;
    end loop;
    -- Restore Get field
    Get_Act := Afpx.Get_Field_Activation (Get_Fld);
    Afpx.Reset_Field (Get_Fld, Reset_String => False);
    Afpx.Set_Field_Activation (Get_Fld, Get_Act);
    if not Get_Prot then
      Afpx.Set_Field_Protection (Get_Fld, False);
    end if;
    return Res;
  end S_Confirm;


  procedure Inform (Msg : in S_Info_List) is
    List_Active : Boolean;
  begin
    case Msg is
      when I_Clear     => Encode_Info ("");
      when I_File_Name => Encode_Info ("Select or enter file name");
      when I_X         => Encode_Info ("Enter X");
      when I_Y         => Encode_Info ("Enter Y");
      when I_Xmin      => Encode_Info ("Enter X min");
      when I_Ymin      => Encode_Info ("Enter Y min");
      when I_Xmax      => Encode_Info ("Enter X max");
      when I_Ymax      => Encode_Info ("Enter Y max");
      when I_Degree    => Encode_Info ("Enter degree from 0 to"
                                     & Natural'Image(Points.P_Nb - 1)
                                     & " (Npoints-1)");
      when I_Scales    => Encode_Info ("Select a scales kind");
      when I_Wait      =>
        Encode_Info ("Computing, please wait");
        List_Active := Afpx.Get_Field_Activation (Afpx.List_Field_No);
        if List_Active then
          Afpx.Set_Field_Activation (Afpx.List_Field_No, False);
        end if;
        Afpx.Put;
        if List_Active then
          Afpx.Set_Field_Activation (Afpx.List_Field_No, True);
        end if;
    end case;
  end Inform;


  function Confirm (Msg : S_Confirm_List; Alert : Boolean;
                    Subtitle : Boolean := False) return Boolean is
    Res : Boolean;
  begin
    -- No menu. Ok or cancel
    Clear_Menu(Subtitle);
    -- Inhibit exit field
    Afpx.Set_Field_Activation (Exit_Button_Fld, False);
    Afpx.Set_Field_Activation(Ok_Button_Fld, True);
    Afpx.Set_Field_Activation(Cancel_Button_Fld, True);
    -- Set colors
    if Alert then
      Afpx.Bell(1);
    end if;
    Afpx.Set_Field_Colors(Info_Fld, Foreground => Con_Io.Color_Of("Red"));
    case Msg is
      when C_File_Exists =>
        Encode_Info ("File exists and will be overwritten!");
      when C_Delete_Point =>
        Encode_Info ("Delete this point!");
      when C_Go_On =>
        Afpx.Set_Field_Colors(Info_Fld,
                              Foreground => Con_Io.Color_Of("Orange"));
        Encode_Info ("Continue with an other?");
      when C_Data_Lost =>
        Encode_Info ("Data is not saved and will be lost!");
    end case;
    Res := S_Confirm;
    -- Reset default colors
    Afpx.Reset_Field(Info_Fld);
    return Res;
  end Confirm;


  procedure Error (Msg : in S_Error_List; Subtitle : in Boolean := False) is
    Dummy_Res : Boolean;
  begin
    -- No menu. Ok
    Clear_Menu(Subtitle);
    -- Inhibit exit field
    Afpx.Set_Field_Activation(Exit_Button_Fld, False);
    Afpx.Set_Field_Activation(Ok_Button_Fld, True);
    Afpx.Set_Field_Activation(Cancel_Button_Fld, False);
    -- Set colors
    Afpx.Set_Field_Colors(Info_Fld, Foreground => Con_Io.Color_Of("Red"));
    if Msg /= E_Done then
      Afpx.Bell(1);
    end if;
    case Msg is
      when E_Done               => Encode_Info ("");
      when E_File_Not_Found     => Encode_Info ("File not found");
      when E_Access_Error       => Encode_Info ("Error accessing file");
      when E_Io_Error           => Encode_Info ("Error I/O on file");
      when E_Format_Error       => Encode_Info ("Error invalid file format");
      when E_File_Name          => Encode_Info ("Error invalid file name");
      when E_No_Data            => Encode_Info ("Error, no data");
      when E_Wrong_Degree       => Encode_Info ("Error, invalid degree");
      when E_Wrong_Coordinate   => Encode_Info ("Error, invalid coordinate");
      when E_Resolution_Problem => Encode_Info ("Internal error while solving"
                                              & " (degree too high?)");
      when E_Curve_Problem      => Encode_Info ("Internal error while drawing");
      when E_Curve_Active       => Encode_Info ("A curve is currently active");
      when E_Too_Many_Points    => Encode_Info ("Too many points");
    end case;
    Dummy_Res := S_Confirm (Msg = E_Curve_Active);
    -- Reset default colors
    Afpx.Reset_Field(Info_Fld);
  end Error;


  procedure Put_Point_Status is
    -- Width of nb_point
    Height : Afpx.Height_Range;
    Width  : Afpx.Width_Range;
  begin
    Afpx.Get_Field_Size(Nb_Point_Fld, Height, Width);
    Afpx.Encode_Field(Nb_Point_Fld, (0, 0), Normal(Points.P_Nb, Width));
    if Points.P_Saved then
      Afpx.Clear_Field(State_Point_Fld);
    else
      Afpx.Reset_Field(State_Point_Fld);
    end if;
  end Put_Point_Status;


  function Init_For_Main1 return Afpx.Field_Range is
  begin
    -- Disable Ok & cancel
    Afpx.Set_Field_Activation(Ok_Button_Fld, False);
    Afpx.Set_Field_Activation(Cancel_Button_Fld, False);
    -- Disable Get
    Afpx.Set_Field_Activation(Get_Fld, False);
    Put_Title(Data);
    Put_Point_Status;
    -- So whatever cursor field
    return 1;
  end Init_For_Main1;

  -- Init for file search
  function Init_For_Get (Subtitle : in Boolean := False)
                        return Afpx.Field_Range is
  begin
    -- No menu.
    Clear_Menu(Subtitle);
    -- Inhibit exit field
    Afpx.Set_Field_Activation (Exit_Button_Fld, False);
    -- Get, Ok or cancel
    Afpx.Set_Field_Activation(Get_Fld, True);
    Afpx.Set_Field_Activation(Ok_Button_Fld, True);
    Afpx.Set_Field_Activation(Cancel_Button_Fld, True);
    return Get_Fld;
  end Init_For_Get;

  -- Store current file_name for further menus
  -- Put stored file
  procedure Store_File is
  begin
    Afpx.Decode_Field (File_Fld, 0, Stored_File_Name);
  end Store_File;

  procedure Put_File is
  begin
    Put_File (Stored_File_Name.Image);
  end Put_File;

  procedure Put_Degree is
  begin
    Afpx.Encode_Field (Degree_Fld, (0, 0),
                       Normal (Resol.R_Degree, Max_Degree_Width));
  end Put_Degree;

  function Init_For_Main2 return Afpx.Field_Range is
  begin
    -- Disable Ok & Cancel
    Afpx.Set_Field_Activation(Ok_Button_Fld, False);
    Afpx.Set_Field_Activation(Cancel_Button_Fld, False);
    -- Disable Get
    Afpx.Set_Field_Activation(Get_Fld, False);
    -- Lock points
    Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
    Afpx.Set_Field_Activation (Afpx_Xref.Points.Center, False);
    Put_Title(Approximate);
    Put_Point_Status;
    Put_Degree;
    -- So whatever cursor field
    return 1;
  end Init_For_Main2;

  function Init_For_Main21 return Afpx.Field_Range is
  begin
    -- Disallow Cancel
    Afpx.Set_Field_Activation(Cancel_Button_Fld, False);
    -- Disable Get
    Afpx.Set_Field_Activation(Get_Fld, False);
    -- Lock points
    Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
    Afpx.Set_Field_Activation (Afpx_Xref.Points.Center, False);
    Put_Title(Boundaries);
    Put_Point_Status;
    -- So whatever cursor field
    return 1;
  end Init_For_Main21;

end Screen;

