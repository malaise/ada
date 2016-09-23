with Con_Io, Afpx, Normal, Language, Str_Util;
with Point_Str, Afpx_Xref;
package body Dialog is

  -- If points are not saved, ask for confirmation
  function Confirm_Lost return Boolean is
  begin
    if Points.P_Saved then
      return True;
    else
      return Screen.Confirm(Screen.C_Data_Lost, True);
    end if;
  end Confirm_Lost;


  -- Remove trailing spaces. No heading nor intermediate spaces allowed
  function Parse_Spaces (Txt : in out As.U.Asu_Us) return Boolean is
    Str : constant String := Txt.Image;
    L : Natural;
  begin
    L := 0;
    for I in reverse Str'Range loop
      if not Str_Util.Is_Separator (Str(I)) then
        -- Significant char
        if L = 0 then
          L := I;
        end if;
      else
        -- space
        if L /= 0 then
          -- Space before significant char
          return False;
        end if;
      end if;
    end loop;
    -- If all spaces, L = 0 => empty
    Txt := As.U.Tus (Str(1 .. L));
    return True;
  end Parse_Spaces;


  function Parse_Leading_Space (Str : String) return String is
  begin
    if Str(Str'First) = ' ' then
      return Str(Natural'Succ(Str'First) .. Str'Last);
    else
      return Str;
    end if;
  end Parse_Leading_Space;

  -- Get a coordinate
  --  If Set is set in, then a Put_Then_Get is performed, else a Get
  --  Validity is checked and Set is set out according to the final result
  -- subtype D_Coordinate_List is Screen.S_Info_List
  --         range (Screen.I_X .. Screen.I_Ymax);
  procedure Read_Coordinate (Kind : in D_Coordinate_List;
           Set : in out Boolean; Coordinate : in out Points.P_T_Coordinate;
           Subtitle : in Boolean := False) is
    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result : Afpx.Result_Rec;

    procedure Encode is
      Coo_Str : Point_Str.Coordinate_String;
    begin
      Coo_Str := Point_Str.Coordinate_Image(Coordinate);
      Afpx.Clear_Field (Screen.Get_Fld);
      Afpx.Encode_Field (Screen.Get_Fld, (0, 0),
                         Parse_Leading_Space(Coo_Str));
    end Encode;

    function Decode return Boolean is
      Buff : As.U.Asu_Us;
      Ok : Boolean;
    begin
      Afpx.Decode_Field (Screen.Get_Fld, 0, Buff);
      Ok := Parse_Spaces(Buff);
      if Ok then
        begin
          Coordinate := Point_Str.Coordinate_Value (Buff.Image);
        exception
          when Constraint_Error =>
            Ok := False;
        end;
      end if;
      if Ok then
        return True;
      else
        Screen.Error (Screen.E_Wrong_Coordinate);
        Get_Handle.Cursor_Field := Screen.Init_For_Get;
        return False;
      end if;
    end Decode;

  begin
    Get_Handle.Cursor_Field := Screen.Init_For_Get (Subtitle);
    if Set then
      Encode;
    else
      Afpx.Clear_Field(Screen.Get_Fld);
    end if;

    loop
      Screen.Inform(Kind);
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              if Decode then
                Set := True;
                exit;
              end if;
            when Afpx.Escape_Key =>
              Set := False;
              exit;
            when Afpx.Break_Key =>
              null;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Screen.List_Scroll_Fld_Range =>
              Screen.Scroll(Ptg_Result.Field_No);
            when Screen.Ok_Button_Fld =>
              if Decode then
                Set := True;
                exit;
              end if;
            when Screen.Cancel_Button_Fld =>
              Set := False;
              exit;
            when others =>
              null;
          end case;
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Refresh =>
          null;
      end case;
    end loop;
  end Read_Coordinate;


  -- Get a new degree
  procedure Read_Degree is

    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result : Afpx.Result_Rec;
    Degree : Natural;

    procedure Encode is
    begin
      Afpx.Encode_Field (Screen.Get_Fld, (0, 0),
                  Normal (Degree, Screen.Get_Get_Width, False));
    end Encode;

    function Decode return Boolean is
      Buff : As.U.Asu_Us;
      Ok : Boolean;
    begin
      Afpx.Decode_Field (Screen.Get_Fld, 0, Buff);
      Ok := Parse_Spaces(Buff);
      if Ok then
        begin
          Degree := Natural'Value(Buff.Image);
          if Degree < Points.P_Nb then
            Resol.R_Set_Degree(Degree);
          else
            Ok := False;
          end if;
        exception
          when Constraint_Error | Resol.R_Degree_Out =>
            Ok := False;
        end;
      end if;
      if Ok then
        return True;
      else
        Screen.Error (Screen.E_Wrong_Degree);
        Get_Handle.Cursor_Field := Screen.Init_For_Get;
        return False;
      end if;
    end Decode;

  begin
    Get_Handle.Cursor_Field := Screen.Init_For_Get;
    Screen.Put_Title (Screen.Get_Degree);
    Degree := Resol.R_Degree;
    Encode;

    loop
      Screen.Inform(Screen.I_Degree);
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              if Decode then
                exit;
              end if;
            when Afpx.Escape_Key =>
              exit;
            when Afpx.Break_Key =>
              null;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Screen.List_Scroll_Fld_Range =>
              Screen.Scroll(Ptg_Result.Field_No);
            when Screen.Ok_Button_Fld =>
              if Decode then
                exit;
              end if;
            when Screen.Cancel_Button_Fld =>
              exit;
            when others =>
              null;
          end case;
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Refresh =>
          null;
      end case;
    end loop;
  end Read_Degree;

  -- Display polynom
  procedure Put_Polynom (Polynom : Resol.Vector) is
    procedure Insert(Str : in String) is
      Rec : Afpx.Line_Rec;
    begin
      Rec.Len := Str'Length;
      Rec.Str (1 .. Rec.Len) := Language.String_To_Unicode (Str);
      Afpx.Line_List.Insert (Rec);
    end Insert;
  begin
    Screen.Put_Title(Screen.Polynom);
    Screen.Inform(Screen.I_Clear);
    -- Encode in list
    Afpx.Line_List.Delete_List;
    for I in Polynom'Range loop
      -- factor * X^ijkl
      Insert (Point_Str.Coordinate_Image(Polynom(I))
        & " * X^" & Normal (I - 1, Screen.Max_Degree_Width, Gap => '0'));
    end loop;
    -- Rewind
    Afpx.Line_List.Rewind;
    -- Go to top
    Afpx.Update_List (Afpx.Top);
    -- Let screen/afpx do the job
    Screen.Error (Screen.E_Done, Subtitle => True);
  end Put_Polynom;

  -- Display y=f(x)
  function Put_Yfx (Point : Points.P_T_One_Point) return Boolean is
    My_Fld : constant Afpx.Field_Range := Afpx_Xref.Compute.Yfx_Val;
    Go_On : Boolean;
  begin
    -- Enable Fx, enable and protect y (get field)
    Afpx.Set_Field_Colors (My_Fld, Foreground => Con_Io.Color_Of ("Cyan"));
    Afpx.Set_Field_Activation(Screen.Get_Fld, True);
    Afpx.Set_Field_Protection(Screen.Get_Fld, True);
    Afpx.Clear_Field(Screen.Get_Fld);

    -- Encode data
    Afpx.Encode_Field (My_Fld, (0,0),
      " F(" &
      Parse_Leading_Space (Point_Str.Coordinate_Image(Point.X))
      & ") =");
    Afpx.Encode_Field (Screen.Get_Fld, (0,0),
      Point_Str.Coordinate_Image(Point.Y));

    -- Let screen/afpx do the job
    Go_On := Screen.Confirm (Screen.C_Go_On, False, Subtitle => True);
    -- Clean up
    Afpx.Set_Field_Colors (My_Fld, Foreground => Con_Io.Color_Of ("Black"));
    Afpx.Clear_Field(My_Fld);
    return Go_On;
  end Put_Yfx;
end Dialog;

