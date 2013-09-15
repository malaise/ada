with Basic_Proc, Argument, Con_Io, Afpx, Normal, Mixed_Str, Language, Images;
procedure T_Dscr is
  Dscr_No : Afpx.Descriptor_Range;
  Cursor_Field : Afpx.Absolute_Field_Range;
  Cursor_Col : Con_Io.Col_Range;
  Insert : Boolean;
  Ptg_Result : Afpx.Result_Rec;
  Line : Afpx.Line_Rec;

  Height : Afpx.Height_Range;
  Width : Afpx.Width_Range;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output("ERROR. Usage " & Argument.Get_Program_Name
                                     & " [ <dscr_no> ]");
  end Usage;

  procedure Set_Dscr(No : in Afpx.Descriptor_Range) is
    Start : Afpx.Absolute_Field_Range;
    use type Afpx.Absolute_Field_Range;
  begin
    Afpx.Use_Descriptor(No);
    Cursor_Field := Afpx.Next_Cursor_Field(0);

    -- Dump descriptor
    Basic_Proc.Put_Output ("Descriptor" & No'Img & " has");
    if Afpx.Has_List then
      Basic_Proc.Put_Output (" a");
      Start := Afpx.List_Field_No;
    else
      Basic_Proc.Put_Output (" no");
      Start := Afpx.Field_Range'First;
    end if;
    Basic_Proc.Put_Line_Output (" list and"
      & Afpx.Absolute_Field_Range'Image (Afpx.Nb_Fields)
      & " fields.");
    for I in Start .. Afpx.Nb_Fields loop
      Basic_Proc.Put_Line_Output (
         (if I = Afpx.List_Field_No then "List"
          else "Field No" & I'Img & " is of kind "
             & Mixed_Str (Afpx.Field_Kind_List'Image(Afpx.Get_Field_Kind (I))) )
         & ", " & Images.Integer_Image (Afpx.Get_Field_Height (I))
         & "x"  & Images.Integer_Image (Afpx.Get_Field_Width (I)));
    end loop;
    Basic_Proc.Put_Line_Output ("Cursor field is" & Cursor_Field'Img);

    -- Fix Cursor_Field for Put_Then_Get
    if Cursor_Field not in Afpx.Field_Range then
      -- No get field
      Cursor_Field := Afpx.Field_Range'First;
    end if;
    Cursor_Col := 0;
    Insert := False;
  end Set_Dscr;

  use Afpx;
begin

  if Argument.Get_Nbre_Arg > 1 then
    Usage;
    return;
  end if;

  if Argument.Get_Nbre_Arg = 1 then
    begin
      Dscr_No := Afpx.Descriptor_Range'Value(Argument.Get_Parameter);
    exception
      when others =>
        Usage;
        return;
    end;
  else
    Dscr_No := 1;
  end if;

  -- Fill list
  for I in 1 .. 999 loop
    Line.Str(1 .. 3) := Language.Copy (Normal(I, 3, Gap => '0'));
    Line.Len := 3;
    Afpx.Line_List.Insert (Line);
  end loop;
  Afpx.Line_List.Rewind;

  -- Set a first dscr to get screen size
  Afpx.Use_Descriptor(Dscr_No);

  -- Get and prind screen size
  Afpx.Get_Screen_Size (Height, Width);
  Basic_Proc.Put_Line_Output ("Screen is " & Images.Integer_Image (Height)
                                     & "x" & Images.Integer_Image (Width));

  -- Set first descriptor
  Set_Dscr(Dscr_No);

  -- Show
  One_Dscr:
  loop
    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result);
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            Next_Dscr:
            loop
              Dscr_No := Dscr_No + 1;
              begin
                Set_Dscr(Dscr_No);
                exit Next_Dscr;
              exception
                when Afpx.No_Descriptor =>
                  exit One_Dscr when Dscr_No > 21;
              end;
            end loop Next_Dscr;
          when Afpx.Escape_Key | Afpx.Break_Key =>
            exit One_Dscr;
        end case;
      when Afpx.Mouse_Button =>
        null;
      when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
         | Afpx.Refresh =>
        null;
    end case;
  end loop One_Dscr;

end T_Dscr;

