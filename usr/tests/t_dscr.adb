with Basic_Proc, Argument, Afpx, Con_Io, Normal, Mixed_Str, Language, Int_Img;
procedure T_Dscr is
  Dscr_No : Afpx.Descriptor_Range;
  Get_Handle : Afpx.Get_Handle_Rec;
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
    Upper_Left, Lower_Right : Con_Io.Square;
    Height : Afpx.Height_Range;
    Width  : Afpx.Width_Range;
    use type Afpx.Field_Kind_List, Afpx.Absolute_Field_Range;
  begin
    Afpx.Use_Descriptor(No);
    Get_Handle.Cursor_Field := Afpx.Next_Cursor_Field(0);

    -- Dump descriptor
    Basic_Proc.New_Line_Output;
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
      Basic_Proc.Put_Output (
         if I = Afpx.List_Field_No then "List"
         else "Field" & I'Img & " kind "
            & Mixed_Str (Afpx.Field_Kind_List'Image(Afpx.Get_Field_Kind (I)))
         );
      Afpx.Get_Field_Geometry (I, Upper_Left, Lower_Right);
      Afpx.Get_Field_Size (I, Height, Width);
      Basic_Proc.Put_Line_Output (
        ": (" & Int_Img (Upper_Left.Row)
        & "," & Int_Img (Upper_Left.Col)
        & ")-(" & Int_Img (Lower_Right.Row)
        & "," & Int_Img (Lower_Right.Col)
        & ") => " & Int_Img (Afpx.Get_Field_Height (I))
         & "x"  & Int_Img (Afpx.Get_Field_Width (I))
         & (if I in Afpx.Field_Range
            and then Afpx.Get_Field_Kind (I) = Afpx.Get_Field then
              "-" & Int_Img (Afpx.Get_Data_Len (I))
            else "") );
    end loop;
    Basic_Proc.Put_Line_Output ("First cursor field is"
                              & Get_Handle.Cursor_Field'Img);

    -- Fix Cursor_Field for Put_Then_Get
    if Get_Handle.Cursor_Field not in Afpx.Field_Range then
      -- No get field
      Get_Handle.Cursor_Field := Afpx.Field_Range'First;
    end if;
    Get_Handle.Cursor_Col := 0;
    Get_Handle.Insert := False;
  end Set_Dscr;

  use type Afpx.Descriptor_Range;
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

  -- Get and prind screen size
  Afpx.Get_Screen_Size (Height, Width);
  Basic_Proc.Put_Line_Output ("Screen is " & Int_Img (Height)
                                     & "x" & Int_Img (Width));

  -- Set first descriptor
  Set_Dscr(Dscr_No);

  -- Show
  One_Dscr:
  loop
    Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            Next_Dscr:
            loop
              Dscr_No := Dscr_No + 1;
              exit Next_Dscr when Afpx.Is_Descriptor_Defined (Dscr_No);
              exit One_Dscr when Dscr_No > 42;
            end loop Next_Dscr;
            Set_Dscr(Dscr_No);
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

