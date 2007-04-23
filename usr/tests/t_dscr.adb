with Ada.Text_Io;
with Argument, Con_Io, Afpx, Normal, Mixed_Str, Language;
procedure T_Dscr is
  Dscr_No : Afpx.Descriptor_Range;
  Cursor_Field : Afpx.Absolute_Field_Range;
  Cursor_Col : Con_Io.Col_Range;
  Redisplay : Boolean;
  Ptg_Result : Afpx.Result_Rec;
  Line : Afpx.Line_Rec;

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line("ERROR. Usage " & Argument.Get_Program_Name
                                     & " [ <dscr_no> ]");
  end Usage;

  procedure Set_Dscr(No : in Afpx.Descriptor_Range) is
  begin
    Afpx.Use_Descriptor(No);
    Cursor_Field := Afpx.Next_Cursor_Field(0);

    -- Dump descriptor
    Ada.Text_Io.Put ("Descriptor" & No'Img & " has");
    if Afpx.Has_List then
      Ada.Text_Io.Put (" a");
    else
      Ada.Text_Io.Put (" no");
    end if;
    Ada.Text_Io.Put_Line (" list and"
      & Afpx.Absolute_Field_Range'Image (Afpx.Nb_Fields)
      & " fields.");
    for I in 1 .. Afpx.Nb_Fields loop
      Ada.Text_Io.Put_Line ("Field No" & I'Img & " is of kind "
         & Mixed_Str (Afpx.Field_Kind_List'Image(Afpx.Get_Field_Kind (I))));
    end loop;
    Ada.Text_Io.Put_Line ("Cursor field is" & Cursor_Field'Img);

    -- Fix Cursor_Field for Put_Then_Get
    if Cursor_Field not in Afpx.Field_Range then
      -- No get field
      Cursor_Field := Afpx.Field_Range'First;
    end if;
    Cursor_Col := 0;
    Redisplay := False;
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

  for I in 1 .. 999 loop
    Line.Str(1 .. 3) := Language.String_To_Wide (Normal(I, 3, Gap => '0'));
    Line.Len := 3;
    Afpx.Line_List_Mng.Insert (Afpx.Line_List, Line);
  end loop;
  Afpx.Line_List_Mng.Rewind(Afpx.Line_List);

  Set_Dscr(Dscr_No);

  loop
    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Ptg_Result, Redisplay);
    Redisplay := False;
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            null;
          when Afpx.Escape_Key =>
            Dscr_No := Dscr_No + 1;
            Set_Dscr(Dscr_No);
          when Afpx.Break_Key =>
            exit;
        end case;
      when Afpx.Mouse_Button =>
        null;
      when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event | Afpx.Wakeup_Event =>
        null;
      when Afpx.Refresh =>
        Redisplay := True;
    end case;
  end loop;

end T_Dscr;

