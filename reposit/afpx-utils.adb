with Str_Util;
package body Afpx.Utils is

  -- Scroll the list according to button
  -- Assumption is that buttons are consecutive in the order:
  -- Top(1), PgUp(2), Up(3), Center(4), Down(5), PgDown(6), Bottom(7)
  procedure Scroll (Offset : in Offset_Range) is
  begin
    case Offset is
      when 1 => Update_List(Top);
      when 2 => Update_List(Page_Up);
      when 3 => Update_List(Up);
      when 4 => Update_List(Center_Selected);
      when 5 => Update_List(Down);
      when 6 => Update_List(Page_Down);
      when 7 => Update_List(Bottom);
    end case;
  end Scroll;

  -- Initialize Afpx list from From list
  procedure Init_List (From : in out Element_List.Dyn_List.List_Type) is
    Pos : Positive;
    Elt : Element_Type;
    Moved : Boolean;
    Line : Line_Rec;
  begin
    -- Delete Afpx list
    Line_List.Delete_List (Deallocate);
    -- Done if empty list
    if From.Is_Empty then
      return;
    end if;
    -- Save position in list
    Pos := From.Get_Position;

    -- Copy list
    From.Rewind;
    loop
      From.Read (Elt, Moved => Moved);
      Set (Line, Elt);
      Line_List.Insert (Line);
      exit when not Moved;
    end loop;

    -- Set both lists at pos
    From.Move_At (Pos);
    Line_List.Move_At (Pos);
  end Init_List;

  -- If Str fits Width then return Str, padded with space if no Align_Left
  -- else return ">>" & tail to match Width (if Keep_Tail)
  --   or return head to match Width and "<<" (if not Keep_Tail)
  function Procuste (Str : String;
                     Len : Positive;
                     Align_Left : Boolean := True;
                     Keep_Tail : Boolean := True) return String is
  begin
    return Str_Util.Procuste (Str, Len, Align_Left, ' ',
                              Trunc_Head => Keep_Tail,
                              Show_Trunc => True,
                              Head_Mark  => ">>",
                              Tail_Mark  => "<<");
  end Procuste;

  -- Protect a field and "revert" its colors, or reset it to its default
  procedure Protect_Field (Field_No : in Afpx.Absolute_Field_Range;
                           Protect  : in Boolean) is
    Foreground, Backround, Selected : Con_Io.Effective_Colors;
  begin
    if Protect then
      Afpx.Set_Field_Protection (Field_No, True);
      Afpx.Get_Field_Colors (Field_No, Foreground, Backround, Selected);
      Afpx.Set_Field_Colors (Field_No,
            Foreground => Foreground,
            Background => Afpx.Get_Descriptor_Background);
    else
      Afpx.Reset_Field (Field_No, Reset_String => False);
    end if;
  end Protect_Field;

  -- Encode a line, procuste on Text, preserving tail or head of Text
  procedure Encode_Line (Head, Text, Tail : in String;
                         Width : in Afpx.Width_Range;
                         Line : in out Afpx.Line_Rec;
                         Keep_Tail : in Boolean := True) is
  begin
    Afpx.Encode_Line (Line,
        Head & Procuste (Text, Width - Head'Length - Tail'Length,
                         True, Keep_Tail)
             & Tail);
  end Encode_Line;

  -- Encode Text in 1st column of Row of Field, procuste,
  --  preserve Tail or head
  procedure Encode_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Row : in Con_Io.Row_Range;
                          Clear : in Boolean := True;
                          Keep_Tail : in Boolean := True) is
  begin
    if Clear then
      Afpx.Clear_Field (Field);
    end if;
    Afpx.Encode_Field (Field, (Row, 0),
        Procuste (Text,
                   (if Afpx.Is_Get_Kind (Field) then Afpx.Get_Data_Len (Field)
                    else Afpx.Get_Field_Width (Field)),
                   True, Keep_Tail));
  end Encode_Field;

  -- Clear field and Center Text in 1st column of Field (row 0 or 1)
  --  procuste, preserve head
  procedure Center_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Row : in Con_Io.Row_Range;
                          Keep_Head : in Boolean := True) is
  begin
    Afpx.Clear_Field (Field);
    if Text'Length <= Afpx.Get_Field_Width (Field) then
      Afpx.Encode_Field (Field, (Row, 0),
          Str_Util.Center (Text, Afpx.Get_Field_Width (Field)));
    else
      Afpx.Encode_Field (Field, (Row, 0),
          Procuste (Text, Afpx.Get_Field_Width (Field),
                    True, not Keep_Head));
    end if;
  end Center_Field;

end Afpx.Utils;

