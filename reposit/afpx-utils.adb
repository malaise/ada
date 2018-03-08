with Aski.Unicode, Str_Util;
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
  procedure Init_List (From : in out Element_List.List_Type) is
    Pos : Line_List_Mng.Ll_Positive;
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
                     Keep_Tail : Boolean := True;
                     Show_Cut : Boolean := True) return String is
  begin
    return Str_Util.Procuste (Str, Len, Align_Left, ' ',
                              Trunc_Head => Keep_Tail,
                              Show_Trunc => True,
                              Head_Mark  => (if Show_Cut then ">>" else ""),
                              Tail_Mark  => (if Show_Cut then "<<" else ""));
  end Procuste;

  -- Protect a field and "revert" its colors, or reset it to its default
  procedure Protect_Field (Field_No : in Afpx.Absolute_Field_Range;
                           Protect  : in Boolean) is
    Foreground, Backround, Selected : Con_Io.Effective_Colors;
    Activated : Boolean;
  begin
    if Protect then
      Afpx.Set_Field_Protection (Field_No, True);
      Afpx.Get_Field_Colors (Field_No, Foreground, Backround, Selected);
      Afpx.Set_Field_Colors (Field_No,
            Foreground => Foreground,
            Background => Afpx.Get_Descriptor_Background);
    else
      -- Reset all except field activation
      Activated := Afpx.Get_Field_Activation (Field_No);
      Afpx.Reset_Field (Field_No, Reset_String => False);
      Afpx.Set_Field_Activation (Field_No, Activated);
    end if;
  end Protect_Field;

  -- Clean (fill with spaces) the Str field of the line
  procedure Clean_Line (Line : in out Afpx.Line_Rec) is
  begin
    Line.Str := (others => Aski.Unicode.Spc_U);
  end Clean_Line;

  -- Encode a line, procuste on Text, preserving tail or head of Text
  procedure Encode_Line (Head, Text, Tail : in String;
                         Width : in Afpx.Width_Range;
                         Line : out Afpx.Line_Rec;
                         Keep_Tail : in Boolean := True;
                         Show_Cut : Boolean := True) is
  begin
  Clean_Line (Line);
    Afpx.Encode_Line (Line,
        Head & Procuste (Text, Width - Head'Length - Tail'Length,
                         True, Keep_Tail, Show_Cut)
             & Tail);
  end Encode_Line;

  -- Center Head+Text+Tail in Line, procuste on Text,
  -- preserving tail or head of Text
  procedure Center_Line (Head, Text, Tail : in String;
                         Width : in Afpx.Width_Range;
                         Line : out Afpx.Line_Rec;
                         Keep_Head : in Boolean := True;
                         Show_Cut : Boolean := True) is
  begin
    if Head'Length + Text'Length + Tail'Length <= Width then
      -- Full text fits => center full text
      Afpx.Encode_Line (Line, Str_Util.Center (Head & Text & Tail, Width));
    elsif Head'Length + 2 + Tail'Length <= Width then
      -- Procusting Text is enough => Procuste only Text
      Afpx.Encode_Line (Line,
        Head & Procuste (Text, Width - Head'Length - Tail'Length,
                         True, not Keep_Head, Show_Cut) & Tail);
    else
      -- Procusting Text is not enough => Procuste full text
      Afpx.Encode_Line (Line,
        Procuste (Head & Text & Tail, Width, True, not Keep_Head, Show_Cut));
    end if;
  end Center_Line;

  -- Encode Text in 1st column of Row of Field, procuste,
  --  preserve Tail or head
  procedure Encode_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Row : in Con_Io.Row_Range;
                          Clear : in Boolean := True;
                          Keep_Tail : in Boolean := True;
                          Show_Cut : Boolean := True) is
  begin
    if Clear then
      Afpx.Clear_Field (Field);
    end if;
    Afpx.Encode_Field (Field, (Row, 0),
        Procuste (Text,
                   (if Afpx.Is_Get_Kind (Field) then Afpx.Get_Data_Len (Field)
                    else Afpx.Get_Field_Width (Field)),
                   True, Keep_Tail, Show_Cut));
  end Encode_Field;

  -- Clear field and Center Text in 1st column of Field (row 0 or 1)
  --  procuste, preserve head
  procedure Center_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Row : in Con_Io.Row_Range;
                          Keep_Head : in Boolean := True;
                          Show_Cut : Boolean := True) is
  begin
    Afpx.Clear_Field (Field);
    if Text'Length <= Afpx.Get_Field_Width (Field) then
      Afpx.Encode_Field (Field, (Row, 0),
          Str_Util.Center (Text, Afpx.Get_Field_Width (Field)));
    else
      Afpx.Encode_Field (Field, (Row, 0),
          Procuste (Text, Afpx.Get_Field_Width (Field),
                    True, not Keep_Head, Show_Cut));
    end if;
  end Center_Field;

end Afpx.Utils;

