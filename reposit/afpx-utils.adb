with Aski.Unicode, Language, Long_Longs;
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

  -- Backup current line of Afpx list in a context, and later restore it
  procedure Backup (In_Context : in out Backup_Context) is
  begin
    if Afpx.Line_List.Is_Empty then
      In_Context.Content.Len := 0;
      In_Context.Position := 0;
    else
      In_Context.Content := Afpx.Line_List.Access_Current.all;
      In_Context.Position := Afpx.Line_List.Get_Position;
    end if;
  end Backup;

  -- Restore based on line content and position
  function Line_Search is new Afpx.Line_List_Mng.Search ("=");
  procedure Restore (From_Context : in Backup_Context;
                     Default_To_Top : in Boolean := True;
                     Force_Position : in Boolean := False) is
    use type Long_Longs.Llu_Natural;
  begin
    if Afpx.Line_List.Is_Empty or else From_Context.Position = 0 then
      return;
    end if;
    if Force_Position then
      -- Restore position or default
      if From_Context.Position <= Afpx.Line_List.List_Length then
        Afpx.Line_List.Move_At (From_Context.Position);
      else
        Afpx.Line_List.Rewind ( (if Default_To_Top then Afpx.Line_List_Mng.Next
                                 else Afpx.Line_List_Mng.Prev));
      end if;
      return;
    end if;
    -- Quick win if saved position matches
    if From_Context.Position <= Afpx.Line_List.List_Length then
      Afpx.Line_List.Move_At (From_Context.Position);
      if Afpx.Line_List.Access_Current.all = From_Context.Content then
        return;
      end if;
    end if;
    -- Search first occurence
    if Line_Search (Afpx.Line_List, From_Context.Content,
                    Afpx.Line_List_Mng.Next, 1,
                    Afpx.Line_List_Mng.Current_Absolute) then
      -- Search second occurence
      if not Line_Search  (Afpx.Line_List, From_Context.Content,
                           Afpx.Line_List_Mng.Next, 1,
                           Afpx.Line_List_Mng.Skip_Current) then
        -- Only one occurence => this one
        return;
      end if;
    end if;
    -- Multiple or no match => Default
    Afpx.Line_List.Rewind ( (if Default_To_Top then Afpx.Line_List_Mng.Next
                             else Afpx.Line_List_Mng.Prev));
  end Restore;

  -- Reset context
  procedure Reset (Context : in out Backup_Context) is
  begin
    Context := (others => <>);
  end Reset;

  -- Internal Procuste on Unicode_Sequence
  subtype Us is Language.Unicode_Sequence;
  -- If Str fits Width then return Str, padded with space if no Align_Left
  -- else return ">>" & tail to match Width (if Keep_Tail)
  --   or return head to match Width and "<<" (if not Keep_Tail)
  function Procuste (Seq : Us;
                     Len : Positive;
                     Align_Left : Boolean := True;
                     Keep_Tail : Boolean := True;
                     Show_Cut : Boolean := True) return Us is
    L : constant Natural := Seq'Length;
    Res : Language.Unicode_Sequence (1 .. Len);
  begin
    if L < Len then
      -- Str is shorter than Len: Pad
      if Align_Left then
        -- Copy L characters at left and pad
        Res(1 .. L) := Seq;
        Res(L + 1 .. Len) := (others => Aski.Unicode.Spc_U);
      else
        -- Copy L characters at right and pad
        Res(Len - L + 1 .. Len) := Seq;
        Res(1 .. Len - L) := (others => Aski.Unicode.Spc_U);
      end if;
    elsif L > Len then
      -- Str is larger than Len: Trunc
      if Keep_Tail then
        if Show_Cut and then Len >= 2 then
          -- Copy ">>" then Len-2 last characters of Str
          Res := Language.String_To_Unicode (">>")
                   & Seq(Seq'Last - Len + 1 + 2 .. Seq'Last);
        else
          -- Copy Len last characters of Str
          Res := Seq(Seq'Last - Len + 1 .. Seq'Last);
        end if;
      else
        if Show_Cut and then Len >= 2 then
          -- Copy Len-2 first characters of Str then " <"
          Res := Seq(Seq'First .. Seq'First + Len - 1 - 2)
                   &  Language.String_To_Unicode ("<<");
        else
          -- Copy Len first characters of Str
          Res := Seq(Seq'First .. Seq'First + Len - 1);
        end if;
      end if;
    else
      -- Str is as Len characters: copy
      Res := Seq;
    end if;
    return Res;
  end Procuste;

  -- Internal Center on Unicode_Sequence
  -- Center a String Str in a fixed size
  -- if Str <= Size pad with Gap after then before Str
  -- if Str > Size  raise Constraint_Error
  function Center (Str : Us;
                   Len : Positive;
                   Gap : Character := ' ';
                   Offset : Integer := 0) return Us is
    Start : Integer;
  begin
    if Str'Length > Len then
      raise Constraint_Error;
    end if;
    -- Start position
    Start := (Len - Str'Length) / 2 + 1 + Offset;
    if Start <= 0 then
      Start := 1;
    end if;
    if Start + Str'Length > Len  + 1 then
      Start := Len - Str'Length + 1;
    end if;
    -- Copy
    declare
      Res : Us(1 .. Len) := (others => Language.Char_To_Unicode (Gap));
    begin
      Res(Start .. Start + Str'Length - 1) := Str;
      return Res;
    end;
  end Center;

  -- If Str fits Width then return Str, padded with space if no Align_Left
  -- else return ">>" & tail to match Width (if Keep_Tail)
  --   or return head to match Width and "<<" (if not Keep_Tail)
  function Procuste (Str : String;
                     Len : Positive;
                     Align_Left : Boolean := True;
                     Keep_Tail : Boolean := True;
                     Show_Cut : Boolean := True) return String is
  begin
    return Language.Unicode_To_String (Procuste (
        Language.String_To_Unicode (Str),
        Len, Align_Left, Keep_Tail, Show_Cut) );
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
    Uhead : constant Us := Language.String_To_Unicode (Head);
    Utext : constant Us := Language.String_To_Unicode (Text);
    Utail : constant Us := Language.String_To_Unicode (Tail);
  begin
    Clean_Line (Line);
    Afpx.Encode_Line (Line,
        Uhead & Procuste (Utext, Width - Uhead'Length - Utail'Length,
                         True, Keep_Tail, Show_Cut)
             & Utail);
  end Encode_Line;

  -- Center Head+Text+Tail in Line, procuste on Text,
  -- preserving tail or head of Text
  procedure Center_Line (Head, Text, Tail : in String;
                         Width : in Afpx.Width_Range;
                         Line : out Afpx.Line_Rec;
                         Keep_Head : in Boolean := True;
                         Show_Cut : Boolean := True) is
    Uhead : constant Us := Language.String_To_Unicode (Head);
    Utext : constant Us := Language.String_To_Unicode (Text);
    Utail : constant Us := Language.String_To_Unicode (Tail);
  begin
    if Uhead'Length + Utext'Length + Utail'Length <= Width then
      -- Full text fits => center full text
      Afpx.Encode_Line (Line,
          Center (Uhead & Utext & Utail, Width));
    elsif Uhead'Length + 2 + Tail'Length <= Width then
      -- Procusting Text is enough => Procuste only Text
      Afpx.Encode_Line (Line,
        Uhead & Procuste (Utext, Width - Uhead'Length - Utail'Length,
                         True, not Keep_Head, Show_Cut) & Utail);
    else
      -- Procusting Text is not enough => Procuste full text
      Afpx.Encode_Line (Line,
        Procuste (Uhead & Utext & Utail, Width, True, not Keep_Head, Show_Cut));
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
    Utext : constant Us := Language.String_To_Unicode (Text);
  begin
    if Clear then
      Afpx.Clear_Field (Field);
    end if;
    Afpx.Encode_Field (Field, (Row, 0),
        Procuste (Utext,
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
                          Show_Cut : Boolean := True;
                          Offset : Integer := 0) is
    Utext : constant Us := Language.String_To_Unicode (Text);
    Width : constant Afpx.Width_Range := Afpx.Get_Field_Width (Field);
  begin
    Afpx.Clear_Field (Field);
    if Utext'Length <= Afpx.Get_Field_Width (Field) then
      Afpx.Set_Half_Col_Offset (Field, Row, Width rem 2 /= Utext'Length rem 2);
      Afpx.Encode_Field (Field, (Row, 0),
          Center (Utext, Width, Offset => Offset) );
    else
      Afpx.Encode_Field (Field, (Row, 0),
          Procuste (Utext, Width, True, not Keep_Head, Show_Cut));
    end if;
  end Center_Field;

end Afpx.Utils;

