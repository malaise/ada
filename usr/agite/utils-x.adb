with Str_Util;
package body Utils.X is

  -- Protect a field and "revert" its colors
  procedure Protect_Field (Field_No : in Afpx.Absolute_Field_Range;
                           Protect  : in Boolean) is
  begin
    if Protect then
      Afpx.Set_Field_Protection (Field_No, True);
      Afpx.Set_Field_Colors (Field_No,
            Foreground => Con_Io.Color_Of ("Black"),
            Background => Afpx.Get_Descriptor_Background);
    else
      Afpx.Reset_Field (Field_No, Reset_String => False);
    end if;
  end Protect_Field;

  -- Image of a Git branch
  function Branch_Image (Git_Branch : String) return String is
  begin
    if Git_Branch = "" then
      return "?";
    else
      return Git_Branch;
    end if;
  end Branch_Image;

  -- Encode Head, Text and Tail in a line,
  -- Procuste on Text, preserve tail of Text
  procedure Encode_Line (Head, Text, Tail : in String;
                         Width : in Afpx.Width_Range;
                         Line : in out Afpx.Line_Rec;
                         Keep_Tail : in Boolean := True) is
  begin
    Afpx.Encode_Line (Line,
        Head & Normalize (Text, Width - Head'Length - Tail'Length, Keep_Tail)
             & Tail);
  end Encode_Line;

  -- Row for encoding in a field
  function Row (Field : Afpx.Field_Range) return Con_Io.Row_Range is
  begin
    return (if Afpx.Get_Field_Height (Field) = 1 then 0 else 1);
  end Row;

  -- Clear field and encode Text in 1st column of Field (row 0)
  --  procuste, preserve tail or head
  procedure Encode_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Keep_Tail : in Boolean := True) is
  begin
    Afpx.Clear_Field (Field);
    Afpx.Encode_Field (Field, (Row (Field), 0),
        Normalize (Text,
                   (if Afpx.Is_Get_Kind (Field) then Afpx.Get_Data_Len (Field)
                    else Afpx.Get_Field_Width (Field)),
                   Keep_Tail));
  end Encode_Field;

  -- Encode Text in 1st column of Row of Field, procuste
  --  preserve Tail or head
  procedure Encode_Row (Text : in String;
                        Field : in Afpx.Field_Range;
                        Row : Con_Io.Row_Range;
                        Keep_Tail : in Boolean := True) is
  begin
    Afpx.Encode_Field (Field, (Row, 0),
        Normalize (Text,
                   (if Afpx.Is_Get_Kind (Field) then Afpx.Get_Data_Len (Field)
                    else Afpx.Get_Field_Width (Field)),
                   Keep_Tail));
  end Encode_Row;

  -- CLear field and Center Text in 1st column of Field (row 0 or 1)
  --  procuste, preserve head
  procedure Center_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Keep_Head : in Boolean := True) is
  begin
    Afpx.Clear_Field (Field);
    if Text'Length <= Afpx.Get_Field_Width (Field) then
      Afpx.Encode_Field (Field, (Row (Field), 0),
          Str_Util.Center (Text, Afpx.Get_Field_Width (Field)));
    else
      Afpx.Encode_Field (Field, (Row (Field), 0),
          Normalize (Text, Afpx.Get_Field_Width (Field),
                     Keep_Tail => not Keep_Head));
    end if;
  end Center_Field;

end Utils.X;

