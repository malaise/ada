with Afpx;
with Afpx_Xref;
package Utils.X is

  -- The scroll buttons, same for all descriptors that have a list
  subtype List_Scroll_Fld_Range is Afpx.Field_Range range
    Afpx_Xref.Main.Up .. Afpx_Xref.Main.Bottom;

  -- Protect a field and "revert" its colors
  procedure Protect_Field (Field_No : in Afpx.Absolute_Field_Range);

  -- Image of a Git branch
  function Branch_Image (Git_Branch : String) return String;

  -- Encode a line, procuste, preserve tail
  procedure Encode_Line (Head, Text, Tail : in String;
                         Width : in Afpx.Width_Range;
                         Line : in out Afpx.Line_Rec;
                         Keep_Tail : in Boolean := True);

  -- Encode Text in 1st column of Field, procuste, preserve Tail or head
  procedure Encode_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Keep_Tail : in Boolean := True);

  -- Center Text in 1st column of Field, procuste, preserve head
  procedure Center_Field (Text : in String;
                          Field : in Afpx.Field_Range);

end Utils.X;

