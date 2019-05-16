with Afpx, Con_Io;
with Afpx_Xref;
package Utils.X is

  -- The scroll buttons, same for all descriptors that have a list
  subtype List_Scroll_Fld_Range is Afpx.Field_Range range
    Afpx_Xref.Main.Top .. Afpx_Xref.Main.Bottom;

  -- Image of a Git branch
  function Branch_Image (Git_Branch : String) return String;

  -- Encode current branch
  procedure Encode_Branch (Field_No : in Afpx.Absolute_Field_Range);

  -- Clear field and encode Text in 1st column of Field (row 0 or 1)
  --  procuste, preserve tail or head
  procedure Encode_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Keep_Tail : in Boolean := True;
                          Show_Cut : Boolean := True);

  -- Clear row and encode Text in 1st column of row of Field
  --  procuste, preserve tail or head
  procedure Encode_Row (Text : in String;
                        Field : in Afpx.Field_Range;
                        Row : in Con_Io.Row_Range;
                        Keep_Tail : in Boolean := True;
                        Show_Cut : Boolean := True);

  -- Clear field and Center Text in 1st column of Field (row 0 or 1)
  --  procuste, preserve head or not
  procedure Center_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Keep_Head : in Boolean := True;
                          Show_Cut : Boolean := True;
                          Offset : Integer := 0);

end Utils.X;

