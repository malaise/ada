with Con_Io, Afpx.Utils;
with Git_If;
package body Utils.X is

  -- Image of a Git branch
  function Branch_Image (Git_Branch : String) return String is
  begin
    if Git_Branch = "" then
      return "?";
    else
      return Git_Branch;
    end if;
  end Branch_Image;

  -- Encode current branch
  procedure Encode_Branch (Field_No : in Afpx.Absolute_Field_Range) is
  begin
    Encode_Field (Branch_Image (Git_If.Current_Branch), Field_No);
  end Encode_Branch;

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
    Afpx.Utils.Encode_Field (Text, Field, Row (Field), True, Keep_Tail);
  end Encode_Field;

  -- CLear field and Center Text in 1st column of Field (row 0 or 1)
  --  procuste, preserve head
  procedure Center_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Keep_Head : in Boolean := True) is
  begin
    Afpx.Utils.Center_Field (Text, Field, Row (Field), Keep_Head);
  end Center_Field;

end Utils.X;

