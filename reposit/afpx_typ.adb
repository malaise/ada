package body Afpx_Typ is

  -- Check is square (relative to field) is in field
  function In_Field (Field  : in Field_Rec;
                     Square : in Con_Io.Square) return Boolean is
  begin
    return   Square.Row < Field.Height
    and then Square.Col < Field.Width;
  end In_Field;

  -- Check is square (absolute) is in field
  function In_Field_Absolute (Field  : in Field_Rec;
                              Square : in Con_Io.Square) return Boolean is
  begin
    return   Square.Row >= Field.Upper_Left.Row
    and then Square.Row <= Field.Lower_Right.Row
    and then Square.Col >= Field.Upper_Left.Col
    and then Square.Col <= Field.Lower_Right.Col;
  end In_Field_Absolute;

end Afpx_Typ;

