with Ada.Strings.Unbounded;
with String_Mng;
package body Afpx_Typ is

  -- Check is square (relative to field) is in field
  function In_Field (Field  : in Field_Rec;
                     Square : in Con_Io.Full_Square) return Boolean is
  begin
    return   Square.Row < Field.Height
    and then Square.Col < Field.Width;
  end In_Field;

  -- Check is square (absolute) is in field
  function In_Field_Absolute (Field  : in Field_Rec;
                              Square : in Con_Io.Full_Square) return Boolean is
  begin
    return   Square.Row >= Field.Upper_Left.Row
    and then Square.Row <= Field.Lower_Right.Row
    and then Square.Col >= Field.Upper_Left.Col
    and then Square.Col <= Field.Lower_Right.Col;
  end In_Field_Absolute;

  -- Make Generic_Con_Io.Colors_Definition from Dscr Color_Names
  function To_Def (Names : Color_Names)
           return Generic_Con_Io.Colors_Definition is
    Colors : Generic_Con_Io.Colors_Definition;
    Last : Natural;
  begin
    -- Set the colors when using the first descriptor
    for I in Colors'Range loop
      Last := String_Mng.Parse_Spaces (Names(I), False);
      Colors(I) := Ada.Strings.Unbounded.To_Unbounded_String (
               Names(I)(1 .. Last));
    end loop;
    return Colors;
  end To_Def;

end Afpx_Typ;

