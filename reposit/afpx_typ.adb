with Str_Util;
package body Afpx_Typ is

  -- Check if square (relative to field) is in field
  function In_Field (Field  : in Field_Rec;
                     Square : in Con_Io.Square) return Boolean is
    (Square.Row < Field.Height and then Square.Col < Field.Data_Len);

  -- Check if square (absolute) is in field
  function In_Field_Absolute (Field  : in Field_Rec;
                              Square : in Con_Io.Square) return Boolean is
    (Square.Row >= Field.Upper_Left.Row
     and then Square.Row <= Field.Lower_Right.Row
     and then Square.Col >= Field.Upper_Left.Col
     and then Square.Col <= Field.Lower_Right.Col);

  -- Make Con_Io.Colors_Definition from Dscr Color_Names
  function To_Def (Names : Color_Names) return Con_Io.Colors_Definition is
    Colors : Con_Io.Colors_Definition;
  begin
    -- Set the colors when using the first descriptor
    for I in Colors'Range loop
      Colors(I) := As.U.Tus (Str_Util.Strip (Names(I), Str_Util.Tail));
    end loop;
    return Colors;
  end To_Def;

  function To_Names (Defs : Con_Io.Colors_Definition)
           return Color_Names is
    Names : Color_Names := (others => No_Color);
  begin
    for I in Names'Range loop
      Names(I)(1 .. Defs(I).Length) := Defs(I).Image;
    end loop;
    return Names;
  end To_Names;
end Afpx_Typ;

