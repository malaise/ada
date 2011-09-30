with Con_Io;
package body Utils.X is

  -- Protect a field and "revert" its colors
  procedure Protect_Field (Field_No : in Afpx.Absolute_Field_Range) is
  begin
    Afpx.Set_Field_Protection (Field_No, True);
    Afpx.Set_Field_Colors (Field_No,
          Foreground => Con_Io.Color_Of ("Black"),
          Background => Afpx.Get_Descriptor_Background);
  end Protect_Field;

end Utils.X;

