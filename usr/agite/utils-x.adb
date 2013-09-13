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

  -- Image of a Git branch
  function Branch_Image (Git_Branch : String; Width : Afpx.Width_Range)
                        return String is
  begin
    if Git_Branch = ("(no branch)") then
      return "None.";
    else
      return "Br: " & Normalize (Git_Branch, Width - 4);
    end if;
  end Branch_Image;


end Utils.X;

