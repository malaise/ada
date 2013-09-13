with Afpx;
with Afpx_Xref;
package Utils.X is

  -- The scroll buttons, same for all descriptors that have a list
  subtype List_Scroll_Fld_Range is Afpx.Field_Range range
    Afpx_Xref.Main.Up .. Afpx_Xref.Main.Bottom;

  -- Protect a field and "revert" its colors
  procedure Protect_Field (Field_No : in Afpx.Absolute_Field_Range);

  -- Image of a Git branch
  function Branch_Image (Git_Branch : String; Width : Afpx.Width_Range)
                        return String;

end Utils.X;

