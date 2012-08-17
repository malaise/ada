with Afpx;
with Afpx_Xref;
package Utils.X is

  -- The scroll buttons, same for all descriptors that have a list
  subtype List_Scroll_Fld_Range is Afpx.Field_Range range
    Afpx_Xref.Main.Up .. Afpx_Xref.Main.Bottom;

  -- Protect a field and "revert" its colors
  procedure Protect_Field (Field_No : in Afpx.Absolute_Field_Range);

end Utils.X;

