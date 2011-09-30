with Afpx;
package Utils.X is

  -- The scroll buttons
  subtype List_Scroll_Fld_Range is Afpx.Field_Range range 2 .. 8;

  -- Protect a field and "revert" its colors
  procedure Protect_Field (Field_No : in Afpx.Absolute_Field_Range);

end Utils.X;

