with Dynamic_List;
package Afpx.List_Manager is

  -- Scroll the list according to button
  -- Assumption is that buttons are consecutive in the order:
  -- Top(1), PgUp(2), Up(3), Center(4), Down(5), PgDown(6), Bottom(7)
  subtype Offset_Range is Field_Range range 1 .. 7;
  procedure Scroll (Offset : in Offset_Range);

  generic

    -- Type of the element of the list
    type Element_Type is private;
    -- List manager of elements
    with package Element_List is new Dynamic_List(Element_Type);
    -- How to set the Afpx line from Element_Type
    with procedure Set (Line : in out Line_Rec; From : in Element_Type);
  -- Initialize Afpx list from From list, move to current position of From
  procedure Init_List (From : in out Element_List.Dyn_List.List_Type);

end Afpx.List_Manager;

