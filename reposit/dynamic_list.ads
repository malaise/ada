with Limited_List;
generic
  -- Type of the element of the list
  type Element_Type is private;

package Dynamic_List is

  procedure Set (To : in out Element_Type; Val : in Element_Type);

  package Dyn_List is new Limited_List (Element_Type, Set);

end Dynamic_List;

