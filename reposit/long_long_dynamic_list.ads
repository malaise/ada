-- Dynamic list of private type
-- Indexes and positions are long_long
with Long_Long_Limited_List;
generic
  -- Type of the element of the list
  type Element_Type is private;

package Long_Long_Dynamic_List is

  procedure Set (To : out Element_Type; Val : in Element_Type);

  -- Instanciation of Limited_List with a private (non limited) type
  package Dyn_List is new Long_Long_Limited_List (Element_Type, Set);

end Long_Long_Dynamic_List;

