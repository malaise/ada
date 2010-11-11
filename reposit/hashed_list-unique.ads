generic
package Hashed_List.Unique is

  -----------------
  -- UNIQUE LIST --
  -----------------
  -- A unique list is hashed list where at most one element of a given value
  --  (in the sense of "=") is stored.
  -- Storing an element leads to either
  --  - insertion (when the element is new)
  --  - replacement of the previous element by the new
  --  - drop the new value and keep the original

  type Unique_List_Type is new List_Type with null record;


  -- Check if an element exists in the list
  procedure Search (List : in out Unique_List_Type;
                    Crit : in Element_Type;
                    Found : out Boolean) renames Search_First;

  -- Check if an element exists in the list
  -- May raise Not_in_List
  procedure Find (List : in out Unique_List_Type;
                  Crit : in Element_Type) renames Find_First;

  -- Of course Search_Next and Find_Next will set not Found / raise Not_In_List

  -- Insert or replace an item
  -- Optionally drops new Item if one already exists
  -- May raise Full_List (no more memory)
  procedure Insert (List : in out Unique_List_Type;
                    Item : in Element_Type;
                    Drop : in Boolean := False);
  -- Insert or replace an item
  -- May raise Full_List (no more memory)
  -- This ensures that Hased_List.Insert is not called on a Unique_List
  overriding procedure Insert (List : in out Unique_List_Type;
                       Item : in Element_Type);

  -- Read the element matching in the list
  -- May raise Not_In_List
  procedure Read (List : in out List_Type;
                  Item : in out Element_Type);

end Hashed_List.Unique;

