with Limited_List, Hash;
pragma Elaborate (Hash);
generic
  -- Type of the element of the list
  type Element_Type is limited private;
  type Element_Access is access all Element_Type;
  -- Affectation of elements
  with procedure Set (To : out Element_Type; Val : in Element_Type);
  -- Criteria of unicity (and search) of elements
  -- Must return True if and only if Current and Criteria have the same key
  with function "=" (Current : Element_Type; Criteria : Element_Type)
                return Boolean;
  -- A string representing the key (search criteria) for hashing.
  -- Must be compatible with the "=" function in the sense that
  --  Image(A) /= Image(B) implies A /= B
  -- For efficiency A /= B should better imply that Image(A) /= Image(B)
  with function Key_Image (Element : Element_Type) return String;

package Unique_List is

  type List_Type is tagged limited private;

  -- For Iterator
  type Reference is (From_First, From_Last);

  -- Insert may raise In_Callback if performed
  --  in an application callback (Iteration);

  -- Check if an element exists in the list
  procedure Search (List : in out List_Type;
                    Crit : in Element_Type;
                    Found : out Boolean);

  -- Insert or replace an item
  -- May raise Full_List (no more memory)
  procedure Insert (List : in out List_Type;
                    Item : in Element_Type);

  -- Read the element matching in the list
  -- May raise Not_In_List
  procedure Read (List : in out List_Type;
                  Crit : in Element_Type;
                  Item : out Element_Type);

  -- Get direct access to element matching in the list
  -- May raise Not_In_List
  procedure Get_Access (List : in out List_Type;
                        Crit : in Element_Type;
                        Item_Access : out Element_Access);

  -- Suppress the element matching in the list
  -- May raise Not_In_List
  procedure Delete (List : in out List_Type;
                    Crit : in Element_Type);

  -- Delete anyway. Set Done to True if matching item was found
  --  and deletion was done
  procedure Delete (List : in out List_Type;
                    Crit : in Element_Type;
                    Done : out Boolean);

  -- Delete the full list
  --  deallocate or not the free list
  procedure Delete_List (List : in out List_Type;
                         Deallocate : in Boolean := True);

  -- Return without exception
  function Is_Empty (List : List_Type) return Boolean;

  -- Return the number of elements in the list (0 if empty, no exception)
  function List_Length (List : List_Type) return Natural;


  -- These two calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and reset it for further
  --  testing
  function Is_Modified (List : List_Type) return Boolean;

  procedure Modification_Ack (List : in out List_Type);


  -- Called with each matching element, which can be updated.
  -- Processing of Iterate can be stopped by resetting Go_On to False
  --  (it is initialised to True).
  type Iteration_Access is access procedure (Current : in Element_Type;
                                             Go_On   : in out Boolean);

  -- Execute Iteration on all items
  procedure Iterate (List      : in out List_Type;
                     Iteration : access
     procedure (Current : in Element_Type;
                Go_On   : in out Boolean);
                From    : in Reference := From_First);

  -- When inserting
  Full_List : exception;

  -- When deleting, reading
  Not_In_List : exception;

  -- When modifying List in an application callback
  In_Callback : exception;

  -- If internal inconsistency (on delete)
  Internal_Error : exception;
private
  -- The limited list of items
  package List_Mng is new Limited_List (Element_Type, Element_Access, Set);
  -- Element hashing
  procedure Dump (Data : in Element_Access);
  package Hash_Mng is
          new Hash.Hash_Mng (Hash.Max_Hash_Value,
                             Element_Access,
                             Dump);
  -- A unique list
  type List_Type is tagged limited record
    List : List_Mng.List_Type;
    Table : Hash_Mng.Hash_Table;
  end record;
end Unique_List;

