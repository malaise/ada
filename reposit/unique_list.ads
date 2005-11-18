generic
  -- Type of the element of the list
  type Element_Type is limited private;
  -- Affectation of elements
  with procedure Set (To : out Element_Type; Val : in Element_Type);
  -- A string representing the element, for hashing (different elements
  --  in term of the following "=" function should rather have different
  --  images
  with function Image (Element : Element_Type) return String;
  -- Criteria of unicity (and search) of elements
  with function "=" (Current : Element_Type; Criteria : Element_Type)
                return Boolean is <>;

package Unique_List is

  -- For Iterator
  type Reference is (From_First, From_Last);


  -- Insert may raise In_Callback if performed
  --  in an application callback (Iteration);

  -- Insert or replace an item
  -- May raise Full_List (no more memory)
  procedure Insert (Item  : in Element_Type);

  -- Read the  the element matching in the list
  -- May raise Not_In_List
  procedure Read (Crit : in Element_Type; Item : out Element_Type);

  -- Suppress the element matching in the list
  -- May raise Not_In_List
  procedure Delete (Crit : in Element_Type);

  -- Delete anyway. Set Done to True if matching item was found
  --  and deletion was done
  procedure Delete (Crit : in Element_Type;
                    Done : out Boolean);

  -- Delete the full list
  --  deallocate or not the free list
  procedure Delete_List (Deallocate : in Boolean := True);


  -- Return without exception
  function Is_Empty return Boolean;

  -- Return the number of elements in the list (0 if empty, no exception)
  function List_Length return Natural;


  -- These two calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and reset it for further
  --  testing
  function Is_Modified return Boolean;

  procedure Modification_Ack;


  -- Called with each matching element, which can be updated.
  -- Processing of Iterate can be stopped by resetting Go_On to False
  --  (it is initialised to True).
  type Iteration_Access is access procedure (Current : in Element_Type;
                                             Go_On   : in out Boolean);

  -- Execute Iteration on all items
  -- Does not raise Empty_List.
  procedure Iterate (Iteration : in Iteration_Access;
                     From      : in Reference := From_First);

  -- When inserting
  Full_List : exception;

  -- When deleting, reading
  Not_In_List : exception;

  -- When modifying List in an application callback
  In_Callback : exception;

end Unique_List;

