generic
  -- Type of the element of the list
  type Element_Type is limited private;
  with procedure Set (To : in out Element_Type; Val : in Element_Type);

package Limited_List is

  -- Descriptor of the list
  type List_Type is limited private;

  -- for Read and Modify to set new position
  type Movement is (Next, Prev, Current);

  -- For Insert, Get and Delete, to set new position
  -- For Move_To and Search to set direction of move
  subtype Direction is Movement range Next .. Prev;

  -- for Get_Position
  type Reference is (From_First, From_Last);


  -- All calls except Insert, Is_Empty, List_Length and Search may raise
  --  Empty_List if the list is empty
  -- All calls which modify List may raise In_Callback if performed
  --  in an application callback (Match, Iteration);


  -- Check if one move in the given direction is possible
  function Check_Move (List : in List_Type;
                       Where : Direction := Next) return Boolean;


  -- Read the current item then moves to another item
  -- May raise Not_In_List (no read nor movement done)
  procedure Read (List : in out List_Type;
                  Item : in out Element_Type;
                  Move : in Movement := Next);

  -- Read anyway. Set Done to True if movement was possible (and done)
  --  and False otherwise (no movement done)
  procedure Read (List : in out List_Type;
                  Item : in out Element_Type;
                  Move : in Movement := Next;
                  Done : out Boolean);


  -- Modify the current item then moves to another item
  -- May raise Not_In_List (no movement done)
  procedure Modify (List : in out List_Type;
                    Item : in Element_Type;
                    Move : in Movement := Next);

  -- Modify anyway. Set Done to True if movement was possible (and done)
  --  and False otherwise (no movement done)
  procedure Modify (List : in out List_Type;
                    Item : in Element_Type;
                    Move : in Movement := Next;
                    Done : out Boolean);


  -- Insert a new item after or before the current item
  --  the new item becomes then the current item
  -- May raise Full_List (no more memory)
  procedure Insert (List  : in out List_Type;
                    Item  : in Element_Type;
                    Where : in Direction := Next);


  -- Read and delete the current item
  -- The current item is then the next or the previous item in the list
  --  except when deleting last item (no movement done!)
  -- May raise Not_In_List (no get nor movement done)
  procedure Get (List : in out List_Type;
                 Item : out Element_Type;
                 Move : in Direction := Next);

  -- Get anyway. Set Done to True if movement was possible (and done)
  --  or lists becomes empty, and False otherwise (movement done in the
  --  opposite direction)
  procedure Get (List : in out List_Type;
                 Item : out Element_Type;
                 Move : in Direction := Next;
                 Done : out Boolean);


  -- Suppress the current element from the list
  --  the current item is then the next or the previous item in the list
  -- May raise Not_In_List (no deletion nor movement done)
  -- Does not raise Empty_List when deleting the last item
  procedure Delete (List : in out List_Type;
                    Move : in Direction := Next);

  -- Delete anyway. Set Done to True if movement was possible (and done)
  --  or lists becomes empty, and False otherwise (movement done in the
  --  opposite direction)
  procedure Delete (List : in out List_Type;
                    Move : in Direction := Next;
                    Done : out Boolean);


  -- Delete the full list
  --  deallocate or not the free list
  procedure Delete_List (List : in out List_Type;
     Deallocate : in Boolean := True);


  -- Set the current element to number elements before or after
  --  If From_Current is False, then counting is from the first
  --  item in the list (Next), or the last (Prev).
  -- May raise Not_In_List (no movement done)
  --  Examples Move_To (List, Next, 1, False) goes to SECOND element
  --           Move_To (List, Next, 0, False) goes to FIRST element
  procedure Move_To (List         : in out List_Type;
                     Where        : in Direction := Next;
                     Number       : in Natural := 1;
                     From_Current : in Boolean := True);

  -- Move to beginning/end of list: Move_To (List, Where, 0, False);
  procedure Rewind (List : in out List_Type; Where : in Direction := Next);


  -- Permute 2 elements
  --  If From_Current is True,  then numbers of elements are relative from
  --   current
  --  If From_Current is False, then counting is from the first
  --   item in the list (Next), or the last (Prev).
  -- May raise Not_In_List (no movement done)
  --  example Permute (List, 0, 1, Next, False) permutes 1st and 2nd elements
  procedure Permute (List         : in out List_Type;
                     Number1      : in Natural;
                     Number2      : in Natural;
                     Where        : in Direction := Next;
                     From_Current : in Boolean   := False);


  -- Return without exception
  function Is_Empty (List : List_Type) return Boolean;


  -- return the number of elements in the list (0 if empty, no exception)
  function List_Length (List : List_Type) return Natural;


  -- Get position from first or last item in list
  -- For first item of list, Get_Position returns 1
  -- May raise Empty_List
  function Get_Position (List : List_Type;
                         From : Reference := From_First) return Positive;


  -- These two calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and reset it for further
  --  testing
  function Is_Modified (List : List_Type) return Boolean;

  procedure Modification_Ack (List : in out List_Type);


  -- Copy the Val list to To list
  -- CARE: Risk of side effect because List_Type only is duplicated
  procedure Assign (To : in out List_Type; Val : in List_Type);


  type Element_Access is access all Element_Type;
  -- Get direct access to current element in list (or null if list is empty).
  function Access_Current (List : List_Type) return Element_Access;


  -- For Search_Match and Iterate
  type Match_Access is access function (Current, Criteria : Element_Type)
                              return Boolean;

  -- Three different strategies to search:
  -- From_Current : current item may match)
  -- Skip_Current : earch starts after/before current)
  -- Absolute     : earch starts fron beginning/end of list
  type Search_Kind_List is (From_Current, Skip_Current, Absolute);

  -- Search from the nth occurence of an item matching the provided criteria
  -- Match is provided as a callback.
  -- Starts from current, skipping it or not if it matches,
  --  or from begin/end of list.
  -- Found is set to True if a matching item is found, then the current
  --  position is set to the item found, otherwise it is unchanged.
  -- If Match is null then any element matches.
  -- Does not raise Empty_List.
  procedure Search_Match (List      : in out List_Type;
                          Found     : out Boolean;
                          Match     : in Match_Access;
                          Criteria  : in Element_Type;
                          Where     : in Direction := Next;
                          Occurence : in Positive := 1;
                          From      : in Search_Kind_List);

  generic
    -- The Criteria is the one provided to Search
    -- Current will be the element of list compared to criteria
    with function Match (Current, Criteria : Element_Type)
                  return Boolean;

  -- Search from the nth occurence of an item matching the provided criteria
  -- Starts from current, skipping it or not if it matches,
  --  or from begin/end of list
  -- Found is set to True if a matching item is found, then the current
  --  position is set to the item found, otherwise it is unchanged.
  -- Does not raise Empty_List.
  procedure Search (List      : in out List_Type;
                    Found     : out Boolean;
                    Criteria  : in Element_Type;
                    Where     : in Direction := Next;
                    Occurence : in Positive := 1;
                    From      : in Search_Kind_List);

  -- Search, may raise Not_In_List if the given element is not found
  --  or if empty list (position not changed).
  generic
    with function Match (Current, Criteria : Element_Type)
                  return Boolean;
  procedure Unsafe_Search (List      : in out List_Type;
                           Criteria  : in Element_Type;
                           Where     : in Direction := Next;
                           Occurence : in Positive := 1;
                           From      : in Search_Kind_List);


  -- Called with each matching element, which can be updated.
  -- Processing of Iterate can be stopped by resetting Go_On to False
  --  (it initialised to True).
  type Iteration_Access is access procedure (Current : in out Element_Type;
                                             Go_On   : in out Boolean);

  -- Search from the next item matching the provided criteria and
  --  call Iteration with this item.
  -- Starts from current, skipping it or not if it matches,
  --  or from begin/end of list.
  -- Does not raise Empty_List.
  procedure Iterate (List      : in out List_Type;
                     Match     : in Match_Access;
                     Criteria  : in Element_Type;
                     Where     : in Direction := Next;
                     From      : in Search_Kind_List;
                     Iteration : in Iteration_Access);


  generic
    -- Comparison function for sorting
    -- WARNING : Less_Than must be strict
    --  (i.e. Less_Than(El1, El1) = False)
    with function Less_Than (El1, El2 : Element_Type) return Boolean;

  -- Sort all the list in crescent order
  -- Current position is reset to first
  procedure Sort (List : in out List_Type);


  -- When reading, getting, moving, permuting, getting position
  Empty_List, Full_List : exception;

  -- When moving, searching, permuting
  Not_In_List           : exception;

  -- When modifying List in an application callback
  In_Callback : exception;

private
  type Cell;
  type Link is access Cell;
  type Cell is record
    Value : Element_Type;
    Next  : Link := null;
    Prev  : Link := null;
  end record;

  type List_Type is record
    Modified  : Boolean := True;
    In_Cb     : Boolean := False;
    Pos_First : Natural := 0;
    Pos_Last  : Natural := 0;
    Current   : Link    := null;
    First     : Link    := null;
    Last      : Link    := null;
  end record;

end Limited_List;

