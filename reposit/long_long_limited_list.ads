-- Dynamic list of limited private type
-- Indexes and positions are long_long
private with Ada.Finalization;
with Long_Longs;
generic
  -- Type of the element of the list
  type Element_Type is limited private;
  with procedure Set (To : out Element_Type; Val : in Element_Type);
package Long_Long_Limited_List is

  -- Descriptor of the list
  type List_Type is tagged limited private;

  -- For Read and Modify to set new position
  type Movement is (Next, Prev, Current);

  -- For Insert, Get and Delete, to set new position
  -- For Move_To and Search to set direction of move
  subtype Direction is Movement range Next .. Prev;

  -- For Get_Position
  type Reference is (From_First, From_Last);

  -- Natural and Positive
  subtype Ll_Natural  is Long_Longs.Llu_Natural;
  subtype Ll_Positive is Long_Longs.Llu_Positive;

  -- All calls except Insert, Is_Empty, List_Length, searches and Iterate
  --  may raise Empty_List if the list is empty.
  -- Also Check_Move, Rewind and Access_Current if Check_Empty is True.
  -- All calls which modify the List may raise In_Callback if performed
  --  in an application callback (Match, Iteration).


  -- Check if one move in the given direction is possible
  function Check_Move (List : in List_Type;
                       Where : Direction := Next;
                       Check_Empty : in Boolean := True) return Boolean;


  -- Read the current item then moves to another item
  -- May raise Not_In_List (no read nor movement done)
  procedure Read (List : in out List_Type;
                  Item : out Element_Type;
                  Move : in Movement := Next);
  function  Read (List : in out List_Type;
                  Move : in Movement := Next) return Element_Type;

  -- Read anyway. Set Moved to True if movement was possible (and done)
  --  and False otherwise (no movement done)
  procedure Read (List  : in out List_Type;
                  Item  : out Element_Type;
                  Move  : in Movement := Next;
                  Moved : out Boolean);


  -- Modify the current item then moves to another item
  -- May raise Not_In_List (no movement done)
  procedure Modify (List : in out List_Type;
                    Item : in Element_Type;
                    Move : in Movement := Next);

  -- Modify anyway. Set Moved to True if movement was possible (and done)
  --  and False otherwise (no movement done)
  procedure Modify (List  : in out List_Type;
                    Item  : in Element_Type;
                    Move  : in Movement := Next;
                    Moved : out Boolean);


  -- Insert a new item after or before the current item
  --  the new item becomes then the current item
  -- May raise Full_List (no more memory or length too big)
  procedure Insert (List  : in out List_Type;
                    Item  : in Element_Type;
                    Where : in Direction := Next);


  -- Read and delete the current item
  -- The current item is then the next or the previous item in the list
  -- Does not raise Not_In_List nor Empty_List when getting the last item
  -- May raise Not_In_List (no deletion nor movement done)
  procedure Get (List : in out List_Type;
                 Item : out Element_Type;
                 Move : in Direction := Next);
  function  Get (List : in out List_Type;
                 Move : in Direction := Next) return Element_Type;

  -- Get anyway. Set Moved to True if movement was possible (and done)
  --  or if list becomes empty, and False otherwise (movement done in the
  --  opposite direction)
  procedure Get (List  : in out List_Type;
                 Item  : out Element_Type;
                 Move  : in Direction := Next;
                 Moved : out Boolean);


  -- Suppress the current element from the list
  -- The current item is then the next or the previous item in the list
  -- Does not raise Not_In_List nor Empty_List when getting the last item
  -- May raise Not_In_List (no deletion nor movement done)
  procedure Delete (List : in out List_Type;
                    Move : in Direction := Next);

  -- Delete anyway. Set Moved to True if movement was possible (and done)
  --  or if list becomes empty, and False otherwise (movement done in the
  --  opposite direction)
  procedure Delete (List  : in out List_Type;
                    Move  : in Direction := Next;
                    Moved : out Boolean);

  -- Suppress and deallocate the current element from the list
  -- The current item is then the next or the previous item in the list
  -- Does not raise Not_In_List nor Empty_List when getting the last item
  -- May raise Not_In_List (no deletion nor movement done)
  procedure Deallocate (List : in out List_Type;
                        Move : in Direction := Next);

  -- Delete and deallocate anyway. Set Moved to True if movement was possible
  --  (and done) or if list becomes empty, and False otherwise (movement done
  --  in the opposite direction)
  procedure Deallocate (List  : in out List_Type;
                        Move  : in Direction := Next;
                        Moved : out Boolean);

  -- Delete the full list
  -- Deallocate or not the free list
  procedure Delete_List (List : in out List_Type;
                         Deallocate : in Boolean := True);


  -- Set the current element to number elements before or after
  --  If From_Current is False, then counting is from the first
  --  item in the list (Next), or the last (Prev).
  -- May raise Not_In_List (no movement done)
  --  Examples: Move_To (List, Next, 1, False) goes to SECOND element
  --            Move_To (List, Next, 0, False) goes to FIRST element
  procedure Move_To (List         : in out List_Type;
                     Where        : in Direction := Next;
                     Number       : in Ll_Natural := 1;
                     From_Current : in Boolean := True);

  -- Set the current element to number elements from first (Direction = Next)
  --  or last (Direction = Prev).
  -- Equivalent to Move_To (List, Where, Position - 1, False)
  -- May raise Not_In_List (no movement done)
  --  Example: Move_At (List, Next, 1) goes to FIRST element
  procedure Move_At (List     : in out List_Type;
                     Position : in Ll_Positive;
                     Where    : in Direction := Next);

  -- Move to beginning/end of list: Move_To (List, Where, 0, False);
  -- Does not raise Empty_list if Check_Empty is False;
  procedure Rewind (List        : in out List_Type;
                    Where       : in Direction := Next;
                    Check_Empty : in Boolean := True);

  -- Permute 2 elements
  --  If From_Current is True,  then numbers of elements are relative from
  --   current,
  --  If From_Current is False, then counting is from the first
  --   item in the list (Next), or the last (Prev).
  -- May raise Not_In_List (no movement done)
  --  example Permute (List, 0, 1, Next, False) permutes 1st and 2nd elements
  procedure Permute (List         : in out List_Type;
                     Number1      : in Ll_Natural;
                     Number2      : in Ll_Natural;
                     Where        : in Direction := Next;
                     From_Current : in Boolean   := False);


  -- Return without exception
  function Is_Empty (List : List_Type) return Boolean;


  -- Return the number of elements in the list (0 if empty, no exception)
  function List_Length (List : List_Type) return Ll_Natural;


  -- Get position from first or last item in list
  -- For first item of list, Get_Position returns 1
  -- May raise Empty_List
  function Get_Position (List : List_Type;
                         From : Reference := From_First)
           return Ll_Positive;


  -- These three calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and to reset it for further
  --  setting and testing
  -- Because of Access_Curent, it is possible to explicitly set the list as
  --   modified
  procedure Set_Modified (List : in out List_Type);

  function Is_Modified (List : List_Type) return Boolean;

  procedure Modification_Ack (List : in out List_Type);


  -- Copy the Val list to To list
  -- CARE: Risk of side effect because List_Type only is duplicated.
  --  While both versions are valid, they should only be used to navigate
  --  (search, move, read). The exception List_Assigned will be raised on
  --  attempt to modify the assigned list, but there is no check on the
  --  original list.
  -- CARE CARE: Only use Unchecked_Assign to make a temporary local copy of
  --  Val, never assign a local list Val to a global list To (because the
  --  finalization of Val will delete the content of To).
  -- Use Insert_Copy instead.
  -- You have been warned.
  procedure Unchecked_Assign (To : in out List_Type; Val : in List_Type);

  -- Completely insert a copy of Val elements after or before current
  --  position in To list. Current becomes the last element copied.
  -- No effect if Val is empty
  procedure Insert_Copy (To    : in out List_Type;
                         Val   : in List_Type;
                         Where : in Direction := Next);

  -- Get direct access to current element in list
  -- CARE: As soon as the element is deleted, the access becomes invalid
  --  and using it will lead to unpredictable results
  function Access_Current (List : List_Type)
           return not null access Element_Type;

  -- Search the element that is at the provided access (move to it)
  -- Return True if the matching item is found, then the current position is
  --  set to this item, otherwise it is unchanged.
  function Search_Access (List      : in out List_Type;
                          Criteria  : not null access Element_Type)
           return Boolean;

  -- Access to the cell (that stores data) for deleting it without searching
  -- Get direct access to the current Cell (that stores the current Element)
  -- CARE: As soon as the element is deleted, the access becomes invalid
  --  and using it will lead to unpredictable results
  type Cell is limited private;
  function Cell_Access_Current (List : List_Type)
           return not null access Cell;

  -- Delete current element and rewind the list
  -- Rewinding is necessary because the impact of this deletion on current
  --  position is unknown
  procedure Delete_Current_Rewind (List     : in out List_Type;
                                   Cell_Acc : not null access Cell;
                                   Where    : in Direction := Next);

  -- Searches
  -----------
  -- Three different strategies to search:
  -- From_Current    : Search starts from current item (that may match)
  -- Skip_Current    : Search starts after/before current item
  -- Absolute        : Search starts from beginning/end of list
  -- Current_Absolute: Try current then absolute. Occurence must be 1 in this
  --                   case (or Constraint_Error will be raised)
  type Search_Kind_List is (From_Current, Skip_Current, Absolute,
                            Current_Absolute);

  -- Search with criteria of any type
  -----------------------------------
  generic
    -- The Criteria is the one provided to Search
    -- Current will be the element of list compared to criteria
    type Criteria_Type is limited private;
    with function Match (Current : Element_Type; Criteria : Criteria_Type)
                  return Boolean;
  -- Search the Nth occurence of an item matching the provided criteria
  -- Starts from current, skipping it or not (usefull if current is the result
  --  of a previous search), or from begin/end of list
  -- Return True if the matching item is found, then the current position is
  --  set to this item, otherwise it is unchanged.
  -- Does not raise Empty_List.
  function Search_Criteria (List      : in out List_Type;
                            Criteria  : in Criteria_Type;
                            Where     : in Direction := Next;
                            Occurence : in Ll_Positive := 1;
                            From      : in Search_Kind_List) return Boolean;

  -- Search with criteria of Element_Type
  ---------------------------------------
  generic
    -- The Criteria is the one provided to Search
    -- Current will be the element of list compared to criteria
    with function Match (Current, Criteria : Element_Type)
                  return Boolean;
  -- Search the Nth occurence of an item matching the provided criteria
  -- Starts from current, skipping it or not (usefull if current is the result
  --  of a previous search), or from begin/end of list
  -- Return True if the matching item is found, then the current position is
  --  set to this item, otherwise it is unchanged.
  -- Does not raise Empty_List.
  function Search (List      : in out List_Type;
                   Criteria  : in Element_Type;
                   Where     : in Direction := Next;
                   Occurence : in Ll_Positive := 1;
                   From      : in Search_Kind_List) return Boolean;

  -- Search with Match access and on Element_Type
  -----------------------------------------------
  -- For Search_Match
  type Match_Access is access function (Current, Criteria : Element_Type)
                              return Boolean;
  -- Search the Nth occurence of an item matching the provided criteria
  -- Match is provided as a callback.
  -- Starts from current, skipping it or not (usefull if current is the result
  --  of a previous search), or from begin/end of list
  -- Return True if the matching item is found, then the current position is
  --  set to this item, otherwise it is unchanged.
  -- If Match is null then any element matches.
  -- Does not raise Empty_List.
  function Search_Match (List      : in out List_Type;
                         Match     : not null access
                   function (Current, Criteria : Element_Type) return Boolean;
                         Criteria  : in Element_Type;
                         Where     : in Direction := Next;
                         Occurence : in Ll_Positive := 1;
                         From      : in Search_Kind_List) return Boolean;


  -- Search with exception
  ------------------------
  -- Search, may raise Not_In_List if the given element is not found
  --  or if empty list (position not changed).
  generic
    with function Match (Current, Criteria : Element_Type)
                  return Boolean;
  procedure Search_Raise (List      : in out List_Type;
                          Criteria  : in Element_Type;
                          Where     : in Direction := Next;
                          Occurence : in Ll_Positive := 1;
                          From      : in Search_Kind_List);


  -- Iteration
  ------------
  -- Called with each matching element, which can be updated but must
  --  remain current.
  -- Processing of Iterate can be stopped by resetting Go_On to False
  --  (it is initialised to True).
  type Iteration_Access is access procedure (Current : in Element_Type;
                                             Go_On   : in out Boolean);

  -- Search the next item matching the provided criteria (next/prev if
  --  Match is null) and call Iteration with this item.
  -- Starts from current, skipping it or not (usefull if current is the result
  --  of a previous search), or from begin/end of list
  -- Does not raise Empty_List.
  procedure Iterate (List      : in out List_Type;
                     Match     : access
                 function (Current, Criteria : Element_Type) return Boolean;
                     Criteria  : in Element_Type;
                     Where     : in Direction := Next;
                     From      : in Search_Kind_List;
                     Iteration : access
                 procedure (Current : in Element_Type;
                            Go_On   : in out Boolean));

  -- Simple iteration on the whole list
  -- Rewinds, iterates on all elements and stops on the last element
  -- Does not raise Empty_List.
  procedure Iterate_All (List : in out List_Type;
                         Iteration : access
                     procedure (Current : in Element_Type;
                                Go_On   : in out Boolean));


  -- Sorting
  ----------
  generic
    -- Comparison function for sorting
    -- WARNING : Less_Than must be strict
    --  (i.e. Less_Than(El1, El1) = False)
    with function Less_Than (El1, El2 : Element_Type) return Boolean;

  -- Sort all the list in crescent order
  -- Current position is reset to first
  procedure Sort (List : in out List_Type);


  -- Exceptions
  -------------
  -- When reading, getting, moving, permuting, getting position
  Empty_List, Full_List : exception;

  -- When moving, searching, permuting
  Not_In_List           : exception;

  -- When modifying List in an application callback
  In_Callback : exception;

  -- When modifying the data of an Unchecked_Assign-ed list
  List_Assigned : exception;

  -- When sorting, most often because Less_Than is not strict
  Sort_Error : exception;

private
  type Link is access all Cell;
  type Cell is record
    Value : Element_Type;
    Next  : Link := null;
    Prev  : Link := null;
  end record;

  type List_Type is limited new Ada.Finalization.Limited_Controlled with record
    Modified  : Boolean := True;
    Assigned  : Boolean := False;
    In_Cb     : Boolean := False;
    Pos_First : Ll_Natural := 0;
    Pos_Last  : Ll_Natural := 0;
    Current   : Link    := null;
    First     : Link    := null;
    Last      : Link    := null;
  end record;
  overriding procedure Finalize (List : in out List_Type);

end Long_Long_Limited_List;

