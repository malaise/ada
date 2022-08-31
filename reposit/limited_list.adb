package body Limited_List is

  -- Check if one move in the given direction is possible
  function Check_Move (List : in List_Type;
                       Where : Direction := Next;
                       Check_Empty : in Boolean := True) return Boolean is
    (List.List.Check_Move (My_List.Direction (Where), Check_Empty));

  -- Read the current item then moves to another item
  -- May raise Not_In_List (no read nor movement done)
  procedure Read (List : in out List_Type;
                  Item : out Element_Type;
                  Move : in Movement := Next) is
  begin
    List.List.Read (Item, My_List.Movement (Move));
  end Read;
  function  Read (List : in out List_Type;
                  Move : in Movement := Next) return Element_Type is
    (List.List.Read (My_List.Movement (Move)));

  -- Read anyway. Set Moved to True if movement was possible (and done)
  --  and False otherwise (no movement done)
  procedure Read (List  : in out List_Type;
                  Item  : out Element_Type;
                  Move  : in Movement := Next;
                  Moved : out Boolean) is
  begin
    List.List.Read (Item, My_List.Movement (Move), Moved);
  end Read;

  -- Modify the current item then moves to another item
  -- May raise Not_In_List (no movement done)
  procedure Modify (List : in out List_Type;
                    Item : in Element_Type;
                    Move : in Movement := Next) is
  begin
    List.List.Modify (Item, My_List.Movement (Move));
  end Modify;

  -- Modify anyway. Set Moved to True if movement was possible (and done)
  --  and False otherwise (no movement done)
  procedure Modify (List  : in out List_Type;
                    Item  : in Element_Type;
                    Move  : in Movement := Next;
                    Moved : out Boolean) is
  begin
    List.List.Modify (Item, My_List.Movement (Move), Moved);
  end Modify;

  -- Insert a new item after or before the current item
  --  the new item becomes then the current item
  -- May raise Full_List (no more memory)
  procedure Insert (List  : in out List_Type;
                    Item  : in Element_Type;
                    Where : in Direction := Next) is
  begin
    if List.List_Length = Natural'Last then
      raise Full_List;
    end if;
    List.List.Insert (Item, My_List.Direction (Where));
  end Insert;

  -- Read and delete the current item
  -- The current item is then the next or the previous item in the list
  -- Does not raise Not_In_List nor Empty_List when getting the last item
  -- May raise Not_In_List (no deletion nor movement done)
  procedure Get (List : in out List_Type;
                 Item : out Element_Type;
                 Move : in Direction := Next) is
  begin
    List.List.Get (Item, My_List.Direction (Move));
  end Get;
  function  Get (List : in out List_Type;
                 Move : in Direction := Next) return Element_Type is
    (List.List.Get (My_List.Movement (Move)));

  -- Get anyway. Set Moved to True if movement was possible (and done)
  --  or if list becomes empty, and False otherwise (movement done in the
  --  opposite direction)
  procedure Get (List  : in out List_Type;
                 Item  : out Element_Type;
                 Move  : in Direction := Next;
                 Moved : out Boolean) is
  begin
    List.List.Get (Item, My_List.Direction (Move), Moved);
  end Get;

  -- Suppress the current element from the list
  -- The current item is then the next or the previous item in the list
  -- Does not raise Not_In_List nor Empty_List when getting the last item
  -- May raise Not_In_List (no deletion nor movement done)
  procedure Delete (List : in out List_Type;
                    Move : in Direction := Next) is
  begin
    List.List.Delete (My_List.Direction (Move));
  end Delete;

  -- Delete anyway. Set Moved to True if movement was possible (and done)
  --  or if list becomes empty, and False otherwise (movement done in the
  --  opposite direction)
  procedure Delete (List  : in out List_Type;
                    Move  : in Direction := Next;
                    Moved : out Boolean) is
  begin
    List.List.Delete (My_List.Direction (Move), Moved);
  end Delete;

  -- Suppress and deallocate the current element from the list
  -- The current item is then the next or the previous item in the list
  -- Does not raise Not_In_List nor Empty_List when getting the last item
  -- May raise Not_In_List (no deletion nor movement done)
  procedure Deallocate (List : in out List_Type;
                        Move : in Direction := Next) is
  begin
    List.List.Deallocate (My_List.Direction (Move));
  end Deallocate;

  -- Delete and deallocate anyway. Set Moved to True if movement was possible
  --  (and done) or if list becomes empty, and False otherwise (movement done
  --  in the opposite direction)
  procedure Deallocate (List  : in out List_Type;
                        Move  : in Direction := Next;
                        Moved : out Boolean) is
  begin
    List.List.Deallocate (My_List.Direction (Move), Moved);
  end Deallocate;

  -- Delete the full list
  -- Deallocate or not the free list
  procedure Delete_List (List : in out List_Type;
                         Deallocate : in Boolean := True) is
  begin
    List.List.Delete_List (Deallocate);
  end Delete_List;

  -- Set the current element to number elements before or after
  --  If From_Current is False, then counting is from the first
  --  item in the list (Next), or the last (Prev).
  -- May raise Not_In_List (no movement done)
  --  Examples: Move_To (List, Next, 1, False) goes to SECOND element
  --            Move_To (List, Next, 0, False) goes to FIRST element
  procedure Move_To (List         : in out List_Type;
                     Where        : in Direction := Next;
                     Number       : in Natural := 1;
                     From_Current : in Boolean := True) is
  begin
    List.List.Move_To (My_List.Direction (Where),
                       My_List.Ll_Natural (Number), From_Current);
  end Move_To;

  -- Set the current element to number elements from first (Direction = Next)
  --  or last (Direction = Prev).
  -- Equivalent to Move_To (List, Where, Position - 1, False)
  -- May raise Not_In_List (no movement done)
  --  Example: Move_At (List, Next, 1) goes to FIRST element
  procedure Move_At (List     : in out List_Type;
                     Position : in Positive;
                     Where    : in Direction := Next) is
  begin
    List.List.Move_At (My_List.Ll_Positive (Position),
                       My_List.Direction (Where));
  end Move_At;

  -- Move to beginning/end of list: Move_To (List, Where, 0, False);
  -- Does not raise Empty_list if Check_Empty is False;
  procedure Rewind (List        : in out List_Type;
                    Where       : in Direction := Next;
                    Check_Empty : in Boolean := True) is
  begin
    List.List.Rewind (My_List.Direction (Where), Check_Empty);
  end Rewind;

  -- Permute 2 elements
  --  If From_Current is True,  then numbers of elements are relative from
  --   current,
  --  If From_Current is False, then counting is from the first
  --   item in the list (Next), or the last (Prev).
  -- May raise Not_In_List (no movement done)
  --  example Permute (List, 0, 1, Next, False) permutes 1st and 2nd elements
  procedure Permute (List         : in out List_Type;
                     Number1      : in Natural;
                     Number2      : in Natural;
                     Where        : in Direction := Next;
                     From_Current : in Boolean   := False) is
  begin
    List.List.Permute (My_List.Ll_Natural (Number1),
                       My_List.Ll_Natural (Number2),
                       My_List.Direction (Where), From_Current);
  end Permute;

  -- Return without exception
  function Is_Empty (List : List_Type) return Boolean is (List.List.Is_Empty);

  -- Return the number of elements in the list (0 if empty, no exception)
  function List_Length (List : List_Type) return Natural is
    (Natural (List.List.List_Length));

  -- Get position from first or last item in list
  -- For first item of list, Get_Position returns 1
  -- May raise Empty_List
  function Get_Position (List : List_Type;
                         From : Reference := From_First) return Positive is
    (Positive (List.List.Get_Position (My_List.Reference (From))));

  -- These three calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and to reset it for further
  --  setting and testing
  -- Because of Access_Curent, it is possible to explicitly set the list as
  --   modified
  procedure Set_Modified (List : in out List_Type) is
  begin
    List.List.Set_Modified;
  end Set_Modified;

  function Is_Modified (List : List_Type) return Boolean is
    (List.List.Is_Modified);

  procedure Modification_Ack (List : in out List_Type) is
  begin
    List.List.Modification_Ack;
  end Modification_Ack;

  -- Copy the Val list to To list
  procedure Unchecked_Assign (To : in out List_Type; Val : in List_Type) is
  begin
    To.List.Unchecked_Assign (Val.List);
  end Unchecked_Assign;

  -- Completely insert a copy of Val elements after or before current
  --  position in To list. Current becomes the last element copied.
  -- No effect if Val is empty
  procedure Insert_Copy (To    : in out List_Type;
                         Val   : in List_Type;
                         Where : in Direction := Next) is
  begin
    To.List.Insert_Copy (Val.List, My_List.Direction (Where));
  end Insert_Copy;

  -- Get direct access to current element in list
  function Access_Current (List : List_Type)
           return not null access Element_Type is
    (List.List.Access_Current );

  -- Search the element that is at the provided access (move to it)
  -- Return True if the matching item is found, then the current position is
  --  set to this item, otherwise it is unchanged.
  function Search_Access (List      : in out List_Type;
                          Criteria  : not null access Element_Type)
           return Boolean is
    (List.List.Search_Access (Criteria));

  -- Access to the cell (that stores data) for deleting it without searching
  -- Get direct access to the current Cell (that stores the current Element)
  -- CARE: As soon as the element is deleted, the access becomes invalid
  --  and using it will lead to unpredictable results
  function Cell_Access_Current (List : List_Type)
           return not null access Cell is
    (List.List.Cell_Access_Current );

  -- Delete current element and rewind the list
  -- Rewinding is necessary because the impact of this deletion on current
  --  position is unknown
  procedure Delete_Current_Rewind (List     : in out List_Type;
                                   Cell_Acc : not null access Cell;
                                   Where    : in Direction := Next) is
  begin
    List.List.Delete_Current_Rewind (Cell_Acc, My_List.Movement (Where));
  end Delete_Current_Rewind;


  -- Search with criteria of any type
  -----------------------------------
  -- Search the Nth occurence of an item matching the provided criteria
  -- Starts from current, skipping it or not (usefull if current is the result
  --  of a previous search), or from begin/end of list
  -- Return True if the matching item is found, then the current position is
  --  set to this item, otherwise it is unchanged.
  -- Does not raise Empty_List.
  function Search_Criteria (List      : in out List_Type;
                            Criteria  : in Criteria_Type;
                            Where     : in Direction := Next;
                            Occurence : in Positive := 1;
                            From      : in Search_Kind_List) return Boolean is
    function My_Search_Criteria is new My_List.Search_Criteria (
      Criteria_Type, Match);
  begin
    return My_Search_Criteria (List.List, Criteria, My_List.Direction (Where),
                        My_List.Ll_Positive (Occurence),
                        My_List.Search_Kind_List (From));
  end Search_Criteria;

  -- Search with criteria of Element_Type
  ---------------------------------------
--     -- The Criteria is the one provided to Search
--     -- Current will be the element of list compared to criteria
  -- generic

  --   with function Match (Current, Criteria : Element_Type)
  --                 return Boolean;

  -- Search the Nth occurence of an item matching the provided criteria
  -- Starts from current, skipping it or not (usefull if current is the result
  --  of a previous search), or from begin/end of list
  -- Return True if the matching item is found, then the current position is
  --  set to this item, otherwise it is unchanged.
  -- Does not raise Empty_List.
  function Search (List      : in out List_Type;
                   Criteria  : in Element_Type;
                   Where     : in Direction := Next;
                   Occurence : in Positive := 1;
                   From      : in Search_Kind_List) return Boolean is
    function My_Search is new My_List.Search (Match);
  begin
    return My_Search (List.List, Criteria, My_List.Direction (Where),
               My_List.Ll_Positive (Occurence),
               My_List.Search_Kind_List (From));
  end Search;

  -- Search with Match access and on Element_Type
  -----------------------------------------------
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
                         Occurence : in Positive := 1;
                         From      : in Search_Kind_List) return Boolean is
  begin
    return List.List.Search_Match (Match, Criteria,
                            My_List.Direction (Where),
                            My_List.Ll_Positive (Occurence),
                            My_List.Search_Kind_List (From));

  end Search_Match;

  -- Search with exception
  ------------------------
  -- Search, may raise Not_In_List if the given element is not found
  --  or if empty list (position not changed).
  procedure Search_Raise (List      : in out List_Type;
                          Criteria  : in Element_Type;
                          Where     : in Direction := Next;
                          Occurence : in Positive := 1;
                          From      : in Search_Kind_List) is
    procedure My_Search_Raise is new My_List.Search_Raise (Match);
  begin
    My_Search_Raise (List.List, Criteria, My_List.Direction (Where),
                     My_List.Ll_Positive (Occurence),
                     My_List.Search_Kind_List (From));

  end Search_Raise;

  -- Iteration
  ------------
  -- Called with each matching element, which can be updated.
  -- Processing of Iterate can be stopped by resetting Go_On to False
  --  (it is initialised to True).
  -- type Iteration_Access is access procedure (Current : in Element_Type;
  --                                            Go_On   : in out Boolean);

  -- Search the next item matching the provided criteria and
  --  call Iteration with this item.
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
               Go_On   : in out Boolean)) is
  begin
    List.List.Iterate (Match, Criteria, My_List.Direction (Where),
                       My_List.Search_Kind_List (From), Iteration);
  end Iterate;

  -- Simple iteration on the whole list
  -- Rewinds, iterates on all elements and stops on the last element
  -- Does not raise Empty_List.
  procedure Iterate_All (List : in out List_Type;
                         Iteration : access
                     procedure (Current : in Element_Type;
                                Go_On   : in out Boolean)) is
  begin
    List.List.Iterate_All (Iteration);
  end Iterate_All;


  -- Sort all the list in crescent order
  -- Current position is reset to first
  procedure Sort (List : in out List_Type) is
    procedure My_Sort is new My_List.Sort (Less_Than);
  begin
    My_Sort (List.List);
  end Sort;

end Limited_List;

