generic
  -- type of the element of the list
  type Element_Type is private;

package Dynamic_List is

  -- descriptor of the list
  type List_Type is limited private;

  -- for READ and MODIFY to set new position
  type Movement is (Next, Prev, Current);

  -- for INSERT, GET and DELETE, to set new position
  -- for MOVE_TO and SEARCH to set direction of move
  subtype Direction is Movement range Next .. Prev;

  -- for GET_POSITION
  type Reference is (From_First, From_Last);


  -- All calls except IS_EMPTY and LIST_LENGTH may raise
  -- EMPTY_LIST if the list is empty

  -- read the current item then moves to another item
  -- may raise NOT_IN_LIST (no read nor movement done)
  procedure Read (List : in out List_Type;
                  Item : out Element_Type;
                  Move : in Movement := Next);

  -- modify the current item then moves to another item
  -- may raise NOT_IN_LIST (no movement done)
  procedure Modify (List : in out List_Type;
                    Item : in Element_Type;
                    Move : in Movement := Next);

  -- insert a new item after or before the current item
  --  the new item becomes then the current item
  -- may raise FULL_LIST (no more memory)
  procedure Insert (List  : in out List_Type;
                    Item  : in Element_Type;
                    Where : in Direction := Next);

  -- read and delete the current item
  --  the current item is then the next or the previous item in the list
  --  except when deleting last item (no movement done!)
  -- may raise NOT_IN_LIST (no get nor movement done)
  procedure Get (List : in out List_Type;
                 Item : out Element_Type;
                 Move : in Direction := Next);

  -- suppress the current element from the list
  --  the current item is then the next or the previous item in the list
  -- may raise NOT_IN_LIST (no deletion nor movement done)
  procedure Delete (List : in out List_Type; Move : in Direction := Next);

  -- delete the full list
  --  deallocate or not the free list
  procedure Delete_List (List : in out List_Type;
     Deallocate : in Boolean := True);

  -- set the current element to number elements before or after
  --  if FROM_CURRENT is FALSE, then counting is from the first
  --  item in the list (NEXT), or the last (PREV).
  -- may raise NOT_IN_LIST (no movement done)
  --  example MOVE_TO (LIST, NEXT, 1, FALSE) goes to SECOND element
  procedure Move_To (List         : in out List_Type;
                     Where        : in Direction := Next;
                     Number       : in Natural := 1;
                     From_Current : in Boolean := True);

  -- permute 2 elements
  --  if FROM_CURRENT is TRUE,  then numbers of elements are relative from
  --   current
  --  if FROM_CURRENT is FALSE, then counting is from the first
  --   item in the list (NEXT), or the last (PREV).
  -- may raise NOT_IN_LIST (no movement done)
  --  example PERMUTE (LIST, 0, 1, NEXT, FALSE) permutes 1st and 2nd elements
  procedure Permute (List         : in out List_Type;
                     Number1      : in Natural;
                     Number2      : in Natural;
                     Where        : in Direction := Next;
                     From_Current : in Boolean   := False);

  -- return without exception
  function Is_Empty (List : List_Type) return Boolean;

  -- return the number of elements in the list (0 if empty, no exception)
  function List_Length (List : List_Type) return Natural;

  -- get position from first or last item in list
  -- For first item of list, GET_POSITION returns 1
  -- may raise EMPTY_LIST
  function Get_Position (List : List_Type;
                         From : Reference := From_First) return Positive;

  -- These two calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and reset it for further
  --  testing
  function Is_Modified (List : List_Type) return Boolean;
  procedure Modification_Ack (List : in out List_Type);

  -- Copy the VAL list to TO list
  -- CARE: Risk of side effect because LIST_TYPE only is duplicated
  procedure Assign (To : in out List_Type; Val : in List_Type);

  generic
    with function Equal (El1, El2 : Element_Type) return Boolean is "=";

  -- search from the nth occurence of an iten EQUAL to the provided one
  -- starts from : same as MOVE_TO
  -- may raise NOT_IN_LIST if the given element is not found
  --  also if empty list
  --  (position not changed) otherwise, the current position is set to
  --  the item found
  -- If OCCURENCE is 1 and current/first/last matches, then it is selected
  procedure Search (List         : in out List_Type;
                    Item         : in Element_Type;
                    Where        : in Direction := Next;
                    Occurence    : in Positive := 1;
                    From_Current : in Boolean := True);

  generic
    -- Comparison function for sorting
    -- WARNING : LESS_TAHN must be strict
    --  (i.e. LESS_THAN(EL1, EL1) = FALSE)
    with function Less_Than (El1, El2 : Element_Type) return Boolean;

  -- sort all the list in crescent order
  -- current position is reset to first
  procedure Sort (List : in out List_Type);

  -- when reading, getting, moving, searching, permuting, getting position
  Empty_List, Full_List : exception;
  -- when moving, searching, permuting
  Not_In_List           : exception;

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
    Pos_First : Natural := 0;
    Pos_Last  : Natural := 0;
    Current   : Link    := null;
    First     : Link    := null;
    Last      : Link    := null;
  end record;

end Dynamic_List;

