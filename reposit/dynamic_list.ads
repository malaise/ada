generic
  -- type of the element of the list
  type Element_Type is private;

package Dynamic_List is

  -- descriptor of the list
  type List_Type is limited private;

  -- for Read and Modify to set new position
  type Movement is (Next, Prev, Current);

  -- for Insert, Get and Delete, to set new position
  -- for Move_To and Search to set direction of move
  subtype Direction is Movement range Next .. Prev;

  -- for Get_Position
  type Reference is (From_First, From_Last);


  -- All calls except Is_Empty and List_Length may raise
  -- Empty_List if the list is empty

  -- read the current item then moves to another item
  -- may raise Not_In_List (no read nor movement done)
  procedure Read (List : in out List_Type;
                  Item : out Element_Type;
                  Move : in Movement := Next);

  -- modify the current item then moves to another item
  -- may raise Not_In_List (no movement done)
  procedure Modify (List : in out List_Type;
                    Item : in Element_Type;
                    Move : in Movement := Next);

  -- insert a new item after or before the current item
  --  the new item becomes then the current item
  -- may raise Full_List (no more memory)
  procedure Insert (List  : in out List_Type;
                    Item  : in Element_Type;
                    Where : in Direction := Next);

  -- read and delete the current item
  --  the current item is then the next or the previous item in the list
  --  except when deleting last item (no movement done!)
  -- may raise Not_In_List (no get nor movement done)
  procedure Get (List : in out List_Type;
                 Item : out Element_Type;
                 Move : in Direction := Next);

  -- suppress the current element from the list
  --  the current item is then the next or the previous item in the list
  -- may raise Not_In_List (no deletion nor movement done)
  -- does not raise Empty_List when deleting the last item
  procedure Delete (List : in out List_Type; Move : in Direction := Next);

  -- delete the full list
  --  deallocate or not the free list
  procedure Delete_List (List : in out List_Type;
     Deallocate : in Boolean := True);

  -- set the current element to number elements before or after
  --  if From_Current is False, then counting is from the first
  --  item in the list (Next), or the last (Prev).
  -- may raise Not_In_List (no movement done)
  --  examples Move_To (List, Next, 1, False) goes to SECOND element
  --           Move_To (List, Next, 0, False) goes to FIRST element
  procedure Move_To (List         : in out List_Type;
                     Where        : in Direction := Next;
                     Number       : in Natural := 1;
                     From_Current : in Boolean := True);

  -- permute 2 elements
  --  if From_CUrrent is True,  then numbers of elements are relative from
  --   current
  --  if From_Current is False, then counting is from the first
  --   item in the list (Next), or the last (Prev).
  -- may raise Not_In_List (no movement done)
  --  example Permute (List, 0, 1, Next, False) permutes 1st and 2nd elements
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
  -- For first item of list, Get_Position returns 1
  -- may raise Empty_List
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

  generic
    with function Equal (El1, El2 : Element_Type) return Boolean is "=";

  -- search from the nth occurence of an item Equal to the provided one
  -- starts from : same as Move_To
  -- may raise Not_In_List if the given element is not found
  --  also if empty list
  --  (position not changed) otherwise, the current position is set to
  --  the item found
  -- If Occurence is 1 and current/first/last matches, then it is selected
  procedure Search (List         : in out List_Type;
                    Item         : in Element_Type;
                    Where        : in Direction := Next;
                    Occurence    : in Positive := 1;
                    From_Current : in Boolean := True);

  generic
    -- Comparison function for sorting
    -- WARNING : Less_Than must be strict
    --  (i.e. Less_Than(El1, El1) = False)
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

