generic
  -- type of the element of the list
  type ELEMENT_TYPE is private;

package DYNAMIC_LIST is

  -- descriptor of the list
  type LIST_TYPE is limited private;

  -- for READ and MODIFY to set new position
  type MOVEMENT is (NEXT, PREV, CURRENT);

  -- for INSERT, GET and DELETE, to set new position
  -- for MOVE_TO and SEARCH to set direction of move
  subtype DIRECTION is MOVEMENT range NEXT .. PREV;

  -- for GET_POSITION
  type REFERENCE is (FROM_FIRST, FROM_LAST);


  -- All calls except IS_EMPTY and LIST_LENGTH may raise
  -- EMPTY_LIST if the list is empty

  -- read the current item then moves to another item
  -- may raise NOT_IN_LIST (no read nor movement done)
  procedure READ (LIST : in out LIST_TYPE;
                  ITEM : out ELEMENT_TYPE;
                  MOVE : in MOVEMENT := NEXT);

  -- modify the current item then moves to another item
  -- may raise NOT_IN_LIST (no movement done)
  procedure MODIFY (LIST : in out LIST_TYPE;
                    ITEM : in ELEMENT_TYPE;
                    MOVE : in MOVEMENT := NEXT);

  -- insert a new item after or before the current item
  --  the new item becomes then the current item
  -- may raise FULL_LIST (no more memory)
  procedure INSERT (LIST  : in out LIST_TYPE;
                    ITEM  : in ELEMENT_TYPE;
                    WHERE : in DIRECTION := NEXT);

  -- read and delete the current item
  --  the current item is then the next or the previous item in the list
  --  except when deleting last item (no movement done!)
  -- may raise NOT_IN_LIST (no get nor movement done)
  procedure GET (LIST : in out LIST_TYPE;
                 ITEM : out ELEMENT_TYPE;
                 MOVE : in DIRECTION := NEXT);

  -- suppress the current element from the list
  --  the current item is then the next or the previous item in the list
  -- may raise NOT_IN_LIST (no deletion nor movement done)
  procedure DELETE (LIST : in out LIST_TYPE; MOVE : in DIRECTION := NEXT);

  -- delete the full list
  --  deallocate or not the free list
  procedure DELETE_LIST (LIST : in out LIST_TYPE;
     DEALLOCATE : in BOOLEAN := TRUE);

  -- set the current element to number elements before or after
  --  if FROM_CURRENT is FALSE, then counting is from the first
  --  item in the list (NEXT), or the last (PREV).
  -- may raise NOT_IN_LIST (no movement done)
  --  example MOVE_TO (LIST, NEXT, 1, FALSE) goes to SECOND element
  procedure MOVE_TO (LIST         : in out LIST_TYPE;
                     WHERE        : in DIRECTION := NEXT;
                     NUMBER       : in NATURAL := 1;
                     FROM_CURRENT : in BOOLEAN := TRUE);

  -- permute 2 elements
  --  if FROM_CURRENT is TRUE,  then numbers of elements are relative from
  --   current
  --  if FROM_CURRENT is FALSE, then counting is from the first
  --   item in the list (NEXT), or the last (PREV).
  -- may raise NOT_IN_LIST (no movement done)
  --  example PERMUTE (LIST, 0, 1, NEXT, FALSE) permutes 1st and 2nd elements
  procedure PERMUTE (LIST         : in out LIST_TYPE;
                     NUMBER1      : in NATURAL;
                     NUMBER2      : in NATURAL;
                     WHERE        : in DIRECTION := NEXT;
                     FROM_CURRENT : in BOOLEAN   := FALSE);

  -- return without exception
  function IS_EMPTY (LIST : LIST_TYPE) return BOOLEAN;

  -- return the number of elements in the list (0 if empty, no exception)
  function LIST_LENGTH (LIST : LIST_TYPE) return NATURAL;

  -- get position from first or last item in list
  -- For first item of list, GET_POSITION returns 1
  -- may raise EMPTY_LIST
  function GET_POSITION (LIST : LIST_TYPE;
                         FROM : REFERENCE := FROM_FIRST) return POSITIVE;

  -- These two calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and reset it for further
  --  testing
  function IS_MODIFIED (LIST : LIST_TYPE) return BOOLEAN;
  procedure MODIFICATION_ACK (LIST : in out LIST_TYPE);

  -- Copy the VAL list to TO list
  -- CARE: Risk of side effect because LIST_TYPE only is duplicated
  procedure ASSIGN (TO : in out LIST_TYPE; VAL : in LIST_TYPE);

  generic
    with function EQUAL (EL1, EL2 : ELEMENT_TYPE) return BOOLEAN is "=";

  -- search from the nth occurence of an iten EQUAL to the provided one
  -- starts from : same as MOVE_TO
  -- may raise NOT_IN_LIST if the given element is not found
  --  also if empty list
  --  (position not changed) otherwise, the current position is set to
  --  the item found
  -- If OCCURENCE is 1 and current/first/last matches, then it is selected
  procedure SEARCH (LIST         : in out LIST_TYPE;
                    ITEM         : in ELEMENT_TYPE;
                    WHERE        : in DIRECTION := NEXT;
                    OCCURENCE    : in POSITIVE := 1;
                    FROM_CURRENT : in BOOLEAN := TRUE);

  generic
    -- Comparison function for sorting
    -- WARNING : LESS_TAHN must be strict
    --  (i.e. LESS_THAN(EL1, EL1) = FALSE)
    with function LESS_THAN (EL1, EL2 : ELEMENT_TYPE) return BOOLEAN;

  -- sort all the list in crescent order
  -- current position is reset to first
  procedure SORT (LIST : in out LIST_TYPE);

  -- when reading, getting, moving, searching, permuting, getting position
  EMPTY_LIST, FULL_LIST : exception;
  -- when moving, searching, permuting
  NOT_IN_LIST           : exception;

private
  type CELL;
  type LINK is access CELL;
  type CELL is record
    VALUE : ELEMENT_TYPE;
    NEXT  : LINK := null;
    PREV  : LINK := null;
  end record;

  type LIST_TYPE is record
    MODIFIED  : BOOLEAN := TRUE;
    POS_FIRST : NATURAL := 0;
    POS_LAST  : NATURAL := 0;
    CURRENT   : LINK    := null;
    FIRST     : LINK    := null;
    LAST      : LINK    := null;
  end record;

end DYNAMIC_LIST;

