with UNCHECKED_DEALLOCATION;
package body DYNAMIC_LIST is

  type ELEMENT_ARRAY is array (POSITIVE range <>) of ELEMENT_TYPE;

  function IS_EMPTY (LIST : LIST_TYPE) return BOOLEAN is
  begin
    return LIST.FIRST = null;
  end IS_EMPTY;

  procedure CHECK (LIST : in LIST_TYPE) is
  begin
    if IS_EMPTY(LIST) then
      raise EMPTY_LIST;
    end if;
  end CHECK;

  procedure CHECK_IN(POS : in LINK) is
  begin
    if POS = null then
      raise NOT_IN_LIST;
    end if;
  end CHECK_IN;

  -- delete the full list
  procedure DELETE_LIST (LIST : in out LIST_TYPE) is
    LOCAL : LINK;
    procedure DEALLOCATION_OF is new
     UNCHECKED_DEALLOCATION(OBJECT=>CELL, NAME=>LINK);
  begin
    -- dealocate the list
    while LIST.FIRST /= null loop
      LOCAL := LIST.FIRST;
      LIST.FIRST := LIST.FIRST.NEXT;
      DEALLOCATION_OF(LOCAL);
    end loop;
    -- dealocate the free list
    while LIST.FREE /= null loop
      LOCAL := LIST.FREE;
      LIST.FREE := LIST.FREE.NEXT;
      DEALLOCATION_OF(LOCAL);
    end loop;
    LIST := (MODIFIED => TRUE, POS_FIRST | POS_LAST => 0,
     CURRENT => null, FIRST => null, LAST => null, FREE => null);
  end DELETE_LIST;

  -- read the current item
  procedure READ (LIST : in out LIST_TYPE;
                  ITEM : out ELEMENT_TYPE;
                  MOVE : in MOVEMENT := NEXT) is
  begin
    CHECK(LIST);
    ITEM := LIST.CURRENT.VALUE;
    if MOVE /= CURRENT then
      MOVE_TO (LIST, MOVE);
    end if;
    -- Modification done my MOVE_TO
  end READ;

  -- modify the current item
  procedure MODIFY (LIST : in out LIST_TYPE;
                    ITEM : in ELEMENT_TYPE;
                    MOVE : in MOVEMENT := NEXT) is
  begin
    CHECK(LIST);
    LIST.CURRENT.VALUE := ITEM;
    if MOVE /= CURRENT then
      MOVE_TO (LIST, MOVE);
    end if;
    LIST.MODIFIED := TRUE;
  end MODIFY;

  -- put a new element in the list
  procedure INSERT (LIST  : in out LIST_TYPE;
                    ITEM  : in ELEMENT_TYPE;
                    WHERE : in DIRECTION := NEXT) is
    NEW_CELL : LINK;
  begin
    LIST.MODIFIED := TRUE;
    if LIST.FREE = null then
      --create the first element of the list
      NEW_CELL := new CELL;
    else
      NEW_CELL := LIST.FREE;
      LIST.FREE := LIST.FREE.NEXT;
    end if;
    -- fill new cell
    NEW_CELL.VALUE := ITEM;
    if IS_EMPTY (LIST) then
      NEW_CELL.NEXT := null;
      NEW_CELL.PREV := null;
      LIST.POS_FIRST := 1;
      LIST.POS_LAST := 1;
    else
      case WHERE is
        when NEXT =>
          NEW_CELL.NEXT := LIST.CURRENT.NEXT;
          NEW_CELL.PREV := LIST.CURRENT;
          LIST.POS_FIRST := LIST.POS_FIRST + 1;
        when PREV =>
          NEW_CELL.NEXT := LIST.CURRENT;
          NEW_CELL.PREV := LIST.CURRENT.PREV;
          LIST.POS_LAST := LIST.POS_LAST + 1;
      end case;
    end if;
    -- update neibours
    LIST.CURRENT := NEW_CELL;
    if NEW_CELL.PREV /= null then
      NEW_CELL.PREV.NEXT := NEW_CELL;
    else
      LIST.FIRST := NEW_CELL;
    end if;
    if NEW_CELL.NEXT /= null then
      NEW_CELL.NEXT.PREV := NEW_CELL;
    else
      LIST.LAST := NEW_CELL;
    end if;

  exception
    when STORAGE_ERROR =>
      raise FULL_LIST;
  end INSERT;

  -- suppress the current element from the list
  procedure DELETE (LIST : in out LIST_TYPE; MOVE : in DIRECTION := NEXT) is
    DEL_CELL : LINK;
  begin
    CHECK(LIST);

    if LIST.POS_FIRST = 1 and then LIST.POS_LAST = 1 then
      -- Last item of the list
      null;
    else
      -- check movement
      if MOVE = NEXT then
        CHECK_IN(LIST.CURRENT.NEXT);
      elsif MOVE = PREV then
        CHECK_IN (LIST.CURRENT.PREV);
      end if;
    end if;

    LIST.MODIFIED := TRUE;
    -- disconnect
    if LIST.CURRENT.NEXT /= null then
      LIST.CURRENT.NEXT.PREV := LIST.CURRENT.PREV;
    else
      LIST.LAST := LIST.CURRENT.PREV;
    end if;
    if LIST.CURRENT.PREV /= null then
      LIST.CURRENT.PREV.NEXT := LIST.CURRENT.NEXT;
    else
      LIST.FIRST := LIST.CURRENT.NEXT;
    end if;
    -- move
    DEL_CELL := LIST.CURRENT;
    case MOVE is
      when NEXT =>
        LIST.CURRENT := LIST.CURRENT.NEXT;
        LIST.POS_LAST := LIST.POS_LAST - 1;
      when PREV =>
        LIST.CURRENT := LIST.CURRENT.PREV;
        LIST.POS_FIRST := LIST.POS_FIRST - 1;
    end case;
    -- insert in free list
    if LIST.FREE /= null then
      LIST.FREE.PREV := DEL_CELL;
    end if;
    DEL_CELL.PREV := null;
    DEL_CELL.NEXT := LIST.FREE;
    LIST.FREE := DEL_CELL;
    -- check the special case when list is empty
    --  (set pos_first AND pos_last to 0)
    if LIST.CURRENT = null then
      LIST.POS_FIRST := 0;
      LIST.POS_LAST := 0;
    end if;
  end DELETE;


  -- reads and deletes the current element
  procedure GET (LIST : in out LIST_TYPE;
                 ITEM : out ELEMENT_TYPE;
                 MOVE : in DIRECTION := NEXT) is
  begin
    READ(LIST, ITEM, CURRENT);
    DELETE(LIST, MOVE);
    -- Modified flag changed by DELETE
  end GET;

  -- changes current position
  procedure MOVE_TO (LIST         : in out LIST_TYPE;
                     WHERE        : in DIRECTION := NEXT;
                     NUMBER       : in NATURAL := 1;
                     FROM_CURRENT : in BOOLEAN := TRUE) is
    NEW_POS                     : LINK;
    NEW_POS_FIRST, NEW_POS_LAST : NATURAL;
  begin
    CHECK(LIST);
    -- start from
    if FROM_CURRENT then
      NEW_POS := LIST.CURRENT;
      NEW_POS_FIRST := LIST.POS_FIRST;
      NEW_POS_LAST := LIST.POS_LAST;
    else
      case WHERE is
        when NEXT =>
          NEW_POS := LIST.FIRST;
          NEW_POS_FIRST := 1;
          NEW_POS_LAST := LIST_LENGTH(LIST);
        when PREV =>
          NEW_POS := LIST.LAST;
          NEW_POS_FIRST := LIST_LENGTH(LIST);
          NEW_POS_LAST := 1;
      end case;
    end if;
    -- move
    case WHERE is
      when NEXT =>
        for I in 1 .. NUMBER loop
          CHECK_IN (NEW_POS.NEXT);
          NEW_POS := NEW_POS.NEXT;
          NEW_POS_FIRST := NEW_POS_FIRST + 1;
          NEW_POS_LAST := NEW_POS_LAST - 1;
        end loop;
      when PREV =>
        for I in 1 .. NUMBER loop
          CHECK_IN (NEW_POS.PREV);
          NEW_POS := NEW_POS.PREV;
          NEW_POS_FIRST := NEW_POS_FIRST - 1;
          NEW_POS_LAST := NEW_POS_LAST + 1;
        end loop;
    end case;
    -- realy move if no problem
    LIST.CURRENT := NEW_POS;
    LIST.POS_FIRST := NEW_POS_FIRST;
    LIST.POS_LAST := NEW_POS_LAST;
    LIST.MODIFIED := TRUE;
  end MOVE_TO;

  -- permute two elements knowing links to them
  -- (internal procedure for permute and sort)
  procedure PERMUTE (LIST : in out LIST_TYPE; LEFT, RIGHT : in LINK) is
    TMP_NEXT, TMP_PREV : LINK;
  begin

    TMP_PREV := LEFT.PREV;
    TMP_NEXT := LEFT.NEXT;
    if LEFT.NEXT /= RIGHT and then LEFT.PREV /= RIGHT then
      -- no adjacent cells
      -- exchange neighbours links
      if LEFT.PREV /= null then
        LEFT.PREV.NEXT := RIGHT;
      else
        LIST.FIRST := RIGHT;
      end if;
      if LEFT.NEXT /= null then
        LEFT.NEXT.PREV := RIGHT;
      else
        LIST.LAST := RIGHT;
      end if;
      if RIGHT.PREV /= null then
        RIGHT.PREV.NEXT := LEFT;
      else
        LIST.FIRST := LEFT;
      end if;
      if RIGHT.NEXT /= null then
        RIGHT.NEXT.PREV := LEFT;
      else
        LIST.LAST := LEFT;
      end if;

      -- exchange swapped cells links to neighbours
      LEFT.PREV := RIGHT.PREV;
      LEFT.NEXT := RIGHT.NEXT;
      RIGHT.PREV := TMP_PREV;
      RIGHT.NEXT := TMP_NEXT;
    elsif LEFT.NEXT = RIGHT then
      -- left just before right
      -- exchange neighbours links
      if LEFT.PREV /= null then
        LEFT.PREV.NEXT := RIGHT;
      else
        LIST.FIRST := RIGHT;
      end if;
      if RIGHT.NEXT /= null then
        RIGHT.NEXT.PREV := LEFT;
      else
        LIST.LAST := LEFT;
      end if;

      -- exchange swapped cells links to neighbours
      LEFT.PREV := RIGHT;
      LEFT.NEXT := RIGHT.NEXT;
      RIGHT.PREV := TMP_PREV;
      RIGHT.NEXT := LEFT;
    elsif LEFT.PREV = RIGHT then
      -- left just after right
      -- exchange neighbours links
      if LEFT.NEXT /= null then
        LEFT.NEXT.PREV := RIGHT;
      else
        LIST.LAST := RIGHT;
      end if;
      if RIGHT.PREV /= null then
        RIGHT.PREV.NEXT := LEFT;
      else
        LIST.FIRST := LEFT;
      end if;

      -- exchange swapped cells links to neighbours
      LEFT.PREV := RIGHT.PREV;
      LEFT.NEXT := RIGHT;
      RIGHT.PREV := LEFT;
      RIGHT.NEXT := TMP_NEXT;
    else
      raise PROGRAM_ERROR;
    end if;
    LIST.MODIFIED := TRUE;
  end PERMUTE;

  -- permutes 2 elements
  procedure PERMUTE (LIST      : in out LIST_TYPE;
                     NUMBER1      : in NATURAL;
                     NUMBER2      : in NATURAL;
                     WHERE        : in DIRECTION := NEXT;
                     FROM_CURRENT : in BOOLEAN   := FALSE) is
    CURRENT_POSITION : constant NATURAL := GET_POSITION (LIST);
    LINK1, LINK2 : LINK;
  begin
    -- move to elements and store links to them
    MOVE_TO (LIST, WHERE, NUMBER1, FROM_CURRENT);
    LINK1 := LIST.CURRENT;
    MOVE_TO (LIST, WHERE, NUMBER2, FROM_CURRENT);
    LINK2 := LIST.CURRENT;

    -- permute items
    PERMUTE (LIST, LINK1, LINK2);

    -- Restore initial position
    MOVE_TO (LIST, NEXT, CURRENT_POSITION - 1, FROM_CURRENT => FALSE);
    LIST.MODIFIED := TRUE;
  exception
    when NOT_IN_LIST =>
      -- Restore initial position
      MOVE_TO (LIST, NEXT, CURRENT_POSITION - 1, FROM_CURRENT => FALSE);
      raise;
  end PERMUTE;

  -- returns the number of elements in the list (0 if empty)
  function LIST_LENGTH (LIST : LIST_TYPE) return NATURAL is
  begin
    if IS_EMPTY(LIST) then
      return 0;
    else
      return LIST.POS_FIRST + LIST.POS_LAST - 1;
    end if;
  end LIST_LENGTH;

  -- get position from first or last item in list
  function GET_POSITION (LIST : LIST_TYPE;
                         FROM : REFERENCE := FROM_FIRST) return NATURAL is
  begin
    CHECK(LIST);
    case FROM is
      when FROM_FIRST =>
        return LIST.POS_FIRST;
      when FROM_LAST =>
        return LIST.POS_LAST;
    end case;
  end GET_POSITION;

  function IS_MODIFIED (LIST : LIST_TYPE) return BOOLEAN is
  begin
    return LIST.MODIFIED;
  end IS_MODIFIED;

  procedure MODIFICATION_ACK (LIST : in out LIST_TYPE)is
  begin
    LIST.MODIFIED := FALSE;
  end MODIFICATION_ACK;

  procedure SEARCH (LIST         : in out LIST_TYPE;
                    ITEM         : in ELEMENT_TYPE;
                    WHERE        : in DIRECTION := NEXT;
                    OCCURENCE    : in POSITIVE := 1;
                    FROM_CURRENT : in BOOLEAN := TRUE) is
    NEW_POS                     : LINK;
    NEW_POS_FIRST, NEW_POS_LAST : NATURAL;
  begin
    if IS_EMPTY (LIST) then
      raise NOT_IN_LIST;
    end if;
    -- start from
    if FROM_CURRENT then
      NEW_POS := LIST.CURRENT;
    else
      case WHERE is
        when NEXT =>
          NEW_POS := LIST.FIRST;
          NEW_POS_FIRST := 1;
          NEW_POS_LAST := LIST_LENGTH(LIST);
        when PREV =>
          NEW_POS := LIST.LAST;
          NEW_POS_FIRST := LIST_LENGTH(LIST);
          NEW_POS_LAST := 1;
      end case;
    end if;
    -- move
    case WHERE is
      when NEXT =>
        for I in 1 .. OCCURENCE loop
          loop
            exit when EQUAL(NEW_POS.VALUE, ITEM);
            CHECK_IN(NEW_POS.NEXT);
            NEW_POS := NEW_POS.NEXT;
            NEW_POS_FIRST := NEW_POS_FIRST + 1;
            NEW_POS_LAST := NEW_POS_LAST - 1;
          end loop;
        end loop;
      when PREV =>
        for I in 1 .. OCCURENCE loop
          loop
            exit when EQUAL(NEW_POS.VALUE, ITEM);
            CHECK_IN(NEW_POS.PREV);
            NEW_POS := NEW_POS.PREV;
            NEW_POS_FIRST := NEW_POS_FIRST - 1;
            NEW_POS_LAST := NEW_POS_LAST + 1;
          end loop;
        end loop;
    end case;

    LIST.CURRENT := NEW_POS;
    LIST.POS_FIRST := NEW_POS_FIRST;
    LIST.POS_LAST := NEW_POS_LAST;
    LIST.MODIFIED := TRUE;
  end SEARCH;


  procedure SORT (LIST : in out LIST_TYPE) is
    LAST : constant NATURAL := LIST_LENGTH (LIST);
  begin
    if LAST <= 1 then
      -- No or 1 element. No sort.
      return;
    end if;

    declare

      -- recursive procedure which sorts a slice of the list
      procedure QUICK (LEFT, RIGHT : in POSITIVE) is
        -- middle of the slice
        I_FRONTIER : constant POSITIVE := (LEFT + RIGHT) / 2;
        L_FRONTIER : LINK;
        -- indexes in both halfs of the slice
        I_LEFT, I_RIGHT : POSITIVE;
        L_LEFT, L_RIGHT : LINK;
     begin
        I_LEFT := LEFT;
        I_RIGHT := RIGHT;
        -- set link to frontier
        MOVE_TO (LIST, NEXT, I_FRONTIER-1, FALSE);
        L_FRONTIER := LIST.CURRENT;

        loop

          -- first element at left of slice and not positioned ok
          --  regarding the frontier
          MOVE_TO (LIST, NEXT, I_LEFT-1, FALSE);
          while LESS_THAN (LIST.CURRENT.VALUE, L_FRONTIER.VALUE) loop
            MOVE_TO (LIST, NEXT, 1, TRUE);
          end loop;
          L_LEFT := LIST.CURRENT;
          I_LEFT := GET_POSITION (LIST);

          -- last  element a right of slice and not positioned ok
          --  regarding the frontier
          MOVE_TO (LIST, NEXT, I_RIGHT-1, FALSE);
          while LESS_THAN (L_FRONTIER.VALUE, LIST.CURRENT.VALUE) loop
            MOVE_TO (LIST, PREV, 1, TRUE);
          end loop;
          L_RIGHT := LIST.CURRENT;
          I_RIGHT := GET_POSITION (LIST);

          -- exchange and go to next elements if not both in frontier
          if I_LEFT < I_RIGHT then
            PERMUTE (LIST, L_LEFT, L_RIGHT);
            I_LEFT  := I_LEFT  + 1;
            I_RIGHT := I_RIGHT - 1;
          elsif I_LEFT = I_RIGHT then
            -- go to next elements if not crossed
            if I_LEFT /= RIGHT then
              I_LEFT  := I_LEFT  + 1;
            end if;
            if I_RIGHT /= LEFT then
              I_RIGHT := I_RIGHT - 1;
            end if;
          end if;

          -- leave if crossed now
          exit when I_LEFT > I_RIGHT or else
                   (I_LEFT = RIGHT and then I_RIGHT = LEFT);
        end loop;

        -- sort both new slices
        if LEFT   < I_RIGHT then QUICK(LEFT,   I_RIGHT); end if;
        if I_LEFT < RIGHT   then QUICK(I_LEFT, RIGHT);   end if;
      end QUICK;

    begin
      QUICK (1, LAST);
    end;
    -- move to first item
    MOVE_TO (LIST, NEXT, 0, FALSE);
    LIST.MODIFIED := TRUE;
  end SORT;

end DYNAMIC_LIST;
