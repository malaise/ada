separate (AFPX)
package body AF_LIST is

  STATUS : STATUS_REC;
  OPENED : BOOLEAN := FALSE;

  LIST_WINDOW : CON_IO.WINDOW;

  -- Compute status
  procedure COMPUTE (FIRST_ITEM_ID : in POSITIVE);

  -- Open / Re-open the list window
  procedure OPEN is
    use AFPX_TYP;
  begin
    -- Check there is a descriptor
    AF_DSCR.CHECK;
    -- Close previous window
    if CON_IO.IS_OPEN (LIST_WINDOW) then
      CON_IO.CLOSE (LIST_WINDOW);
    end if;
    -- Check there is a window in the dscr
    if AF_DSCR.FIELDS(0).KIND = AFPX_TYP.BUTTON then
      CON_IO.OPEN (LIST_WINDOW,
                   AF_DSCR.FIELDS(0).UPPER_LEFT,
                   AF_DSCR.FIELDS(0).LOWER_RIGHT);
      OPENED := TRUE;
      -- Start at top
      COMPUTE (1);
    else
      OPENED := FALSE;
    end if;

  end OPEN;

  procedure MOVE (ID : in POSITIVE) is
  begin
    LINE_LIST_MNG.MOVE_TO (LINE_LIST, LINE_LIST_MNG.NEXT, ID - 1, FALSE);
  end MOVE;

  procedure GET_CURRENT_ITEM (ITEM : out LINE_REC) is
  begin
    LINE_LIST_MNG.READ (LINE_LIST, ITEM, LINE_LIST_MNG.NEXT);
  exception
    when LINE_LIST_MNG.NOT_IN_LIST =>
      LINE_LIST_MNG.READ (LINE_LIST, ITEM, LINE_LIST_MNG.CURRENT);
  end GET_CURRENT_ITEM;

  procedure PUT (ROW : in CON_IO.ROW_RANGE; STATE : in AF_PTG.STATE_LIST;
                 ITEM : in LINE_REC) is
    STR : STRING (1 .. AF_DSCR.FIELDS(0).WIDTH) := (others => ' ');
    FOREGROUND : CON_IO.EFFECTIVE_COLORS;
    BACKGROUND : CON_IO.EFFECTIVE_BASIC_COLORS;
  begin
    -- Set colors
    AF_PTG.SET_COLORS (AF_DSCR.FIELDS(0), STATE,
                       FOREGROUND, BACKGROUND);
    -- Set str
    if ITEM.LEN > STR'LAST then
      STR := ITEM.STR (STR'RANGE);
    else
      STR (1 .. ITEM.LEN) := ITEM.STR (1 .. ITEM.LEN);
    end if;
    -- Move
    CON_IO.MOVE ( (ROW, 0), LIST_WINDOW);
    -- Put
    CON_IO.PUT (S => STR,
                NAME => LIST_WINDOW,
                FOREGROUND => FOREGROUND,
                BLINK_STAT => AF_DSCR.FIELDS(0).COLORS.BLINK_STAT,
                BACKGROUND => BACKGROUND,
                MOVE => FALSE);
  end PUT;

  procedure CLEAR (ROW : in CON_IO.ROW_RANGE) is
    STR : constant STRING (1 .. AF_DSCR.FIELDS(0).WIDTH) := (others => ' ');
    FOREGROUND : CON_IO.EFFECTIVE_COLORS;
    BACKGROUND : CON_IO.EFFECTIVE_BASIC_COLORS;
  begin
    -- Set colors
    AF_PTG.SET_COLORS (AF_DSCR.FIELDS(0), AF_PTG.NORMAL,
                       FOREGROUND, BACKGROUND);
    -- Move
    CON_IO.MOVE ( (ROW, 0), LIST_WINDOW);
    -- Put
    CON_IO.PUT (S => STR,
                NAME => LIST_WINDOW,
                FOREGROUND => FOREGROUND,
                BLINK_STAT => AF_DSCR.FIELDS(0).COLORS.BLINK_STAT,
                BACKGROUND => BACKGROUND,
                MOVE => FALSE);
  end CLEAR;

  procedure PUT (ROW : in CON_IO.ROW_RANGE; STATE : in AF_PTG.STATE_LIST) is
    ID : POSITIVE;
    ITEM : LINE_REC;
  begin
    if not OPENED then
      raise NOT_OPENED;
    end if;
    ID := STATUS.ID_TOP + ROW;
    MOVE (ID);
    GET_CURRENT_ITEM (ITEM);
    PUT (ROW, STATE, ITEM);
  exception
    when others =>
      raise AFPX_INTERNAL_ERROR;
  end PUT;


  procedure SET_COLORS is
  begin
    CON_IO.SET_FOREGROUND (AF_DSCR.FIELDS(0).COLORS.FOREGROUND,
                           AF_DSCR.FIELDS(0).COLORS.BLINK_STAT, LIST_WINDOW);
    CON_IO.SET_BACKGROUND (AF_DSCR.FIELDS(0).COLORS.BACKGROUND, LIST_WINDOW);
  end SET_COLORS;

  -- Compute status
  procedure COMPUTE (FIRST_ITEM_ID : in POSITIVE) is
  begin
    if not OPENED then
      raise NOT_OPENED;
    end if;
    if LINE_LIST_MNG.IS_EMPTY (LINE_LIST) then
      STATUS.NB_ROWS := 0;
      STATUS.ID_TOP := 0;
      STATUS.ID_BOTTOM := 0;
      STATUS.ID_SELECTED := 0;
      return;
    end if;

    if STATUS.ID_SELECTED > LINE_LIST_MNG.LIST_LENGTH (LINE_LIST) then
      raise LINE_LIST_MNG.NOT_IN_LIST;
    end if;
    STATUS.ID_TOP := FIRST_ITEM_ID;
    -- top + height - 1 <= length => can display height items
    if LINE_LIST_MNG.LIST_LENGTH (LINE_LIST) - FIRST_ITEM_ID >=
       AF_DSCR.FIELDS(0).HEIGHT then
      -- Can display HEIGHT items
      STATUS.NB_ROWS := AF_DSCR.FIELDS(0).HEIGHT;
      STATUS.ID_TOP := FIRST_ITEM_ID;
    elsif LINE_LIST_MNG.LIST_LENGTH (LINE_LIST) <
          AF_DSCR.FIELDS(0).HEIGHT then
      -- Cannot display LIST length items whatever first
      STATUS.NB_ROWS := LINE_LIST_MNG.LIST_LENGTH (LINE_LIST);
      STATUS.ID_TOP := 1;
   else
      -- Can display LIST length items but not with this first.
      -- Set top to display last page
      STATUS.NB_ROWS := AF_DSCR.FIELDS(0).HEIGHT;
      STATUS.ID_TOP := LINE_LIST_MNG.LIST_LENGTH (LINE_LIST)
                     - AF_DSCR.FIELDS(0).HEIGHT + 1;
    end if;
    STATUS.ID_BOTTOM := STATUS.ID_TOP + STATUS.NB_ROWS - 1;
    -- Select by default
    if STATUS.ID_SELECTED = 0 then
      STATUS.ID_SELECTED := STATUS.ID_TOP;
    end if;
  exception
    when others =>
      raise AFPX_INTERNAL_ERROR;
  end COMPUTE;

  -- Display the list, starting from FIRST_ITEM
  procedure DISPLAY (FIRST_ITEM_ID : in POSITIVE) is
    ITEM : LINE_REC;
  begin
    -- Set status
    COMPUTE (FIRST_ITEM_ID);

    SET_COLORS;
    -- I clear the window in all cases
    --  otherwise I get strange colors up to the current pos
    --  when refreshing twice the list
    if LINE_LIST_MNG.IS_EMPTY (LINE_LIST) then
      CON_IO.CLEAR (LIST_WINDOW);
      return;
    end if;

    -- Display list
    MOVE (STATUS.ID_TOP);
    for I in 1 .. STATUS.NB_ROWS loop
      GET_CURRENT_ITEM (ITEM);
      if not AF_DSCR.FIELDS(0).ISPROTECTED
      and then STATUS.ID_TOP + I - 1 = STATUS.ID_SELECTED then
        PUT (I - 1, AF_PTG.SELECTED, ITEM);
      else
        PUT (I - 1, AF_PTG.NORMAL, ITEM);
      end if;
    end loop;
    MOVE (STATUS.ID_SELECTED);

    -- Display empty end of list (if any)
    for I in STATUS.NB_ROWS + 1 .. AF_DSCR.FIELDS(0).HEIGHT loop
      CLEAR (I - 1);
    end loop;


  exception
    when others =>
      raise AFPX_INTERNAL_ERROR;
  end DISPLAY;

  -- Actions on the list
  -- type ACTION_LIST is (UP, DOWN, PAGE_UP, PAGE_DOWN);

  -- Update the list due to an action
  procedure UPDATE (ACTION : in LIST_ACTION_LIST) is
    FIRST_ITEM_ID : NATURAL;
  begin
    if not OPENED then
      raise NOT_OPENED;
    end if;
    if LINE_LIST_MNG.IS_EMPTY (LINE_LIST)
    or else STATUS.NB_ROWS /= AF_DSCR.FIELDS(0).HEIGHT then
      return;
    end if;

    case ACTION is
      when UP =>
        -- Scroll 1 row down
        if STATUS.ID_TOP /= 1 then
          FIRST_ITEM_ID := STATUS.ID_TOP - 1;
          DISPLAY (FIRST_ITEM_ID);
        end if;
      when DOWN =>
        -- Scroll 1 row down
        if STATUS.ID_BOTTOM /= LINE_LIST_MNG.LIST_LENGTH (LINE_LIST) then
          FIRST_ITEM_ID := STATUS.ID_TOP + 1;
          DISPLAY (FIRST_ITEM_ID);
        end if;
      when PAGE_DOWN =>
        -- Display next page
        -- Bottom + height < length => Bottom + height exists
        if LINE_LIST_MNG.LIST_LENGTH (LINE_LIST) - STATUS.ID_BOTTOM >
        AF_DSCR.FIELDS(0).HEIGHT then
          FIRST_ITEM_ID := STATUS.ID_TOP + AF_DSCR.FIELDS(0).HEIGHT;
        elsif STATUS.ID_BOTTOM /= LINE_LIST_MNG.LIST_LENGTH (LINE_LIST) then
          -- End at last item
          FIRST_ITEM_ID := LINE_LIST_MNG.LIST_LENGTH (LINE_LIST)
                           - AF_DSCR.FIELDS(0).HEIGHT + 1;
        else
          -- Already at bottom of list
          return;
        end if;
        DISPLAY (FIRST_ITEM_ID);
      when PAGE_UP =>
        -- Display previous page
        -- top - height > 1 => top - height exists
        if STATUS.ID_TOP > AF_DSCR.FIELDS(0).HEIGHT + 1 then
          FIRST_ITEM_ID := STATUS.ID_TOP - AF_DSCR.FIELDS(0).HEIGHT;
        elsif STATUS.ID_TOP /= 1 then
          -- Start at first item
          FIRST_ITEM_ID := 1;
        else
          -- Already at top of list
          return;
        end if;
        DISPLAY (FIRST_ITEM_ID);
      when TOP =>
        -- Move to top of list
        if STATUS.ID_TOP = 1 then
          -- Already at top of list
          return;
        end if;
        FIRST_ITEM_ID := 1;
        DISPLAY (FIRST_ITEM_ID);
      when BOTTOM =>
        -- Move to bottom of list
        if STATUS.ID_BOTTOM = LINE_LIST_MNG.LIST_LENGTH (LINE_LIST) then
          -- Already at bottom of list
          return;
        end if;
        FIRST_ITEM_ID := LINE_LIST_MNG.LIST_LENGTH (LINE_LIST)
                         - AF_DSCR.FIELDS(0).HEIGHT + 1;
        DISPLAY (FIRST_ITEM_ID);
    end case;
  exception
    when others =>
      raise AFPX_INTERNAL_ERROR;
  end UPDATE;

  -- Set the current item (selected_color) of the lis
  procedure SET_SELECTED (ITEM_ID : in POSITIVE) is
  begin
    if not OPENED then
      raise NOT_OPENED;
    end if;
    if ITEM_ID > LINE_LIST_MNG.LIST_LENGTH (LINE_LIST) then
      raise LINE_LIST_MNG.NOT_IN_LIST;
    end if;
    STATUS.ID_SELECTED := ITEM_ID;
  end SET_SELECTED;

  -- Status of the list
  function GET_STATUS return STATUS_REC is
  begin
    return STATUS;
  end GET_STATUS;

  procedure SET_CURRENT is
  begin
    if not OPENED then
      raise NOT_OPENED;
    end if;
    if LINE_LIST_MNG.IS_EMPTY (LINE_LIST) then
      return;
    end if;
    MOVE (STATUS.ID_SELECTED);
  exception
    when others =>
      raise AFPX_INTERNAL_ERROR;
  end SET_CURRENT;

  -- Is an ID, a row displayed
  function ID_DISPLAYED (ID : POSITIVE) return BOOLEAN is
  begin
    if not OPENED then
      raise NOT_OPENED;
    end if;
    return ID >= STATUS.ID_TOP and then ID <= STATUS.ID_BOTTOM;
  end ID_DISPLAYED;

  function ROW_DISPLAYED (ROW : CON_IO.ROW_RANGE) return BOOLEAN is
  begin
    if not OPENED then
      raise NOT_OPENED;
    end if;
    return ROW < STATUS.NB_ROWS;
  end ROW_DISPLAYED;

  -- ROW <-> Item ID
  function TO_ROW (ID : POSITIVE) return CON_IO.ROW_RANGE is
  begin
    if not ID_DISPLAYED (ID) then
      raise AFPX_INTERNAL_ERROR;
    end if;
    return ID - STATUS.ID_TOP;
  end TO_ROW;

  function TO_ID  (ROW : CON_IO.ROW_RANGE) return POSITIVE is
  begin
    if not ROW_DISPLAYED (ROW) then
      raise AFPX_INTERNAL_ERROR;
    end if;
    return ROW + STATUS.ID_TOP;
  end TO_ID;

end AF_LIST;
