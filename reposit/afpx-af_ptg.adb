with CALENDAR;
separate (AFPX)
package body AF_PTG is

  type MOUSE_ACTION_REC(KIND : AFPX_TYP.FIELD_KIND_LIST := AFPX_TYP.PUT)
  is record
    case KIND is
      when AFPX_TYP.PUT =>
        -- Nothing to do, discarded, or list action already handled,
        -- no action put
        null;
      when AFPX_TYP.BUTTON | AFPX_TYP.GET =>
        -- Click and release in a BUTTON field
        -- Click and release in a GET field
        FIELD_NO : AFPX_TYP.FIELD_RANGE;
    end case;
  end record;

  LAST_POS : CON_IO.SQUARE;


  -- Sets Foreground and background according to state
  procedure SET_COLORS (FIELD : in AFPX_TYP.FIELD_REC;
                        STATE : in STATE_LIST;
                        FOREGROUND : out CON_IO.EFFECTIVE_COLORS;
                        BACKGROUND : out CON_IO.EFFECTIVE_BASIC_COLORS) is
  begin
    -- Set colors
    case STATE is
      when NORMAL =>
        FOREGROUND := FIELD.COLORS.FOREGROUND;
        BACKGROUND := FIELD.COLORS.BACKGROUND;
      when CLICKED =>
        FOREGROUND := FIELD.COLORS.BACKGROUND;
        BACKGROUND := FIELD.COLORS.FOREGROUND;
      when SELECTED =>
        FOREGROUND := FIELD.COLORS.FOREGROUND;
        BACKGROUND := FIELD.COLORS.SELECTED;
    end case;
  end SET_COLORS;

  -- Put a whole field in attribute
  procedure PUT_FIELD (FIELD_NO : in AFPX_TYP.FIELD_RANGE;
                       STATE    : in STATE_LIST) is
    FIELD : constant AFPX_TYP.FIELD_REC := AF_DSCR.FIELDS(FIELD_NO);
    CHAR_INDEX : AFPX_TYP.CHAR_STR_RANGE;
    FOREGROUND : CON_IO.EFFECTIVE_COLORS;
    BACKGROUND : CON_IO.EFFECTIVE_BASIC_COLORS;
  begin
    -- Set colors
    SET_COLORS (FIELD, STATE, FOREGROUND, BACKGROUND);

    -- Set index to start of field's data
    CHAR_INDEX := FIELD.CHAR_INDEX;
    -- Put SPACES in each row
    for I in 1 .. FIELD.HEIGHT loop
      -- Go to row, left of field
      CON_IO.MOVE (FIELD.UPPER_LEFT.ROW + I - 1, FIELD.UPPER_LEFT.COL);
      CON_IO.PUT (
        S => AF_DSCR.CHARS(CHAR_INDEX .. CHAR_INDEX + FIELD.WIDTH - 1),
        NAME       => CON_IO.SCREEN,
        FOREGROUND => FOREGROUND,
        BLINK_STAT => FIELD.COLORS.BLINK_STAT,
        BACKGROUND => BACKGROUND,
        MOVE       => FALSE);
      -- Update CHAR_INDEX to first char of next row (except after last row)
      if I /= FIELD.HEIGHT then
        CHAR_INDEX := CHAR_INDEX + FIELD.WIDTH;
      end if;
    end loop;
  end PUT_FIELD;

  -- Put a whole row of a field in attribute
  procedure PUT_ROW (FIELD_NO : in AFPX_TYP.FIELD_RANGE;
                     ROW      : in CON_IO.ROW_RANGE;
                     STATE    : in STATE_LIST) is
    FIELD : constant AFPX_TYP.FIELD_REC := AF_DSCR.FIELDS(FIELD_NO);
    CHAR_INDEX : AFPX_TYP.CHAR_STR_RANGE;
    FOREGROUND : CON_IO.EFFECTIVE_COLORS;
    BACKGROUND : CON_IO.EFFECTIVE_BASIC_COLORS;
  begin
    -- Set colors
    SET_COLORS (FIELD, STATE, FOREGROUND, BACKGROUND);
    -- Check Row in field
    if not AFPX_TYP.IN_FIELD (FIELD, (ROW, 0)) then
      raise AFPX_INTERNAL_ERROR;
    end if;

    -- Set index to start of row's data
    CHAR_INDEX := FIELD.CHAR_INDEX + ROW * FIELD.WIDTH;
    -- Go to row, left of field
    CON_IO.MOVE (FIELD.UPPER_LEFT.ROW + ROW, FIELD.UPPER_LEFT.COL);
    CON_IO.PUT (
      S => AF_DSCR.CHARS(CHAR_INDEX .. CHAR_INDEX + FIELD.WIDTH - 1),
      NAME       => CON_IO.SCREEN,
      FOREGROUND => FOREGROUND,
      BLINK_STAT => FIELD.COLORS.BLINK_STAT,
      BACKGROUND => BACKGROUND,
      MOVE       => FALSE);
  end PUT_ROW;

  -- Put a string somwhere in a field
  procedure PUT_STR (FIELD_NO : in AFPX_TYP.FIELD_RANGE;
                     POS      : in CON_IO.SQUARE;
                     STR      : in STRING;
                     STATE    : in STATE_LIST) is
    FIELD : constant AFPX_TYP.FIELD_REC := AF_DSCR.FIELDS(FIELD_NO);
    CHAR_INDEX : AFPX_TYP.CHAR_STR_RANGE;
    FOREGROUND : CON_IO.EFFECTIVE_COLORS;
    BACKGROUND : CON_IO.EFFECTIVE_BASIC_COLORS;
    LEN        : POSITIVE;
  begin
    -- Set colors
    SET_COLORS (FIELD, STATE, FOREGROUND, BACKGROUND);
    -- Check POS in field
    if not AFPX_TYP.IN_FIELD (FIELD, (POS.ROW, POS.COL)) then
      raise AFPX_INTERNAL_ERROR;
    end if;

    -- Set index to start of row's data
    CHAR_INDEX := FIELD.CHAR_INDEX + POS.ROW * FIELD.WIDTH + POS.COL;
    -- Go to row, left of field
    CON_IO.MOVE (FIELD.UPPER_LEFT.ROW + POS.ROW,
                 FIELD.UPPER_LEFT.COL + POS.COL);
    -- Adjust str length to truncate it if is too long
    if AFPX_TYP.IN_FIELD (FIELD, (POS.ROW, POS.COL + STR'LENGTH - 1)) then
      LEN := STR'LENGTH;
    else
      LEN := FIELD.WIDTH - POS.COL;
    end if;

    CON_IO.PUT (
      S          => STR (STR'FIRST .. STR'FIRST + LEN - 1),
      NAME       => CON_IO.SCREEN,
      FOREGROUND => FOREGROUND,
      BLINK_STAT => FIELD.COLORS.BLINK_STAT,
      BACKGROUND => BACKGROUND,
      MOVE       => FALSE);
  end PUT_STR;

  -- Erase a field (screen_background, screen_background)
  procedure ERASE_FIELD (FIELD_NO : in AFPX_TYP.ABSOLUTE_FIELD_RANGE) is
    FIELD : constant AFPX_TYP.FIELD_REC := AF_DSCR.FIELDS(FIELD_NO);
    SPACES : constant STRING (1 .. FIELD.WIDTH) := (others => ' ');
  begin
    -- Put SPACES in each row
    for I in 1 .. FIELD.HEIGHT loop
      -- Go to row, left of field
      CON_IO.MOVE (FIELD.UPPER_LEFT.ROW + I - 1, FIELD.UPPER_LEFT.COL);
      CON_IO.PUT (S          => SPACES,
                  NAME       => CON_IO.SCREEN,
                  FOREGROUND => CON_IO.GET_BACKGROUND(CON_IO.SCREEN),
                  BLINK_STAT => CON_IO.NOT_BLINK,
                  BACKGROUND => CON_IO.GET_BACKGROUND(CON_IO.SCREEN),
                  MOVE       => FALSE);
    end loop;
  end ERASE_FIELD;

  function VALID_CLICK return BOOLEAN is
    MOUSE_STATUS : CON_IO.MOUSE_EVENT_REC;
    VALID : BOOLEAN;
    use CON_IO;
  begin
    -- Check if mouse button is clicked
    CON_IO.GET_MOUSE_EVENT (MOUSE_STATUS);
    VALID := MOUSE_STATUS.BUTTON = CON_IO.LEFT
             and then MOUSE_STATUS.STATUS = CON_IO.PRESSED;
    if VALID then
      LAST_POS := (MOUSE_STATUS.ROW, MOUSE_STATUS.COL);
    end if;
    return VALID;
  end VALID_CLICK;

  function LAST_CLICK return CON_IO.SQUARE is
  begin
    return LAST_POS;
  end LAST_CLICK;

  function WAIT_RELEASE return CON_IO.SQUARE is
    TIME : CALENDAR.TIME := CALENDAR.CLOCK;
    MOUSE_STATUS : CON_IO.MOUSE_EVENT_REC;
    EVENT : CON_IO.EVENT_LIST;
    KEY : NATURAL;
    IS_CHAR, CTRL, SHIFT : BOOLEAN;
    use CON_IO;
  begin
    -- Wait until button released
    loop
      CON_IO.GET_KEY_TIME (TIME, TRUE, TRUE,
        EVENT, KEY, IS_CHAR, CTRL, SHIFT);
      if EVENT = CON_IO.MOUSE_BUTTON then
        CON_IO.GET_MOUSE_EVENT (MOUSE_STATUS);
        exit when MOUSE_STATUS.BUTTON = CON_IO.LEFT
             and then MOUSE_STATUS.STATUS = CON_IO.RELEASED;
      end if;
    end loop;
    -- Get pos when last released
    return (MOUSE_STATUS.ROW, MOUSE_STATUS.COL);
  end WAIT_RELEASE;

  -- Handle a click action.
  -- Discard any position not in clickable field
  -- Reverse colors of field (or row of list)
  -- Wait for release
  -- Restore BUTTON color
  -- GET selection is left to be achieved (restoring previous and setting
  --  new color)
  procedure HANDLE_CLICK (LIST_PRESENT : in BOOLEAN;
                          RESULT : out MOUSE_ACTION_REC) is
    CURSOR_POS : constant CON_IO.SQUARE := CON_IO.POSITION;
    VALID_FIELD : BOOLEAN;
    CLICK_POS : CON_IO.SQUARE;
    CLICK_FIELD : AFPX_TYP.ABSOLUTE_FIELD_RANGE;
    RELEASE_POS : CON_IO.SQUARE;
    CLICK_ROW_LIST : CON_IO.ROW_RANGE;
    CLICK_ON_SELECTED : BOOLEAN;
    FIELD : AFPX_TYP.FIELD_REC;
    LIST_STATUS : AF_LIST.STATUS_REC;
    use AFPX_TYP;

  begin
    -- Result event discarded
    RESULT := (KIND => AFPX_TYP.PUT);
    if not VALID_CLICK then
      return;
    end if;

    VALID_FIELD := TRUE;
    -- Get pos, find field
    CLICK_POS := LAST_CLICK;
    if LIST_PRESENT and then IN_FIELD_ABSOLUTE(0, CLICK_POS)
    and then not AF_DSCR.FIELDS(0).ISPROTECTED then
      CLICK_FIELD := 0;
      CLICK_ROW_LIST := CLICK_POS.ROW - AF_DSCR.FIELDS(0).UPPER_LEFT.ROW;
      LIST_STATUS := AF_LIST.GET_STATUS;
      -- Check that an item is displayed at this row
      if CLICK_ROW_LIST >= LIST_STATUS.NB_ROWS then
        -- No data in this row
        VALID_FIELD := FALSE;
      end if;
      -- Click on already selected item?
      if AF_LIST.ID_DISPLAYED (LIST_STATUS.ID_SELECTED) then
        CLICK_ON_SELECTED :=
          CLICK_ROW_LIST = AF_LIST.TO_ROW (LIST_STATUS.ID_SELECTED);
      else
        CLICK_ON_SELECTED := FALSE;
     end if;
    else
      CLICK_FIELD := 0;
      for I in 1 .. AF_DSCR.CURRENT_DSCR.NB_FIELDS loop
        if AF_DSCR.FIELDS(I).KIND /= AFPX_TYP.PUT and then
           AF_DSCR.FIELDS(I).ACTIVATED            and then
           not AF_DSCR.FIELDS(I).ISPROTECTED      and then
           IN_FIELD_ABSOLUTE(I, CLICK_POS) then
          CLICK_FIELD := I;
          exit;
        end if;
      end loop;
      if CLICK_FIELD = 0 then
        -- Invalid field
        VALID_FIELD := FALSE;
      end if;
    end if;
    if VALID_FIELD then
      -- reverse colors of field/row
      if CLICK_FIELD = 0 then
        -- Reverse
        AF_LIST.PUT (CLICK_ROW_LIST, CLICKED);
      else
        PUT_FIELD (CLICK_FIELD, CLICKED);
      end if;
    end if;

    -- Wait release. No keyboard input
    RELEASE_POS := WAIT_RELEASE;

    -- Done if click not valid
    if not VALID_FIELD then
      CON_IO.MOVE (CURSOR_POS);
      return;
    end if;
    FIELD := AF_DSCR.FIELDS(CLICK_FIELD);

    -- Check release in same field/row than click
    if not IN_FIELD_ABSOLUTE (CLICK_FIELD, RELEASE_POS) then
      VALID_FIELD := FALSE;
    elsif CLICK_FIELD = 0 and then RELEASE_POS.ROW /= CLICK_POS.ROW then
      VALID_FIELD := FALSE;
    end if;

    if CLICK_FIELD = 0 then
      if CLICK_ON_SELECTED then
        -- Valid or not, restore selected
        AF_LIST.PUT (CLICK_ROW_LIST, SELECTED);
      else
        if not VALID_FIELD then
          -- Invalid release, restore clicked field as normal
          AF_LIST.PUT (CLICK_ROW_LIST, NORMAL);
        else
          -- Valid release
          -- Un-select previous if it was shown
          if AF_LIST.ID_DISPLAYED (LIST_STATUS.ID_SELECTED) then
            AF_LIST.PUT (AF_LIST.TO_ROW(LIST_STATUS.ID_SELECTED), NORMAL);
          end if;
          -- change selected if valid and new
          AF_LIST.PUT (CLICK_ROW_LIST, SELECTED);
          -- Set new selected
          AF_LIST.SET_SELECTED (AF_LIST.TO_ID(CLICK_ROW_LIST));
        end if;
      end if;
      -- Result is PUT
    elsif FIELD.KIND = AFPX_TYP.GET then
      -- If field is get: restore color if not valid
      if not VALID_FIELD then
        PUT_FIELD (CLICK_FIELD, NORMAL);
        RESULT := (KIND => AFPX_TYP.PUT);
      else
        RESULT := (KIND => AFPX_TYP.GET, FIELD_NO => CLICK_FIELD);
      end if;
    else
      -- If field is button: restore color
      PUT_FIELD (CLICK_FIELD, NORMAL);
      if VALID_FIELD then
        RESULT := (KIND => AFPX_TYP.BUTTON, FIELD_NO => CLICK_FIELD);
      end if;
    end if;

    -- Skip any keyboard entry during handle click
    CON_IO.MOVE (CURSOR_POS);
  end HANDLE_CLICK;


  -- Print the fields and the list, then gets
  procedure PTG (
                 CURSOR_FIELD : in out AFPX_TYP.FIELD_RANGE;
                 CURSOR_COL   : in out CON_IO.COL_RANGE;
                 RESULT       : out RESULT_REC;
                 REDISPLAY    : in BOOLEAN;
                 GET_ACTIVE   : in BOOLEAN) is
    LIST_PRESENT : BOOLEAN;
    NEW_FIELD : BOOLEAN;
    FIELD : AFPX_TYP.FIELD_REC;
    LAST : NATURAL;
    STAT : CON_IO.CURS_MVT;
    POS : POSITIVE;
    INSERT : BOOLEAN;
    FOREGROUND : CON_IO.EFFECTIVE_COLORS;
    BACKGROUND : CON_IO.EFFECTIVE_BASIC_COLORS;
    DONE : BOOLEAN;

    use AFPX_TYP;

  begin
    -- List present : defined, activated and not empty
    LIST_PRESENT := AF_DSCR.FIELDS(0).KIND = AFPX_TYP.BUTTON
           and then AF_DSCR.FIELDS(0).ACTIVATED
           and then not LINE_LIST_MNG.IS_EMPTY(LINE_LIST);
    -- Init list if needed
    if LIST_PRESENT then
      AF_LIST.SET_SELECTED (LINE_LIST_MNG.GET_POSITION(LINE_LIST));
    end if;

    -- Redisplay list if requested or needed
    if (REDISPLAY or else LINE_LIST_MNG.IS_MODIFIED (LINE_LIST))
    and then AF_DSCR.FIELDS(0).KIND = AFPX_TYP.BUTTON then
      -- list defined
      if LIST_PRESENT then
        if AF_LIST.GET_STATUS.ID_TOP = 0 then
          -- First display of the list, start at current
          AF_LIST.DISPLAY(AF_LIST.GET_STATUS.ID_SELECTED);
        else
          AF_LIST.DISPLAY(AF_LIST.GET_STATUS.ID_TOP);
        end if;
      elsif not AF_DSCR.FIELDS(0).ACTIVATED then
        -- List not active
        ERASE_FIELD (0);
      else
        -- Empty list
        AF_LIST.DISPLAY(1);
      end if;
    end if;

    -- Redisplay all fields if requested or needed
    if REDISPLAY or else AF_DSCR.CURRENT_DSCR.MODIFIED then
      for I in 1 .. AF_DSCR.CURRENT_DSCR.NB_FIELDS loop
        if AF_DSCR.FIELDS(I).ACTIVATED then
          PUT_FIELD (I, NORMAL);
        else
          ERASE_FIELD (I);
        end if;
      end loop;
    end if;

    -- A new field at start up if some get field
    NEW_FIELD := GET_ACTIVE;

    -- The infinite loop
    loop
      -- Get field, set colors when field changes
      if NEW_FIELD then
        FIELD := AF_DSCR.FIELDS(CURSOR_FIELD);
        SET_COLORS (FIELD, SELECTED, FOREGROUND, BACKGROUND);
        INSERT := FALSE;
        NEW_FIELD := FALSE;
      end if;
      if GET_ACTIVE then
        POS := CURSOR_COL + 1;
        -- Move at beginning of field and put_then_get
        CON_IO.MOVE (FIELD.UPPER_LEFT.ROW, FIELD.UPPER_LEFT.COL);
        CON_IO.PUT_THEN_GET (
         STR    => AF_DSCR.CHARS
                    (FIELD.CHAR_INDEX .. FIELD.CHAR_INDEX + FIELD.WIDTH - 1),
         LAST   => LAST,
         STAT   => STAT,
         POS    => POS,
         INSERT => INSERT,
         NAME   => CON_IO.SCREEN,
         FOREGROUND => FOREGROUND,
         BLINK_STAT => FIELD.COLORS.BLINK_STAT,
         BACKGROUND => BACKGROUND);
        CURSOR_COL := POS - 1;
      else
        -- Blind get
        CON_IO.MOVE (CON_IO.ROW_RANGE'LAST, CON_IO.COL_RANGE'LAST);
        CON_IO.PUT_THEN_GET (
         STR    => AF_DSCR.CHARS (1 .. 0),
         LAST   => LAST,
         STAT   => STAT,
         POS    => POS,
         INSERT => INSERT,
         NAME   => CON_IO.SCREEN,
         FOREGROUND => CON_IO.GET_BACKGROUND(CON_IO.SCREEN),
         BLINK_STAT => CON_IO.NOT_BLINK,
         BACKGROUND => CON_IO.GET_BACKGROUND(CON_IO.SCREEN));
       end if;

      DONE := FALSE;
      -- Now the BIG case
      case STAT is
        when CON_IO.UP =>
          -- List scroll down
          if LIST_PRESENT then
            AF_LIST.UPDATE (UP);
          end if;
        when CON_IO.DOWN =>
          -- List scroll up
          if LIST_PRESENT then
            AF_LIST.UPDATE (DOWN);
          end if;
        when CON_IO.PGUP =>
          -- List page up
          if LIST_PRESENT then
            AF_LIST.UPDATE (PAGE_UP);
          end if;
        when CON_IO.PGDOWN =>
          -- List page down
          if LIST_PRESENT then
            AF_LIST.UPDATE (PAGE_DOWN);
          end if;
        when CON_IO.CTRL_PGUP =>
          -- List page up
          if LIST_PRESENT then
            AF_LIST.UPDATE (TOP);
          end if;
        when CON_IO.CTRL_PGDOWN =>
          -- List page down
          if LIST_PRESENT then
            AF_LIST.UPDATE (BOTTOM);
          end if;
        when CON_IO.RIGHT | CON_IO.FULL | CON_IO.TAB =>
          if GET_ACTIVE then
            -- Beginning of next get field
            -- Restore normal color of previous field
            PUT_FIELD (CURSOR_FIELD, NORMAL);
            CURSOR_FIELD := NEXT_GET_FIELD (CURSOR_FIELD);
            CURSOR_COL := 0;
            NEW_FIELD := TRUE;
          end if;
        when CON_IO.LEFT =>
          if GET_ACTIVE then
            -- End of prev get field
            -- Restore normal color of previous field
            PUT_FIELD (CURSOR_FIELD, NORMAL);
            CURSOR_FIELD := PREV_GET_FIELD (CURSOR_FIELD);
            CURSOR_COL := AF_DSCR.FIELDS(CURSOR_FIELD).WIDTH - 1;
            NEW_FIELD := TRUE;
          end if;
        when CON_IO.STAB =>
          if GET_ACTIVE then
            -- Beginning of prev get field
            -- Restore normal color of previous field
            PUT_FIELD (CURSOR_FIELD, NORMAL);
            CURSOR_FIELD := PREV_GET_FIELD (CURSOR_FIELD);
            CURSOR_COL := 0;
            NEW_FIELD := TRUE;
          end if;
        when CON_IO.RET =>
          -- End put_then_get on keyboard ret
          if LIST_PRESENT then
            AF_LIST.SET_CURRENT;
          end if;
          RESULT := (ID_SELECTED  => AF_LIST.GET_STATUS.ID_SELECTED,
                     EVENT        => KEYBOARD,
                     KEYBOARD_KEY => RETURN_KEY);
          DONE := TRUE;
        when CON_IO.ESC =>
          -- End put_then_get on keyboard esc
          if LIST_PRESENT then
            AF_LIST.SET_CURRENT;
          end if;
          RESULT := (ID_SELECTED  => AF_LIST.GET_STATUS.ID_SELECTED,
                     EVENT        => KEYBOARD,
                     KEYBOARD_KEY => ESCAPE_KEY);
          DONE := TRUE;
        when CON_IO.MOUSE_BUTTON =>
          declare
            CLICK_RESULT : MOUSE_ACTION_REC;
          begin
            HANDLE_CLICK (LIST_PRESENT, CLICK_RESULT);
            case CLICK_RESULT.KIND is
              when AFPX_TYP.PUT =>
                null;
              when AFPX_TYP.GET =>
                if CLICK_RESULT.FIELD_NO /= CURSOR_FIELD then
                  -- Restore normal color of previous field
                  PUT_FIELD (CURSOR_FIELD, NORMAL);
                  -- Change field
                  CURSOR_FIELD := CLICK_RESULT.FIELD_NO;
                  CURSOR_COL := 0;
                  NEW_FIELD := TRUE;
                end if;
              when AFPX_TYP.BUTTON =>
                -- End of put_then_get
                if LIST_PRESENT then
                  AF_LIST.SET_CURRENT;
                end if;
                RESULT := (ID_SELECTED  => AF_LIST.GET_STATUS.ID_SELECTED,
                           EVENT        => MOUSE_BUTTON,
                           FIELD_NO     => FIELD_RANGE(CLICK_RESULT.FIELD_NO));
                DONE := TRUE;
            end case;
          end;
        when CON_IO.BREAK =>
          RESULT := (ID_SELECTED  => AF_LIST.GET_STATUS.ID_SELECTED,
                     EVENT        => KEYBOARD,
                     KEYBOARD_KEY => BREAK_KEY);
          DONE := TRUE;
        when CON_IO.REFRESH =>
          RESULT := (ID_SELECTED  => AF_LIST.GET_STATUS.ID_SELECTED,
                     EVENT        => REFRESH);
          DONE := TRUE;
        when CON_IO.TIMEOUT =>
          null;
      end case;

      exit when DONE;
    end loop;

    AF_DSCR.CURRENT_DSCR.MODIFIED := FALSE;
  end PTG;

end AF_PTG;
