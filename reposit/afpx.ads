with TEXT_HANDLER, CON_IO, DYNAMIC_LIST;

package AFPX is

  -- Descriptor, field index
  type DESCRIPTOR_RANGE is new POSITIVE range 1 .. 50;
  type ABSOLUTE_FIELD_RANGE is new NATURAL range 0 .. 200;
  subtype FIELD_RANGE is ABSOLUTE_FIELD_RANGE
          range 1 .. ABSOLUTE_FIELD_RANGE 'LAST;

  -- The content of one row of one field (encode, decode)
  subtype STR_TXT is TEXT_HANDLER.TEXT (CON_IO.COL_RANGE_LAST + 1);

  -- Width and height of a field
  subtype HEIGHT_RANGE is POSITIVE range 1 .. CON_IO.ROW_RANGE_LAST + 1;
  subtype WIDTH_RANGE  is POSITIVE range 1 .. CON_IO.COL_RANGE_LAST + 1;

  -- Set current descriptor (read from file)
  -- Previous descriptor modifications (from encode, set_colors, put_then_get)
  --  are lost
  -- Exceptions : NO_DESCRIPTOR (DESCRIPTOR not found)
  procedure USE_DESCRIPTOR (DESCRIPTOR_NO : in DESCRIPTOR_RANGE);

  -- Clear the content of a field
  -- Exceptions : NO_DESCRIPTOR (no DESCRIPTOR in use),
  --              INVALID_FIELD (FIELD_NO too big)
  procedure CLEAR_FIELD (FIELD_NO : in FIELD_RANGE);

  -- Reset the field from initial definition in file (colors, content,
  --  activation, protection)
  -- The field becomes activated.
  -- Exceptions : NO_DESCRIPTOR (no DESCRIPTOR in use),
  --              INVALID_FIELD (FIELD_NO too big)
  procedure RESET_FIELD (FIELD_NO : in ABSOLUTE_FIELD_RANGE);

  -- Width and height of a field
  -- Exceptions : NO_DESCRIPTOR (no DESCRIPTOR in use),
  --              INVALID_FIELD (FIELD_NO too big)
  procedure GET_FIELD_SIZE (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                            HEIGHT : out HEIGHT_RANGE;
                            WIDTH  : out WIDTH_RANGE);

  -- Encode a string in a field.
  -- The ROW is filled with spaces, then with STR starting at COL
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  --              INVALID_SQUARE (not in field),
  --              STRING_TOO_LONG (due to SQUARE.COL)
  procedure ENCODE_FIELD (FIELD_NO : in FIELD_RANGE;
                          FROM_POS : in CON_IO.SQUARE;
                          STR      : in STRING);
  procedure ENCODE_FIELD (FIELD_NO : in FIELD_RANGE;
                          FROM_POS : in CON_IO.SQUARE;
                          STR      : in STR_TXT);

  -- Decode the content of a row of a field
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD, INVALID_ROW
  function DECODE_FIELD (FIELD_NO : FIELD_RANGE;
                         ROW      : CON_IO.ROW_RANGE)
                        return STRING;
  procedure DECODE_FIELD (FIELD_NO : in FIELD_RANGE;
                          ROW      : in CON_IO.ROW_RANGE;
                          STR      : in out STR_TXT);


  -- Get field colors
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  procedure GET_FIELD_COLORS (
    FIELD_NO   : in ABSOLUTE_FIELD_RANGE;
    FOREGROUND : out CON_IO.EFFECTIVE_COLORS;
    BLINK_STAT : out CON_IO.EFFECTIVE_BLINK_STATS;
    BACKGROUND : out CON_IO.EFFECTIVE_BASIC_COLORS;
    SELECTED   : out CON_IO.EFFECTIVE_BASIC_COLORS);

  -- Set field colors
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  --              INVALID_COLOR
  --       - FOREGROUND has to be BASIC_COLORS for list, get and button fields
  --       - SELECTED has to be CURRENT for put and button fields
  --       - BLINK_STAT has to be CURRENT except for put fields
  procedure SET_FIELD_COLORS (
    FIELD_NO   : in ABSOLUTE_FIELD_RANGE;
    FOREGROUND : in CON_IO.COLORS       := CON_IO.CURRENT;
    BLINK_STAT : in CON_IO.BLINK_STATS  := CON_IO.CURRENT;
    BACKGROUND : in CON_IO.BASIC_COLORS := CON_IO.CURRENT;
    SELECTED   : in CON_IO.BASIC_COLORS := CON_IO.CURRENT);

  -- Activate/Desactivate a field for further put_then_gets
  -- All fields are activated by default (when USE_DESCRIPTOR or RESET_FIELD)
  -- A non active field is not displayed by put_then get
  --  (when USE_DESCRIPTOR or RESET_FIELD)
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  procedure SET_FIELD_ACTIVATION (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                                  ACTIVATE : in BOOLEAN);
  procedure GET_FIELD_ACTIVATION (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                                  ACTIVATE : out BOOLEAN);

  -- Protect/Unprotect a GET or BUTTON for further put_then_gets
  -- A protected get field is displayed like a put field (but cannot blink)
  -- A protected button filed is displayed like a put (but no click/release)
  -- A protected list is displayed (but no item can be selected)
  -- All get fields are unprotected by default
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  procedure SET_FIELD_PROTECTION (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                                  PROTECT  : in BOOLEAN);
  procedure GET_FIELD_PROTECTION (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                                  PROTECT  : out BOOLEAN);

  -- Erase all the fields of the descriptor from the screen
  --  (Fill them with current screen's background color)
  -- Exceptions : NO_DESCRIPTOR
  procedure ERASE;

  -- Put a descriptor content
  -- Any list has to be des activated
  -- Exceptions : NO_DESCRIPTOR, LIST_IN_PUT;
  procedure PUT;

  -- Computes next cursor field after current one:
  --  The criteria is the next unprotected and active get field
  --  If FROM is 0, then the first field matching is returned
  --  Else the next matching after FROM is returned
  -- 0 is returned if no matching field is found
  function NEXT_CURSOR_FIELD (FROM : ABSOLUTE_FIELD_RANGE)
  return ABSOLUTE_FIELD_RANGE;

  -- Same with previous field
  function PREV_CURSOR_FIELD (FROM : ABSOLUTE_FIELD_RANGE)
  return ABSOLUTE_FIELD_RANGE;

  -- List of items to put in list field in put_then_get
  subtype LINE_LEN_RANGE is NATURAL range 0 .. CON_IO.COL_RANGE'LAST;
  type LINE_REC is record
    STR : STRING (1 .. LINE_LEN_RANGE'LAST);
    LEN : LINE_LEN_RANGE;
  end record;

  package LINE_LIST_MNG is new DYNAMIC_LIST (LINE_REC);
  LINE_LIST : LINE_LIST_MNG.LIST_TYPE;

  type EVENT_LIST is (KEYBOARD, MOUSE_BUTTON, REFRESH);
  type KEYBOARD_KEY_LIST is (RETURN_KEY, ESCAPE_KEY, BREAK_KEY);

  type RESULT_REC (EVENT : EVENT_LIST := KEYBOARD) is record
    ID_SELECTED : NATURAL;
    case EVENT is
      when KEYBOARD =>
        KEYBOARD_KEY : KEYBOARD_KEY_LIST;
      when MOUSE_BUTTON =>
        FIELD_NO : FIELD_RANGE;
      when REFRESH =>
        null;
    end case;
  end record;

  -- Print the fields and the list (if REDISPLAY), then gets.
  -- REDISPLAY should be set if modif if some other screen actions (con_io)
  --  justify a redisplay
  -- In LIST: mouse click changes current list element (ID_SELECTED)
  --      up/down arrows, page up/down scroll list (affecting ID_TOP)
  -- In PUT fields : nothing
  -- In GET fields : cursor right/left, characters, bakcspace, delete,
  --                  Home, end, insert edit field
  --                 Tab, shift Tab  change field
  --                 Return / Esc to end put_then_get
  --                 mouse click to move at home of field
  -- In MOUSE fields : mouse click then release ends put_then_get
  -- This call affects the content of GET fields, the cursor field and col,
  -- and the current element of the list
  -- If no field is GET (or all protected or desactivated,
  --  then cursor field and col are not significant
  -- Exceptions :  NO_DESCRIPTOR,
  --               INVALID_FIELD, INVALID_COL (for cursor)
  procedure PUT_THEN_GET (CURSOR_FIELD : in out FIELD_RANGE;
                          CURSOR_COL   : in out CON_IO.COL_RANGE;
                          RESULT       : out RESULT_REC;
                          REDISPLAY    : in BOOLEAN := FALSE);

  -- At elaboration
  FILE_NOT_FOUND, FILE_READ_ERROR, FILE_VERSION_ERROR : exception;
  -- On call
  NO_DESCRIPTOR, INVALID_FIELD, INVALID_SQUARE, INVALID_ROW, INVALID_COL,
  STRING_TOO_LONG, INVALID_COLOR, LIST_IN_PUT : exception;

  -- Elaboration:
  -- Read the description file (erasing current content)
  -- Check syntax, semantic, non overlapping of fields
  -- Exceptions : FILE_NOT_FOUND, FILE_READ_ERROR, FILE_SYNTAX_ERROR
  --              FIELD_OVERLAP (if some fields overlap)

end AFPX;
