with TEXT_IO;
with AFPX_TYP;
package body AFPX is

  AFPX_INTERNAL_ERROR : exception;

  package AF_DSCR is

    -- Current descriptor
    CURRENT_DSCR : AFPX_TYP.DSCR_REC;

    -- The fields of current descriptor
    FIELDS : AFPX_TYP.FIELDS_ARRAY;

    -- Characters of the fields
    CHARS : AFPX_TYP.CHAR_STR;

    -- Load a descriptor rais NO_DESCRIPTOR if invalid no
    --  failure of version check
    procedure LOAD_DSCR (DSCR_NO : in AFPX_TYP.DESCRIPTOR_RANGE);

    -- Check if a descriptor has been used (raise NO_DESCRPTOR)
    procedure CHECK;

    -- Check if a descriptor has been used (raise NO_DESCRPTOR)
    --  and if FIELD_NO is valid in it (raise INVALID_FIELD)
    procedure CHECK (FIELD_NO : in AFPX_TYP.ABSOLUTE_FIELD_RANGE);

    -- Load a field's characters and/or colors from init
    procedure LOAD_FIELD (FIELD_NO : in AFPX_TYP.ABSOLUTE_FIELD_RANGE;
        LOAD_COLORS : in BOOLEAN;
        LOAD_CHARS  : in BOOLEAN);

  end AF_DSCR;


  function IN_FIELD (FIELD_NO : in AFPX_TYP.ABSOLUTE_FIELD_RANGE;
                     SQUARE   : in CON_IO.SQUARE) return BOOLEAN is
  begin
   return AFPX_TYP.IN_FIELD (AF_DSCR.FIELDS(FIELD_NO), SQUARE);
  end IN_FIELD;

  function IN_FIELD_ABSOLUTE (FIELD_NO : in AFPX_TYP.ABSOLUTE_FIELD_RANGE;
                              SQUARE   : in CON_IO.SQUARE) return BOOLEAN is
  begin
   return AFPX_TYP.IN_FIELD_ABSOLUTE (AF_DSCR.FIELDS(FIELD_NO), SQUARE);
  end IN_FIELD_ABSOLUTE;

  package body AF_DSCR is separate;


  function NEXT_FIELD (FIELD_NO : AFPX_TYP.ABSOLUTE_FIELD_RANGE)
  return AFPX_TYP.ABSOLUTE_FIELD_RANGE is
    RET_NO : AFPX_TYP.ABSOLUTE_FIELD_RANGE;
    use AFPX_TYP;
  begin
    RET_NO := FIELD_NO;
    loop
      if RET_NO /= AF_DSCR.CURRENT_DSCR.NB_FIELDS then
        RET_NO := RET_NO + 1;
      elsif AF_DSCR.FIELDS(0).KIND = AFPX_TYP.BUTTON then
        RET_NO := 0;
      else
        RET_NO := 1;
      end if;
      exit when AF_DSCR.FIELDS(RET_NO).ACTIVATED;
    end loop;
    return RET_NO;
  end NEXT_FIELD;

  function PREV_FIELD (FIELD_NO : AFPX_TYP.ABSOLUTE_FIELD_RANGE)
  return AFPX_TYP.ABSOLUTE_FIELD_RANGE is
    RET_NO : AFPX_TYP.ABSOLUTE_FIELD_RANGE;
    use AFPX_TYP;
  begin
    RET_NO := FIELD_NO;
    loop
      if RET_NO = 0 then
        RET_NO := AF_DSCR.CURRENT_DSCR.NB_FIELDS;
      elsif RET_NO /= 1 then
        RET_NO := RET_NO - 1;
      elsif AF_DSCR.FIELDS(0).KIND = AFPX_TYP.BUTTON then
        RET_NO := 0;
      else
        RET_NO := AF_DSCR.CURRENT_DSCR.NB_FIELDS;
      end if;
      exit when AF_DSCR.FIELDS(RET_NO).ACTIVATED;
    end loop;
    return RET_NO;
  end PREV_FIELD;

  function NEXT_GET_FIELD (FIELD_NO : AFPX_TYP.FIELD_RANGE)
  return AFPX_TYP.FIELD_RANGE is
    FN : AFPX_TYP.ABSOLUTE_FIELD_RANGE;
    use AFPX_TYP;
  begin
    FN := FIELD_NO;
    loop
      FN := NEXT_FIELD(FN);
      exit when FN /= 0
      and then AF_DSCR.FIELDS(FN).KIND = AFPX_TYP.GET
      and then not AF_DSCR.FIELDS(FN).ISPROTECTED;
    end loop;
    return FN;
  end NEXT_GET_FIELD;

  function PREV_GET_FIELD (FIELD_NO : AFPX_TYP.FIELD_RANGE)
  return AFPX_TYP.FIELD_RANGE is
    FN : AFPX_TYP.ABSOLUTE_FIELD_RANGE;
    use AFPX_TYP;
  begin
    FN := FIELD_NO;
    loop
      FN := PREV_FIELD(FN);
      exit when FN /= 0
      and then AF_DSCR.FIELDS(FN).KIND = AFPX_TYP.GET
      and then not AF_DSCR.FIELDS(FN).ISPROTECTED;
    end loop;
    return FN;
  end PREV_GET_FIELD;

  function NEXT_MOUSE_FIELD (FIELD_NO : AFPX_TYP.FIELD_RANGE)
  return AFPX_TYP.FIELD_RANGE is
    FN : AFPX_TYP.ABSOLUTE_FIELD_RANGE;
    use AFPX_TYP;
  begin
    FN := FIELD_NO;
    loop
      FN := NEXT_FIELD(FN);
      exit when FN /= 0 and then AF_DSCR.FIELDS(FN).KIND = AFPX_TYP.BUTTON
                and then not AF_DSCR.FIELDS(FN).ISPROTECTED;
    end loop;
    return FN;
  end NEXT_MOUSE_FIELD;

  -- All the actions related to screen, keyboard and mouse
  package AF_PTG is

    -- States of a field (or a row in list)
    -- Normal   => Foreground, Background
    -- Clicked  => Background, Foreground
    -- Selected => Foreground, Selected
    type STATE_LIST is (NORMAL, CLICKED, SELECTED);

    -- Sets Foreground and background according to state
    procedure SET_COLORS (FIELD : in AFPX_TYP.FIELD_REC;
                          STATE : in STATE_LIST;
                          FOREGROUND : out CON_IO.EFFECTIVE_COLORS;
                          BACKGROUND : out CON_IO.EFFECTIVE_BASIC_COLORS);

    -- Put a whole field in attribute
    procedure PUT_FIELD (FIELD_NO : in AFPX_TYP.FIELD_RANGE;
                         STATE    : in STATE_LIST);

    -- Put a whole row of a field in attribute
    procedure PUT_ROW (FIELD_NO : in AFPX_TYP.FIELD_RANGE;
                       ROW      : in CON_IO.ROW_RANGE;
                       STATE    : in STATE_LIST);

    -- Put a string somewhere in a field
    procedure PUT_STR (FIELD_NO : in AFPX_TYP.FIELD_RANGE;
                       POS      : in CON_IO.SQUARE;
                       STR      : in STRING;
                       STATE    : in STATE_LIST);

    -- Erase a field (screen_background, screen_background)
    procedure ERASE_FIELD (FIELD_NO : in AFPX_TYP.ABSOLUTE_FIELD_RANGE);

    -- The put_then get
    procedure PTG (CURSOR_FIELD : in out AFPX_TYP.FIELD_RANGE;
                   CURSOR_COL   : in out CON_IO.COL_RANGE;
                   RESULT       : out RESULT_REC;
                   REDISPLAY    : in BOOLEAN;
                   GET_ACTIVE   : in BOOLEAN);

  end AF_PTG;

  package AF_LIST is

    -- Open / Re-open the list window
    procedure OPEN;

    -- Display the list, starting from FIRST_ITEM
    -- Has to be called each time the list changes
    --  or colors are modified
    procedure DISPLAY (FIRST_ITEM_ID : in POSITIVE);

    -- Actions on the list
    type ACTION_LIST is (UP, DOWN, PAGE_UP, PAGE_DOWN);

    -- Update the list due to an action
    procedure UPDATE (ACTION : in ACTION_LIST);

    -- Set the current item (selected_color) of the list
    procedure SET_SELECTED (ITEM_ID : in POSITIVE);

    -- The current status of the list
    type STATUS_REC is record
      -- The number of items diplayed
      -- (width if list_length >= width), list_length otherwise
      NB_ROWS : NATURAL;
      -- First and last items displayed in the window
      ID_TOP    : NATURAL;
      ID_BOTTOM : NATURAL;
      -- Item selected
      ID_SELECTED : NATURAL;
    end record;

    function GET_STATUS return STATUS_REC;

    -- Set current item of list according to ID_SELECTED
    procedure SET_CURRENT;

    -- Put a row in a state
    procedure PUT (ROW : in CON_IO.ROW_RANGE; STATE : in AF_PTG.STATE_LIST);

    -- Is an ID, a row displayed
    function ID_DISPLAYED (ID : POSITIVE) return BOOLEAN;
    function ROW_DISPLAYED (ROW : CON_IO.ROW_RANGE) return BOOLEAN;

    -- ROW <-> Item ID
    function TO_ROW (ID : POSITIVE) return CON_IO.ROW_RANGE;
    function TO_ID  (ROW : CON_IO.ROW_RANGE) return POSITIVE;

    NOT_OPENED, NOT_DISPLAYED : exception;
  end AF_LIST;

  package body AF_PTG is separate;
  package body AF_LIST is separate;



  -- Set current descriptor (read descriptor description)
  procedure USE_DESCRIPTOR (DESCRIPTOR_NO : in DESCRIPTOR_RANGE) is
  begin
    CON_IO.INIT;
    AF_DSCR.LOAD_DSCR (AFPX_TYP.DESCRIPTOR_RANGE (DESCRIPTOR_NO));
    AF_LIST.OPEN;
    AF_DSCR.CURRENT_DSCR.MODIFIED := TRUE;
    CON_IO.CLEAR (CON_IO.SCREEN);
  end USE_DESCRIPTOR;

  -- Clear the content of a field
  procedure CLEAR_FIELD (FIELD_NO : in FIELD_RANGE) is
    FN : constant AFPX_TYP.FIELD_RANGE := AFPX_TYP.FIELD_RANGE(FIELD_NO);
    FIELD : AFPX_TYP.FIELD_REC;
    FIELD_SIZE : POSITIVE;
  begin
    AF_DSCR.CHECK(FN);
    FIELD := AF_DSCR.FIELDS(FN);
    FIELD_SIZE := FIELD.HEIGHT * FIELD.WIDTH;
    -- Copy the nb_chars from init_str to char_str
    for I in FIELD.CHAR_INDEX .. FIELD.CHAR_INDEX + FIELD_SIZE - 1 loop
      AF_DSCR.CHARS(I) := ' ';
    end loop;
    AF_DSCR.CURRENT_DSCR.MODIFIED := TRUE;
  end CLEAR_FIELD;

  -- Reset the field from initial definition in file
  procedure RESET_FIELD (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                         RESET_COLORS : in BOOLEAN := TRUE;
                         RESET_STRING : in BOOLEAN := TRUE) is
    FN : constant AFPX_TYP.FIELD_RANGE := AFPX_TYP.FIELD_RANGE(FIELD_NO);
  begin
    AF_DSCR.CHECK(FN);
    AF_DSCR.LOAD_FIELD (FN, RESET_COLORS, RESET_STRING);
    AF_DSCR.CURRENT_DSCR.MODIFIED := TRUE;
  end RESET_FIELD;

  -- Field size
  procedure GET_FIELD_SIZE (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                            HEIGHT : out HEIGHT_RANGE;
                            WIDTH  : out WIDTH_RANGE) is
    FN : constant AFPX_TYP.ABSOLUTE_FIELD_RANGE
       := AFPX_TYP.ABSOLUTE_FIELD_RANGE(FIELD_NO);
  begin
    AF_DSCR.CHECK(FN);
    HEIGHT := AF_DSCR.FIELDS(FN).HEIGHT;
    WIDTH  := AF_DSCR.FIELDS(FN).WIDTH;
  end GET_FIELD_SIZE;

  -- Encode a string in a row of a field
  procedure ENCODE_FIELD (FIELD_NO : in FIELD_RANGE;
                          FROM_POS : in CON_IO.SQUARE;
                          STR      : in STRING) is
    FN : constant AFPX_TYP.FIELD_RANGE := AFPX_TYP.FIELD_RANGE(FIELD_NO);
    FIELD : AFPX_TYP.FIELD_REC;
    INIT_INDEX : AFPX_TYP.CHAR_STR_RANGE;
  begin
    AF_DSCR.CHECK(FN);
    FIELD := AF_DSCR.FIELDS(FN);
    -- Check that square is in field
    if not AFPX_TYP.IN_FIELD (FIELD, FROM_POS) then
      raise INVALID_SQUARE;
    end if;

    -- Check that FROM_POS.COL + STR is length compatible with field width
    if not AFPX_TYP.IN_FIELD (FIELD,
           (FROM_POS.ROW, FROM_POS.COL + STR'LENGTH - 1)) then
	  raise STRING_TOO_LONG;
    end if;
    -- Copy in init string
    INIT_INDEX := FIELD.CHAR_INDEX
           + FROM_POS.ROW * FIELD.WIDTH
           + FROM_POS.COL;
    AF_DSCR.CHARS (INIT_INDEX .. INIT_INDEX + STR'LENGTH - 1) := STR;
    AF_DSCR.CURRENT_DSCR.MODIFIED := TRUE;
  end ENCODE_FIELD;

  procedure ENCODE_FIELD (FIELD_NO : in FIELD_RANGE;
                          FROM_POS : in CON_IO.SQUARE;
                          STR      : in STR_TXT) is
  begin
    ENCODE_FIELD (FIELD_NO, FROM_POS, TEXT_HANDLER.VALUE (STR));
  end ENCODE_FIELD;



  -- Decode the content of a row of a field
  procedure DECODE_FIELD (FIELD_NO : in FIELD_RANGE;
                          ROW      : in CON_IO.ROW_RANGE;
                          STR      : in out STR_TXT) is
    FN : constant AFPX_TYP.FIELD_RANGE := AFPX_TYP.FIELD_RANGE(FIELD_NO);
    FIELD : AFPX_TYP.FIELD_REC;
    INIT_INDEX : AFPX_TYP.CHAR_STR_RANGE;
  begin
    AF_DSCR.CHECK(FN);
    FIELD := AF_DSCR.FIELDS(FN);
    -- Check that row is in field
    if not AFPX_TYP.IN_FIELD (FIELD, (ROW, 0)) then
      raise INVALID_ROW;
    end if;
    -- Copy in init string
    INIT_INDEX := FIELD.CHAR_INDEX + ROW * FIELD.WIDTH;
	-- Return FIELD.WIDTH characters
	TEXT_HANDLER.SET (STR,
      AF_DSCR.CHARS (INIT_INDEX .. INIT_INDEX + FIELD.WIDTH - 1));
  end DECODE_FIELD;

  -- Decode the content of a row of a field
  function DECODE_FIELD (FIELD_NO : FIELD_RANGE; ROW : CON_IO.ROW_RANGE)
                  return STRING is
    STR : STR_TXT;
  begin
    DECODE_FIELD (FIELD_NO, ROW, STR);
    return TEXT_HANDLER.VALUE (STR);
  end DECODE_FIELD;


  -- Get field colors
  procedure GET_FIELD_COLORS (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                 FOREGROUND : out CON_IO.EFFECTIVE_COLORS;
                 BLINK_STAT : out CON_IO.EFFECTIVE_BLINK_STATS;
                 BACKGROUND : out CON_IO.EFFECTIVE_BASIC_COLORS;
                 SELECTED   : out CON_IO.EFFECTIVE_BASIC_COLORS) is
    FN : constant AFPX_TYP.ABSOLUTE_FIELD_RANGE
       := AFPX_TYP.ABSOLUTE_FIELD_RANGE(FIELD_NO);
    FIELD : AFPX_TYP.FIELD_REC;
  begin
    AF_DSCR.CHECK(FN);
    FIELD := AF_DSCR.FIELDS(FN);
    FOREGROUND := FIELD.COLORS.FOREGROUND;
    BLINK_STAT := FIELD.COLORS.BLINK_STAT;
    BACKGROUND := FIELD.COLORS.BACKGROUND;
    SELECTED   := FIELD.COLORS.SELECTED;
 end GET_FIELD_COLORS;

  -- Set field colors
  procedure SET_FIELD_COLORS (FIELD_NO   : in ABSOLUTE_FIELD_RANGE;
                  FOREGROUND : in CON_IO.COLORS       := CON_IO.CURRENT;
                  BLINK_STAT : in CON_IO.BLINK_STATS  := CON_IO.CURRENT;
                  BACKGROUND : in CON_IO.BASIC_COLORS := CON_IO.CURRENT;
                  SELECTED   : in CON_IO.BASIC_COLORS := CON_IO.CURRENT) is
    FN : constant AFPX_TYP.ABSOLUTE_FIELD_RANGE
       := AFPX_TYP.ABSOLUTE_FIELD_RANGE(FIELD_NO);
    FIELD : AFPX_TYP.FIELD_REC;
    use CON_IO;
    use AFPX_TYP;
  begin
    AF_DSCR.CHECK(FN);
    FIELD := AF_DSCR.FIELDS(FN);
    -- Check FOREGROUND is BASIC_COLORS for list, get and button fields
    if FOREGROUND not in CON_IO.BASIC_COLORS
    and then FIELD.KIND /= AFPX_TYP.PUT then
      raise INVALID_COLOR;
    end if;
    -- Check BLINK_STAT is CURRENT except for PUT
    if BLINK_STAT /= CON_IO.CURRENT
    and then (FIELD.KIND /= AFPX_TYP.PUT) then
      raise INVALID_COLOR;
    end if;

    -- Check SELECTED is CURRENT for put and button fields
    if SELECTED /= CON_IO.CURRENT
    and then (        FIELD.KIND = AFPX_TYP.PUT
              or else FIELD.KIND = AFPX_TYP.BUTTON) then
      raise INVALID_COLOR;
    end if;

    -- Copy colors if not current
    if FOREGROUND /= CON_IO.CURRENT then
      FIELD.COLORS.FOREGROUND := FOREGROUND;
    end if;
    if BLINK_STAT /= CON_IO.CURRENT then
      FIELD.COLORS.BLINK_STAT := BLINK_STAT;
    end if;
    if BACKGROUND /= CON_IO.CURRENT then
      FIELD.COLORS.BACKGROUND := BACKGROUND;
    end if;
    if SELECTED /= CON_IO.CURRENT then
      FIELD.COLORS.SELECTED := SELECTED;
    end if;
    -- Affect if fields_array
    AF_DSCR.FIELDS(FN) := FIELD;
    AF_DSCR.CURRENT_DSCR.MODIFIED := TRUE;
  end SET_FIELD_COLORS;

  -- Activate/Desactivate a field for further put_then_get
  procedure SET_FIELD_ACTIVATION (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                                  ACTIVATE : in BOOLEAN) is
    FN : constant AFPX_TYP.ABSOLUTE_FIELD_RANGE
       := AFPX_TYP.ABSOLUTE_FIELD_RANGE(FIELD_NO);
  begin
    AF_DSCR.CHECK(FN);
    AF_DSCR.FIELDS(FN).ACTIVATED := ACTIVATE;
    AF_DSCR.CURRENT_DSCR.MODIFIED := TRUE;
  end SET_FIELD_ACTIVATION;

  procedure GET_FIELD_ACTIVATION (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                                  ACTIVATE : out BOOLEAN) is
    FN : constant AFPX_TYP.ABSOLUTE_FIELD_RANGE
       := AFPX_TYP.ABSOLUTE_FIELD_RANGE(FIELD_NO);
  begin
    AF_DSCR.CHECK(FN);
    ACTIVATE := AF_DSCR.FIELDS(FN).ACTIVATED;
  end GET_FIELD_ACTIVATION;

  -- Protect/Unprotect a GET for further put_then_gets
  -- A non active field is not displayed by put_then get
  -- All fields are activated by default (when USE_DESCRIPTOR or RESET_FIELD)
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  procedure SET_FIELD_PROTECTION (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                                  PROTECT  : in BOOLEAN) is
    FN : constant AFPX_TYP.ABSOLUTE_FIELD_RANGE
       := AFPX_TYP.ABSOLUTE_FIELD_RANGE(FIELD_NO);
    use AFPX_TYP;
  begin
    AF_DSCR.CHECK(FN);
    if AF_DSCR.FIELDS(FN).KIND = AFPX_TYP.PUT then
      raise INVALID_FIELD;
    end if;
    AF_DSCR.FIELDS(FN).ISPROTECTED := PROTECT;
    AF_DSCR.CURRENT_DSCR.MODIFIED := TRUE;
  end SET_FIELD_PROTECTION;

  procedure GET_FIELD_PROTECTION (FIELD_NO : in ABSOLUTE_FIELD_RANGE;
                                  PROTECT  : out BOOLEAN) is
    FN : constant AFPX_TYP.ABSOLUTE_FIELD_RANGE
       := AFPX_TYP.ABSOLUTE_FIELD_RANGE(FIELD_NO);
    use AFPX_TYP;
  begin
    AF_DSCR.CHECK(FN);
    if AF_DSCR.FIELDS(FN).KIND = AFPX_TYP.PUT then
      raise INVALID_FIELD;
    end if;
    PROTECT := AF_DSCR.FIELDS(FN).ISPROTECTED;
  end GET_FIELD_PROTECTION;

  -- Erase all the fields of the descriptor from the screen
  procedure ERASE is
    BACKGROUND : constant CON_IO.EFFECTIVE_BASIC_COLORS
               := CON_IO.GET_BACKGROUND (CON_IO.SCREEN);
    use AFPX_TYP;
  begin
    AF_DSCR.CHECK;
    if AF_DSCR.FIELDS(0).KIND = AFPX_TYP.BUTTON then
      AF_PTG.ERASE_FIELD (0);
    end if;
    for I in 1 .. AF_DSCR.CURRENT_DSCR.NB_FIELDS loop
      AF_PTG.ERASE_FIELD (I);
    end loop;
    CON_IO.FLUSH;
  end ERASE;

  -- Put all the fields of the descriptor on the screen
  procedure PUT is
    use AFPX_TYP;
  begin
    AF_DSCR.CHECK;
    -- Check no list active in descriptor
    if AF_DSCR.FIELDS(0).KIND = AFPX_TYP.BUTTON then
      if AF_DSCR.FIELDS (0).ACTIVATED then
        raise LIST_IN_PUT;
      else
        AF_PTG.ERASE_FIELD (0);
      end if;
    end if;

    -- Put all fields
    for I in 1 .. AF_DSCR.CURRENT_DSCR.NB_FIELDS loop
      if AF_DSCR.FIELDS (I).ACTIVATED then
        AF_PTG.PUT_FIELD (I, AF_PTG.NORMAL);
      else
        AF_PTG.ERASE_FIELD (I);
      end if;
    end loop;
    CON_IO.FLUSH;
  end PUT;

  -- Computes next cursor field after current one:
  function NEXT_CURSOR_FIELD (FROM : ABSOLUTE_FIELD_RANGE)
                             return ABSOLUTE_FIELD_RANGE is
    RET_NO : AFPX_TYP.ABSOLUTE_FIELD_RANGE;
    use AFPX_TYP;
  begin
    RET_NO := AFPX_TYP.ABSOLUTE_FIELD_RANGE(FROM);
    loop
      if RET_NO /= AF_DSCR.CURRENT_DSCR.NB_FIELDS then
        RET_NO := RET_NO + 1;
      else
        RET_NO := 1;
      end if;
      if AF_DSCR.FIELDS(RET_NO).KIND = AFPX_TYP.GET
      and then AF_DSCR.FIELDS(RET_NO).ACTIVATED
      and then not AF_DSCR.FIELDS(RET_NO).ISPROTECTED then
        return ABSOLUTE_FIELD_RANGE(RET_NO);
      elsif RET_NO = AFPX_TYP.ABSOLUTE_FIELD_RANGE(FROM) then
        return 0;
      elsif FROM = 0 and then RET_NO = AF_DSCR.CURRENT_DSCR.NB_FIELDS then
        return 0;
      end if;
    end loop;
  end NEXT_CURSOR_FIELD;

  -- Computes previous cursor field before current one:
  function PREV_CURSOR_FIELD (FROM : ABSOLUTE_FIELD_RANGE)
                             return ABSOLUTE_FIELD_RANGE is
    RET_NO : AFPX_TYP.ABSOLUTE_FIELD_RANGE;
    use AFPX_TYP;
  begin
    RET_NO := AFPX_TYP.ABSOLUTE_FIELD_RANGE(FROM);
    loop
      if RET_NO /= 1 then
        RET_NO := RET_NO - 1;
      else
        RET_NO := AF_DSCR.CURRENT_DSCR.NB_FIELDS;
      end if;
      if AF_DSCR.FIELDS(RET_NO).KIND = AFPX_TYP.GET
      and then AF_DSCR.FIELDS(RET_NO).ACTIVATED
      and then not AF_DSCR.FIELDS(RET_NO).ISPROTECTED then
        return ABSOLUTE_FIELD_RANGE(RET_NO);
      elsif RET_NO = AFPX_TYP.ABSOLUTE_FIELD_RANGE(FROM) then
        return 0;
      elsif FROM = 0 and then RET_NO = 1 then
        return 0;
      end if;
    end loop;
  end PREV_CURSOR_FIELD;


  -- Print the fields and the list, then gets
  procedure PUT_THEN_GET (
                         CURSOR_FIELD : in out FIELD_RANGE;
                         CURSOR_COL   : in out CON_IO.COL_RANGE;
                         RESULT       : out RESULT_REC;
                         REDISPLAY    : in BOOLEAN := FALSE) is
    SOME_GET : BOOLEAN;
    CF : AFPX_TYP.FIELD_RANGE := AFPX_TYP.FIELD_RANGE(CURSOR_FIELD);
    use AFPX_TYP;
  begin
    AF_DSCR.CHECK;
    -- Check if some active get field in the descriptor
    SOME_GET := FALSE;
    for I in 1 .. AF_DSCR.CURRENT_DSCR.NB_FIELDS loop
      if AF_DSCR.FIELDS(I).KIND = AFPX_TYP.GET
      and then AF_DSCR.FIELDS (I).ACTIVATED
      and then not AF_DSCR.FIELDS (I).ISPROTECTED then
        SOME_GET := TRUE;
        exit;
      end if;
    end loop;
    -- Check cursor pos if some get field active
    if SOME_GET then
      AF_DSCR.CHECK (CF);
      if           AF_DSCR.FIELDS(CF).KIND /= AFPX_TYP.GET
      or else  not AF_DSCR.FIELDS(CF).ACTIVATED
      or else      AF_DSCR.FIELDS(CF).ISPROTECTED then
        raise INVALID_FIELD;
      end if;
      if CURSOR_COL >= AF_DSCR.FIELDS(CF).WIDTH then
        raise INVALID_COL;
      end if;
    end if;

    AF_PTG.PTG (CF, CURSOR_COL, RESULT, REDISPLAY, SOME_GET);
    CURSOR_FIELD := FIELD_RANGE(CF);
  end PUT_THEN_GET;

end AFPX;
