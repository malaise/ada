with CALENDAR, SYSTEM;
package body X_MNG is

  -- Result of a call to C
  subtype RESULT is INTEGER;
  OK : constant RESULT := 0;

  -- Line access for X
  subtype LINE_FOR_C  is SYSTEM.ADDRESS;
  NO_LINE_FOR_C : constant LINE_FOR_C := SYSTEM.NULL_ADDRESS;

  -- True if the connection to X has been initialised
  INITIALISED : BOOLEAN := FALSE; 
 
  -- Boolean on 32 bits for C
  type    BOOL_FOR_C is (FALSE, TRUE);
  for BOOL_FOR_C'SIZE use 32;
  for BOOL_FOR_C use (FALSE => 0, TRUE => 1);

  function FOR_C(ADA_BOOLEAN : in BOOLEAN) return BOOL_FOR_C is
  begin
    return BOOL_FOR_C'VAL(BOOLEAN'POS(ADA_BOOLEAN));
  end FOR_C;

  function FOR_ADA(C_BOOLEAN : in BOOL_FOR_C) return BOOLEAN is
  begin
    return BOOLEAN'VAL(BOOL_FOR_C'POS(C_BOOLEAN));
  end FOR_ADA;


  ------------------------------------------------------------------
  -------------------- T H E   I N T E R F A C E -------------------
  ------------------------------------------------------------------
  -- Initialise X for one host
  -- int x_initialise (char *server_name);
  ------------------------------------------------------------------
  function X_INITIALISE (SERVER_NAME : SYSTEM.ADDRESS) return RESULT;
  pragma IMPORT(C, X_INITIALISE, "x_initialise");

  ------------------------------------------------------------------
  -- Opens a line
  -- int x_open_line (int screen_id, int row, int column,
  --                  int height, int width,
  --                  int background, int border, int no_font,
  --                  void **p_line_id);
  ------------------------------------------------------------------
  function X_OPEN_LINE (SCREEN_ID          : INTEGER;
                        ROW, COLUMN        : INTEGER;
                        HEIGHT, WIDTH      : INTEGER;
                        BACKGROUND, BORDER : INTEGER;
                        NO_FONT            : INTEGER;
                        P_LINE_ID          : SYSTEM.ADDRESS) return RESULT;
  pragma IMPORT(C, X_OPEN_LINE, "x_open_line");
 
  ------------------------------------------------------------------
  -- Closes a line
  -- int x_close_line (void *line_id);
  ------------------------------------------------------------------
  function X_CLOSE_LINE(LINE_ID : LINE_FOR_C) return RESULT;
  pragma IMPORT(C, X_CLOSE_LINE, "x_close_line");

  ------------------------------------------------------------------
  -- Set the name of a line
  -- int x_set_line_name (void *line_id, char *line_name);
  ------------------------------------------------------------------
  function X_SET_LINE_NAME (LINE_ID   : LINE_FOR_C;
                            LINE_NAME : SYSTEM.ADDRESS) return RESULT;
  pragma IMPORT(C, X_SET_LINE_NAME, "x_set_line_name");

  ------------------------------------------------------------------
  -- Flushes all the lines of the host (really display them)
  -- int x_flush (void)
  ------------------------------------------------------------------
  function X_FLUSH return RESULT;
  pragma IMPORT(C, X_FLUSH, "x_flush");

  ------------------------------------------------------------------
  -- Clears a line
  -- int x_clear_line (void *line_id);
  ------------------------------------------------------------------
  function X_CLEAR_LINE(LINE_ID : LINE_FOR_C) return RESULT;
  pragma IMPORT(C, X_CLEAR_LINE, "x_clear_line");

  ------------------------------------------------------------------
  -- Sets the attributes for a further put in the same window
  -- int x_set_attributes (void *line_id;
  --                       int paper, int ink,
  --                       boolean superbright, boolean underline,
  --                       boolean blink, boolean reverse);
  ------------------------------------------------------------------
  function X_SET_ATTRIBUTES(LINE_ID     : LINE_FOR_C;
                            PAPER, INK  : INTEGER;
                            SUPERBRIGHT : BOOL_FOR_C;
                            UNDERLINE   : BOOL_FOR_C;
                            BLINK       : BOOL_FOR_C;
                            INVERSE     : BOOL_FOR_C) return RESULT;
  pragma IMPORT(C, X_SET_ATTRIBUTES, "x_set_attributes");
 
  ------------------------------------------------------------------
  -- Set XOR mode for further outputs
  -- int x_set_xor_mode (void *line_id, boolean xor_mode);
  ------------------------------------------------------------------
  function X_SET_XOR_MODE(LINE_ID : LINE_FOR_C;
                          XOR_MODE  : BOOL_FOR_C) return RESULT;
  pragma IMPORT(C, X_SET_XOR_MODE, "x_set_xor_mode");

  ------------------------------------------------------------------
  -- Writes a char whith the attributes previously set
  -- int x_put_char (void *line_id, int  car, int row, int column);
  ------------------------------------------------------------------
  function X_PUT_CHAR(LINE_ID     : LINE_FOR_C;
                      CAR         : INTEGER;
                      ROW, COLUMN : in INTEGER) return RESULT;
  pragma IMPORT(C, X_PUT_CHAR, "x_put_char");

  ------------------------------------------------------------------
  -- Writes a char whith the attributes previously set
  -- Does not erase character at current position
  -- int x_overwrite_char (void *line_id, int  car,
  --                       int row, int column);
  ------------------------------------------------------------------
  function X_OVERWRITE_CHAR(LINE_ID     : LINE_FOR_C;
                            CAR         : INTEGER;
                            ROW, COLUMN : in INTEGER) return RESULT;
  pragma IMPORT(C, X_OVERWRITE_CHAR, "x_overwrite_char");

  ------------------------------------------------------------------
  -- Writes a string at location with the attributes previously set
  -- int x_put_stringt (void *line_id, char *p_char, int number,
  --                    int row, int column);
  ------------------------------------------------------------------
  function X_PUT_STRING(LINE_ID     : LINE_FOR_C;
                        STR_ADDR    : SYSTEM.ADDRESS;
                        LENGTH      : INTEGER;
                        ROW, COLUMN : INTEGER) return RESULT;
  pragma IMPORT(C, X_PUT_STRING, "x_put_string");

  ------------------------------------------------------------------
  -- Writes a char on a line with specified characteristics
  -- int x_put_char_attributes (void *line_id; int car,
  --                            int row, int column, int paper, int ink,
  --                            boolean superbright, boolean underline,
  --                            boolean blink, boolean reverse);
  ------------------------------------------------------------------
  function X_PUT_CHAR_ATTRIBUTES(LINE_ID     : LINE_FOR_C;
                                 CAR         : INTEGER;
                                 ROW, COLUMN : INTEGER;
                                 PAPER, INK  : INTEGER;
                                 SUPERBRIGHT : BOOL_FOR_C;
                                 UNDERLINE   : BOOL_FOR_C;
                                 BLINK       : BOOL_FOR_C;
                                 INVERSE     : BOOL_FOR_C) 
   return RESULT;
  pragma IMPORT(C, X_PUT_CHAR_ATTRIBUTES, "x_put_char_attributes");

  ------------------------------------------------------------------
  -- Draws a rectangle (width * height) from position
  --  with current background color.
  --  New position is updated to lower-left square of rectangle.
  -- int x_draw_area (void *line_id, int width, int height);
  ------------------------------------------------------------------
  function X_DRAW_AREA(LINE_ID       : LINE_FOR_C;
                       WIDTH, HEIGHT : INTEGER;
                       ROW, COLUMN   : INTEGER) return RESULT;
  pragma IMPORT(C, X_DRAW_AREA, "x_draw_area");
 
  ------------------------------------------------------------------
  -- Puts a char with current characteristics
  --  at specified position in pixels
  -- int x_put_char_pixels (void *line_id, int car, int x, int y);
  ------------------------------------------------------------------
  function X_PUT_CHAR_PIXELS(LINE_ID : LINE_FOR_C;
                             CAR     : INTEGER;
                             X, Y    : INTEGER) return RESULT;
  pragma IMPORT(C, X_PUT_CHAR_PIXELS, "x_put_char_pixels");

  ------------------------------------------------------------------
  -- Gets the graphic characteristics of a line when it was created
  -- int x_get_graph_charact (void *line_id, int *p_w_width, int *p_w_height,
  --                  int *p_f_width, int *p_f_height, int *p_f_offset);
  ------------------------------------------------------------------
  function X_GET_GRAPHIC_CHARACTERISTICS(LINE_ID : LINE_FOR_C;
                                         WINDOW_WIDTH  : SYSTEM.ADDRESS;
                                         WINDOW_HEIGHT : SYSTEM.ADDRESS;
                                         FONT_WIDTH    : SYSTEM.ADDRESS;
                                         FONT_HEIGHT   : SYSTEM.ADDRESS;
                                         FONT_OFFSET   : SYSTEM.ADDRESS)
           return RESULT;
  pragma IMPORT(C, X_GET_GRAPHIC_CHARACTERISTICS, "x_get_graph_charact");

  ------------------------------------------------------------------
  -- Draw a point with current characteristics
  -- int x_draw_point (void *line_id, int x, int y);
  ------------------------------------------------------------------
  function X_DRAW_POINT(LINE_ID : LINE_FOR_C;
                        X, Y    : INTEGER) return RESULT;
  pragma IMPORT(C, X_DRAW_POINT, "x_draw_point");

  ------------------------------------------------------------------
  -- Draw a line with current characteristics
  -- int x_draw_line (void *line_id, int x1, int y1, int x2, int y2);
  ------------------------------------------------------------------
  function X_DRAW_LINE(LINE_ID : LINE_FOR_C;
                       X1, Y1, X2, Y2 : NATURAL) return RESULT;
  pragma IMPORT(C, X_DRAW_LINE, "x_draw_line");

  ------------------------------------------------------------------
  -- Draw a rectangle with current characteristics
  -- int x_draw_rectangle (void *line_id, int x1, int y1, int x2, int y2);
  ------------------------------------------------------------------
  function X_DRAW_RECTANGLE(LINE_ID : LINE_FOR_C;
                            X1, Y1, X2, Y2 : NATURAL) return RESULT;
  pragma IMPORT(C, X_DRAW_RECTANGLE, "x_draw_rectangle");

  ------------------------------------------------------------------
  -- Get current position in pixels, independently from events
  -- int x_get_pointer_pos (void *line_id, int *p_x, int *p_y);
  ------------------------------------------------------------------
  function X_GET_CURRENT_POINTER_POSITION(LINE_ID : LINE_FOR_C;
                                          X, Y : SYSTEM.ADDRESS)
           return RESULT;
  pragma IMPORT(C, X_GET_CURRENT_POINTER_POSITION, "x_get_pointer_pos");

  ------------------------------------------------------------------
  -- Set mouse pointer in graphic (cross) or standard (arrow)
  -- int x_set_graphic_pointer (void *line_id, boolean graphic);
  ------------------------------------------------------------------
  function X_SET_GRAPHIC_POINTER(LINE_ID : LINE_FOR_C;
                                 GRAPHIC : BOOL_FOR_C) return RESULT;
  pragma IMPORT(C, X_SET_GRAPHIC_POINTER, "x_set_graphic_pointer");

  ------------------------------------------------------------------
  -- Wait for some events
  -- int x_select (fd_set *p_mask, boolean *p_x_event, int *timeout_ms);
  ------------------------------------------------------------------
  function X_SELECT (P_MASK : SYSTEM.ADDRESS;
                     P_X_EVENT : SYSTEM.ADDRESS;
                     TIMEOUT_MS : SYSTEM.ADDRESS) return RESULT;
  pragma IMPORT(C, X_SELECT, "x_select");

  ------------------------------------------------------------------
  -- Process a X event (TID or Keyboard or other) 
  -- int x_process_event (void **p_line_id, int *p_kind, boolean *p_next);
  ------------------------------------------------------------------
  function X_PROCESS_EVENT(P_LINE_ID : SYSTEM.ADDRESS;
                           P_KEYB    : SYSTEM.ADDRESS;
                           P_NEXT    : SYSTEM.ADDRESS) return RESULT;
  pragma IMPORT(C, X_PROCESS_EVENT, "x_process_event");
 
  ------------------------------------------------------------------
  -- Reads the position on TID
  -- int x_read_tid (void *line_id, boolean row_col,
  --                 int *p_button, int *p_row, int *p_column);
  ------------------------------------------------------------------
  function X_READ_TID(LINE_ID         : LINE_FOR_C;
                      ROW_COL         : BOOL_FOR_C;
                      P_BUTTON        : SYSTEM.ADDRESS;
                      P_ROW, P_COLUMN : SYSTEM.ADDRESS) return RESULT;
  pragma IMPORT(C, X_READ_TID, "x_read_tid");

  ------------------------------------------------------------------
  -- Reads a key of a sequence
  -- int x_read_key (void *line_id, int *p_key, int *p_nbre);
  ------------------------------------------------------------------
  function X_READ_KEY(LINE_ID : LINE_FOR_C;
                      P_KEYS  : SYSTEM.ADDRESS;
                      P_NBRE  : SYSTEM.ADDRESS) return RESULT;
  pragma IMPORT(C, X_READ_KEY, "x_read_key");

  ------------------------------------------------------------------
  -- Enable / disable cursor motion events
  -- extern int x_enable_motion_events (void *line_id, boolean enable_motion);
  ------------------------------------------------------------------
  function X_ENABLE_MOTION_EVENTS (LINE_ID : LINE_FOR_C;
                                   MOTION_ENABLE : BOOL_FOR_C) return RESULT;
  pragma IMPORT(C, X_ENABLE_MOTION_EVENTS, "x_enable_motion_events");
 
  ------------------------------------------------------------------
  -- Assumes blinking of X
  -- int x_blink(void)
  ------------------------------------------------------------------
  function X_BLINK return RESULT;
  pragma IMPORT(C, X_BLINK, "x_blink");

  ------------------------------------------------------------------
  -- Stops the blinking task
  -- int x_stop_blinking(void) 
  ------------------------------------------------------------------
  function X_STOP_BLINKING return RESULT;
  pragma IMPORT(C, X_STOP_BLINKING, "x_stop_blinking");

  ------------------------------------------------------------------
  -- Start the blinking task
  -- int x_start_blinking(void) 
  ------------------------------------------------------------------
  function X_START_BLINKING return RESULT;
  pragma IMPORT(C, X_START_BLINKING, "x_start_blinking");

  ------------------------------------------------------------------
  -- Rings a bell several times
  -- int x_bell (int nbre_bell;
  ------------------------------------------------------------------
  function X_BELL (REPEAT : INTEGER) return RESULT;
  pragma IMPORT(C, X_BELL, "x_bell");


  ------------------------------------------------------------------
  --------------- T H E   D I S P A T C H E R   S P E C ------------
  ------------------------------------------------------------------
  -- Dispatcher of X calls and X events
  task DISPATCHER is
    -- Start it
    entry START;

    -- Register / Unregister
    entry REGISTER (CLIENT : out LINE_RANGE);
    entry UNREGISTER (CLIENT : in out LINE_RANGE);

    -- Two calls to encapsulate a call to X
    entry CALL_ON  (CLIENT : in CLIENT_RANGE;
                    LINE_FOR_C_ID : out LINE_FOR_C);
    entry CALL_OFF (CLIENT : in CLIENT_RANGE;
                    NEW_LINE_FOR_C_ID : in LINE_FOR_C);

    -- Ready to wait
    entry WAIT (CLIENT : in CLIENT_RANGE; TIMEOUT : in DURATION);
    -- Relay wait
    entry SOME_EVENT(CLIENT_RANGE) (SOME : out BOOLEAN);
    entry GET_EVENT (CLIENT : in CLIENT_RANGE; KIND : out EVENT_KIND;
                                               SOME : out BOOLEAN);
  end DISPATCHER;



  ------------------------------------------------------------------
  ------------------------ T H E   C A L L S -----------------------
  ------------------------------------------------------------------

  procedure X_INITIALISE (SERVER_NAME : in STRING) is

    SERV_NAME_FOR_C : constant STRING(1 .. SERVER_NAME'LENGTH+1)
                    := SERVER_NAME & ASCII.NUL;
  begin
    if not INITIALISED then
      if X_INITIALISE (SERV_NAME_FOR_C(SERV_NAME_FOR_C'FIRST)'ADDRESS)
                      /= OK then
        raise X_FAILURE;
      end if;
      DISPATCHER.START;
      INITIALISED := TRUE;
    end if;
  end X_INITIALISE;

  ------------------------------------------------------------------
  procedure X_OPEN_LINE(LINE_DEFINITION : in LINE_DEFINITION_REC;
                        LINE_ID         : in out LINE) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID /= NO_CLIENT then
      raise X_FAILURE;
    end if;
    -- Register
    DISPATCHER.REGISTER(LINE_ID.NO);
    if LINE_ID = NO_CLIENT then
      -- Too many clients
      raise X_FAILURE;
    end if;
    -- open window
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_OPEN_LINE (LINE_DEFINITION.SCREEN_ID,
                        LINE_DEFINITION.ROW,
                        LINE_DEFINITION.COLUMN, 
                        LINE_DEFINITION.HEIGHT,
                        LINE_DEFINITION.WIDTH, 
                        LINE_DEFINITION.BACKGROUND,
                        LINE_DEFINITION.BORDER, 
                        LINE_DEFINITION.NO_FONT,
                        LINE_FOR_C_ID'ADDRESS) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      DISPATCHER.UNREGISTER(LINE_ID.NO);
      raise X_FAILURE;
    end if;
  end X_OPEN_LINE;

  ------------------------------------------------------------------
  procedure X_CLOSE_LINE(LINE_ID : in out LINE) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_CLOSE_LINE(LINE_FOR_C_ID) = OK;
    RES := RES and then X_FLUSH = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    -- Unregister
    DISPATCHER.UNREGISTER(LINE_ID.NO);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_CLOSE_LINE;

  ------------------------------------------------------------------
  procedure X_SET_LINE_NAME (LINE_ID : in LINE;
                             LINE_NAME : in STRING) is
    LINE_NAME_FOR_C : constant STRING(1 .. LINE_NAME'LENGTH+1)
                    := LINE_NAME & ASCII.NUL;
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_SET_LINE_NAME(LINE_FOR_C_ID,
                       LINE_NAME_FOR_C(LINE_NAME_FOR_C'FIRST)'ADDRESS) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_SET_LINE_NAME;

  ------------------------------------------------------------------
  procedure X_FLUSH (LINE_ID : in LINE) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_FLUSH = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_FLUSH;   

  ------------------------------------------------------------------
  procedure X_CLEAR_LINE(LINE_ID : in LINE) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_CLEAR_LINE(LINE_FOR_C_ID) = OK;
    RES := RES and then X_FLUSH = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_CLEAR_LINE;


  ------------------------------------------------------------------
  procedure X_SET_ATTRIBUTES(LINE_ID     : in LINE;
                             PAPER, INK  : in COLOR;
                             SUPERBRIGHT : in BOOLEAN := FALSE;
                             UNDERLINE   : in BOOLEAN := FALSE;
                             BLINK       : in BOOLEAN := FALSE;
                             INVERSE     : in BOOLEAN:= FALSE) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_SET_ATTRIBUTES(LINE_FOR_C_ID, 
                            INTEGER(PAPER), INTEGER(INK), 
                            FOR_C(SUPERBRIGHT), FOR_C(UNDERLINE),
                            FOR_C(BLINK), FOR_C(INVERSE)) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_SET_ATTRIBUTES;
 
  ------------------------------------------------------------------
  procedure X_SET_XOR_MODE(LINE_ID     : in LINE;
                           XOR_MODE    : in BOOLEAN) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_SET_XOR_MODE(LINE_FOR_C_ID, FOR_C(XOR_MODE)) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_SET_XOR_MODE;

  ------------------------------------------------------------------
  procedure X_PUT_CHAR(LINE_ID : in LINE; CAR : in CHARACTER;
                       ROW, COLUMN : in NATURAL) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_PUT_CHAR (LINE_FOR_C_ID,
                       INTEGER(CHARACTER'POS(CAR)),
                       INTEGER(ROW), INTEGER(COLUMN)) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_PUT_CHAR;
 
  ------------------------------------------------------------------
  procedure X_PUT_CHAR(LINE_ID : in LINE; CAR : in BYTE;
                       ROW, COLUMN : in NATURAL) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_PUT_CHAR (LINE_FOR_C_ID, INTEGER(CAR),
                       INTEGER(ROW), INTEGER(COLUMN)) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_PUT_CHAR;
 
  ------------------------------------------------------------------
  procedure X_OVERWRITE_CHAR(LINE_ID : in LINE; CAR : in BYTE;
                             ROW, COLUMN : in NATURAL) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_OVERWRITE_CHAR (LINE_FOR_C_ID, INTEGER(CAR),
                             INTEGER(ROW), INTEGER(COLUMN)) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_OVERWRITE_CHAR;
 
  ------------------------------------------------------------------
  procedure X_PUT_STRING(LINE_ID     : in LINE;
                         STR         : in STRING;
                         ROW, COLUMN : in NATURAL) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_PUT_STRING (LINE_FOR_C_ID,
                         STR (STR'FIRST)'ADDRESS, STR'LENGTH,
                         INTEGER(ROW), INTEGER(COLUMN)) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_PUT_STRING;

  ------------------------------------------------------------------
  procedure X_PUT_CHAR_ATTRIBUTES(LINE_ID     : in LINE;
                                  CAR         : in CHARACTER;
                                  ROW, COLUMN : in NATURAL;
                                  PAPER, INK  : in COLOR;
                                  SUPERBRIGHT : in BOOLEAN := FALSE;
                                  UNDERLINE   : in BOOLEAN := FALSE;
                                  BLINK       : in BOOLEAN := FALSE;
                                  INVERSE     : in BOOLEAN := FALSE) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_PUT_CHAR_ATTRIBUTES (
                              LINE_FOR_C_ID, 
                              INTEGER(CHARACTER'POS(CAR)),
                              INTEGER(ROW), 
                              INTEGER(COLUMN),
                              INTEGER(PAPER),
                              INTEGER(INK), 
                              FOR_C(SUPERBRIGHT),
                              FOR_C(UNDERLINE),
                              FOR_C(BLINK),
                              FOR_C(INVERSE)) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_PUT_CHAR_ATTRIBUTES;
 
  ------------------------------------------------------------------
  procedure  X_DRAW_AREA(LINE_ID : in LINE;
                         WIDTH, HEIGHT : in POSITIVE;
                         ROW, COLUMN : in NATURAL) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_DRAW_AREA (LINE_FOR_C_ID,
                        INTEGER(WIDTH), INTEGER(HEIGHT),
                        INTEGER(ROW), INTEGER(COLUMN)) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_DRAW_AREA;


  ------------------------------------------------------------------
  procedure X_PUT_CHAR_PIXELS(LINE_ID : in LINE; CAR : in BYTE;
                              X, Y    : in NATURAL) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
       RES := X_PUT_CHAR_PIXELS (LINE_FOR_C_ID, INTEGER(CAR), X, Y) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_PUT_CHAR_PIXELS;
 
  ------------------------------------------------------------------
  procedure X_GET_GRAPHIC_CHARACTERISTICS(LINE_ID       : in LINE;
                                          WINDOW_WIDTH  : out NATURAL;
                                          WINDOW_HEIGHT : out NATURAL;
                                          FONT_WIDTH    : out NATURAL;
                                          FONT_HEIGHT   : out NATURAL;
                                          FONT_OFFSET   : out NATURAL) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_GET_GRAPHIC_CHARACTERISTICS(LINE_FOR_C_ID,
        WINDOW_WIDTH'ADDRESS, WINDOW_HEIGHT'ADDRESS,
        FONT_WIDTH'ADDRESS, FONT_HEIGHT'ADDRESS, FONT_OFFSET'ADDRESS) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_GET_GRAPHIC_CHARACTERISTICS;

  ------------------------------------------------------------------
  procedure X_DRAW_POINT(LINE_ID       : in LINE;
                         X, Y          : in NATURAL) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_DRAW_POINT(LINE_FOR_C_ID, X, Y) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_DRAW_POINT;

  ------------------------------------------------------------------
  procedure X_DRAW_LINE(LINE_ID       : in LINE;
                        X1, Y1, X2, Y2 : in NATURAL) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_DRAW_LINE(LINE_FOR_C_ID, X1, Y1, X2, Y2) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_DRAW_LINE;

  ------------------------------------------------------------------
  procedure X_DRAW_RECTANGLE(LINE_ID       : in LINE;
                             X1, Y1, X2, Y2 : in NATURAL) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_DRAW_RECTANGLE(LINE_FOR_C_ID, X1, Y1, X2, Y2) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_DRAW_RECTANGLE;

  ------------------------------------------------------------------
  procedure X_GET_CURRENT_POINTER_POSITION(LINE_ID : in LINE;
                                           X, Y    : out INTEGER) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_GET_CURRENT_POINTER_POSITION (LINE_FOR_C_ID,
                   X'ADDRESS, Y'ADDRESS) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_GET_CURRENT_POINTER_POSITION;

  ------------------------------------------------------------------
  procedure X_SET_GRAPHIC_POINTER(LINE_ID : in LINE;
                                  GRAPHIC : in BOOLEAN) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_SET_GRAPHIC_POINTER(LINE_FOR_C_ID,
                            FOR_C(GRAPHIC)) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_SET_GRAPHIC_POINTER;

  ------------------------------------------------------------------
  procedure X_SELECT (LINE_ID : in LINE; TIMEOUT_MS : in out INTEGER;
                      X_EVENT : out BOOLEAN) is
    EXP : CALENDAR.TIME;
    use CALENDAR;
    TIMEOUT : DURATION;
    SECS : INTEGER;
    MSECS : FLOAT;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    -- Compute expiration and set timeout in duration
    EXP := CALENDAR.CLOCK;
    if TIMEOUT_MS > 0 then
      SECS := TIMEOUT_MS / 1_000;
      MSECS := FLOAT(TIMEOUT_MS - SECS * 1_000) / 1_000.0;
      TIMEOUT := DURATION(SECS)
               + DURATION(MSECS);
      EXP := EXP + TIMEOUT;
    else
      TIMEOUT := -1.0;
    end if;
    -- Ready to wait
    DISPATCHER.WAIT(LINE_ID.NO, TIMEOUT);
    -- Here we wait
    DISPATCHER.SOME_EVENT(LINE_ID.NO)(X_EVENT);
    -- Compute remaining time
    if TIMEOUT_MS > 0 then
      TIMEOUT_MS := INTEGER ( FLOAT(EXP - CALENDAR.CLOCK) * 1_000.0);
      if TIMEOUT_MS < 0 then
        TIMEOUT_MS := 0;
      end if;
    end if;
  end X_SELECT;

  ------------------------------------------------------------------
  procedure X_PROCESS_EVENT(LINE_ID : in LINE; 
                            KIND    : out EVENT_KIND;
                            NEXT    : out BOOLEAN) is
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.GET_EVENT(LINE_ID.NO, KIND, NEXT);
  end X_PROCESS_EVENT;

  ------------------------------------------------------------------
  procedure X_READ_TID(LINE_ID : in LINE;
                       ROW_COL : in BOOLEAN;
                       BUTTON : out BUTTON_LIST;
                       ROW, COLUMN : out INTEGER) is
    LOC_BUTTON : INTEGER;
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_READ_TID (LINE_FOR_C_ID, FOR_C(ROW_COL),
                       LOC_BUTTON'ADDRESS,
                       ROW'ADDRESS, 
                       COLUMN'ADDRESS) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
    -- check returned coordinates
    if LOC_BUTTON = 0 then
      BUTTON := NONE;
    elsif LOC_BUTTON = 1 then
      BUTTON := LEFT;
    elsif LOC_BUTTON = 2 then
      BUTTON := MIDDLE;
    elsif LOC_BUTTON = 3 then
      BUTTON := RIGHT;
    end if;
  end X_READ_TID;

  ------------------------------------------------------------------
  procedure X_READ_KEY(LINE_ID : in LINE; KEY : out KBD_TAB_CODE) is
      LOC_TAB : array (NATURAL range 1..KBD_MAX_CODE) of INTEGER;
      LOC_NBRE : INTEGER;
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_READ_KEY (LINE_FOR_C_ID, LOC_TAB'ADDRESS, LOC_NBRE'ADDRESS) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
    -- Fill table
    for I in KBD_INDEX_CODE range KBD_INDEX_CODE'FIRST..NATURAL(LOC_NBRE) loop
        KEY.TAB(I) := BYTE(LOC_TAB(I));
    end loop;
    KEY.NBRE := NATURAL (LOC_NBRE);
  end X_READ_KEY;

  ------------------------------------------------------------------
  procedure X_ENABLE_MOTION_EVENTS (LINE_ID : in LINE; MOTION_ENABLE : in BOOLEAN) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_ENABLE_MOTION_EVENTS (LINE_FOR_C_ID, FOR_C(MOTION_ENABLE)) = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_ENABLE_MOTION_EVENTS;


  ------------------------------------------------------------------
  procedure X_BLINK_ALTERNATE (LINE_ID : in LINE) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_BLINK = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
  end X_BLINK_ALTERNATE;

  ------------------------------------------------------------------
  procedure X_STOP_BLINKING_TASK (LINE_ID : in LINE) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_STOP_BLINKING = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
  end X_STOP_BLINKING_TASK;  

  ------------------------------------------------------------------
  procedure X_START_BLINKING_TASK (LINE_ID : in LINE) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_START_BLINKING = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
  end X_START_BLINKING_TASK;  

  ------------------------------------------------------------------
  procedure X_BELL (LINE_ID : in LINE; REPEAT : in BELL_REPEAT) is
    LINE_FOR_C_ID : LINE_FOR_C;
    RES : BOOLEAN;
  begin
    if not INITIALISED or else LINE_ID = NO_CLIENT then
      raise X_FAILURE;
    end if;
    DISPATCHER.CALL_ON (LINE_ID.NO, LINE_FOR_C_ID);
    RES := X_BELL (INTEGER(REPEAT)) = OK;
    RES := RES and then X_FLUSH = OK;
    DISPATCHER.CALL_OFF(LINE_ID.NO, LINE_FOR_C_ID);
    if not RES then
      raise X_FAILURE;
    end if;
  end X_BELL;


  ------------------------------------------------------------------
  --------------- T H E   D I S P A T C H E R   B O D Y ------------
  ------------------------------------------------------------------

  procedure XX_SELECT (TIMEOUT_MS : in INTEGER; X_EVENT : out BOOLEAN) is
    DUMMY : RESULT;
    EVENT_FOR_C : BOOL_FOR_C;
  begin
    DUMMY := X_SELECT (SYSTEM.NULL_ADDRESS,
                       EVENT_FOR_C'ADDRESS, TIMEOUT_MS'ADDRESS);
    X_EVENT := FOR_ADA(EVENT_FOR_C);
  end XX_SELECT;


  procedure XX_PROCESS_EVENT (LINE_FOR_C_ID : out LINE_FOR_C;
                              KIND : out EVENT_KIND;
                              NEXT : out BOOLEAN) is
    DUMMY : RESULT;
    NEXT_FOR_C : BOOL_FOR_C;
  begin
    DUMMY := X_PROCESS_EVENT (LINE_FOR_C_ID'ADDRESS,
                              KIND'ADDRESS, NEXT_FOR_C'ADDRESS);
    NEXT := FOR_ADA(NEXT_FOR_C);
  end XX_PROCESS_EVENT;

  task body DISPATCHER is
    -- The clients
    NB_CLIENTS : LINE_RANGE;
    NB_WAIT : LINE_RANGE;
    type CLIENT_DESC_REC is record
      KNOWN : BOOLEAN;
      -- Will be LINE_FOR_C
      LINE_FOR_C_ID : LINE_FOR_C;
      WAIT_INF : BOOLEAN;
      WAIT_EXP : CALENDAR.TIME;
    end record;
    CLIENTS : array (CLIENT_RANGE) of CLIENT_DESC_REC;
    -- The one from WAIT, SOME_EVENT
    SELECTED_CLIENT : CLIENT_RANGE;
    -- One event to give in SOME_EVENT
    SOME_EVENT_PRESENT : BOOLEAN;
    LOC_KIND : EVENT_KIND;
    LOC_NEXT : BOOLEAN;
    -- Local X line
    LOC_LINE_FOR_C_ID : LINE_FOR_C;
    use SYSTEM, CALENDAR;
    -- Delay in msec, or -1
    DELAY_MS : INTEGER;
    SELECT_SOMETHING : BOOLEAN;
  
    procedure COMPUTE_SMALLER_DELAY(DELAY_MS : out INTEGER;
                                    SELECTED_CLIENT : out CLIENT_RANGE) is
      INFINITE : BOOLEAN;
      DUR, MIN_DUR : DURATION; 
      CURRENT_TIME : CALENDAR.TIME := CALENDAR.CLOCK;
    begin

      INFINITE := TRUE;
      MIN_DUR := DURATION'LAST;
      SELECTED_CLIENT := CLIENT_RANGE'FIRST;
      -- Look for smallest delay. Check if all infinite.
      for I in CLIENT_RANGE loop
        if CLIENTS(I).KNOWN then
          if not CLIENTS(I).WAIT_INF then
            INFINITE := FALSE;
            DUR := CLIENTS(I).WAIT_EXP - CURRENT_TIME;
            if DUR < 0.0 then
              DUR := 0.0;
            end if;
            if DUR < MIN_DUR then
              MIN_DUR := DUR;
              SELECTED_CLIENT := I;
            end if;
          end if;
        end if;
      end loop;
      if INFINITE then
        DELAY_MS := -1;
      else
        DELAY_MS := INTEGER (MIN_DUR * 1_000);
      end if;
    end COMPUTE_SMALLER_DELAY;

    procedure GET_CLIENT_FROM_LINE(LINE_FOR_C_ID : in LINE_FOR_C;
                                   CLIENT : out CLIENT_RANGE;
                                   FOUND : out BOOLEAN) is
    begin
      for I in CLIENT_RANGE loop
        if CLIENTS(I).KNOWN
        and then CLIENTS(I).LINE_FOR_C_ID = LINE_FOR_C_ID then
          FOUND := TRUE;
          CLIENT := I;
          return;
        end if;
      end loop;
      CLIENT := CLIENT_RANGE'FIRST;
      FOUND := FALSE;
    end GET_CLIENT_FROM_LINE;

  begin
    -- Do you need me?
    select
      accept START;
    or
      terminate;
    end select;
    -- No client known
    NB_CLIENTS := 0;
    NB_WAIT := 0;
    for I in CLIENT_RANGE loop
      CLIENTS(I).KNOWN := FALSE;
      CLIENTS(I).LINE_FOR_C_ID := NO_LINE_FOR_C;
    end loop;
    -- No event
    SOME_EVENT_PRESENT := FALSE;
    -- To avoid a warning: may not have a value
    SELECTED_CLIENT := CLIENT_RANGE'FIRST;
    LOC_KIND := DISCARD;
    LOC_NEXT := FALSE;

    loop
      select
        -- Accept call to X, one at a time
        accept CALL_ON (CLIENT : in CLIENT_RANGE;
                        LINE_FOR_C_ID : out LINE_FOR_C) do
          LINE_FOR_C_ID := CLIENTS(CLIENT).LINE_FOR_C_ID;
        end CALL_ON;
        accept CALL_OFF (CLIENT : in CLIENT_RANGE;
                         NEW_LINE_FOR_C_ID : in LINE_FOR_C) do
          CLIENTS(CLIENT).LINE_FOR_C_ID := NEW_LINE_FOR_C_ID;
        end CALL_OFF;
      or
        accept REGISTER (CLIENT : out LINE_RANGE) do
          -- Find a slot
          for I in CLIENT_RANGE loop
            if not CLIENTS(I).KNOWN then
              CLIENTS(I).KNOWN := TRUE;
              CLIENT := I;
              NB_CLIENTS := NB_CLIENTS + 1;
              return;
            end if;
          end loop;
          -- Too many clients
          CLIENT := NO_CLIENT_NO;
        end REGISTER;
      or
        accept UNREGISTER (CLIENT : in out LINE_RANGE) do
          if CLIENT /= NO_CLIENT_NO then
            CLIENTS(CLIENT).KNOWN := FALSE;
            CLIENTS(CLIENT).LINE_FOR_C_ID := NO_LINE_FOR_C;
            NB_CLIENTS := NB_CLIENTS - 1;
          end if;
          CLIENT := NO_CLIENT_NO;
          -- Select a new client for a dummy event
          SOME_EVENT_PRESENT := FALSE;
          for I in CLIENT_RANGE loop
            if CLIENTS(I).KNOWN then
              SELECTED_CLIENT := I;
              exit;
            end if;
          end loop;
        end UNREGISTER;
      or
        -- Client is ready to wait
        accept WAIT (CLIENT : in CLIENT_RANGE; TIMEOUT : in DURATION) do
          NB_WAIT := NB_WAIT + 1;
          -- Some pending event for this client?
          if not SOME_EVENT_PRESENT or else CLIENT /= SELECTED_CLIENT then
            -- This client will wait
            -- Compute expiration
            if TIMEOUT < 0.0 then
              CLIENTS(CLIENT).WAIT_INF := TRUE;
            else
              CLIENTS(CLIENT).WAIT_INF := FALSE;
              CLIENTS(CLIENT).WAIT_EXP := CALENDAR.CLOCK + TIMEOUT;
            end if;
          end if;
        end WAIT;
        -- Can we freeze the whole stuff?
        --  no event and all clients waiting
        if not SOME_EVENT_PRESENT and then NB_WAIT = NB_CLIENTS then
          -- Loop until timeout or valid client for the event is found
          loop
            -- This gives the client with smallest delay
            -- If all infinite, the first known
            COMPUTE_SMALLER_DELAY(DELAY_MS, SELECTED_CLIENT);
            XX_SELECT (DELAY_MS, SELECT_SOMETHING);
            if not SELECT_SOMETHING then
              -- Timeout
              SOME_EVENT_PRESENT := FALSE;
              exit;
            else
              -- An event: Get&store it and it's client
              XX_PROCESS_EVENT (LOC_LINE_FOR_C_ID, LOC_KIND, LOC_NEXT);
              GET_CLIENT_FROM_LINE(LOC_LINE_FOR_C_ID, SELECTED_CLIENT,
                                 SOME_EVENT_PRESENT);
              exit when SOME_EVENT_PRESENT;
            end if;
          end loop;
        end if;
      or
        when NB_CLIENTS /= 0 and then NB_WAIT = NB_CLIENTS =>
        -- Release the client for stored event
        accept SOME_EVENT(SELECTED_CLIENT) (SOME : out BOOLEAN) do
          SOME := SELECT_SOMETHING;
          NB_WAIT := NB_WAIT - 1;
        end SOME_EVENT;
      or
        accept GET_EVENT (CLIENT : in CLIENT_RANGE; KIND : out EVENT_KIND;
                                                    SOME : out BOOLEAN) do
          if CLIENT /= SELECTED_CLIENT then
            -- Invalid client
            KIND := DISCARD;
            SOME := FALSE;
            return;
          end if;
          -- Event got from previous wait?
          if SOME_EVENT_PRESENT then
            KIND := LOC_KIND;
            SOME := LOC_NEXT;
            SOME_EVENT_PRESENT := FALSE;
          else
            -- No stored event
            XX_PROCESS_EVENT (LOC_LINE_FOR_C_ID, KIND, SOME);
            -- New event for the same client?
            if LOC_LINE_FOR_C_ID /= CLIENTS(CLIENT).LINE_FOR_C_ID then
              -- Current client has to give up
              SOME := FALSE;
              -- Find new client
              GET_CLIENT_FROM_LINE(LOC_LINE_FOR_C_ID, SELECTED_CLIENT,
                                   SOME_EVENT_PRESENT);
            end if;
          end if;
        end GET_EVENT;
      or
        terminate;
      end select;
    end loop;
  end DISPATCHER;

end X_MNG;

