with CALENDAR, SYSTEM;
with DYNAMIC_LIST, MY_IO, TEXT_IO;
package body X_MNG is

  -- Duration outputs
  package DUR_IO is new TEXT_IO.FIXED_IO(DURATION);
  DEBUG : constant BOOLEAN := FALSE;
  -- DEBUG : constant BOOLEAN := TRUE;

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

  -- Callback list
  type CB_REC is record
    FD : FILE_DESC;
    CB : FD_CALLBACK;
  end record;
  package CB_MNG is new DYNAMIC_LIST(CB_REC);
  CB_LIST : CB_MNG.LIST_TYPE;

  -- Same FD
  function SAME_FD (CB1, CB2 : CB_REC) return BOOLEAN is
  begin
    return CB1.FD = CB2.FD;
  end SAME_FD;
  procedure CB_SEARCH is new CB_MNG.SEARCH(SAME_FD);

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
  -- Add a fd for select
  -- int x_add_fd (int fd);
  ------------------------------------------------------------------
  function X_ADD_FD (FD : INTEGER) return RESULT;
  pragma IMPORT(C, X_ADD_FD, "x_add_fd");

  ------------------------------------------------------------------
  -- Del a fd from select
  -- int x_del_fd (int fd);
  ------------------------------------------------------------------
  function X_DEL_FD (FD : INTEGER) return RESULT;
  pragma IMPORT(C, X_DEL_FD, "x_del_fd");

  ------------------------------------------------------------------
  -- Wait for some events
  -- int x_select (int *p_fd, int *timeout_ms);
  ------------------------------------------------------------------
  C_SELECT_NO_EVENT : constant INTEGER := -2;
  C_SELECT_X_EVENT  : constant INTEGER := -1;
  function X_SELECT (P_FD : SYSTEM.ADDRESS;
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
  procedure X_ADD_CALLBACK (FD : in FILE_DESC; CALLBACK : in FD_CALLBACK) is
    RES : BOOLEAN;
  begin
    -- Append
    if not CB_MNG.IS_EMPTY (CB_LIST) then
      CB_MNG.MOVE_TO (CB_LIST, CB_MNG.PREV, 0, FALSE);
    end if;
    CB_MNG.INSERT (CB_LIST, (FD, CALLBACK));
    -- Add fd to select
    RES := X_ADD_FD (INTEGER(FD)) = OK;
    if not RES then
      raise X_FAILURE;
    end if;
  exception
    when others =>
      raise X_FAILURE;
  end X_ADD_CALLBACK;

  ------------------------------------------------------------------
  procedure X_DEL_CALLBACK (FD : in FILE_DESC) is
    RES : BOOLEAN;
    CB_SEARCHED : CB_REC;
  begin
    -- Del fd from select
    RES := X_DEL_FD (INTEGER(FD)) = OK;
    -- del from list
    CB_SEARCHED.FD := FD;
    CB_SEARCHED.CB := null;
    CB_SEARCH (CB_LIST, CB_SEARCHED, FROM_CURRENT => FALSE);
    if CB_MNG.GET_POSITION (CB_LIST) /=  CB_MNG.LIST_LENGTH(CB_LIST) then
      CB_MNG.DELETE (CB_LIST, CB_MNG.NEXT);
    else
      CB_MNG.DELETE (CB_LIST, CB_MNG.PREV);
    end if;
    if not RES then
      raise X_FAILURE;
    end if;
  exception
    when others =>
      raise X_FAILURE;
  end X_DEL_CALLBACK;
  
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

  type XX_SELECT_RESULT_LIST is (SELECT_X_EVENT, SELECT_FD, SELECT_TIMEOUT);

  function XX_SELECT (TIMEOUT_MS : INTEGER) return XX_SELECT_RESULT_LIST is
    FD    : INTEGER;
    TIMEOUT : INTEGER;
    DUMMY : RESULT;
    CB_SEARCHED : CB_REC;
  begin
    TIMEOUT := TIMEOUT_MS;
    DUMMY := X_SELECT (FD'ADDRESS, TIMEOUT'ADDRESS);
    if DEBUG then
      MY_IO.PUT_LINE ("  XX_SELECT -> " & INTEGER'IMAGE(FD));
    end if;
    if FD = C_SELECT_X_EVENT then
      return SELECT_X_EVENT;
    elsif FD = C_SELECT_NO_EVENT then
      return SELECT_TIMEOUT;
    else
      CB_SEARCHED.FD := FILE_DESC(FD);
      CB_SEARCHED.CB := null;
      begin
        -- Search and read callback
        CB_SEARCH (CB_LIST, CB_SEARCHED, FROM_CURRENT => FALSE);
        CB_MNG.READ (CB_LIST, CB_SEARCHED,  CB_MNG.CURRENT);
        -- Call it
        CB_SEARCHED.CB (CB_SEARCHED.FD);
      exception
        when CB_MNG.NOT_IN_LIST =>
        if DEBUG then
          MY_IO.PUT_LINE ("**** XX_SELECT: " & INTEGER'IMAGE(FD) 
                        & " fd not found ****");
        end if;
      end;
    end if;
    return SELECT_FD;
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
      -- Refreshing this client due to a unregister
      REFRESH : BOOLEAN;
    end record;
    CLIENTS : array (CLIENT_RANGE) of CLIENT_DESC_REC;
    -- The TIMEOUT/EVENT and CLIENT from WAIT to SOME_EVENT to GET_EVENT
    SELECT_RESULT : XX_SELECT_RESULT_LIST;
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
                                   CLIENT : in out CLIENT_RANGE;
                                   FOUND : out BOOLEAN) is
    begin
      -- Same as current?
      if CLIENTS(CLIENT).KNOWN
      and then CLIENTS(CLIENT).LINE_FOR_C_ID = LINE_FOR_C_ID then
        FOUND := TRUE;
        return;
      end if;
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
              CLIENTS(I).REFRESH := FALSE;
              CLIENTS(I).LINE_FOR_C_ID := NO_LINE_FOR_C;
              CLIENT := I;
              NB_CLIENTS := NB_CLIENTS + 1;
              if DEBUG then
                MY_IO.PUT_LINE ("Register -> " & LINE_RANGE'IMAGE(I));
              end if;
              return;
            end if;
          end loop;
          -- Too many clients
          CLIENT := NO_CLIENT_NO;
        end REGISTER;
      or
        accept UNREGISTER (CLIENT : in out LINE_RANGE) do
          if DEBUG then
            MY_IO.PUT_LINE ("Unregister " & LINE_RANGE'IMAGE(CLIENT));
          end if;
          -- Update administration
          if CLIENT /= NO_CLIENT_NO then
            CLIENTS(CLIENT).KNOWN := FALSE;
            NB_CLIENTS := NB_CLIENTS - 1;
          end if;
          CLIENT := NO_CLIENT_NO;
          -- Generate a dummy refresh event for all client
          -- Wake up all waiting clients
          for I in CLIENT_RANGE loop
            if CLIENTS(I).KNOWN then
              CLIENTS(I).REFRESH := TRUE;
              select
                accept SOME_EVENT(I) (SOME : out BOOLEAN) do
                  SOME := TRUE;
                  NB_WAIT := NB_WAIT - 1;
                end SOME_EVENT;
              or
                delay 0.0;
              end select; 
            end if;
          end loop;
        end UNREGISTER;
      or
        -- Client is ready to wait
        accept WAIT (CLIENT : in CLIENT_RANGE; TIMEOUT : in DURATION) do
          NB_WAIT := NB_WAIT + 1;
          if DEBUG then
            MY_IO.PUT ("Wait " & LINE_RANGE'IMAGE(CLIENT) & "  timeout: ");
            DUR_IO.PUT(timeout);
            MY_IO.NEW_LINE;
            MY_IO.PUT_LINE ("    Waiting nb " & LINE_RANGE'IMAGE(NB_WAIT));
          end if;
          -- Some pending event for this client?
          if not SOME_EVENT_PRESENT or else CLIENT /= SELECTED_CLIENT then
            -- This client will wait
            -- Compute expiration
            if TIMEOUT < 0.0 then
              CLIENTS(CLIENT).WAIT_INF := TRUE;
              if DEBUG then
                MY_IO.PUT_LINE ("    Wait inf");
              end if;
            else
              CLIENTS(CLIENT).WAIT_INF := FALSE;
              CLIENTS(CLIENT).WAIT_EXP := CALENDAR.CLOCK + TIMEOUT;
              if DEBUG then
                MY_IO.PUT_LINE ("    Wait timeout");
              end if;
            end if;
          elsif DEBUG then
            MY_IO.PUT_LINE ("    Wait client is selected");
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
            if DEBUG then
              MY_IO.PUT_LINE ("        Wait select " & INTEGER'IMAGE(DELAY_MS));
            end if;
            SELECT_RESULT := XX_SELECT (DELAY_MS);
            case SELECT_RESULT is
              when SELECT_TIMEOUT =>
                -- Timeout
                SOME_EVENT_PRESENT := FALSE;
                if DEBUG then
                  MY_IO.PUT_LINE ("            Wait select timeout for -> "
                                & LINE_RANGE'IMAGE(SELECTED_CLIENT));
                end if;
                exit;
              when SELECT_X_EVENT =>
                -- An event: Get&store it and it's client
                XX_PROCESS_EVENT (LOC_LINE_FOR_C_ID, LOC_KIND, LOC_NEXT);
                GET_CLIENT_FROM_LINE(LOC_LINE_FOR_C_ID, SELECTED_CLIENT,
                                   SOME_EVENT_PRESENT);
                if DEBUG then
                  MY_IO.PUT_LINE ("            Wait select event for -> "
                                & LINE_RANGE'IMAGE(SELECTED_CLIENT)
                                & " found " & BOOLEAN'IMAGE(SOME_EVENT_PRESENT));
                end if;
                exit when SOME_EVENT_PRESENT;
              when SELECT_FD =>
                SOME_EVENT_PRESENT := TRUE;
                LOC_KIND := FD_EVENT;
                LOC_NEXT := FALSE;
                if DEBUG then
                  MY_IO.PUT_LINE ("            Wait select fd event for -> "
                                & LINE_RANGE'IMAGE(SELECTED_CLIENT));
                end if;
                exit;
            end case;
          end loop;
        end if;
      or
        when NB_CLIENTS /= 0 and then NB_WAIT = NB_CLIENTS =>
        -- Release the client for stored event
        accept SOME_EVENT(SELECTED_CLIENT) (SOME : out BOOLEAN) do
          if DEBUG then
            MY_IO.PUT_LINE ("Some_event " & LINE_RANGE'IMAGE(SELECTED_CLIENT)
                   & " -> "
                   & "Select result:" & XX_SELECT_RESULT_LIST'IMAGE(SELECT_RESULT)
                   & ", Event present:" & BOOLEAN'IMAGE(SOME_EVENT_PRESENT));
          end if;
          SOME := SELECT_RESULT /= SELECT_TIMEOUT or else SOME_EVENT_PRESENT;
          NB_WAIT := NB_WAIT - 1;
        end SOME_EVENT;
      or
        accept GET_EVENT (CLIENT : in CLIENT_RANGE; KIND : out EVENT_KIND;
                                                    SOME : out BOOLEAN) do
          -- Artificial refresh for this client?
          if CLIENTS(CLIENT).REFRESH then
            CLIENTS(CLIENT).REFRESH := FALSE;
            KIND := REFRESH;
            SOME := FALSE;
            if DEBUG then
              MY_IO.PUT_LINE ("Get_event " & LINE_RANGE'IMAGE(CLIENT)
                            & " -> artificial refresh");
            end if;
            return;
          end if;
          if CLIENT /= SELECTED_CLIENT then
            -- Invalid client
            KIND := DISCARD;
            SOME := FALSE;
            if DEBUG then
              MY_IO.PUT_LINE ("Get_event " & LINE_RANGE'IMAGE(CLIENT)
                            & " -> not selected");
            end if;
            return;
          end if;
          -- Event got from previous wait or get_event?
          if SOME_EVENT_PRESENT then
            KIND := LOC_KIND;
            SOME := LOC_NEXT;
            SOME_EVENT_PRESENT := FALSE;
            if DEBUG then
              MY_IO.PUT_LINE ("Get_event " & LINE_RANGE'IMAGE(CLIENT)
                            & " -> from previous wait/get_event");
            end if;
          else
            -- No stored event
            XX_PROCESS_EVENT (LOC_LINE_FOR_C_ID, LOC_KIND, LOC_NEXT);
            -- New event for the same client?
            if LOC_LINE_FOR_C_ID /= CLIENTS(CLIENT).LINE_FOR_C_ID then
              -- Current client has to give up
              SOME := FALSE;
              KIND := DISCARD;
              SOME_EVENT_PRESENT := TRUE;
              -- Find client of the event
              GET_CLIENT_FROM_LINE(LOC_LINE_FOR_C_ID, SELECTED_CLIENT,
                                   SOME_EVENT_PRESENT);
              if DEBUG then
                MY_IO.PUT_LINE ("Get_event " & LINE_RANGE'IMAGE(CLIENT)
                              & " -> give up to "
                              & LINE_RANGE'IMAGE(SELECTED_CLIENT));
              end if;
            else
              -- This event is for this client. Deliver.
              SOME := LOC_NEXT;
              KIND := LOC_KIND;
              if DEBUG then
                MY_IO.PUT_LINE ("Get_event " & LINE_RANGE'IMAGE(CLIENT)
                              & " -> got it");
              end if;
            end if; -- event is for client
          end if; -- SOME_EVENT_PRESENT
          if DEBUG then
            MY_IO.PUT_LINE ("    Get_event -> " & EVENT_KIND'IMAGE(LOC_KIND)
                          & " next: " & BOOLEAN'IMAGE(LOC_NEXT));
          end if;
        end GET_EVENT;
      or
        terminate;
      end select;
    end loop;
  end DISPATCHER;

  ------------------------------------------------------------------
  -- Specific select without X
  function SELECT_NO_X (TIMEOUT_MS : INTEGER) return BOOLEAN is
    SELECT_RESULT : XX_SELECT_RESULT_LIST;
  begin
    if INITIALISED  then
      raise X_FAILURE;
    end if;
    SELECT_RESULT := XX_SELECT (TIMEOUT_MS);
    if SELECT_RESULT = SELECT_X_EVENT then
      if DEBUG then
        MY_IO.PUT_LINE ("**** SELECT_NO_X: Got a X event");
      end if;
      raise X_FAILURE;
    else
      return SELECT_RESULT = SELECT_FD;
    end if;
  end SELECT_NO_X;

end X_MNG;

