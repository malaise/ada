
package body X_MNG is

    -- Result of a call to C
    subtype RESULT is INTEGER;
    OK : constant RESULT := 0;

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
    pragma IMPORT (C, X_INITIALISE, "x_initialise");

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
    -- Set the name of a line
    -- int x_set_line_name (void *line_id, char *line_name);
    ------------------------------------------------------------------
    function X_SET_LINE_NAME (LINE_ID   : SYSTEM.ADDRESS;
                              LINE_NAME : SYSTEM.ADDRESS) return RESULT;
    pragma IMPORT(C, X_SET_LINE_NAME, "x_set_line_name");

    ------------------------------------------------------------------
    -- Closes a line
    -- int x_close_line (void *line_id);
    ------------------------------------------------------------------
    function X_CLOSE_LINE(LINE_ID : LINE_FOR_C) return RESULT;
    pragma IMPORT(C, X_CLOSE_LINE, "x_close_line");

    ------------------------------------------------------------------
    -- Clears a line
    -- int x_clear_line (void *line_id);
    ------------------------------------------------------------------
    function X_CLEAR_LINE(LINE_ID : LINE_FOR_C) return RESULT;
    pragma IMPORT(C, X_CLEAR_LINE, "x_clear_line");

    ------------------------------------------------------------------
    -- Flushes all the lines of the host (really display them)
    -- int x_flush (void)
    ------------------------------------------------------------------
    function X_FLUSH return RESULT;
    pragma IMPORT(C, X_FLUSH, "x_flush");

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
    -- Sets the position for a further put in the same window
    -- int x_move (void *line_id, int row, int column);
    ------------------------------------------------------------------
    function X_MOVE(LINE_ID     : LINE_FOR_C;
                    ROW, COLUMN : INTEGER) return RESULT;
    pragma IMPORT(C, X_MOVE, "x_move");
 
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
    -- int x_put_char (void *line_id, int  car);
    ------------------------------------------------------------------
    function X_PUT_CHAR(LINE_ID : LINE_FOR_C;
                        CAR     : INTEGER) return RESULT;
    pragma IMPORT(C, X_PUT_CHAR, "x_put_char");

    ------------------------------------------------------------------
    -- Writes a char whith the attributes previously set
    -- Does not erase character at current position
    -- int x_overwrite_char (void *line_id, int  car);
    ------------------------------------------------------------------
    function X_OVERWRITE_CHAR(LINE_ID : LINE_FOR_C;
                              CAR     : INTEGER) return RESULT;
    pragma IMPORT(C, X_OVERWRITE_CHAR, "x_overwrite_char");

    ------------------------------------------------------------------
    -- Writes a string with the attributes previously set
    -- int x_put_string (void *line_id, char *p_char, int number);
    ------------------------------------------------------------------
    function X_PUT_STRING(LINE_ID  : LINE_FOR_C;
                          STR_ADDR : SYSTEM.ADDRESS;
                          LENGTH   : INTEGER) return RESULT;
    pragma IMPORT(C, X_PUT_STRING, "x_put_string");
 
    ------------------------------------------------------------------
    -- Draws a rectangle (width * height) from current position
    --  with current background color.
    --  New position is updated to lower-left square of rectangle.
    -- int x_draw_area (void *line_id, int width, int height);
    ------------------------------------------------------------------
    function X_DRAW_AREA(LINE_ID       : LINE_FOR_C;
                         WIDTH, HEIGHT : INTEGER) return RESULT;
    pragma IMPORT(C, X_DRAW_AREA, "x_draw_area");
 
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
    -- Rings a bell several times
    -- int x_bell (int nbre_bell;
    ------------------------------------------------------------------
    function X_BELL (REPEAT : INTEGER) return RESULT;
    pragma IMPORT(C, X_BELL, "x_bell");

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
    ------------------------ T H E   C O D E -------------------------
    ------------------------------------------------------------------

    procedure X_INITIALISE (SERVER_NAME    : in STRING) is

        SERV_NAME_FOR_C : constant STRING(1 .. SERVER_NAME'LENGTH+1)
         := SERVER_NAME & ASCII.NUL;
    begin
        if INITIALISED or else
         X_INITIALISE (SERV_NAME_FOR_C (SERV_NAME_FOR_C'FIRST)'ADDRESS)
         /= OK then
            raise X_FAILURE;
        end if;
        INITIALISED := TRUE;
    end X_INITIALISE;
  

    ------------------------------------------------------------------
    procedure X_OPEN_LINE(LINE_DEFINITION : in LINE_DEFINITION_REC;
                          LINE_ID         : in out LINE) is
    begin
        if not INITIALISED or else
         X_OPEN_LINE (LINE_DEFINITION.SCREEN_ID,
                      LINE_DEFINITION.ROW,
                      LINE_DEFINITION.COLUMN, 
                      LINE_DEFINITION.HEIGHT,
                      LINE_DEFINITION.WIDTH, 
                      LINE_DEFINITION.BACKGROUND,
                      LINE_DEFINITION.BORDER, 
                      LINE_DEFINITION.NO_FONT,
                      LINE_ID.NO'ADDRESS) /= OK then
            raise X_FAILURE;
         end if;
    end X_OPEN_LINE;

    ------------------------------------------------------------------
    procedure X_SET_LINE_NAME (LINE_ID : in LINE;
                               LINE_NAME : in STRING) is
        LINE_NAME_FOR_C : constant STRING(1 .. LINE_NAME'LENGTH+1)
         := LINE_NAME & ASCII.NUL;
    begin
        if not INITIALISED or else
         X_SET_LINE_NAME(LINE_ID.NO,
                         LINE_NAME_FOR_C(LINE_NAME_FOR_C'FIRST)'ADDRESS)
          /= OK then
            raise X_FAILURE;
        end if;
    end X_SET_LINE_NAME;

    ------------------------------------------------------------------
    procedure X_CLOSE_LINE(LINE_ID : in out LINE) is
    begin
        if not INITIALISED or else
         X_CLOSE_LINE(LINE_ID.NO) /= OK then
            raise X_FAILURE;
        end if;
        X_FLUSH;
    end X_CLOSE_LINE;


    ------------------------------------------------------------------
    procedure X_CLEAR_LINE(LINE_ID : in LINE) is
    begin
        if not INITIALISED or else
         X_CLEAR_LINE(LINE_ID.NO) /= OK then
            raise X_FAILURE;
        end if;
        X_FLUSH;
    end X_CLEAR_LINE;


    ------------------------------------------------------------------
    procedure X_FLUSH is
    begin
        if not INITIALISED or else X_FLUSH /= OK then
            raise X_FAILURE;
        end if;
    end X_FLUSH;   


    ------------------------------------------------------------------
    procedure X_PUT_CHAR_ATTRIBUTES(LINE_ID     : in LINE;
                                    CAR         : in CHARACTER;
                                    ROW, COLUMN : in NATURAL;
                                    PAPER, INK  : in COLOR;
                                    SUPERBRIGHT : in BOOLEAN := FALSE;
                                    UNDERLINE   : in BOOLEAN := FALSE;
                                    BLINK       : in BOOLEAN := FALSE;
                                    INVERSE     : in BOOLEAN := FALSE) is
    begin
        if not INITIALISED or else
         X_PUT_CHAR_ATTRIBUTES (LINE_ID.NO, 
                                INTEGER(CHARACTER'POS(CAR)),
                                INTEGER(ROW), 
                                INTEGER(COLUMN),
                                INTEGER(PAPER),
                                INTEGER(INK), 
                                FOR_C(SUPERBRIGHT),
                                FOR_C(UNDERLINE),
                                FOR_C(BLINK),
                                FOR_C(INVERSE)) /= OK then
            raise X_FAILURE;
        end if;
    end X_PUT_CHAR_ATTRIBUTES;
 

    ------------------------------------------------------------------
    procedure X_MOVE(LINE_ID     : in LINE;
                     ROW, COLUMN : in NATURAL) is
    begin
        if not INITIALISED or else
         X_MOVE(LINE_ID.NO,
          INTEGER(ROW), INTEGER(COLUMN)) /= OK then
            raise X_FAILURE;
        end if;
    end X_MOVE;


    ------------------------------------------------------------------
    procedure X_SET_ATTRIBUTES(LINE_ID     : in LINE;
                               PAPER, INK  : in COLOR;
                               SUPERBRIGHT : in BOOLEAN := FALSE;
                               UNDERLINE   : in BOOLEAN := FALSE;
                               BLINK       : in BOOLEAN := FALSE;
                               INVERSE     : in BOOLEAN:= FALSE) is
    begin
        if not INITIALISED or else
         X_SET_ATTRIBUTES(LINE_ID.NO, 
          INTEGER(PAPER), INTEGER(INK), 
          FOR_C(SUPERBRIGHT), FOR_C(UNDERLINE),
          FOR_C(BLINK), FOR_C(INVERSE)) /= OK then
            raise X_FAILURE;
        end if;
    end X_SET_ATTRIBUTES;
 

    ------------------------------------------------------------------
    procedure X_SET_XOR_MODE(LINE_ID     : in LINE;
                             XOR_MODE    : in BOOLEAN) is
    begin
        if not INITIALISED or else
         X_SET_XOR_MODE(LINE_ID.NO, FOR_C(XOR_MODE)) /= OK then
            raise X_FAILURE;
        end if;
    end X_SET_XOR_MODE;
 

    ------------------------------------------------------------------
    procedure X_PUT_CHAR(LINE_ID : in LINE; CAR : in CHARACTER) is
    begin
        if not INITIALISED or else
         X_PUT_CHAR (LINE_ID.NO, INTEGER(CHARACTER'POS(CAR))) 
         /= OK then
            raise X_FAILURE;
        end if;
    end X_PUT_CHAR;
 
 
    ------------------------------------------------------------------
    procedure X_PUT_CHAR(LINE_ID : in LINE; CAR : in BYTE) is
    begin
        if not INITIALISED or else
         X_PUT_CHAR (LINE_ID.NO, INTEGER(CAR)) 
         /= OK then
            raise X_FAILURE;
        end if;
    end X_PUT_CHAR;
 
 
    ------------------------------------------------------------------
    procedure X_OVERWRITE_CHAR(LINE_ID : in LINE; CAR : in BYTE) is
    begin
        if not INITIALISED or else
         X_OVERWRITE_CHAR (LINE_ID.NO, INTEGER(CAR)) 
         /= OK then
            raise X_FAILURE;
        end if;
    end X_OVERWRITE_CHAR;
 
 
    ------------------------------------------------------------------
    procedure X_PUT_STRING(LINE_ID : in LINE;
                           STR     : in STRING) is
    begin
        if not INITIALISED
        or else X_PUT_STRING (LINE_ID.NO,
                              STR (STR'FIRST)'ADDRESS,
                              STR'LENGTH) /= OK then
            raise X_FAILURE;
        end if;
    end X_PUT_STRING;

    ------------------------------------------------------------------
    procedure  X_DRAW_AREA(LINE_ID : in LINE;
                           WIDTH, HEIGHT : in POSITIVE) is
    begin
        if not INITIALISED or else X_DRAW_AREA (LINE_ID.NO,
                                                INTEGER(WIDTH),
                                                INTEGER(HEIGHT))
         /= OK then
            raise X_FAILURE;
        end if;
    end X_DRAW_AREA;


    ------------------------------------------------------------------
    procedure X_SELECT (TIMEOUT_MS : in out INTEGER; X_EVENT : out BOOLEAN) is
        EVENT_FOR_C : BOOL_FOR_C;
    begin
        if not INITIALISED or else X_SELECT (SYSTEM.NULL_ADDRESS,
                                             EVENT_FOR_C'ADDRESS,
                                             TIMEOUT_MS'ADDRESS)
         /= OK then
            raise X_FAILURE;
        end if;
        X_EVENT := FOR_ADA(EVENT_FOR_C);
    end X_SELECT;


    ------------------------------------------------------------------
    procedure X_PROCESS_EVENT(LINE_ID : out LINE; 
                              KIND    : out EVENT_KIND;
                              NEXT    : out BOOLEAN) is
        NEXT_FOR_C : BOOL_FOR_C;
    begin
        if not INITIALISED or else X_PROCESS_EVENT (LINE_ID.NO'ADDRESS,
                                                    KIND'ADDRESS,
                                                    NEXT_FOR_C'ADDRESS)
         /= OK then
            raise X_FAILURE;
        end if;
        NEXT := FOR_ADA(NEXT_FOR_C);
    end X_PROCESS_EVENT;

 
    ------------------------------------------------------------------
    procedure X_READ_TID(LINE_ID : in LINE;
                         ROW_COL : BOOLEAN;
                         BUTTON : out BUTTON_LIST;
                         ROW, COLUMN : out INTEGER) is
        LOC_BUTTON : INTEGER;
    begin
        if not INITIALISED or else
         X_READ_TID (LINE_ID.NO, FOR_C(ROW_COL),
                     LOC_BUTTON'ADDRESS,
                     ROW'ADDRESS, 
                     COLUMN'ADDRESS) /= OK then
            raise X_FAILURE;
        end if;
        -- check returned coordinates
        if LOC_BUTTON = 1 then
          BUTTON := LEFT;
        elsif LOC_BUTTON = 2 then
          BUTTON := MIDDLE;
        else
          BUTTON := RIGHT;
        end if;
    end X_READ_TID;

 
    ------------------------------------------------------------------
    procedure X_READ_KEY(LINE_ID : in LINE; KEY : out KBD_TAB_CODE) is
        LOC_TAB : array (NATURAL range 1..KBD_MAX_CODE) of INTEGER;
        LOC_NBRE : INTEGER;
    begin
        if not INITIALISED or else
         X_READ_KEY (LINE_ID.NO, LOC_TAB'ADDRESS, LOC_NBRE'ADDRESS) 
         /= OK then
            raise X_FAILURE;
        end if;
        -- Fill table
        for I in KBD_INDEX_CODE range KBD_INDEX_CODE'FIRST..NATURAL(LOC_NBRE) loop
            KEY.TAB(I) := BYTE(LOC_TAB(I));
        end loop;
        KEY.NBRE := NATURAL (LOC_NBRE);
    end X_READ_KEY;


    ------------------------------------------------------------------
    procedure X_PUT_CHAR_PIXELS(LINE_ID : in LINE; CAR : in BYTE;
                                X, Y    : in NATURAL) is
    begin
        if not INITIALISED or else
         X_PUT_CHAR_PIXELS (LINE_ID.NO, INTEGER(CAR), X, Y) /= OK then
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
    begin
        if not INITIALISED or else
        X_GET_GRAPHIC_CHARACTERISTICS(LINE_ID.NO,
          WINDOW_WIDTH'ADDRESS, WINDOW_HEIGHT'ADDRESS,
          FONT_WIDTH'ADDRESS, FONT_HEIGHT'ADDRESS, FONT_OFFSET'ADDRESS)
            /= OK then
            raise X_FAILURE;
        end if;
    end X_GET_GRAPHIC_CHARACTERISTICS;


    ------------------------------------------------------------------
    procedure X_DRAW_POINT(LINE_ID       : in LINE;
                           X, Y          : in NATURAL) is
    begin
        if not INITIALISED or else
        X_DRAW_POINT(LINE_ID.NO, X, Y) /= OK then
            raise X_FAILURE;
        end if;
    end X_DRAW_POINT;


    ------------------------------------------------------------------
    procedure X_DRAW_RECTANGLE(LINE_ID       : in LINE;
                               X1, Y1, X2, Y2 : in NATURAL) is

    begin
        if not INITIALISED or else
        X_DRAW_RECTANGLE(LINE_ID.NO, X1, Y1, X2, Y2) /= OK then
            raise X_FAILURE;
        end if;
    end X_DRAW_RECTANGLE;


    ------------------------------------------------------------------
    procedure X_GET_CURRENT_POINTER_POSITION(LINE_ID : in LINE;
                                             X, Y    : out INTEGER) is
    begin
        if not INITIALISED or else
         X_GET_CURRENT_POINTER_POSITION (LINE_ID.NO,
                     X'ADDRESS, 
                     Y'ADDRESS) /= OK then
            raise X_FAILURE;
        end if;
    end X_GET_CURRENT_POINTER_POSITION;


    ------------------------------------------------------------------
    procedure X_BELL (REPEAT : in BELL_REPEAT) is
    begin
        if not INITIALISED or else
         X_BELL (INTEGER(REPEAT)) /= OK then
            raise X_FAILURE;
        end if;
        X_FLUSH;
    end X_BELL;


    ------------------------------------------------------------------
    procedure X_BLINK_ALTERNATE is
        DUMMY : RESULT;
    begin
        if not INITIALISED then
            raise X_FAILURE;
        end if;
        -- Dont't care of the result;
        DUMMY := X_BLINK;
    end X_BLINK_ALTERNATE;


    ------------------------------------------------------------------
    procedure X_STOP_BLINKING_TASK is
        DUMMY : RESULT;
    begin
        if not INITIALISED then
            raise X_FAILURE;
        end if;
        -- Dont't care of the result;
        DUMMY := X_STOP_BLINKING;
    end X_STOP_BLINKING_TASK;  

end X_MNG;

