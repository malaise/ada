package X_MNG is
 
  ----- TYPES -----
  MAX_LINE_NUMBER : constant := 5;
  type LINE is private;
 
  type BYTE is new NATURAL range 0 .. 255;
  for BYTE'SIZE use 8;

  subtype COLOR       is NATURAL range 0 .. 14;
  subtype FONT        is NATURAL range 0 .. 3;
  subtype BELL_REPEAT is POSITIVE range 1 .. 5;

  type LINE_DEFINITION_REC is record
    SCREEN_ID               : INTEGER;
    ROW, COLUMN             : NATURAL;
    HEIGHT, WIDTH           : NATURAL;
    BACKGROUND, BORDER      : COLOR;
    NO_FONT                 : FONT;
  end record;

  -- keyboard codes for 1 key 
  KBD_MAX_CODE : constant := 6;
  subtype KBD_INDEX_CODE is INTEGER range 1 .. KBD_MAX_CODE;
  type KBD_ARRAY is array (KBD_INDEX_CODE) of BYTE;

  type KBD_TAB_CODE is record
    TAB : KBD_ARRAY;
    NBRE : KBD_INDEX_CODE;
  end record;

  -- For X_DRAW_POINTS
  type BYTE_ARRAY is array (POSITIVE range <>) of BYTE;

  -- Mouse buttons
  type BUTTON_LIST is (NONE, LEFT, MIDDLE, RIGHT);

  -- Returned events (see timers)
  type EVENT_KIND is (DISCARD, TID_RELEASE, TID_PRESS, KEYBOARD, REFRESH,
                      TID_MOTION, FD_EVENT, TIMER_EVENT);

  -- Fd management
  type FILE_DESC is new NATURAL;
  type FD_CALLBACK is access procedure (FD : in FILE_DESC);
 
  ----- EXCEPTIONS -----

  X_FAILURE : exception;
 
  ----- LINE MANAGEMENT -----

  -- Initialise connection to X server on a host
  --  this call should be done only once, and before any other call
  procedure X_INITIALISE (SERVER_NAME    : in STRING);

  -- Opens a line on the host
  -- screen_id is integer, (a negative value for the default screen)
  -- row and column are the coordonates of the upper-left corner of the
  --  window in the screen (in characters)
  -- heigh and width are the dimention of the window in characters
  -- no_font can be 0 for (8x13) character, or 1 for (9x15)
  -- background and border are colors for the window
  -- line_id is the returned value (token for every further operation)
  procedure X_OPEN_LINE(LINE_DEFINITION : in LINE_DEFINITION_REC;
                        LINE_ID         : in out LINE);
  
  -- Closes a line
  -- The line_id is the token, previously given by open_line
  procedure X_CLOSE_LINE(LINE_ID : in out LINE);

  -- Set the name of a line
  -- This name will be displayed by the window manager if possible
  -- The line_id is the token, previously given by open_line
  procedure X_SET_LINE_NAME (LINE_ID : in LINE;
                             LINE_NAME : in STRING);

  -- Flushes all the outputs of all the lines on the host
  procedure X_FLUSH(LINE_ID : in LINE);

  -- Clears a line
  -- The line_id is the token, previously given by open_line
  -- The character attributes are lost.
  -- A flush is done
  procedure X_CLEAR_LINE(LINE_ID : in LINE);

  ----- PUT and ATTRIBUTE MANAGEMENT -----

  -- Sets the attributes for a further put in the same window
  -- The line_id is the token, previously given by open_line
  -- The paper and ink are color numbers (from 0 to 7)
  -- The attributes are True or False
  procedure X_SET_ATTRIBUTES(LINE_ID     : in LINE;
                             PAPER, INK  : in COLOR;
                             SUPERBRIGHT : in BOOLEAN := FALSE;
                             UNDERLINE   : in BOOLEAN := FALSE;
                             BLINK       : in BOOLEAN := FALSE;
                             INVERSE     : in BOOLEAN := FALSE);

  -- Sets the xor mode or a further put in the same window
  -- The line_id is the token, previously given by open_line
  -- if XOR_MORE is set, all further puts and drawings will be in xor
  procedure X_SET_XOR_MODE(LINE_ID     : in LINE;
                           XOR_MODE    : in BOOLEAN);

 
  -- Writes a char with the attributes previously set
  -- The line_id is the token, previously given by open_line
  -- The character is the one to be written
  procedure X_PUT_CHAR(LINE_ID : in LINE;
                       CAR : in CHARACTER;
                       ROW, COLUMN : in NATURAL);
 
  -- Writes a char with the attributes previously set
  -- The line_id is the token, previously given by open_line
  -- The character is the one to be written
  procedure X_PUT_CHAR(LINE_ID : in LINE;
                       CAR : in BYTE;
                       ROW, COLUMN : in NATURAL);
 
  -- Writes a char with the attributes previously set
  -- The line_id is the token, previously given by open_line
  -- The character is the one to be written
  procedure X_OVERWRITE_CHAR(LINE_ID : in LINE;
                             CAR : in BYTE;
                             ROW, COLUMN : in NATURAL);
 
  -- Writes a string with the attributes previously set
  --  at a specified position
  -- The line_id is the token, previously given by open_line
  -- The string is the one to be written
  -- The output is flushed
  procedure X_PUT_STRING(LINE_ID     : in LINE;
                         STR         : in STRING;
                         ROW, COLUMN : in NATURAL);

  -- Writes a char on a line with specified characteristics
  -- The line_id is the token, previously given by open_line
  -- The row and column are in characters, relative to the window
  -- The character is the one to be written
  -- The paper and ink are color numbers (from 0 to 7)
  -- The attributes are True or False
  procedure X_PUT_CHAR_ATTRIBUTES(LINE_ID     : in LINE;
                                  CAR         : in CHARACTER;
                                  ROW, COLUMN : in NATURAL;
                                  PAPER, INK  : in COLOR;
                                  SUPERBRIGHT : in BOOLEAN := FALSE;
                                  UNDERLINE   : in BOOLEAN := FALSE;
                                  BLINK       : in BOOLEAN := FALSE;
                                  INVERSE     : in BOOLEAN := FALSE);

  -- Draws a rectangle (width * height) at position 
  --  with current foreground color.
  -- New position is updated to lower-right square of rectangle.
  procedure X_DRAW_AREA(LINE_ID : in LINE;
                        WIDTH, HEIGHT : in POSITIVE;
                        ROW, COLUMN : in NATURAL);

  ----- GRAPHIC MANAGEMENT -----

  -- Writes a char on a line with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The current row and column are not affected
  -- The character is the one to be written
  -- The X and Y are position of the character
  procedure X_PUT_CHAR_PIXELS(LINE_ID     : in LINE;
                              CAR         : in BYTE;
                              X, Y        : in NATURAL);

  -- Get graphic info of window
  -- The line_id is the token, previously given by open_line
  -- Window size in pixels
  -- Font size in pixels
  -- Height offset from top of font
  procedure X_GET_GRAPHIC_CHARACTERISTICS(LINE_ID       : in LINE;
                                          WINDOW_WIDTH  : out NATURAL;
                                          WINDOW_HEIGHT : out NATURAL;
                                          FONT_WIDTH    : out NATURAL;
                                          FONT_HEIGHT   : out NATURAL;
                                          FONT_OFFSET   : out NATURAL);

  -- Draw a point with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The X and Y are position of the point
  procedure X_DRAW_POINT(LINE_ID       : in LINE;
                         X, Y          : in NATURAL);

  -- Draw a line with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The X and Y are coordinates of the 2 points
  procedure X_DRAW_LINE(LINE_ID        : in LINE;
                        X1, Y1, X2, Y2 : in NATURAL);

  -- Draw a rectangle (border) with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The X and Y are coordinates of the 4 corners
  procedure X_DRAW_RECTANGLE(LINE_ID        : in LINE;
                             X1, Y1, X2, Y2 : in NATURAL);

  -- Fill a rectangle with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The X and Y are coordinates of the 4 corners
  procedure X_FILL_RECTANGLE(LINE_ID        : in LINE;
                             X1, Y1, X2, Y2 : in NATURAL);

  -- Get current position of pointer (independant from events)
  -- The line_id is the token, previously given by open_line
  -- The X and Y are coordinates of the pointer
  procedure X_GET_CURRENT_POINTER_POSITION(LINE_ID : in LINE;
                                           X, Y    : out INTEGER);

  -- Draw points in a rectangle, starting at X1, Y1 and of width * height pixels
  -- The points array has to be width * height and contains a list of Zero (no put)
  --  or not Zero (put)
  procedure X_DRAW_POINTS(LINE_ID       : in LINE;
                          X, Y          : in NATURAL;
                          WIDTH, HEIGHT : in NATURAL; 
                          POINTS        : in BYTE_ARRAY);


  -- Set mouse cursor to cross (graphic) or arrow
  procedure X_SET_GRAPHIC_POINTER(LINE_ID : in LINE;
                                  GRAPHIC : in BOOLEAN);

  ----- EVENT MANAGEMENT -----
  -- Register a callback on a fd for read or write.
  -- The callback will be called (within X_SELECT) with one arg: the fd
  procedure X_ADD_CALLBACK (FD : in FILE_DESC; READ : in BOOLEAN;
                            CALLBACK : in FD_CALLBACK);
  -- Unregister the callback from a fd
  procedure X_DEL_CALLBACK (FD : in FILE_DESC; READ : in BOOLEAN);

  -- Wait for some ms. Initialisation MUST NOT HAVE BEEN DONE
  --  (or X_FAILURE will be raised)
  -- Return True if an event has occured
  function SELECT_NO_X (TIMEOUT_MS : INTEGER) return BOOLEAN;

  -- Wait for some ms or until a X event is availble
  -- If timeout is < 0, infinite wait
  -- The remaining time is set
  procedure X_SELECT (LINE_ID : in LINE;
                      TIMEOUT_MS : in out INTEGER; X_EVENT : out BOOLEAN);

  -- Processes a X Event (TID or Keyboard or other)
  -- kind is KEYBOARD or TID (PRESS or RELEASE or MOTION), or DISCARD
  --  or REFRESH or FD_EVENT
  -- NEXT indicates if there is another event pendinig in X'queue
  procedure X_PROCESS_EVENT(LINE_ID : in LINE; 
                            KIND : out EVENT_KIND;
                            NEXT : out BOOLEAN);
 
  -- Reads the position on TID in ROW/COL or X/Y
  -- The line_id must be the one given by wait_event
  -- Button can be left, middle or right
  -- row and column are the position of the "finger" on the TID
  --  in row/col or X/Y(pixels)
  procedure X_READ_TID(LINE_ID : in LINE; ROW_COL : in BOOLEAN;
                       BUTTON : out BUTTON_LIST;
                       ROW, COLUMN : out INTEGER);
 
  -- Reads a key of a sequence
  -- The line_id must be the one given by wait_event
  -- key is the byte read
  procedure X_READ_KEY(LINE_ID : in LINE; KEY : out KBD_TAB_CODE);

  -- Enable disable cursor motion events
  -- The line_id must be the one given by wait_event
  procedure X_ENABLE_MOTION_EVENTS (LINE_ID : in LINE; MOTION_ENABLE : in BOOLEAN);

  ----- BLINK MANAGEMENT -----

  -- This procedure hides the the text which has blink attribute
  --  (gives the same ink as paper) or restores it, alternatively.
  -- It has no effect UNLESS the internal task has been
  --  stoped with X_STOP_BLINKING_TASK. 
  -- In this case, it has to be called twice a second to provide
  --  blinking effect.
  procedure X_BLINK_ALTERNATE(LINE_ID : in LINE);

  -- This procedure stops the task which, internaly to x_vdu_mng,
  --  manages the blinking of text.
  -- If a process calls this procedure, no blinking of text will
  --  be impliciptly assumed any more, and the process must
  --  call X_BLINK_ALTERNATE regulary;
  procedure X_STOP_BLINKING_TASK(LINE_ID : in LINE);

  -- This procedure restarts  the task which, internaly to x_vdu_mng,
  --  manages the blinking of text.
  -- The task should be stopped when this call is done
  procedure X_START_BLINKING_TASK(LINE_ID : in LINE);

  -- This procedures rings a bell at 400Hz for 100ms and repeats it the number
  -- specified. 
  procedure X_BELL (LINE_ID : in LINE; REPEAT : in BELL_REPEAT);

 
private
 
  for EVENT_KIND'SIZE use 32;
  for EVENT_KIND use (
   DISCARD     => 0, 
   TID_RELEASE => 1, 
   TID_PRESS   => 2, 
   KEYBOARD    => 3,
   REFRESH     => 4,
   TID_MOTION  => 5,
   FD_EVENT    => 9,
   TIMER_EVENT => 10);
 
 subtype LINE_RANGE is NATURAL range 0 .. MAX_LINE_NUMBER;
  subtype CLIENT_RANGE is POSITIVE range 1 .. MAX_LINE_NUMBER;
  NO_CLIENT_NO : constant LINE_RANGE := 0;

  type LINE is record
    -- NO_CLIENT
    NO : LINE_RANGE := NO_CLIENT_NO;
  end record;

  NO_CLIENT : constant LINE := (NO => NO_CLIENT_NO);

end X_MNG;

