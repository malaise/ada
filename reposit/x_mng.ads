with SYSTEM;

 
package X_MNG is
 
  ----- TYPES -----
  type    LINE       is private;
 
  type BYTE is new NATURAL range 0 .. 255;
  for BYTE'SIZE use 8;

  subtype COLOR       is NATURAL range 0 .. 14;
  subtype FONT        is NATURAL range 0 .. 1;
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

  type BUTTON_LIST is (LEFT, MIDDLE, RIGHT);
    
  -- mask of socket ids on which select events
  type EXTERNAL_MASK is new INTEGER;

  type EVENT_KIND is (DISCARD, TID_RELEASE, TID_PRESS, KEYBOARD, REFRESH);
 
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

  -- Clears a line
  -- The line_id is the token, previously given by open_line
  -- The character attributes are lost.
  -- A flush is done
  procedure X_CLEAR_LINE(LINE_ID : in LINE);

  -- Flushes all the outputs of all the lines on the host
  procedure X_FLUSH;

  ----- PUT and ATTRIBUTE MANAGEMENT -----
 
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

  -- Sets the position for a further put in the same window
  -- The row and column are in characters, relative to the window
  procedure X_MOVE(LINE_ID     : in LINE;
                   ROW, COLUMN : in NATURAL);
 
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
  procedure X_PUT_CHAR(LINE_ID : in LINE; CAR : in CHARACTER);
 
  -- Writes a char with the attributes previously set
  -- The line_id is the token, previously given by open_line
  -- The character is the one to be written
  procedure X_PUT_CHAR(LINE_ID : in LINE; CAR : in BYTE);
 
  -- Writes a char with the attributes previously set
  -- The line_id is the token, previously given by open_line
  -- The character is the one to be written
  procedure X_OVERWRITE_CHAR(LINE_ID : in LINE; CAR : in BYTE);
 
  -- Writes a string with the attributes previously set
  -- The line_id is the token, previously given by open_line
  -- The string is the one to be written
  -- The output is flushed
  procedure X_PUT_STRING(LINE_ID : in LINE;
                         STR     : in STRING);

  -- Draws a rectangle (width * height) from current position
  --  with current foreground color.
  -- New position is updated to lower-right square of rectangle.
  procedure X_DRAW_AREA(LINE_ID : in LINE;
                        WIDTH, HEIGHT : in POSITIVE);

  -- This procedures rings a bell at 400Hz for 100ms and repeats it the number
  -- specified. 
  procedure X_BELL (REPEAT : in BELL_REPEAT);

  ----- EVENT MANAGEMENT -----
  -- Wait for some ms or until a X event is availble
  -- If timeout is < 0, infinite wait
  -- The remaining time is set
  procedure X_SELECT (TIMEOUT_MS : in out INTEGER; X_EVENT : out BOOLEAN);

  -- Processes a X Event (TID or Keyboard or other)
  -- line_id is the line on which the event has occured
  -- kind is KEYBOARD or TID, or DISCARD
  -- NEXT indicates if there is another event pendinig in X'queue
  procedure X_PROCESS_EVENT(LINE_ID : out LINE; 
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

  -- Draw a rectangle with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The X and Y are coordinates of the 4 segments
  procedure X_DRAW_RECTANGLE(LINE_ID        : in LINE;
                             X1, Y1, X2, Y2 : in NATURAL);

  -- Get current position of pointer (independant from events)
  -- The line_id is the token, previously given by open_line
  -- The X and Y are coordinates of the pointer
  procedure X_GET_CURRENT_POINTER_POSITION(LINE_ID : in LINE;
                                           X, Y    : out INTEGER);

  ----- BLINK MANAGEMENT -----

  -- This procedure stops the task which, internaly to x_vdu_mng,
  --  manages the blinking of text.
  -- If a process calls this procedure, no blinking of text will
  --  be impliciptly assumed any more, and the process must
  --  call X_BLINK_ALTERNATE regulary;
  -- No effect on a IMAGE line.
  procedure X_STOP_BLINKING_TASK;


  -- This procedure hides the the text which has blink attribute
  --  (gives the same ink as paper) or restores it, alternatively.
  -- It has no effect UNLESS the internal task has been
  --  stoped with X_STOP_BLINKING_TASK. 
  -- In this case, it has to be called twice a second to provide
  --  blinking effect.
  -- No effect on a IMAGE line.
  procedure X_BLINK_ALTERNATE;

 
private
 
  for EVENT_KIND'SIZE use 32;
  for EVENT_KIND use (
   DISCARD     => 0, 
   TID_RELEASE => 1, 
   TID_PRESS   => 2, 
   KEYBOARD    => 3,
   REFRESH     => 4);
 
  subtype LINE_FOR_C is SYSTEM.ADDRESS;
 
  type X_REFERENCE is record
    -- NULL pointers for C at creation
    SERVER : LINE_FOR_C := SYSTEM.NULL_ADDRESS;
    WINDOW : LINE_FOR_C := SYSTEM.NULL_ADDRESS;
  end record;

  type LINE is record
    -- NULL pointer for C at creation
    NO : LINE_FOR_C := SYSTEM.NULL_ADDRESS;
  end record;

end X_MNG;

