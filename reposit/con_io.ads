with CALENDAR;
package CON_IO is

  ROW_RANGE_FIRST : constant NATURAL := 0;
  ROW_RANGE_LAST  : constant NATURAL := 24;
  COL_RANGE_FIRST : constant NATURAL := 0;
  COL_RANGE_LAST  : constant NATURAL := 79;

  -- text column and row
  subtype ROW_RANGE is NATURAL range ROW_RANGE_FIRST .. ROW_RANGE_LAST;
  subtype COL_RANGE is NATURAL range COL_RANGE_FIRST .. COL_RANGE_LAST;

  -- a square on the screen
  type SQUARE is record
      ROW : ROW_RANGE;
      COL : COL_RANGE;
    end record;

  -- upper left square
  HOME : constant SQUARE := (ROW => ROW_RANGE'FIRST, COL => COL_RANGE'FIRST);

  -- list of possible colors
  type COLORS is (CURRENT, BLACK, BLUE, GREEN, CYAN, RED, MAGENTA, BROWN,
    LIGHT_GRAY, DARK_GRAY, LIGHT_BLUE, LIGHT_GREEN, ORANGE,
    YELLOW, WHITE);
  subtype BASIC_COLORS is COLORS range CURRENT .. LIGHT_GRAY;

  -- list of colors for outputs
  subtype EFFECTIVE_COLORS is COLORS range BLACK .. COLORS'LAST;
  subtype EFFECTIVE_BASIC_COLORS is BASIC_COLORS range BLACK .. BASIC_COLORS
    'LAST;

  -- list of possible blink states of foreground
  type BLINK_STATS is (CURRENT, BLINK, NOT_BLINK);
  subtype EFFECTIVE_BLINK_STATS is BLINK_STATS range BLINK .. NOT_BLINK;

  -- standard attributes when reset
  DEFAULT_FOREGROUND : constant EFFECTIVE_COLORS := LIGHT_GRAY;
  DEFAULT_BACKGROUND : constant EFFECTIVE_BASIC_COLORS := BLACK;
  DEFAULT_BLINK_STAT : constant EFFECTIVE_BLINK_STATS := NOT_BLINK;


  type WINDOW is limited private;

  -- Has to be called to initialize con_io.
  -- Should be called prior to any con_io action
  procedure INIT;

  -- the window which is screen. Not a constant because of Meridians'bug.
  -- always open
  function SCREEN return WINDOW;

  -- clear screen, and reset keyboard
  procedure RESET_TERM; 

  -- flushes data to X
  procedure FLUSH;

  -- set / get colors
  procedure SET_FOREGROUND (FOREGROUND : in COLORS := CURRENT;
                            BLINK_STAT : in BLINK_STATS := CURRENT;
                            NAME       : in WINDOW := SCREEN);

  procedure SET_BACKGROUND (BACKGROUND : in BASIC_COLORS := CURRENT;
                            NAME       : in WINDOW := SCREEN);

  function GET_FOREGROUND (NAME : WINDOW := SCREEN) return EFFECTIVE_COLORS;
  function GET_BACKGROUND (NAME : WINDOW := SCREEN) return
    EFFECTIVE_BASIC_COLORS;
  function GET_BLINK_STAT(NAME : WINDOW := SCREEN) return
    EFFECTIVE_BLINK_STATS;

  -- get UPPER_LEFT / LOWER_RIGHT absolute coordinates of a window
  function GET_ABSOLUTE_UPPER_LEFT  (NAME : WINDOW) return SQUARE;
  function GET_ABSOLUTE_LOWER_RIGHT (NAME : WINDOW) return SQUARE;

  -- get LOWER_RIGHT relative coordinates of a window (UPPER_LEFT is (0, 0)).
  function GET_RELATIVE_LOWER_RIGHT (NAME : WINDOW) return SQUARE;


  -- open a window (screen is already open)
  procedure OPEN (NAME                    : in out WINDOW;
                  UPPER_LEFT, LOWER_RIGHT : in SQUARE);
  function IS_OPEN (NAME : WINDOW) return BOOLEAN;

  -- TRUE if the absolute square (relative to screen) is in the window.
  -- FALSE otherwise
  function IN_WINDOW (ABSOLUTE_SQUARE : SQUARE;
                      NAME            : WINDOW) return BOOLEAN;

  -- Returns the relative square (relative to window), being the same
  --  physical position as the absolute square (relative to screen).
  -- May raise INVALID_SQUARE if the absolute position is not in window.
  function TO_RELATIVE (ABSOLUTE_SQUARE : SQUARE;
                        NAME            : WINDOW) return SQUARE;

  -- Returns the absolute square (in screen) corresponding to the relative
  --  square in the window
  -- May raise INVALID_SQUARE if the relative square is not in window
  function TO_ABSOLUTE (RELATIVE_SQUARE : SQUARE;
                        NAME            : WINDOW) return SQUARE;

  -- draw a frame around a window (must be open)
  -- the frame is OUTSIDE the window (so no frame for screen)
  -- FRAME_IMPOSSIBLE if part of the frame is not in the screen
  procedure FRAME (BLINK : in BOOLEAN := FALSE;
                   NAME : in WINDOW);
  procedure CLEAR_FRAME (NAME : in WINDOW);

  -- clear window and move to home
  procedure CLEAR (NAME : in WINDOW := SCREEN);

  -- make window re-usable (have to re_open it)
  -- screen cannot be closed
  procedure CLOSE (NAME : in out WINDOW);

  -- move cursor for use with put or get. Position is relativ to window.
  procedure MOVE (POSITION : in SQUARE := HOME;
                  NAME     : in WINDOW := SCREEN);
  procedure MOVE (ROW  : in ROW_RANGE;
                  COL  : in COL_RANGE;
                  NAME : in WINDOW := SCREEN);
  function POSITION (NAME : WINDOW := SCREEN) return SQUARE;

  -- Writes a character at the current cursor position and with the
  --  curent attributes. Position can be set by using move.
  -- CR is the only special ASCII character which is interpreted.
  -- If not MOVE, the cursor position is not updated (CR would be ignored then)
  procedure PUT (C          : in CHARACTER;
                 NAME       : in WINDOW := SCREEN;
                 FOREGROUND : in COLORS := CURRENT;
                 BLINK_STAT : in BLINK_STATS := CURRENT;
                 BACKGROUND : in BASIC_COLORS := CURRENT;
                 MOVE       : in BOOLEAN := TRUE);

  -- Idem with a string
  -- If not MOVE, the cursor position is not updated
  --  (last CR would be ignored then)
  procedure PUT (S          : in STRING;
                 NAME       : in WINDOW := SCREEN;
                 FOREGROUND : in COLORS := CURRENT;
                 BLINK_STAT : in BLINK_STATS := CURRENT;
                 BACKGROUND : in BASIC_COLORS := CURRENT;
                 MOVE       : in BOOLEAN := TRUE);

  -- Idem but appends a CR
  procedure PUT_LINE (S          : in STRING;
                      NAME       : in WINDOW := SCREEN;
                      FOREGROUND : in COLORS := CURRENT;
                      BLINK_STAT : in BLINK_STATS := CURRENT;
                      BACKGROUND : in BASIC_COLORS := CURRENT);

  -- same than PUT(CHAR) not MOVE, but allows semi graphic characters
  subtype INT_CHAR is NATURAL range 0 .. 255;
  procedure PUT_INT (INT        : in INT_CHAR;
                     NAME       : in WINDOW := SCREEN;
                     FOREGROUND : in COLORS := CURRENT;
                     BLINK_STAT : in BLINK_STATS := CURRENT;
                     BACKGROUND : in BASIC_COLORS := CURRENT);

  -- Puts CR
  procedure NEW_LINE (NAME   : in WINDOW := SCREEN;
                      NUMBER : in POSITIVE := 1);

  -- Take first character of keyboard buffer (no echo)
  procedure PAUSE;

  -- Gives first key code of keyboard buffer, (waits if it is empty) no echo
  -- if not is_char, key is the key code. If is_char, key is the ascii code.
  -- CARE : is_char can be set and key not compatible with ADA characters.
  -- KEY = 0 and IS_CHAR and other flags FALSE indicate refresh has to be done
  procedure GET_KEY (KEY     : out NATURAL;
                     IS_CHAR : out BOOLEAN;
                     CTRL    : out BOOLEAN;
                     SHIFT   : out BOOLEAN);

  -- Gets first character (echo)
  function GET (NAME : WINDOW := SCREEN) return CHARACTER;

  -- Gets a string of at most width characters
  -- The string must be short enought to be put in 1 line at current position
  --  in the window.
  -- The current cursor position is updated by the call
  -- The arrows, Insert, suppr, backspace, Home, End, PageUp and PageDown
  --  are managed!
  -- The get ends if an Up/Down arrow, Page Up/Down, Return(CR), Escape,
  --  Tab or Shift Tab is pressed or if the cursor leaves the field
  --  (on character input or Right/Left arrow), on mouse click or release,
  --  on time_out expiration or on CtrlC/CtrlBreak
  -- Mouse_button event can only be generated if the mouse cursor is shown
  -- Break event can only be generated if break_server is installed
  --  and breaks are inhibitted
  -- The returned string ends at last significant digit (gaps with spaces),
  --  tailling spaces are parsed out and last is the index in STR of
  --  the last non-space character
  -- The in and out positions mean 1 for 1st character ..., not indexes
  --  of STR
  -- If STR'length is 0 then last=0 and stat is significant (full if normal
  --  character), but pos_out is not significant.
  -- Note that is STR'LENGHT is 0, the cursor is hidden
  type CURS_MVT is (UP, DOWN, PGUP, PGDOWN, LEFT, RIGHT, FULL, TAB, STAB,
                    RET, ESC, BREAK, MOUSE_BUTTON, TIMEOUT, REFRESH);
  procedure GET (STR        : out STRING;
                 LAST       : out NATURAL;
                 STAT       : out CURS_MVT;
                 POS        : out POSITIVE;
                 INSERT     : out BOOLEAN;
                 NAME       : in WINDOW := SCREEN;
                 FOREGROUND : in COLORS := CURRENT;
                 BLINK_STAT : in BLINK_STATS := CURRENT;
                 BACKGROUND : in BASIC_COLORS := CURRENT;
                 TIME_OUT   : in DURATION :=  -1.0);

  -- Idem but the get is initialised with the initial content of the string
  --  and cursor's initial location can be set
  procedure PUT_THEN_GET (STR        : in out STRING;
                          LAST       : out NATURAL;
                          STAT       : out CURS_MVT;
                          POS        : in out POSITIVE;
                          INSERT     : in out BOOLEAN;
                          NAME       : in WINDOW := SCREEN;
                          FOREGROUND : in COLORS := CURRENT;
                          BLINK_STAT : in BLINK_STATS := CURRENT;
                          BACKGROUND : in BASIC_COLORS := CURRENT;
                          TIME_OUT   : in DURATION :=  -1.0);

  -- Get_key_time can return if key pressed (ESC event),
  -- mouse action, refresh or timeout
  subtype EVENT_LIST is CURS_MVT range ESC .. REFRESH;

  -- check if a key is available, or another event, until a certain time. 
  -- ESC means any key
  procedure GET_KEY_TIME (LAST_TIME     : in CALENDAR.TIME;
                          INFINITE_TIME : in BOOLEAN;
                          CHECK_BREAK   : in BOOLEAN;
                          EVENT         : out EVENT_LIST;
                          KEY           : out NATURAL;
                          IS_CHAR       : out BOOLEAN;
                          CTRL          : out BOOLEAN;
                          SHIFT         : out BOOLEAN);

  -- Button status
  type MOUSE_BUTTON_STATUS_LIST is (PRESSED, RELEASED);
  -- List of button
  type MOUSE_BUTTON_LIST is (LEFT, MIDDLE, RIGHT);
  -- Mouse status
  type MOUSE_EVENT_REC is record
    VALID : BOOLEAN;
    BUTTON : MOUSE_BUTTON_LIST;
    STATUS : MOUSE_BUTTON_STATUS_LIST;
    ROW : ROW_RANGE;
    COL : COL_RANGE;
  end record;

  -- Get a mouse event. If valid is FALSE, it means that a release
  -- has occured outside the screen, then only BUTTON and status
  -- are significant
  procedure GET_MOUSE_EVENT (MOUSE_EVENT : out MOUSE_EVENT_REC);

  -- failure when allocating data for window
  OPEN_FAILURE        : exception;
  -- position out of screen (or out of window)
  INVALID_SQUARE      : exception;
  -- window close to screen limit
  FRAME_IMPOSSIBLE    : exception;
  -- self explanatory
  WINDOW_NOT_OPEN     : exception;
  WINDOW_ALREADY_OPEN : exception;
  -- String lenght incompatible with current position and window width
  --  for get and put_then get
  STRING_TOO_LONG     : exception;

private

  type WINDOW_DATA is
    record
      UPPER_LEFT         : SQUARE;
      LOWER_RIGHT        : SQUARE;
      CURRENT_POS        : SQUARE := HOME;
      CURRENT_FOREGROUND : EFFECTIVE_COLORS;
      CURRENT_BACKGROUND : EFFECTIVE_BASIC_COLORS;
      CURRENT_BLINK_STAT : EFFECTIVE_BLINK_STATS;
    end record;

  type WINDOW is access WINDOW_DATA;

  SCREEN_DATA   : constant WINDOW_DATA := (
    UPPER_LEFT         => (ROW => ROW_RANGE_FIRST, COL => COL_RANGE_FIRST),
    LOWER_RIGHT        => (ROW => ROW_RANGE_LAST,  COL => COL_RANGE_LAST),
    CURRENT_POS        => HOME,
    CURRENT_FOREGROUND => DEFAULT_FOREGROUND,
    CURRENT_BACKGROUND => DEFAULT_BACKGROUND,
    CURRENT_BLINK_STAT => DEFAULT_BLINK_STAT);

  SCREEN_WINDOW : constant WINDOW := new WINDOW_DATA'(SCREEN_DATA);


end CON_IO;
