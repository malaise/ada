with ARGUMENT, DYN_DATA, X_MNG, SYS_CALLS;
package body GENERIC_CON_IO is

  X_INIT_DONE : BOOLEAN := FALSE;

  procedure X_INITIALISE is
  begin
    if not X_INIT_DONE then
      X_MNG.X_INITIALISE ("");
      X_INIT_DONE := TRUE;
    end if;
  end X_INITIALISE;

  package body ONE_CON_IO is

    FONT_ENV_NAME : constant STRING := "CON_IO_FONT";
    FONT_ENV_SMALL : constant STRING := "small";
    FONT_ENV_LARGE : constant STRING := "large";

    ID : X_MNG.LINE;
    LINE_DEF : constant X_MNG.LINE_DEFINITION_REC := (
      SCREEN_ID => 0,
      ROW => 1,
      COLUMN => 1,
      HEIGHT => ROW_RANGE_LAST - ROW_RANGE_FIRST + 1,
      WIDTH => COL_RANGE_LAST - COL_RANGE_FIRST + 1,
      BACKGROUND => 0,
      BORDER => 0,
      NO_FONT => FONT_NO);

    type BORDER_LIST is (ERASE, SIMPLE, BLINK);

    X_EVENT_WAITING : BOOLEAN;
    MOTION_ENABLING : BOOLEAN;

    -- DISCARD or TID_xxx
    MOUSE_STATUS : X_MNG.EVENT_KIND;

    LINE_FOREGROUND : EFFECTIVE_COLORS := DEFAULT_FOREGROUND;
    LINE_BACKGROUND : EFFECTIVE_BASIC_COLORS := DEFAULT_BACKGROUND;
    LINE_BLINK_STAT : EFFECTIVE_BLINK_STATS := DEFAULT_BLINK_STAT;
    LINE_XOR_MODE   : EFFECTIVE_XOR_MODES := DEFAULT_XOR_MODE;

    X_MAX : GRAPHICS.X_RANGE;
    Y_MAX : GRAPHICS.Y_RANGE;
    FONT_WIDTH  : NATURAL;
    FONT_HEIGHT : NATURAL;
    FONT_OFFSET : NATURAL;

    -- Dynamic alloc/desaloc of windows
    package DYN_WIN is new DYN_DATA(WINDOW_DATA, WINDOW);

    procedure SET_ATTRIBUTES (FOREGROUND : in EFFECTIVE_COLORS;
                              BLINK_STAT : in EFFECTIVE_BLINK_STATS;
                              BACKGROUND : in EFFECTIVE_COLORS;
                              XOR_MODE   : in EFFECTIVE_XOR_MODES;
                              FORCED     : in BOOLEAN := FALSE);

    INIT_DONE : BOOLEAN := FALSE;
    procedure INIT is
      LINE : X_MNG.LINE_DEFINITION_REC := LINE_DEF;
      ENV_SET, ENV_TRU : BOOLEAN;
      ENV_STR : STRING (1 .. FONT_ENV_SMALL'LENGTH);
      ENV_LEN : NATURAL;
    begin
      if INIT_DONE then
        return;
      end if;
      X_INITIALISE;
      SYS_CALLS.GETENV (FONT_ENV_NAME, ENV_SET, ENV_TRU, ENV_STR, ENV_LEN);
      if ENV_SET and then not ENV_TRU then
        if ENV_STR (1 .. ENV_LEN) = FONT_ENV_SMALL
        and then FONT_NO /= FONT_NO_RANGE'FIRST then
          LINE.NO_FONT := FONT_NO - 1;
        elsif ENV_STR (1 .. ENV_LEN) = FONT_ENV_LARGE
        and then FONT_NO /= FONT_NO_RANGE'LAST then
          LINE.NO_FONT := FONT_NO + 1;
        end if;
      end if;
      X_MNG.X_OPEN_LINE (LINE, ID);
      X_MNG.X_SET_LINE_NAME (ID, ARGUMENT.GET_PROGRAM_NAME);
      MOUSE_STATUS := X_MNG.DISCARD;
      X_EVENT_WAITING := TRUE;
      MOTION_ENABLING := FALSE;
      INIT_DONE := TRUE;
      X_MNG.X_GET_GRAPHIC_CHARACTERISTICS(ID, X_MAX, Y_MAX,
            FONT_WIDTH, FONT_HEIGHT, FONT_OFFSET);
      -- Max is width - 1 so that range is 0 .. max
      X_MAX := X_MAX - 1;
      Y_MAX := Y_MAX - 1;
      SET_ATTRIBUTES (DEFAULT_FOREGROUND, DEFAULT_BLINK_STAT, DEFAULT_BACKGROUND,
                      DEFAULT_XOR_MODE, FORCED => TRUE);
      FLUSH;
    exception
      when others =>
        raise INIT_FAILURE;
    end INIT;

    procedure DESTROY is
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      X_MNG.X_CLOSE_LINE(ID);
      INIT_DONE := FALSE;
    end DESTROY;

    -- screen characteristics
    function SCREEN return WINDOW is
    begin
      return SCREEN_WINDOW;
    end SCREEN;

    -- reset screen, windows and keyboard
    procedure RESET_TERM is
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      X_MNG.X_CLEAR_LINE (ID);
      -- Set current attributes in cache
      SET_ATTRIBUTES (DEFAULT_FOREGROUND, DEFAULT_BLINK_STAT, DEFAULT_BACKGROUND,
                      DEFAULT_XOR_MODE, FORCED => TRUE);
    end RESET_TERM;

    -- flushes X
    procedure FLUSH is
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      X_MNG.X_FLUSH (ID);
    end FLUSH;

    -- set / get colors
    procedure SET_FOREGROUND (FOREGROUND : in COLORS      := CURRENT;
                              BLINK_STAT : in BLINK_STATS := CURRENT;
                              NAME       : in WINDOW      := SCREEN) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      if FOREGROUND /= CURRENT then
        NAME.CURRENT_FOREGROUND := FOREGROUND;
      end if;
      if BLINK_STAT /= CURRENT then
        NAME.CURRENT_BLINK_STAT := BLINK_STAT;
      end if;
    end SET_FOREGROUND;

    procedure SET_BACKGROUND (BACKGROUND : in BASIC_COLORS := CURRENT;
                              NAME       : in WINDOW       := SCREEN) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      if BACKGROUND /= CURRENT then
        NAME.CURRENT_BACKGROUND := BACKGROUND;
      end if;
    end SET_BACKGROUND;

    function GET_FOREGROUND (NAME : WINDOW := SCREEN)
      return EFFECTIVE_COLORS is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      return NAME.CURRENT_FOREGROUND;
    end GET_FOREGROUND;

    function GET_BACKGROUND (NAME : WINDOW := SCREEN)
      return EFFECTIVE_BASIC_COLORS is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      return NAME.CURRENT_BACKGROUND;
    end GET_BACKGROUND;

    function GET_BLINK_STAT (NAME : WINDOW := SCREEN)
      return EFFECTIVE_BLINK_STATS is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      return NAME.CURRENT_BLINK_STAT;
    end GET_BLINK_STAT;

    procedure SET_XOR_MODE(XOR_MODE : in XOR_MODES := CURRENT;
                           NAME : in WINDOW := SCREEN) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      if XOR_MODE /= CURRENT then
        NAME.CURRENT_XOR_MODE := XOR_MODE;
      end if;
    end SET_XOR_MODE;

    function GET_XOR_MODE(NAME : WINDOW := SCREEN) return EFFECTIVE_XOR_MODES is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      return NAME.CURRENT_XOR_MODE;
    end GET_XOR_MODE;


    -- get UPPER_LEFT / LOWER_RIGHT absolute coordinates of a window
    function GET_ABSOLUTE_UPPER_LEFT (NAME : WINDOW) return SQUARE is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      return NAME.UPPER_LEFT;
    end GET_ABSOLUTE_UPPER_LEFT;

    function GET_ABSOLUTE_LOWER_RIGHT (NAME : WINDOW) return SQUARE is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      return NAME.UPPER_LEFT;
    end GET_ABSOLUTE_LOWER_RIGHT;

    -- get LOWER_RIGHT relative coordinates of a window (UPPER_LEFT is (0, 0)).
    function GET_RELATIVE_LOWER_RIGHT (NAME : WINDOW) return SQUARE is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      return (NAME.LOWER_RIGHT.ROW - NAME.UPPER_LEFT.ROW,
              NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL);
    end GET_RELATIVE_LOWER_RIGHT;

    -- open a window
    procedure OPEN (NAME                    : in out WINDOW;
                    UPPER_LEFT, LOWER_RIGHT : in SQUARE) is
    begin
      if NAME /= null then
        raise WINDOW_ALREADY_OPEN;
      end if;
      if UPPER_LEFT.ROW > LOWER_RIGHT.ROW or else
         UPPER_LEFT.COL > LOWER_RIGHT.COL then
        raise INVALID_SQUARE;
      end if;
      begin
        NAME := DYN_WIN.ALLOCATE (SCREEN_DATA);
      exception
        when STORAGE_ERROR =>
          raise OPEN_FAILURE;
      end;
      NAME.UPPER_LEFT := UPPER_LEFT;
      NAME.LOWER_RIGHT := LOWER_RIGHT;
      NAME.CURRENT_POS := HOME;
      NAME.CURRENT_FOREGROUND := DEFAULT_FOREGROUND;
      NAME.CURRENT_BACKGROUND := DEFAULT_BACKGROUND;
      NAME.CURRENT_BLINK_STAT := DEFAULT_BLINK_STAT;
      NAME.CURRENT_XOR_MODE   := DEFAULT_XOR_MODE;
    exception
      when CONSTRAINT_ERROR =>
        raise INVALID_SQUARE;
    end OPEN;

    function IS_OPEN (NAME : WINDOW) return BOOLEAN is
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      return NAME /= null;
    end IS_OPEN;

    -- TRUE if the absolute square (relative to screen) is in the window.
    -- FALSE otherwise
    function IN_WINDOW (ABSOLUTE_SQUARE : SQUARE;
                        NAME            : WINDOW) return BOOLEAN is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      return   ABSOLUTE_SQUARE.ROW >= NAME.UPPER_LEFT.ROW
      and then ABSOLUTE_SQUARE.ROW <= NAME.LOWER_RIGHT.ROW
      and then ABSOLUTE_SQUARE.COL >= NAME.UPPER_LEFT.COL
      and then ABSOLUTE_SQUARE.COL <= NAME.LOWER_RIGHT.COL;
    end IN_WINDOW;

    -- Returns the relative square (relative to window), being the same
    --  physical position as the absolute square (relative to screen).
    -- May raise INVALID_SQUARE if the absolute position is not in window.
    function TO_RELATIVE (ABSOLUTE_SQUARE : SQUARE;
                          NAME            : WINDOW) return SQUARE is
    begin
      if not IN_WINDOW(ABSOLUTE_SQUARE, NAME) then
        raise INVALID_SQUARE;
      end if;
      return (ROW => ABSOLUTE_SQUARE.ROW - NAME.UPPER_LEFT.ROW,
              COL => ABSOLUTE_SQUARE.COL - NAME.UPPER_LEFT.COL);
    end TO_RELATIVE;

    -- Returns the absolute square (in screen) corresponding to the relative
    --  square in the window
    -- May raise INVALID_SQUARE if the relative square is not in window
    function TO_ABSOLUTE (RELATIVE_SQUARE : SQUARE;
                          NAME            : WINDOW) return SQUARE is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      if (RELATIVE_SQUARE.ROW > NAME.LOWER_RIGHT.ROW-NAME.UPPER_LEFT.ROW) or else
         (RELATIVE_SQUARE.COL > NAME.LOWER_RIGHT.COL-NAME.UPPER_LEFT.COL) then
        raise INVALID_SQUARE;
      end if;
      return (ROW => RELATIVE_SQUARE.ROW + NAME.UPPER_LEFT.ROW, COL =>
        RELATIVE_SQUARE.COL + NAME.UPPER_LEFT.COL);
    end TO_ABSOLUTE;

    procedure SET_ATTRIBUTES (FOREGROUND : in EFFECTIVE_COLORS;
                              BLINK_STAT : in EFFECTIVE_BLINK_STATS;
                              BACKGROUND : in EFFECTIVE_COLORS;
                              XOR_MODE   : in EFFECTIVE_XOR_MODES;
                              FORCED     : in BOOLEAN := FALSE) is
    begin
      if FORCED or else FOREGROUND /= LINE_FOREGROUND
                or else BLINK_STAT /= LINE_BLINK_STAT
                or else BACKGROUND /= LINE_BACKGROUND then
        X_MNG.X_SET_ATTRIBUTES (ID, COLORS'POS(BACKGROUND) - 1,
                                    COLORS'POS(FOREGROUND) - 1,
                                    BLINK => BLINK_STAT = BLINK,
                                    SUPERBRIGHT => TRUE);
        LINE_FOREGROUND := FOREGROUND;
        LINE_BLINK_STAT := BLINK_STAT;
        LINE_BACKGROUND := BACKGROUND;
      end if;
      if FORCED or else XOR_MODE /= LINE_XOR_MODE then
        X_MNG.X_SET_XOR_MODE (ID, XOR_MODE = XOR_ON);
        LINE_XOR_MODE := XOR_MODE;
      end if;
    end SET_ATTRIBUTES;

    procedure QUICK_DRAW (UPPER_LEFT, LOWER_RIGHT : in SQUARE;
                          KIND                    : in BORDER_LIST;
                          NAME                    : in WINDOW) is

      type DESCRIPT is array(1 .. 6) of X_MNG.BYTE;
      DESC : constant array(BORDER_LIST) of DESCRIPT := (
        ERASE  => (others => CHARACTER'POS(' ')),
        SIMPLE => (13, 12, 11, 14, 18, 25),
        BLINK  => (13, 12, 11, 14, 18, 25));
    begin
    -- check
      if UPPER_LEFT.ROW = ROW_RANGE'FIRST or else
         UPPER_LEFT.COL = COL_RANGE'FIRST or else
         LOWER_RIGHT.ROW = ROW_RANGE'LAST or else
         LOWER_RIGHT.COL = COL_RANGE'LAST then
        raise FRAME_IMPOSSIBLE;
      end if;
      case KIND is
        when BLINK =>
          SET_ATTRIBUTES (NAME.CURRENT_FOREGROUND, BLINK,
                          NAME.CURRENT_BACKGROUND, NAME.CURRENT_XOR_MODE);
        when SIMPLE =>
          SET_ATTRIBUTES (NAME.CURRENT_FOREGROUND, NOT_BLINK,
                          NAME.CURRENT_BACKGROUND, NAME.CURRENT_XOR_MODE);
        when ERASE =>
          SET_ATTRIBUTES (NAME.CURRENT_FOREGROUND, NAME.CURRENT_BLINK_STAT,
                          NAME.CURRENT_BACKGROUND, XOR_OFF);
      end case;
      -- draw corners
      X_MNG.X_PUT_CHAR (ID, DESC(KIND)(1),
            ROW_RANGE'PRED(UPPER_LEFT.ROW), COL_RANGE'PRED(UPPER_LEFT.COL));
      X_MNG.X_PUT_CHAR (ID, DESC(KIND)(2),
            ROW_RANGE'PRED(UPPER_LEFT.ROW), COL_RANGE'SUCC(LOWER_RIGHT.COL));
      X_MNG.X_PUT_CHAR (ID, DESC(KIND)(3),
            ROW_RANGE'SUCC(LOWER_RIGHT.ROW), COL_RANGE'SUCC(LOWER_RIGHT.COL));
      X_MNG.X_PUT_CHAR (ID, DESC(KIND)(4),
            ROW_RANGE'SUCC(LOWER_RIGHT.ROW), COL_RANGE'PRED(UPPER_LEFT.COL));
      -- draw horiz
      for I in UPPER_LEFT.COL .. LOWER_RIGHT.COL loop
        X_MNG.X_PUT_CHAR (ID, DESC(KIND)(5),
              ROW_RANGE'PRED(UPPER_LEFT.ROW), I);
        X_MNG.X_PUT_CHAR (ID, DESC(KIND)(5),
              ROW_RANGE'SUCC(LOWER_RIGHT.ROW), I);
      end loop;
      -- draw verti
      for I in UPPER_LEFT.ROW .. LOWER_RIGHT.ROW loop
        X_MNG.X_PUT_CHAR (ID, DESC(KIND)(6),
              I, COL_RANGE'PRED(UPPER_LEFT.COL));
        X_MNG.X_PUT_CHAR (ID, DESC(KIND)(6),
              I, COL_RANGE'SUCC(LOWER_RIGHT.COL));
      end loop;
    end QUICK_DRAW;

    -- draw a frame around a window
    procedure FRAME (BLINK : in BOOLEAN := FALSE;
                     NAME : in WINDOW) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      if BLINK then
        QUICK_DRAW(NAME.UPPER_LEFT, NAME.LOWER_RIGHT, ONE_CON_IO.BLINK, NAME);
      else
        QUICK_DRAW(NAME.UPPER_LEFT, NAME.LOWER_RIGHT, SIMPLE, NAME);
      end if;
    end FRAME;

    procedure CLEAR_FRAME (NAME : in WINDOW) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      QUICK_DRAW(NAME.UPPER_LEFT, NAME.LOWER_RIGHT, ERASE, NAME);
    end CLEAR_FRAME;


    -- make window re-usable (have to re_open it)
    procedure CLOSE (NAME : in out WINDOW) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      DYN_WIN.FREE(NAME);
    end CLOSE;

    -- move cursor for use with put or get
    procedure MOVE (POSITION : in SQUARE := HOME;
                    NAME     : in WINDOW := SCREEN) is
    begin
      MOVE(POSITION.ROW, POSITION.COL, NAME);
    end MOVE;

    procedure CLEAR (NAME : in WINDOW := SCREEN) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      -- upper left and lower right, set foreground as our background
      SET_ATTRIBUTES (NAME.CURRENT_BACKGROUND,
                      NOT_BLINK,
                      NAME.CURRENT_BACKGROUND, XOR_OFF);
      X_MNG.X_DRAW_AREA (ID, NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL + 1,
                             NAME.LOWER_RIGHT.ROW - NAME.UPPER_LEFT.ROW + 1,
                             NAME.UPPER_LEFT.ROW, NAME.UPPER_LEFT.COL);
      SET_ATTRIBUTES (NAME.CURRENT_FOREGROUND,
                      NAME.CURRENT_BLINK_STAT,
                      NAME.CURRENT_BACKGROUND,
                      NAME.CURRENT_XOR_MODE);
      MOVE (NAME => NAME);
      X_MNG.X_FLUSH (ID);
    end CLEAR;

    procedure MOVE (ROW  : in ROW_RANGE;
                    COL  : in COL_RANGE;
                    NAME : in WINDOW := SCREEN) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      if (ROW > NAME.LOWER_RIGHT.ROW - NAME.UPPER_LEFT.ROW) or else
         (COL > NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL) then
        raise INVALID_SQUARE;
      end if;
      NAME.CURRENT_POS := (ROW, COL);
    end MOVE;

    function POSITION (NAME : WINDOW := SCREEN) return SQUARE is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      return NAME.CURRENT_POS;
    end POSITION;

    procedure BELL (REPEAT : in POSITIVE := 1) is
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      if REPEAT in X_MNG.BELL_REPEAT then
        X_MNG.X_BELL (ID, REPEAT);
      else
        X_MNG.X_BELL (ID, X_MNG.BELL_REPEAT'LAST);
      end if;
    end BELL;

    -- Get window attributes when current, and set the whole
    procedure SET_ATTRIBUTES_FROM_WINDOW (
                       NAME       : in WINDOW;
                       FOREGROUND : in COLORS;
                       BLINK_STAT : in BLINK_STATS;
                       BACKGROUND : in BASIC_COLORS) is
      FG : EFFECTIVE_COLORS;
      BL : EFFECTIVE_BLINK_STATS;
      BG : EFFECTIVE_BASIC_COLORS;
    begin
      if FOREGROUND = CURRENT then
        FG := NAME.CURRENT_FOREGROUND;
      else
        FG := FOREGROUND;
      end if;
      if BLINK_STAT = CURRENT then
        BL := NAME.CURRENT_BLINK_STAT;
      else
        BL := BLINK_STAT;
      end if;
      if BACKGROUND = CURRENT then
        BG := NAME.CURRENT_BACKGROUND;
      else
        BG := BACKGROUND;
      end if;
      SET_ATTRIBUTES (FG, BL, BG, NAME.CURRENT_XOR_MODE);
    end SET_ATTRIBUTES_FROM_WINDOW;

    -- Writes a character at the current cursor position and with attributes.
    -- Position is not updated.
    procedure PUT_INT (INT        : in INT_CHAR;
                       NAME       : in WINDOW := SCREEN;
                       FOREGROUND : in COLORS := CURRENT;
                       BLINK_STAT : in BLINK_STATS := CURRENT;
                       BACKGROUND : in BASIC_COLORS := CURRENT) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      if INT /= CHARACTER'POS(ASCII.CR) then
        SET_ATTRIBUTES_FROM_WINDOW (NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
        -- put character
        X_MNG.X_PUT_CHAR (ID, X_MNG.BYTE(INT),
                          NAME.UPPER_LEFT.ROW + NAME.CURRENT_POS.ROW,
                          NAME.UPPER_LEFT.COL + NAME.CURRENT_POS.COL);
      end if;
    end PUT_INT;

    procedure PUT_NOT_MOVE (C          : in CHARACTER;
                            NAME       : in WINDOW := SCREEN;
                            FOREGROUND : in COLORS;
                            BLINK_STAT : in BLINK_STATS;
                            BACKGROUND : in BASIC_COLORS) is
    begin
      PUT_INT(CHARACTER'POS(C), NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
    end PUT_NOT_MOVE;

    -- Increment col by one or row by one...
    procedure MOVE_ONE (NAME : in WINDOW := SCREEN) is
    begin
      if NAME.CURRENT_POS.COL /= NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL then
        -- next col
        NAME.CURRENT_POS.COL := COL_RANGE'SUCC(NAME.CURRENT_POS.COL);
      else
        -- 1st col
        NAME.CURRENT_POS.COL := COL_RANGE'FIRST;
        if NAME.CURRENT_POS.ROW /=
           NAME.LOWER_RIGHT.ROW  - NAME.UPPER_LEFT.ROW then
          -- next_line
          NAME.CURRENT_POS.ROW := ROW_RANGE'SUCC(NAME.CURRENT_POS.ROW);
        else
          -- No scroll :-( first row
          NAME.CURRENT_POS.ROW := ROW_RANGE'FIRST;
        end if;
      end if;
    end MOVE_ONE;

    -- Writes a character at the current cursor position and with attributes.
    -- CR only is interpreted
    procedure PUT (C          : in CHARACTER;
                   NAME       : in WINDOW := SCREEN;
                   FOREGROUND : in COLORS := CURRENT;
                   BLINK_STAT : in BLINK_STATS := CURRENT;
                   BACKGROUND : in BASIC_COLORS := CURRENT;
                   MOVE       : in BOOLEAN := TRUE) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      if C /= ASCII.CR then
        PUT_NOT_MOVE(C, NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
      end if;
      if MOVE then
        if C = ASCII.CR then
          -- End of current row
          NAME.CURRENT_POS.COL := NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL;
        end if;
        MOVE_ONE (NAME);
      end if;
    end PUT;

    -- Idem with a string
    procedure PUT (S          : in STRING;
                   NAME       : in WINDOW := SCREEN;
                   FOREGROUND : in COLORS := CURRENT;
                   BLINK_STAT : in BLINK_STATS := CURRENT;
                   BACKGROUND : in BASIC_COLORS := CURRENT;
                   MOVE       : in BOOLEAN := TRUE) is
      SFIRST, SLAST, RLAST : NATURAL;
      SAVED_POS : constant SQUARE := NAME.CURRENT_POS;
      WIN_LAST_COL : constant COL_RANGE
                   := NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL;
      PCR : BOOLEAN;

      procedure X_PUT (STR : in STRING) is
      begin
        if STR'LENGTH /= 0 then
          X_MNG.X_PUT_STRING (ID, STR, NAME.UPPER_LEFT.ROW + NAME.CURRENT_POS.ROW,
                                       NAME.UPPER_LEFT.COL + NAME.CURRENT_POS.COL);
        end if;
      end X_PUT;

    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      -- Check empty string
      if S = "" then
        return;
      end if;
      SET_ATTRIBUTES_FROM_WINDOW (NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
      -- Put chunks of string due to CRs or too long slices
      SFIRST := S'FIRST;
      loop
        SLAST  := S'FIRST;
        PCR := FALSE;
        -- Look for CR or end of string
        while SLAST /= S'LAST and then S(SLAST) /= ASCII.CR loop
          SLAST := SLAST + 1;
        end loop;
        -- Skip CR
        if S(SLAST) = ASCII.CR then
          RLAST := SLAST - 1;
          PCR := TRUE;
        else
          RLAST := SLAST;
        end if;
        -- Truncate to fit window
        -- Last - first <= Win_las_col - Pos 
        if NAME.CURRENT_POS.COL + RLAST - SFIRST  > WIN_LAST_COL then
           RLAST := SFIRST + WIN_LAST_COL - NAME.CURRENT_POS.COL;
        end if;
        -- Put the chunk
        X_PUT (S(SFIRST .. RLAST));
        -- Update position : last character + one
        ONE_CON_IO.MOVE (NAME.CURRENT_POS.ROW,
                         NAME.CURRENT_POS.COL + RLAST - SFIRST,
                         NAME);
        MOVE_ONE (NAME);
        -- Issue CR
        if PCR then
          PUT(ASCII.CR, NAME);
        end if;
        -- Move to next chunk
        exit when SLAST = S'LAST;
        SFIRST := SLAST + 1;
      end loop;

      -- Resore pos
      if not MOVE then
        ONE_CON_IO.MOVE (SAVED_POS, NAME);
      end if;

    end PUT;

    -- Idem but appends a CR
    procedure PUT_LINE (S          : in STRING;
                        NAME       : in WINDOW := SCREEN;
                        FOREGROUND : in COLORS := CURRENT;
                        BLINK_STAT : in BLINK_STATS := CURRENT;
                        BACKGROUND : in BASIC_COLORS := CURRENT) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      -- Puts the string
      PUT(S, NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
      -- New line
      NEW_LINE(NAME);
    end PUT_LINE;

    -- Puts CR
    procedure NEW_LINE (NAME   : in WINDOW := SCREEN;
                        NUMBER : in POSITIVE := 1) is
    begin
      if NAME = null then
        raise WINDOW_NOT_OPEN;
      end if;
      for I in 1 .. NUMBER loop
        PUT(ASCII.CR, NAME);
      end loop;
    end NEW_LINE;

    procedure NEXT_X_EVENT (TIMEOUT_MS : in out INTEGER;
                            X_EVENT : out X_MNG.EVENT_KIND) is
      EVENT : BOOLEAN;
      LOC_X_EVENT : X_MNG.EVENT_KIND;
      use X_MNG;
    begin
      loop
        if not X_EVENT_WAITING then
          -- Wait
          X_MNG.X_SELECT (ID, TIMEOUT_MS, EVENT);
          if not EVENT then
            X_EVENT := X_MNG.DISCARD;
            return;
          end if;
        end if;
        X_MNG.X_PROCESS_EVENT (ID, LOC_X_EVENT, X_EVENT_WAITING);
        if LOC_X_EVENT /= X_MNG.DISCARD then
          X_EVENT := LOC_X_EVENT;
          return;
        end if;
      end loop;
    end NEXT_X_EVENT;

    procedure TRANSLATE_X_KEY (KEY     : in out NATURAL;
                               IS_CHAR : in out BOOLEAN;
                               CTRL    : in out BOOLEAN;
                               SHIFT   : in out BOOLEAN) is
    begin
      -- No translation of chars
      if IS_CHAR then
        return;
      end if;
      case KEY is
        when 16#8D# =>
          -- Enter
          KEY := 16#0D#;
        when  16#AA# .. 16#B9# =>
          -- Oper or Num
          IS_CHAR := TRUE;
          KEY := KEY - 16#80#;
        when 16#95# .. 16#9C# =>
          -- Key movement
          KEY := KEY - 16#45#;
        when 16#9D# =>
          -- 5 not num : discard
          KEY := 16#00#;
        when 16#9E# =>
          -- Insert
          KEY := 16#63#;
        when 16#9F# =>
          -- Suppre
          KEY := 16#FF#;
        when others =>
          -- No translation
          null;
      end case;
    end TRANSLATE_X_KEY;

    procedure GET_X_KEY (KEY     : out NATURAL;
                         IS_CHAR : out BOOLEAN;
                         CTRL    : out BOOLEAN;
                         SHIFT   : out BOOLEAN) is
      KBD_TAB : X_MNG.KBD_TAB_CODE;
      LOC_KEY : NATURAL;
      LOC_IS_CHAR : BOOLEAN;
      LOC_CTRL : BOOLEAN;
      LOC_SHIFT : BOOLEAN;
      use X_MNG;
    begin
      X_MNG.X_READ_KEY(ID, KBD_TAB);

      LOC_KEY := NATURAL (KBD_TAB.TAB(KBD_TAB.NBRE));
      LOC_IS_CHAR := TRUE;
      LOC_CTRL := FALSE;
      LOC_SHIFT := FALSE;

      -- Optimisation
      if KBD_TAB.NBRE = 1 then
        TRANSLATE_X_KEY (LOC_KEY, LOC_IS_CHAR, LOC_CTRL, LOC_SHIFT);
        KEY := LOC_KEY;
        IS_CHAR := LOC_IS_CHAR;
        CTRL := LOC_CTRL;
        SHIFT := LOC_SHIFT;
        return;
      elsif KBD_TAB.NBRE = 2 then
        LOC_IS_CHAR := FALSE;
        TRANSLATE_X_KEY (LOC_KEY, LOC_IS_CHAR, LOC_CTRL, LOC_SHIFT);
        KEY := LOC_KEY;
        IS_CHAR := LOC_IS_CHAR;
        CTRL := LOC_CTRL;
        SHIFT := LOC_SHIFT;
        return;
      end if;

      if KBD_TAB.NBRE mod 2 = 0 then
        LOC_IS_CHAR := FALSE;
        KBD_TAB.NBRE := KBD_TAB.NBRE - 2;
      else
        LOC_IS_CHAR := TRUE;
        KBD_TAB.NBRE := KBD_TAB.NBRE - 1;
      end if;

      if KBD_TAB.TAB(2) = 16#E3# then
        -- Ctrl
        LOC_CTRL := TRUE;
      else
        -- Shift
        LOC_SHIFT := TRUE;
      end if;
      if KBD_TAB.NBRE = 4 then
        -- Ctrl Shift
        LOC_CTRL := TRUE;
        LOC_SHIFT := TRUE;
      end if;
      
      TRANSLATE_X_KEY (LOC_KEY, LOC_IS_CHAR, LOC_CTRL, LOC_SHIFT);
      KEY := LOC_KEY;
      IS_CHAR := LOC_IS_CHAR;
      CTRL := LOC_CTRL;
      SHIFT := LOC_SHIFT;
    end GET_X_KEY;

    -- check if a key is available until a certain time.
    procedure GET_KEY_TIME (CHECK_BREAK : in BOOLEAN;
                            EVENT       : out EVENT_LIST;
                            KEY         : out NATURAL;
                            IS_CHAR     : out BOOLEAN;
                            CTRL        : out BOOLEAN;
                            SHIFT       : out BOOLEAN;
                            TIME_OUT    : in DELAY_REC := INFINITE_DELAY) is

      X_EVENT : X_MNG.EVENT_KIND;
      CUR_TIME : CALENDAR.TIME;
      DUR : DURATION;
      TIMEOUT_MS : INTEGER;
      LOC_KEY : NATURAL;
      LOC_IS_CHAR : BOOLEAN;
      LOC_CTRL : BOOLEAN;
      LOC_SHIFT : BOOLEAN;
      use X_MNG, CALENDAR;
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;

      if TIME_OUT = INFINITE_DELAY then
        TIMEOUT_MS := -1;
      elsif TIME_OUT.DELAY_KIND = DELAY_SEC then
        TIMEOUT_MS := INTEGER (FLOAT(TIME_OUT.DELAY_SECONDS) * 1_000.0);
      else
        CUR_TIME := CALENDAR.CLOCK;
        if CUR_TIME > TIME_OUT.EXPIRATION_TIME then
          TIMEOUT_MS := 0;
        else
          DUR := TIME_OUT.EXPIRATION_TIME - CUR_TIME;
          TIMEOUT_MS := INTEGER (FLOAT(DUR) * 1_000.0);
        end if;
      end if;

      NEXT_X_EVENT (TIMEOUT_MS, X_EVENT);
      case X_EVENT is
        when X_MNG.REFRESH =>
          -- Refresh
          EVENT := REFRESH;
          return;
        when X_MNG.DISCARD =>
          -- Timeout
          EVENT := TIMEOUT;
          return;
        when X_MNG.TID_PRESS | X_MNG.TID_RELEASE | X_MNG.TID_MOTION =>
          EVENT := MOUSE_BUTTON;
          MOUSE_STATUS := X_EVENT;
          return;
        when X_MNG.KEYBOARD =>
          GET_X_KEY (LOC_KEY, LOC_IS_CHAR, LOC_CTRL, LOC_SHIFT);
          KEY := LOC_KEY;
          IS_CHAR := LOC_IS_CHAR;
          CTRL := LOC_CTRL;
          SHIFT := LOC_SHIFT;
          -- Check break
          if CHECK_BREAK then
            if (LOC_KEY = CHARACTER'POS('c') or else LOC_KEY = 0)
            and then LOC_IS_CHAR and then LOC_CTRL and then not LOC_SHIFT then
              -- Ctrl C or Ctrl break
              EVENT := BREAK;
              return;
            end if;
          end if;
          -- Escape for any other keyboard key 
          EVENT := ESC;
          return;
      end case;

    end GET_KEY_TIME;

    -- Gives first key code of keyboard buffer, (waits if it is empty)
    -- no echo
    procedure GET_KEY (KEY     : out NATURAL;
                       IS_CHAR : out BOOLEAN;
                       CTRL    : out BOOLEAN;
                       SHIFT   : out BOOLEAN) is
      EVENT : EVENT_LIST;
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      -- Wait for keyboard
      loop
        GET_KEY_TIME(FALSE, EVENT, KEY, IS_CHAR, CTRL, SHIFT);
        if EVENT = REFRESH then
          KEY := 0;
          IS_CHAR := TRUE;
          CTRL := FALSE;
          SHIFT := FALSE;
          return;
        elsif EVENT = ESC then
          -- A key
          return;
        end if;
      end loop;
    end GET_KEY;
        


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
                            TIME_OUT   : in DELAY_REC :=  INFINITE_DELAY;
                            ECHO       : in BOOLEAN := TRUE) is
      WIDTH         : constant NATURAL := STR'LENGTH;
      LSTR          : STRING(1 .. WIDTH) := STR;
      KEY           : NATURAL;
      IS_CHAR       : BOOLEAN;
      CTRL, SHIFT   : BOOLEAN;
      REDRAW        : BOOLEAN;
      FIRST_POS     : constant SQUARE := NAME.CURRENT_POS;
      LAST_TIME     : DELAY_REC;
      EVENT         : EVENT_LIST;

      function PARSE return NATURAL is
      begin
        for I in reverse 1 .. WIDTH loop
          if LSTR(I) /= ' ' then
            -- this character is the last meaningfull
            return STR'FIRST + I - 1;
          end if;
        end loop;
        -- all is spaces
        return 0;
      end PARSE;

      procedure CURSOR (SHOW : in BOOLEAN) is
        ABSOLUTE_POS : SQUARE;
      begin
        MOVE(FIRST_POS.ROW, FIRST_POS.COL + POS - 1, NAME);
        ABSOLUTE_POS := TO_ABSOLUTE (NAME.CURRENT_POS, NAME);
        if SHOW then
          if INSERT then
            X_MNG.X_OVERWRITE_CHAR (ID, 16#5E#,
                  ABSOLUTE_POS.ROW, ABSOLUTE_POS.COL);
          else
            X_MNG.X_OVERWRITE_CHAR (ID, 16#5F#,
                  ABSOLUTE_POS.ROW, ABSOLUTE_POS.COL);
          end if;
        else
          X_MNG.X_PUT_CHAR (ID, LSTR(POS),
                ABSOLUTE_POS.ROW, ABSOLUTE_POS.COL);
        end if;
      end CURSOR;


    begin
      -- Time at which the get ends
      if TIME_OUT = INFINITE_DELAY or else TIME_OUT.DELAY_KIND = DELAY_EXP then
        LAST_TIME := TIME_OUT;
      else
        LAST_TIME := (DELAY_KIND => DELAY_EXP,
                      EXPIRATION_TIME => CALENDAR."+"(CALENDAR.CLOCK, TIME_OUT.DELAY_SECONDS) );
      end if;

      -- Emtpy string
      if WIDTH = 0 then
        LAST := STR'LAST;

        loop
          GET_KEY_TIME (TRUE, EVENT, KEY, IS_CHAR, CTRL, SHIFT, LAST_TIME);
          if EVENT /= ESC then
            -- No key ==> mouse or time out
            STAT := EVENT;
            return;
          elsif not IS_CHAR then
            -- Function key
            case KEY is
              when 16#0D# =>
                -- Return
                STAT := RET;
                return;
              when 16#1B# =>
                -- Escape
                STAT := ESC;
                return;
              when 16#09# =>
                if CTRL then 
                  -- Ctrl Tab
                  STAT := STAB;
                else
                  -- Tab
                  STAT := TAB;
                end if;
                return;
              when 16#08# =>
                -- Backspace
                null;
              when 16#50# =>
                -- Home
                STAT := LEFT;
                return;
              when 16#57# =>
                -- End
                STAT := RIGHT;
                return;
              when 16#51# =>
                -- <--
                STAT := LEFT;
                return;
              when 16#53# =>
                -- -->
                STAT := RIGHT;
                return;
              when 16#52# =>
                -- Up
                STAT := UP;
                return;
              when 16#55# =>
                -- Page Up
                if not CTRL then
                  STAT := PGUP;
                else
                  STAT := CTRL_PGUP;
                end if;
                return;
              when 16#54# =>
                -- Down
                STAT := DOWN;
                return;
              when 16#56# =>
                -- Page Down
                if not CTRL then
                  STAT := PGDOWN;
                else
                  STAT := CTRL_PGDOWN;
                end if;
                return;
              when 16#63# =>
                -- Insert
                INSERT := not INSERT;
              when others =>
                null;
            end case;
          else  -- IS_CHAR
            if KEY >= CHARACTER'POS(' ')
            and then KEY <= CHARACTER'POS(CHARACTER'LAST) then
              -- every other valid char
              STAT := FULL;
              return;
            end if;
          end if;  -- function key or normal key
        end loop;  -- dicard any unaccepted key
      end if;  -- string'length = 0

      -- Check width and current_pos / window's width
      if WIDTH > NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL  + 1 then
        raise STRING_TOO_LONG;
      end if;

      -- put the string
      MOVE(FIRST_POS, NAME);
      if ECHO then
        PUT(LSTR, NAME, FOREGROUND, BLINK_STAT, BACKGROUND, MOVE => FALSE);
      end if;

      loop
        -- show cursor
        if ECHO then
          CURSOR (TRUE);
        end if;
        REDRAW := FALSE;
        -- try to get a key
        GET_KEY_TIME (TRUE, EVENT, KEY, IS_CHAR, CTRL, SHIFT, LAST_TIME);
        -- hide cursor
        if ECHO then
          CURSOR (FALSE);
        end if;
        if EVENT /= ESC then
          -- No key ==> mouse or time out or refresh
          STR := LSTR;
          LAST := PARSE;
          STAT := EVENT;
          return;
        elsif  not IS_CHAR then
          case KEY is
            when 16#0D# =>
              -- Return
              STR := LSTR;
              LAST := PARSE;
              STAT := RET;
              return;
            when 16#1B# =>
              -- Escape
              STR := LSTR;
              LAST := PARSE;
              STAT := ESC;
              return;
            when 16#09# =>
              if CTRL then
                -- Ctrl Tab
                STR := LSTR;
                LAST := PARSE;
                STAT := STAB;
              else
                -- Tab
                STR := LSTR;
                LAST := PARSE;
                STAT := TAB;
              end if;
              return;
            when 16#08# =>
              -- backspace
              if POS /= 1 then
                POS := POS - 1;
                LSTR(POS .. WIDTH - 1) := LSTR(POS + 1 .. WIDTH);
                LSTR(WIDTH) := ' ';
                REDRAW := TRUE;
              end if;
            when 16#50# =>
              -- Home
              POS := 1;
            when 16#57# =>
              -- End
              POS := WIDTH;
            when 16#51# =>
              -- <--
              if POS /= 1 then
                POS := POS - 1;
              else
                STR := LSTR;
                LAST := PARSE;
                STAT := LEFT;
                return;
              end if;
            when 16#53# =>
              -- -->
              if POS /= WIDTH then
                POS := POS + 1;
              else
                STR := LSTR;
                LAST := PARSE;
                STAT := RIGHT;
                return;
              end if;
            when 16#52# =>
              -- Up
              STR := LSTR;
              LAST := PARSE;
              STAT := UP;
              return;
            when 16#55# =>
              -- Page Up
              STR := LSTR;
              LAST := PARSE;
              if not CTRL then
                STAT := PGUP;
              else
                STAT := CTRL_PGUP;
              end if;
              return;
            when 16#54# =>
              -- Down
              STR := LSTR;
              LAST := PARSE;
              STAT := DOWN;
              return;
            when 16#56# =>
              -- Page Down
              STR := LSTR;
              LAST := PARSE;
              if not CTRL then
                STAT := PGDOWN;
              else
                STAT := CTRL_PGDOWN;
              end if;
              return;
            when 16#63# =>
              -- Insert
              INSERT := not INSERT;
            when 16#FF# =>
              if not CTRL then
                -- Suppr
                LSTR(POS .. WIDTH - 1) := LSTR(POS + 1 .. WIDTH);
                LSTR(WIDTH) := ' ';
                REDRAW := TRUE;
              else
                -- Ctrl Suppr : clear field + home
                POS := 1;
                LSTR(1 .. WIDTH) := (others => ' ');
                REDRAW := TRUE;
              end if;
            when others =>
              null;
          end case;
        else  -- is_char
          if KEY >= CHARACTER'POS(' ')
          and then KEY <= CHARACTER'POS(CHARACTER'LAST) then
            -- all other valid chars
            if INSERT then
              if POS /= WIDTH then
                LSTR(POS + 1 .. WIDTH) := LSTR(POS .. WIDTH - 1);
                REDRAW := TRUE;
              end if;
            end if;
            LSTR(POS) := CHARACTER'VAL(KEY);
            if POS /= WIDTH then
              POS := POS + 1;
            else
              STR := LSTR;
              LAST := PARSE;
              STAT := FULL;
              return;
            end if;
            if not REDRAW and then ECHO then
              PUT(CHARACTER'VAL(KEY), NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
            end if;
          end if;
        end if;  -- is_char

        -- redraw if necessary
        if REDRAW and then ECHO then
          MOVE(FIRST_POS, NAME);
          PUT(LSTR, NAME, FOREGROUND, BLINK_STAT, BACKGROUND, MOVE => FALSE);
       end if;
      end loop;
    end PUT_THEN_GET;

    -- Gets a string of at most width characters
    procedure GET (STR        : out STRING;
                   LAST       : out NATURAL;
                   STAT       : out CURS_MVT;
                   POS        : out POSITIVE;
                   INSERT     : out BOOLEAN;
                   NAME       : in WINDOW := SCREEN;
                   FOREGROUND : in COLORS := CURRENT;
                   BLINK_STAT : in BLINK_STATS := CURRENT;
                   BACKGROUND : in BASIC_COLORS := CURRENT;
                   TIME_OUT   : in DELAY_REC :=  INFINITE_DELAY;
                   ECHO       : in BOOLEAN := TRUE) is
      LSTR : STRING(STR'RANGE ) := (others => ' ');
      LPOS : POSITIVE;
      LINS : BOOLEAN;
    begin
      LPOS := 1;
      LINS := FALSE;
      PUT_THEN_GET(LSTR, LAST, STAT, LPOS, LINS, NAME,
          FOREGROUND, BLINK_STAT, BACKGROUND, TIME_OUT, ECHO);
      STR := LSTR;
      POS := LPOS;
      INSERT := LINS;
    end GET;

    -- Take first character of keyboard buffer (no echo) or refresh event
    procedure PAUSE is
      STR  : STRING(1 .. 0);
      LAST : NATURAL;
      STAT : CURS_MVT;
      POS  : POSITIVE;
      INS  : BOOLEAN;
    begin
      loop
        -- STR is empty so no echo at all
        GET(STR, LAST, STAT, POS, INS);
        exit when STAT /= MOUSE_BUTTON;
      end loop;
    end PAUSE;


    -- Gets first character (echo or not)
    -- No echo for RET, ESC, BREAK and REFRESH where
    --  ASCII.CR, ESC, EOT and NUL are returned respectively
    -- Cursor movements (UP to RIGHT, TAB and STAB) and mouse events are
    --  discarded (get does not return).
    function GET (NAME : WINDOW := SCREEN; ECHO : in BOOLEAN := TRUE) return CHARACTER is
      STR  : STRING(1 .. 1);
      LAST : NATURAL;
      STAT : CURS_MVT;
      POS  : POSITIVE;
      INS  : BOOLEAN;
    begin
      loop
        STR := (others => ' ');
        POS := 1;
        INS := FALSE;
        PUT_THEN_GET(STR, LAST, STAT, POS, INS, NAME, ECHO => ECHO);
        case STAT is
          when UP .. RIGHT | TAB .. STAB =>
            -- Cursor movement
            null;
          when FULL =>
            -- Character input
            return STR(1);
          when RET =>
            return ASCII.CR;
          when ESC =>
            return ASCII.ESC;
          when BREAK =>
            return ASCII.EOT;
          when REFRESH =>
            return ASCII.NUL;
          when MOUSE_BUTTON | TIMEOUT =>
            -- Ignore mouse. Timeout impossible.
            null;
        end case;
      end loop;
    end GET;

    procedure ENABLE_MOTION_EVENTS (MOTION_ENABLED : in BOOLEAN) is
    begin
      if MOTION_ENABLED /= MOTION_ENABLING then
        X_MNG.X_ENABLE_MOTION_EVENTS(ID, MOTION_ENABLED);
        MOTION_ENABLING := MOTION_ENABLED;
      end if;
    end ENABLE_MOTION_EVENTS;

    package body GRAPHICS is

      function X_MAX return X_RANGE is
      begin
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        return ONE_CON_IO.X_MAX;
      end X_MAX;

      function Y_MAX return Y_RANGE is
      begin
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        return ONE_CON_IO.Y_MAX;
      end Y_MAX;

      -- Font characteristics
      function FONT_WIDTH  return NATURAL is
      begin
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        return ONE_CON_IO.FONT_WIDTH;
      end FONT_WIDTH;

      function FONT_HEIGHT return NATURAL is
      begin
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        return ONE_CON_IO.FONT_HEIGHT;
      end FONT_HEIGHT;

      function FONT_OFFSET return NATURAL is
      begin
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        return ONE_CON_IO.FONT_OFFSET;
      end FONT_OFFSET;


      procedure SET_SCREEN_ATTRIBUTES is
      begin
        SET_ATTRIBUTES (SCREEN.CURRENT_FOREGROUND,
                        SCREEN.CURRENT_BLINK_STAT,
                        SCREEN.CURRENT_BACKGROUND,
                        SCREEN.CURRENT_XOR_MODE);
      end SET_SCREEN_ATTRIBUTES;

      procedure PUT (C : in CHARACTER;
                     X : in X_RANGE;
                     Y : in Y_RANGE) is
      begin
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        SET_SCREEN_ATTRIBUTES;
        X_MNG.X_PUT_CHAR_PIXELS(ID, X_MNG.BYTE(CHARACTER'POS(C)),
                                X, ONE_CON_IO.Y_MAX - Y);
      end PUT;

      procedure PUT (S : in STRING;
                     X : in X_RANGE;
                     Y : in Y_RANGE) is
        LX : X_RANGE;
        LY : Y_RANGE;
      begin
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        SET_SCREEN_ATTRIBUTES;
        LX := X;
        LY := ONE_CON_IO.Y_MAX - Y;
        for I in S'RANGE loop
          X_MNG.X_PUT_CHAR_PIXELS(ID, X_MNG.BYTE(CHARACTER'POS(S(I))),
                                  LX, LY);
          LX := LX + FONT_WIDTH;
        end loop;
      end PUT;

      procedure DRAW_POINT (X : in X_RANGE;
                            Y : in Y_RANGE) is
      begin
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        SET_SCREEN_ATTRIBUTES;
        X_MNG.X_DRAW_POINT(ID, X, ONE_CON_IO.Y_MAX - Y);
      end DRAW_POINT;

      procedure DRAW_LINE (X1 : in X_RANGE;
                           Y1 : in Y_RANGE;
                           X2 : in X_RANGE;
                           Y2 : in Y_RANGE) is
      begin
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        SET_SCREEN_ATTRIBUTES;
        X_MNG.X_DRAW_LINE(ID, X1, ONE_CON_IO.Y_MAX - Y1, X2, ONE_CON_IO.Y_MAX - Y2);
      end DRAW_LINE;


      procedure DRAW_RECTANGLE (X1 : in X_RANGE;
                                Y1 : in Y_RANGE;
                                X2 : in X_RANGE;
                                Y2 : in Y_RANGE) is
      begin
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        SET_SCREEN_ATTRIBUTES;
        X_MNG.X_DRAW_RECTANGLE(ID, X1, ONE_CON_IO.Y_MAX - Y1, X2, ONE_CON_IO.Y_MAX - Y2);
      end DRAW_RECTANGLE;


      procedure GET_CURRENT_POINTER_POS (VALID : out BOOLEAN;
                                         X     : out X_RANGE;
                                         Y     : out Y_RANGE) is
        LX, LY : INTEGER;
      begin
        VALID := FALSE;
        if not INIT_DONE then
          raise NOT_INIT;
        end if;
        X_MNG.X_GET_CURRENT_POINTER_POSITION(ID, LX, LY);
        -- In screen? (avoiding function call for X/Y_MAX) 
        if       LX in GRAPHICS.X_RANGE and then LX <= ONE_CON_IO.X_MAX
        and then LY in GRAPHICS.Y_RANGE and then LY <= ONE_CON_IO.Y_MAX then
          X := LX;
          Y := ONE_CON_IO.Y_MAX - LY;
          VALID := TRUE;
        end if;
      end GET_CURRENT_POINTER_POS;


    end GRAPHICS;

    -- Set pointer shape
    procedure SET_POINTER_SHAPE (POINTER_SHAPE : in POINTER_SHAPE_LIST) is
    begin
      X_MNG.X_SET_GRAPHIC_POINTER(ID, POINTER_SHAPE=CROSS);
    end SET_POINTER_SHAPE;


    -- Get a mouse event. If valid is FALSE, it means that a release
    -- has occured outside the screen, then only BUTTON and status
    -- are significant
    procedure GET_MOUSE_EVENT (MOUSE_EVENT : out MOUSE_EVENT_REC;
                      COORDINATE_MODE : in COORDINATE_MODE_LIST := ROW_COL) is
      LOC_EVENT : MOUSE_EVENT_REC(COORDINATE_MODE);
      BUTTON : X_MNG.BUTTON_LIST;
      ROW, COL : INTEGER;
      use X_MNG;
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      -- Init result : Press not valid
      LOC_EVENT.VALID := FALSE;
      LOC_EVENT.STATUS := PRESSED;
      LOC_EVENT.BUTTON := MOUSE_BUTTON_LIST'FIRST;
      if COORDINATE_MODE = ROW_COL then
        LOC_EVENT.ROW := ROW_RANGE'FIRST;
        LOC_EVENT.COL := COL_RANGE'FIRST;
      else
        LOC_EVENT.X := GRAPHICS.X_RANGE'FIRST;
        LOC_EVENT.Y := GRAPHICS.Y_RANGE'FIRST;
      end if;
      MOUSE_EVENT := LOC_EVENT;

      -- Mouse event pending?
      if MOUSE_STATUS = X_MNG.DISCARD then
        return;
      end if;

      -- Get button and pos
      X_MNG.X_READ_TID (ID, COORDINATE_MODE = ROW_COL, BUTTON, ROW, COL);

      -- Event was a press release or motion?
      case MOUSE_STATUS is
        when X_MNG.TID_PRESS | X_MNG.TID_RELEASE =>

          case BUTTON is
            when X_MNG.NONE =>
              return;
            when X_MNG.LEFT =>
              LOC_EVENT.BUTTON := LEFT;
            when X_MNG.MIDDLE =>
              LOC_EVENT.BUTTON := MIDDLE;
            when X_MNG.RIGHT =>
              LOC_EVENT.BUTTON := RIGHT;
          end case;
          if MOUSE_STATUS = X_MNG.TID_PRESS then
            LOC_EVENT.STATUS := PRESSED;
          else
            LOC_EVENT.STATUS := RELEASED;
          end if;
        when X_MNG.TID_MOTION =>
          if BUTTON /= X_MNG.NONE or else not MOTION_ENABLING then
            return;
          end if;
          LOC_EVENT.STATUS := MOTION;
          LOC_EVENT.BUTTON := MOTION;
        when others =>
          return;
      end case;

      -- Coordinates
      if COORDINATE_MODE = ROW_COL then
        if ROW - 1 in ROW_RANGE and then COL - 1 in COL_RANGE then
          -- In screen
          LOC_EVENT.VALID := TRUE;
          LOC_EVENT.ROW := ROW - 1;
          LOC_EVENT.COL := COL - 1;
        else
          -- Out of screen : set to boundaries
          LOC_EVENT.VALID := FALSE;
          if ROW - 1 in ROW_RANGE then
            LOC_EVENT.ROW := ROW - 1;
          elsif ROW - 1 < ROW_RANGE'FIRST then
            LOC_EVENT.ROW := ROW_RANGE'FIRST;
          elsif ROW - 1 > ROW_RANGE'LAST then
            LOC_EVENT.ROW := ROW_RANGE'LAST;
          end if;
          if COL - 1 in COL_RANGE then
            LOC_EVENT.COL := COL - 1;
          elsif COL - 1 < COL_RANGE'FIRST then
            LOC_EVENT.COL := COL_RANGE'FIRST;
          elsif COL - 1 > COL_RANGE'LAST then
            LOC_EVENT.COL := COL_RANGE'LAST;
          end if;
        end if;
      else
        if       ROW in GRAPHICS.X_RANGE and then ROW <= ONE_CON_IO.X_MAX
        and then COL in GRAPHICS.Y_RANGE and then COL <= ONE_CON_IO.Y_MAX then
          LOC_EVENT.VALID := TRUE;
          LOC_EVENT.X := ROW;
          LOC_EVENT.Y := ONE_CON_IO.Y_MAX - COL;
        else
          LOC_EVENT.VALID := FALSE;
          if ROW in GRAPHICS.X_RANGE and then ROW <= ONE_CON_IO.X_MAX then
            LOC_EVENT.X := ROW;
          elsif ROW < GRAPHICS.X_RANGE'FIRST then
            LOC_EVENT.X := GRAPHICS.X_RANGE'FIRST;
          elsif ROW > X_MAX then
            LOC_EVENT.X := X_MAX;
          end if;
          if COL in GRAPHICS.Y_RANGE and then COL <= ONE_CON_IO.Y_MAX then
            LOC_EVENT.Y := Y_MAX - COL;
          elsif COL < GRAPHICS.Y_RANGE'FIRST then
            LOC_EVENT.Y := Y_MAX;
          elsif COL > Y_MAX then
            LOC_EVENT.Y := GRAPHICS.Y_RANGE'FIRST;
          end if;
        end if;
      end if;
      MOUSE_EVENT := LOC_EVENT;
      MOUSE_STATUS := X_MNG.DISCARD;
    exception
      when X_MNG.X_FAILURE =>
        null;
    end GET_MOUSE_EVENT;

  end ONE_CON_IO;

end GENERIC_CON_IO;
   
