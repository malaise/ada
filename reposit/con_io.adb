with ARGUMENT, DYN_DATA, X_MNG, SYS_CALLS;
package body CON_IO is

  FONT_ENV_NAME : constant STRING := "CON_IO_FONT";
  FONT_NO_ENV_0 : constant STRING := "small";

  ID : X_MNG.LINE;
  LINE_DEF : constant X_MNG.LINE_DEFINITION_REC := (
    SCREEN_ID => 0,
    ROW => 1,
    COLUMN => 1,
    HEIGHT => ROW_RANGE_LAST - ROW_RANGE_FIRST + 1,
    WIDTH => COL_RANGE_LAST - COL_RANGE_FIRST + 1,
    BACKGROUND => 0,
    BORDER => 0,
    NO_FONT => 1);

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
    ENV_STR : STRING (1 .. FONT_NO_ENV_0'LENGTH);
    ENV_LEN : NATURAL;
  begin
    if INIT_DONE then
      return;
    end if;
    X_MNG.X_INITIALISE ("");
    SYS_CALLS.GETENV (FONT_ENV_NAME, ENV_SET, ENV_TRU, ENV_STR, ENV_LEN);
    if ENV_SET and then not ENV_TRU and then ENV_STR (1 .. ENV_LEN) = FONT_NO_ENV_0 then
      LINE.NO_FONT := 0;
    end if;
    X_MNG.X_OPEN_LINE (LINE, ID);
    X_MNG.X_SET_LINE_NAME (ID, ARGUMENT.GET_PROGRAM_NAME);
    MOUSE_STATUS := X_MNG.DISCARD;
    X_EVENT_WAITING := TRUE;
    MOTION_ENABLING := FALSE;
    INIT_DONE := TRUE;
    X_MNG.X_GET_GRAPHIC_CHARACTERISTICS(ID, X_MAX, Y_MAX,
          FONT_WIDTH, FONT_HEIGHT, FONT_OFFSET);
    SET_ATTRIBUTES (LINE_FOREGROUND, LINE_BLINK_STAT, LINE_BACKGROUND,
                    LINE_XOR_MODE, FORCED => TRUE);
  end INIT;

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
  end RESET_TERM;

  -- flushes X
  procedure FLUSH is
  begin
    X_MNG.X_FLUSH;
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
    MOVE(ROW_RANGE'PRED(UPPER_LEFT.ROW), COL_RANGE'PRED(UPPER_LEFT.COL));
    X_MNG.X_PUT_CHAR (ID, DESC(KIND)(1));
    MOVE(ROW_RANGE'PRED(UPPER_LEFT.ROW), COL_RANGE'SUCC(LOWER_RIGHT.COL));
    X_MNG.X_PUT_CHAR (ID, DESC(KIND)(2));
    MOVE(ROW_RANGE'SUCC(LOWER_RIGHT.ROW), COL_RANGE'SUCC(LOWER_RIGHT.COL));
    X_MNG.X_PUT_CHAR (ID, DESC(KIND)(3));
    MOVE(ROW_RANGE'SUCC(LOWER_RIGHT.ROW), COL_RANGE'PRED(UPPER_LEFT.COL));
    X_MNG.X_PUT_CHAR (ID, DESC(KIND)(4));
    -- draw horiz
    for I in UPPER_LEFT.COL .. LOWER_RIGHT.COL loop
      MOVE(ROW_RANGE'PRED(UPPER_LEFT.ROW), I);
      X_MNG.X_PUT_CHAR (ID, DESC(KIND)(5));
      MOVE(ROW_RANGE'SUCC(LOWER_RIGHT.ROW), I);
      X_MNG.X_PUT_CHAR (ID, DESC(KIND)(5));
    end loop;
    -- draw verti
    for I in UPPER_LEFT.ROW .. LOWER_RIGHT.ROW loop
      MOVE(I, COL_RANGE'PRED(UPPER_LEFT.COL));
      X_MNG.X_PUT_CHAR (ID, DESC(KIND)(6));
      MOVE(I, COL_RANGE'SUCC(LOWER_RIGHT.COL));
      X_MNG.X_PUT_CHAR (ID, DESC(KIND)(6));
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
      QUICK_DRAW(NAME.UPPER_LEFT, NAME.LOWER_RIGHT, CON_IO.BLINK, NAME);
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
                    NAME.CURRENT_BLINK_STAT,
                    NAME.CURRENT_BACKGROUND, XOR_OFF);
    X_MNG.X_MOVE (ID, NAME.UPPER_LEFT.ROW, NAME.UPPER_LEFT.COL);
    X_MNG.X_DRAW_AREA (ID, NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL + 1,
                           NAME.LOWER_RIGHT.ROW - NAME.UPPER_LEFT.ROW + 1);
    SET_ATTRIBUTES (NAME.CURRENT_FOREGROUND,
                    NAME.CURRENT_BLINK_STAT,
                    NAME.CURRENT_BACKGROUND,
                    NAME.CURRENT_XOR_MODE);
    MOVE (NAME => NAME);
    X_MNG.X_FLUSH;
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
    X_MNG.X_MOVE (ID, NAME.UPPER_LEFT.ROW + ROW, NAME.UPPER_LEFT.COL + COL);
    NAME.CURRENT_POS := (ROW, COL);
  end MOVE;

  function POSITION (NAME : WINDOW := SCREEN) return SQUARE is
  begin
    if NAME = null then
      raise WINDOW_NOT_OPEN;
    end if;
    return NAME.CURRENT_POS;
  end POSITION;

  -- Writes a character at the current cursor position and with attributes.
  -- Position is not updated.
  procedure PUT_INT (INT        : in INT_CHAR;
                     NAME       : in WINDOW := SCREEN;
                     FOREGROUND : in COLORS := CURRENT;
                     BLINK_STAT : in BLINK_STATS := CURRENT;
                     BACKGROUND : in BASIC_COLORS := CURRENT) is
    FG : EFFECTIVE_COLORS;
    BL : EFFECTIVE_BLINK_STATS;
    BG : EFFECTIVE_BASIC_COLORS;
  begin
    if INT /= CHARACTER'POS(ASCII.CR) then
      -- put character
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
      X_MNG.X_MOVE(ID, NAME.UPPER_LEFT.ROW + NAME.CURRENT_POS.ROW,
                       NAME.UPPER_LEFT.COL + NAME.CURRENT_POS.COL);
      SET_ATTRIBUTES (FG, BL, BG, NAME.CURRENT_XOR_MODE);
      X_MNG.X_PUT_CHAR (ID, CHARACTER'VAL(INT));
      X_MNG.X_MOVE(ID, NAME.UPPER_LEFT.ROW + NAME.CURRENT_POS.ROW,
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

  -- Writes a character at the current cursor position and with attributes.
  -- CR only is interpreted
  procedure PUT (C          : in CHARACTER;
                 NAME       : in WINDOW := SCREEN;
                 FOREGROUND : in COLORS := CURRENT;
                 BLINK_STAT : in BLINK_STATS := CURRENT;
                 BACKGROUND : in BASIC_COLORS := CURRENT;
                 MOVE       : in BOOLEAN := TRUE) is
    ROW : ROW_RANGE;
    COL : COL_RANGE;
  begin
    if NAME = null then
      raise WINDOW_NOT_OPEN;
    end if;
    if not MOVE then
      PUT_NOT_MOVE(C, NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
    else
      ROW := NAME.CURRENT_POS.ROW;
      if C = ASCII.CR then
        COL := NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL;
      else
        -- put character
        PUT_NOT_MOVE(C, NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
        COL := NAME.CURRENT_POS.COL;
      end if;

      -- update position
      if COL /= NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL then
        -- next col
        COL := COL_RANGE'SUCC(COL);
      else
        -- 1st col
        COL := COL_RANGE'FIRST;
        if ROW /= NAME.LOWER_RIGHT.ROW - NAME.UPPER_LEFT.ROW then
          -- next_line
          ROW := ROW_RANGE'SUCC(ROW);
        else
          -- scroll up 1
          CON_IO.MOVE(ROW, 0, NAME);
        end if;
      end if;
      -- move cursor
      CON_IO.MOVE(ROW, COL, NAME);
    end if;

  end PUT;

  -- Idem with a string
  procedure PUT (S          : in STRING;
                 NAME       : in WINDOW := SCREEN;
                 FOREGROUND : in COLORS := CURRENT;
                 BLINK_STAT : in BLINK_STATS := CURRENT;
                 BACKGROUND : in BASIC_COLORS := CURRENT;
                 MOVE       : in BOOLEAN := TRUE) is
  begin
    if NAME = null then
      raise WINDOW_NOT_OPEN;
    end if;
    -- Check empty string
    if S = "" then
      return;
    end if;
    -- put all the string except last character
    for I in INTEGER range S'FIRST .. INTEGER'PRED(S'LAST) loop
      -- put character
      PUT(S(I), NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
    end loop;
    -- put last character
    PUT(S(S'LAST), NAME, FOREGROUND, BLINK_STAT, BACKGROUND, MOVE);
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

  -- Take first character of keyboard buffer
  -- no echo
  procedure PAUSE is
    KEY     : NATURAL;
    IS_CHAR : BOOLEAN;
    CTRL    : BOOLEAN;
    SHIFT   : BOOLEAN;
  begin
    GET_KEY(KEY, IS_CHAR, CTRL, SHIFT);
  end PAUSE;

  procedure NEXT_X_EVENT (TIMEOUT_MS : in out INTEGER;
                          X_EVENT : out X_MNG.EVENT_KIND) is
    EVENT : BOOLEAN;
    use X_MNG;
  begin
    loop
      if not X_EVENT_WAITING then
        -- Wait
        X_MNG.X_SELECT (TIMEOUT_MS, EVENT);
        if not EVENT then
          X_EVENT := X_MNG.DISCARD;
          return;
        end if;
      end if;
      X_MNG.X_PROCESS_EVENT (ID, X_EVENT, X_EVENT_WAITING);
      if X_EVENT /= X_MNG.DISCARD then
        return;
      end if;
    end loop;
  end NEXT_X_EVENT;

  procedure GET_X_KEY (KEY     : out NATURAL;
                       IS_CHAR : out BOOLEAN;
                       CTRL    : out BOOLEAN;
                       SHIFT   : out BOOLEAN) is
    KBD_TAB : X_MNG.KBD_TAB_CODE;
    use X_MNG;
  begin
    X_MNG.X_READ_KEY(ID, KBD_TAB);

    KEY := NATURAL (KBD_TAB.TAB(KBD_TAB.NBRE));
    IS_CHAR := TRUE;
    CTRL := FALSE;
    SHIFT := FALSE;

    -- Optimisation
    if KBD_TAB.NBRE = 1 then
      return;
    elsif KBD_TAB.NBRE = 2 then
      IS_CHAR := FALSE;
      return;
    end if;

    if KBD_TAB.NBRE mod 2 = 0 then
      IS_CHAR := FALSE;
      KBD_TAB.NBRE := KBD_TAB.NBRE - 2;
    else
      IS_CHAR := TRUE;
      KBD_TAB.NBRE := KBD_TAB.NBRE - 1;
    end if;

    if KBD_TAB.TAB(2) = 16#E3# then
      -- Ctrl
      CTRL := TRUE;
    else
      -- Shift
      SHIFT := TRUE;
    end if;
    if KBD_TAB.NBRE = 4 then
      -- Ctrl Shift
      CTRL := TRUE;
      SHIFT := TRUE;
    end if;
    
  end GET_X_KEY;

  -- Gives first key code of keyboard buffer, (waits if it is empty)
  -- no echo
  procedure GET_KEY (KEY     : out NATURAL;
                     IS_CHAR : out BOOLEAN;
                     CTRL    : out BOOLEAN;
                     SHIFT   : out BOOLEAN) is
    X_EVENT : X_MNG.EVENT_KIND;
    TIMEOUT : INTEGER := -1;
    use X_MNG;
  begin
    if not INIT_DONE then
      raise NOT_INIT;
    end if;
    -- Wait for keyboard
    loop
      NEXT_X_EVENT (TIMEOUT, X_EVENT);
      if X_EVENT = X_MNG.REFRESH then
        KEY := 0;
        IS_CHAR := TRUE;
        CTRL := FALSE;
        SHIFT := FALSE;
        return;
      end if;
      exit when X_EVENT = X_MNG.KEYBOARD;
    end loop;

    GET_X_KEY (KEY, IS_CHAR, CTRL, SHIFT);
  end GET_KEY;
      

  -- Gets a character (echo)
  function GET (NAME : WINDOW := SCREEN) return CHARACTER is
    KEY     : NATURAL;
    IS_CHAR : BOOLEAN;
    CTRL    : BOOLEAN;
    SHIFT   : BOOLEAN;
    CHAR    : CHARACTER;
  begin
    loop
      GET_KEY(KEY, IS_CHAR, CTRL, SHIFT);
      exit when IS_CHAR and then not CTRL and then not SHIFT;
    end loop;
    CHAR := CHARACTER'VAL(KEY);
    PUT(CHAR, NAME);
    return CHAR;
  end GET;

  -- check if a key is available until a certain time.
  procedure GET_KEY_TIME (LAST_TIME     : in CALENDAR.TIME;
                          INFINITE_TIME : in BOOLEAN;
                          CHECK_BREAK   : in BOOLEAN;
                          EVENT         : out EVENT_LIST;
                          KEY           : out NATURAL;
                          IS_CHAR       : out BOOLEAN;
                          CTRL          : out BOOLEAN;
                          SHIFT         : out BOOLEAN) is

    X_EVENT : X_MNG.EVENT_KIND;
    CUR_TIME : CALENDAR.TIME;
    DUR : DURATION;
    TIMEOUT_MS : INTEGER;
    use X_MNG, CALENDAR;
  begin
    if not INIT_DONE then
      raise NOT_INIT;
    end if;
    if INFINITE_TIME then
      TIMEOUT_MS := -1;
    else
      CUR_TIME := CALENDAR.CLOCK;
      if CUR_TIME > LAST_TIME then
        TIMEOUT_MS := 0;
      else
        DUR := LAST_TIME - CUR_TIME;
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
        GET_X_KEY (KEY, IS_CHAR, CTRL, SHIFT);
        -- Check break
        if CHECK_BREAK then
          if (KEY = CHARACTER'POS('c') or else KEY = 0)
          and then IS_CHAR and then CTRL and then not SHIFT then
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
                 TIME_OUT   : in DURATION :=  -1.0) is
    LSTR : STRING(STR'range ) := (others => ' ');
    LPOS : POSITIVE;
    LINS : BOOLEAN;
  begin
    LPOS := 1;
    LINS := FALSE;
    PUT_THEN_GET(LSTR, LAST, STAT, LPOS, LINS, NAME, FOREGROUND, BLINK_STAT,
      BACKGROUND, TIME_OUT);
    STR := LSTR;
    POS := LPOS;
    INSERT := LINS;
  end GET;

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
                          TIME_OUT   : in DURATION :=  -1.0) is
    WIDTH         : constant NATURAL := STR'LENGTH;
    LSTR          : STRING(1 .. WIDTH) := STR;
    KEY           : NATURAL;
    IS_CHAR       : BOOLEAN;
    CTRL, SHIFT   : BOOLEAN;
    REDRAW        : BOOLEAN;
    FIRST_POS     : constant SQUARE := NAME.CURRENT_POS;
    LAST_TIME     : CALENDAR.TIME;
    INFINITE_TIME : constant BOOLEAN := TIME_OUT < 0.0;
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
    begin
      MOVE(FIRST_POS.ROW, FIRST_POS.COL + POS - 1, NAME);
      if SHOW then
        if INSERT then
          X_MNG.X_OVERWRITE_CHAR (ID, 2);
        else
          X_MNG.X_OVERWRITE_CHAR (ID, 95);
        end if;
      else
        X_MNG.X_PUT_CHAR (ID, LSTR(POS));
      end if;
      MOVE(FIRST_POS.ROW, FIRST_POS.COL + POS - 1, NAME);
    end CURSOR;


  begin
    if not INFINITE_TIME then
      -- time at which the get ends
      LAST_TIME := CALENDAR."+"(CALENDAR.CLOCK, TIME_OUT);
    else
      -- to give a value (which won't be used)
      LAST_TIME := CALENDAR.CLOCK;
    end if;

    -- Emtpy string
    if WIDTH = 0 then
      LAST := STR'LAST;

      loop
        GET_KEY_TIME (LAST_TIME, INFINITE_TIME, TRUE,
                      EVENT, KEY, IS_CHAR, CTRL, SHIFT);
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
              STAT := LEFT;
              return;
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
              STAT := PGUP;
              return;
            when 16#54# =>
              -- Down
              STAT := DOWN;
              return;
            when 16#56# =>
              -- Page Down
              STAT := PGDOWN;
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
      MOVE (POSITION);
    end if;  -- string'length = 0

    -- Check width and current_pos / window's width
    if FIRST_POS.COL + WIDTH - 1 > NAME.LOWER_RIGHT.COL - NAME.UPPER_LEFT.COL
      then
      raise STRING_TOO_LONG;
    end if;

    -- put the string
    for I in LSTR'range loop
      MOVE(FIRST_POS.ROW, FIRST_POS.COL + I - 1, NAME);
      PUT_NOT_MOVE(LSTR(I), NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
    end loop;

    loop
      -- show cursor
      CURSOR (TRUE);
      REDRAW := FALSE;
      -- try to get a key
      GET_KEY_TIME (LAST_TIME, INFINITE_TIME, TRUE,
                    EVENT, KEY, IS_CHAR, CTRL, SHIFT);
      -- hide cursor
      CURSOR (FALSE);
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
            else
              LSTR(1 .. WIDTH - 1) := LSTR(2 .. WIDTH);
              LSTR(WIDTH) := ' ';
              STR := LSTR;
              LAST := PARSE;
              STAT := LEFT;
              return;
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
            STAT := PGUP;
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
            STAT := PGDOWN;
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
          if not REDRAW then
            PUT(CHARACTER'VAL(KEY), NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
          end if;
        end if;
      end if;  -- is_char

      -- redraw if necessary
      if REDRAW then
        for I in LSTR'RANGE loop
          MOVE(FIRST_POS.ROW, FIRST_POS.COL + I - 1, NAME);
          PUT_NOT_MOVE(LSTR(I), NAME, FOREGROUND, BLINK_STAT, BACKGROUND);
        end loop;
     end if;
    end loop;
  end PUT_THEN_GET;

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
      return CON_IO.X_MAX;
    end X_MAX;

    function Y_MAX return Y_RANGE is
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      return CON_IO.Y_MAX;
    end Y_MAX;

    -- Font characteristics
    function FONT_WIDTH  return NATURAL is
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      return CON_IO.FONT_WIDTH;
    end FONT_WIDTH;

    function FONT_HEIGHT return NATURAL is
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      return CON_IO.FONT_HEIGHT;
    end FONT_HEIGHT;

    function FONT_OFFSET return NATURAL is
    begin
      if not INIT_DONE then
        raise NOT_INIT;
      end if;
      return CON_IO.FONT_OFFSET;
    end FONT_OFFSET;


    procedure SET_SCREEN_ATTRIBUTES is
    begin
      SET_ATTRIBUTES (SCREEN.CURRENT_FOREGROUND, NOT_BLINK,
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
                              X, CON_IO.Y_MAX - Y);
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
      LY := CON_IO.Y_MAX - Y;
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
      X_MNG.X_DRAW_POINT(ID, X, CON_IO.Y_MAX - Y);
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
      X_MNG.X_DRAW_LINE(ID, X1, CON_IO.Y_MAX - Y1, X2, CON_IO.Y_MAX - Y2);
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
      X_MNG.X_DRAW_RECTANGLE(ID, X1, CON_IO.Y_MAX - Y1, X2, CON_IO.Y_MAX - Y2);
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
      -- In screen? 
      if       LX in GRAPHICS.X_RANGE and then LX <= X_MAX
      and then LY in GRAPHICS.Y_RANGE and then LY <= Y_MAX then
        X := LX;
        Y := CON_IO.Y_MAX - LY;
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
      if       ROW in GRAPHICS.X_RANGE and then ROW <= X_MAX
      and then COL in GRAPHICS.Y_RANGE and then COL <= Y_MAX then
        LOC_EVENT.VALID := TRUE;
        LOC_EVENT.X := ROW;
        LOC_EVENT.Y := CON_IO.Y_MAX - COL;
      else
        LOC_EVENT.VALID := FALSE;
        if ROW in GRAPHICS.X_RANGE and then ROW <= X_MAX then
          LOC_EVENT.X := ROW;
        elsif ROW < GRAPHICS.X_RANGE'FIRST then
          LOC_EVENT.X := GRAPHICS.X_RANGE'FIRST;
        elsif ROW > X_MAX then
          LOC_EVENT.X := X_MAX;
        end if;
        if COL in GRAPHICS.Y_RANGE and then COL <= Y_MAX then
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

end CON_IO;

 
