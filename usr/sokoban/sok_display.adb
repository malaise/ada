with NORMAL;
with DAY_MNG;
with CON_IO;
with TIMERS;
with DOS;
with SOK_INPUT;
with SOK_TIME;

-- displaying of sokoban
package body SOK_DISPLAY is

  LEN_MOVES  : constant := 5;
  LEN_PUSHES : constant := 5;
  LEN_DAYS   : constant := 3;

  TITLE_WIN : CON_IO.WINDOW;
  FRAME_WIN : CON_IO.WINDOW;
  LINE_WIN  : CON_IO.WINDOW;
  SCORE_WIN : CON_IO.WINDOW;
  TIME_WIN  : CON_IO.WINDOW;
  HELP_WIN  : CON_IO.WINDOW;
  MENU_WIN  : CON_IO.WINDOW;
  ERROR_WIN : CON_IO.WINDOW;
  GET_WIN   : CON_IO.WINDOW;

  CURRENT_ACTION : ACTION_LIST;
  CURRENT_ALLOW_WRITE : BOOLEAN;

  procedure INIT is
    HELP_ROW : constant := 1;
    HELP_COL : constant := 1;
  begin
    CON_IO.INIT;
    CON_IO.RESET_TERM;

    -- TITLE      : row 00 to 00 col 00 to 57 (01 row, 57 col)
    CON_IO.OPEN (TITLE_WIN, (00, 00), (00, 55) );

    -- TIME ZONE  : row 00 to 00 col 57 to 76 (01 row 20 col)
    CON_IO.OPEN (TIME_WIN,  (00, 57), (00, 76) );

    -- FRAME      : row 02 to 17 col 05 to 42 (16 row 38 col) (38=19*2)
    CON_IO.OPEN (FRAME_WIN, (02, 05), (17, 42) );

    -- STATE_LINE : row 20 to 20 col 10 to 75 (01 row 66 col)
    CON_IO.OPEN (LINE_WIN,  (20, 10), (20, 75) );

    -- SCORE_LINE : row 21 to 20 col 10 to 75 (01 row 66 col)
    CON_IO.OPEN (SCORE_WIN,  (21, 10), (21, 75) );

    -- HELP BESIDE: row 05 to 15 col 55 to 77 (left to frame)
    CON_IO.OPEN (HELP_WIN,  (05, 55), (15, 77) );
    CON_IO.SET_FOREGROUND (CON_IO.LIGHT_GREEN, NAME => HELP_WIN);

    -- MENU       : row 19 to 21 col 02 to 79 (bottom)
    CON_IO.OPEN (MENU_WIN,  (19, 00), (21, 79) );

    -- ERROR      : row 19 to 21 col 02 to 79 (bottom)
    CON_IO.OPEN (ERROR_WIN, (19, 00), (21, 79) );

    -- GET        : row 22 to 25 col 19 to 59 (bottom)
    CON_IO.OPEN (GET_WIN,   (22, 19), (24, 59) );

  end INIT;

  procedure PUT_HELP (HELP : in ACTION_LIST) is
  begin
    CON_IO.CLEAR (HELP_WIN);
    case HELP is
      when FRAME =>
        CON_IO.PUT_LINE (" Arrows", NAME => HELP_WIN);
        CON_IO.PUT ("  for movements", NAME => HELP_WIN);
        CON_IO.NEW_LINE (HELP_WIN, 2);

        CON_IO.PUT_LINE (" u  or  Backspace", NAME => HELP_WIN);
        CON_IO.PUT ("  for undo", NAME => HELP_WIN);
        CON_IO.NEW_LINE (HELP_WIN, 2);

        CON_IO.PUT_LINE (" Ctrl Break or Ctrl C", NAME => HELP_WIN);
        CON_IO.PUT ("  to quit", NAME => HELP_WIN);
      when DONE =>
        CON_IO.PUT_LINE ("  - FRAME completed -", BLINK_STAT => CON_IO.BLINK,
                         NAME => HELP_WIN);
        CON_IO.NEW_LINE (HELP_WIN, 2);

        CON_IO.PUT_LINE (" Space or Return", NAME => HELP_WIN);
        CON_IO.PUT ("  for next frame", NAME => HELP_WIN);
        CON_IO.NEW_LINE (HELP_WIN, 2);

        CON_IO.PUT_LINE (" Ctrl Break or Ctrl C", NAME => HELP_WIN);
        CON_IO.PUT ("  to quit", NAME => HELP_WIN);
      when WRITE =>
        CON_IO.MOVE ( (02, 00), NAME => HELP_WIN);
        CON_IO.PUT_LINE (" Save current", NAME => HELP_WIN);
        CON_IO.PUT_LINE ("  frame and movements", NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE (" Only one frame saved", NAME => HELP_WIN);
        CON_IO.PUT_LINE ("  at a time", NAME => HELP_WIN);
      when READ =>
        CON_IO.MOVE ( (02, 00), NAME => HELP_WIN);
        CON_IO.PUT_LINE (" Restore last saved", NAME => HELP_WIN);
        CON_IO.PUT_LINE ("  frame and movements", NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE (" Only one frame saved", NAME => HELP_WIN);
        CON_IO.PUT_LINE ("  at a time", NAME => HELP_WIN);
      when RESET =>
        CON_IO.MOVE ( (03, 00), NAME => HELP_WIN);
        CON_IO.PUT_LINE (" Restart current frame", NAME => HELP_WIN);
        CON_IO.PUT_LINE ("  from the beginning", NAME => HELP_WIN);
      when GET_NEW =>
        CON_IO.MOVE ( (03, 00), NAME => HELP_WIN);
        CON_IO.PUT_LINE (" Start a new frame", NAME => HELP_WIN);
        CON_IO.PUT_LINE ("  from the beginning", NAME => HELP_WIN);
      when BREAK =>
        CON_IO.MOVE ( (05, 00), NAME => HELP_WIN);
        CON_IO.PUT_LINE (" Exit SOKOBAN", NAME => HELP_WIN);

    end case;

    case HELP is
      when FRAME | DONE =>
        CON_IO.MOVE ( (09, 00) ,HELP_WIN);
        CON_IO.PUT_LINE (" Esc", NAME => HELP_WIN);
        CON_IO.PUT ("  for command menu", NAME => HELP_WIN);
      when others =>
        CON_IO.MOVE ( (09, 00) ,HELP_WIN);
        CON_IO.PUT_LINE (" Esc", NAME => HELP_WIN);
        CON_IO.PUT ("  to play again", NAME => HELP_WIN);
    end case;
  end PUT_HELP;

  -- puts all the frame
  procedure PUT_FRAME (FRAME : in SOK_TYPES.FRAME_TAB) is
  begin
    CON_IO.MOVE ( (00, 20), NAME => TITLE_WIN);
    CON_IO.PUT ("S O K O B A N", TITLE_WIN,
     FOREGROUND => CON_IO.WHITE, MOVE => FALSE);
    CON_IO.MOVE ( (0, 50), TITLE_WIN);
    CON_IO.PUT ("Time :", TITLE_WIN, MOVE => FALSE);

    CON_IO.CLEAR (FRAME_WIN);
    for I in SOK_TYPES.ROW_RANGE loop
      for J in SOK_TYPES.COL_RANGE loop
        PUT_SQUARE (FRAME (I,J), (ROW =>I, COL =>J) );
      end loop;
    end loop;
  end PUT_FRAME;

  -- puts a square
  procedure PUT_SQUARE (SQUARE     : in SOK_TYPES.SQUARE_REC;
                        COORDINATE : in SOK_TYPES.COORDINATE_REC;
                        BLINK      : in BOOLEAN := FALSE) is
  begin

    CON_IO.MOVE ((COORDINATE.ROW - 1, (COORDINATE.COL - 1) * 2),
     NAME => FRAME_WIN);
    case SQUARE.PATTERN is
      when SOK_TYPES.WALL =>
        CON_IO.PUT ("  ", FRAME_WIN, BACKGROUND => CON_IO.LIGHT_GRAY,
         MOVE => FALSE);
      when SOK_TYPES.TARGET =>
        case SQUARE.CONTENT is
          when SOK_TYPES.NOTHING =>
            CON_IO.PUT ("* ", FRAME_WIN, FOREGROUND => CON_IO.RED,
             MOVE => FALSE);
          when SOK_TYPES.MAN =>
            CON_IO.PUT ("!!", FRAME_WIN, FOREGROUND => CON_IO.RED,
             MOVE => FALSE);
          when SOK_TYPES.BOX =>
            if BLINK then
              CON_IO.PUT ("[]", FRAME_WIN,
                                FOREGROUND => CON_IO.RED,
                                BLINK_STAT => CON_IO.BLINK,
                                MOVE => FALSE);
            else
              CON_IO.PUT ("[]", FRAME_WIN,
                                FOREGROUND => CON_IO.RED,
                                BLINK_STAT => CON_IO.NOT_BLINK,
                                MOVE => FALSE);
            end if;
        end case;
      when SOK_TYPES.FREE =>
        case SQUARE.CONTENT is
          when SOK_TYPES.NOTHING =>
            CON_IO.PUT ("  ", FRAME_WIN, MOVE => FALSE);
          when SOK_TYPES.MAN =>
            CON_IO.PUT ("!!", FRAME_WIN,  FOREGROUND => CON_IO.CYAN,
                        MOVE => FALSE);
          when SOK_TYPES.BOX =>
            CON_IO.PUT ("[]", FRAME_WIN, MOVE => FALSE);
        end case;
    end case;
  end PUT_SQUARE;


  -- puts the down line
  procedure PUT_LINE (MOVES : in NATURAL; PUSHES : in NATURAL;
                      BOXES_IN : in NATURAL; NB_BOXES : in POSITIVE;
                      FRAME : in SOK_TYPES.FRAME_RANGE) is
  begin
    CON_IO.MOVE (NAME => LINE_WIN);
    CON_IO.PUT ("Frame : "      & NORMAL (FRAME, 2),          LINE_WIN);
    CON_IO.PUT ("    Moves : "  & NORMAL (MOVES, LEN_MOVES),  LINE_WIN);
    CON_IO.PUT ("    Pushes : " & NORMAL (PUSHES, LEN_MOVES), LINE_WIN);
    CON_IO.PUT ("    Boxes : " & NORMAL (BOXES_IN, 2) & '/' & NORMAL (NB_BOXES, 2), LINE_WIN,
     MOVE => FALSE);
  end PUT_LINE;

  function TIME_IMAGE (DAY : NATURAL; TIME : CALENDAR.DAY_DURATION) return STRING is
    HOURS    : DAY_MNG.T_HOURS;
    MINUTES  : DAY_MNG.T_MINUTES;
    SECONDS  : DAY_MNG.T_SECONDS;
    MILLISEC : DAY_MNG.T_MILLISEC;
    STR : STRING (1 .. LEN_DAYS+16);
  begin
    DAY_MNG.SPLIT (TIME, HOURS, MINUTES, SECONDS, MILLISEC);
    if DAY = 0 then
      STR(1 .. LEN_DAYS+5) := "   " & "     ";
    elsif DAY = 1 then
      STR(1 .. LEN_DAYS+5) := NORMAL (DAY, LEN_DAYS) & " day ";
    else
      STR(1 .. LEN_DAYS+5) := NORMAL (DAY, LEN_DAYS) & " days";
    end if;
    STR(LEN_DAYS+6 .. LEN_DAYS+16) := " " &
                NORMAL (HOURS,   2, GAP => '0') & "h" &
                NORMAL (MINUTES, 2, GAP => '0') & "mn" &
                NORMAL (SECONDS, 2, GAP => '0') & "s";
    return STR;
  end TIME_IMAGE;

  procedure PUT_TIME (DAY : in NATURAL; TIME : in CALENDAR.DAY_DURATION) is

  begin
    CON_IO.MOVE (NAME => TIME_WIN);
    CON_IO.PUT (TIME_IMAGE(DAY, TIME),
                TIME_WIN,
                MOVE => FALSE);
  end PUT_TIME;

  procedure PUT_SCORE (SCORE : in SOK_TYPES.SCORE_REC) is
  begin
    CON_IO.MOVE (NAME => SCORE_WIN);
    if SCORE.SET then
      CON_IO.PUT ("Best results", SCORE_WIN);
      CON_IO.PUT ("  Moves : "  & NORMAL (SCORE.MOVES, LEN_MOVES),  SCORE_WIN);
      CON_IO.PUT ("    Pushes : " & NORMAL (SCORE.PUSHES, LEN_MOVES), SCORE_WIN);
      CON_IO.PUT ("  " & TIME_IMAGE(SCORE.DAY, SCORE.DUR),
                  SCORE_WIN,
                  MOVE => FALSE);
    else
      CON_IO.CLEAR (SCORE_WIN);
    end if;
  end PUT_SCORE;

  procedure PUT_ACTION (ACTION : in MENU_ACTION_LIST; SELECTED : in BOOLEAN) is
    COLOR : CON_IO.EFFECTIVE_COLORS;
    BLINK : CON_IO.EFFECTIVE_BLINK_STATS;
    LEN_FIELD : constant := 11;
    PAD       : constant :=  4;
    WRITE_COL   : constant := PAD;
    READ_COL    : constant := WRITE_COL   + LEN_FIELD + PAD;
    RESET_COL   : constant := READ_COL    + LEN_FIELD + PAD;
    GET_NEW_COL : constant := RESET_COL   + LEN_FIELD + PAD;
    BREAK_COL   : constant := GET_NEW_COL + LEN_FIELD + PAD;
  begin
    if not SELECTED then
      COLOR := CON_IO.LIGHT_GRAY;
      BLINK := CON_IO.NOT_BLINK;
    else
      COLOR := CON_IO.GREEN;
      BLINK := CON_IO.BLINK;
    end if;

    case ACTION is
      when WRITE =>
        if CURRENT_ALLOW_WRITE then
          CON_IO.MOVE ( (0, WRITE_COL), MENU_WIN);
          CON_IO.PUT ("           ", MENU_WIN, FOREGROUND => COLOR,
           BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
          CON_IO.MOVE ( (1, WRITE_COL), MENU_WIN);
          CON_IO.PUT ("   SAVE    ", MENU_WIN, FOREGROUND => COLOR,
           BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
          CON_IO.MOVE ( (2, WRITE_COL), MENU_WIN);
          CON_IO.PUT ("           ", MENU_WIN, FOREGROUND => COLOR,
           BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
        else
          CON_IO.MOVE ( (1, WRITE_COL), MENU_WIN);
          CON_IO.PUT ("   SAVE    ", MENU_WIN, MOVE => FALSE);
        end if;
      when READ =>
        CON_IO.MOVE ( (0, READ_COL), MENU_WIN);
        CON_IO.PUT ("           ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
        CON_IO.MOVE ( (1, READ_COL), MENU_WIN);
        CON_IO.PUT ("  RESTORE  ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
        CON_IO.MOVE ( (2, READ_COL), MENU_WIN);
        CON_IO.PUT ("           ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
      when RESET =>
        CON_IO.MOVE ( (0, RESET_COL), MENU_WIN);
        CON_IO.PUT ("           ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
        CON_IO.MOVE ( (1, RESET_COL), MENU_WIN);
        CON_IO.PUT ("   RESET   ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
        CON_IO.MOVE ( (2, RESET_COL), MENU_WIN);
        CON_IO.PUT ("           ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
      when GET_NEW =>
        CON_IO.MOVE ( (0, GET_NEW_COL), MENU_WIN);
        CON_IO.PUT ("           ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
        CON_IO.MOVE ( (1, GET_NEW_COL), MENU_WIN);
        CON_IO.PUT (" GOTO NEW  ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
        CON_IO.MOVE ( (2, GET_NEW_COL), MENU_WIN);
        CON_IO.PUT ("           ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
      when BREAK =>
        CON_IO.MOVE ( (0, BREAK_COL), MENU_WIN);
        CON_IO.PUT ("           ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
        CON_IO.MOVE ( (1, BREAK_COL), MENU_WIN);
        CON_IO.PUT ("    EXIT   ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
        CON_IO.MOVE ( (2, BREAK_COL), MENU_WIN);
        CON_IO.PUT ("           ", MENU_WIN, FOREGROUND => COLOR,
         BLINK_STAT => BLINK, BACKGROUND => CON_IO.BLUE, MOVE => FALSE);
    end case;
  end PUT_ACTION;

  procedure PUT_MENU (
   INIT_ACTION : in MENU_ACTION_LIST;
   ALLOW_WRITE : in BOOLEAN) is
  begin
    if not ALLOW_WRITE and then INIT_ACTION = WRITE then
      raise CONSTRAINT_ERROR;
    end if;
    CURRENT_ACTION := INIT_ACTION;
    CURRENT_ALLOW_WRITE := ALLOW_WRITE;
    for I in MENU_ACTION_LIST loop
      PUT_ACTION (I, I = INIT_ACTION);
    end loop;
  end PUT_MENU;

  procedure UPDATE_MENU (NEW_ACTION : in MENU_ACTION_LIST) is
  begin
    PUT_ACTION (CURRENT_ACTION, FALSE);
    CURRENT_ACTION := NEW_ACTION;
    PUT_ACTION (CURRENT_ACTION, TRUE);
  end UPDATE_MENU;

  procedure CLEAR_MENU is
  begin
    CON_IO.CLEAR (MENU_WIN);
  end CLEAR_MENU;

  procedure PUT_ERROR (ERROR : in ERROR_LIST) is
  begin
    CON_IO.SET_BACKGROUND (CON_IO.RED, NAME => ERROR_WIN);
    CON_IO.CLEAR (ERROR_WIN);
    CON_IO.MOVE ( (1, 30) , ERROR_WIN);
    case ERROR is
      when NO_DATA =>
        CON_IO.PUT ("DATA FILE NOT FOUND", ERROR_WIN);
      when READ =>
        CON_IO.PUT ("ERROR READING DATA", ERROR_WIN);

      when NO_FRAME =>
        CON_IO.PUT ("NO FRAME SAVED", ERROR_WIN);
      when RESTORE =>
        CON_IO.PUT ("ERROR RESTORING FRAME", ERROR_WIN);
      when SAVE =>
        CON_IO.PUT ("ERROR SAVING FRAME", ERROR_WIN);

      when INIT_SCORE =>
        CON_IO.PUT ("ERROR INITIALIZING SCORES", ERROR_WIN);
      when SCORE_IO =>
        CON_IO.PUT ("ERROR READ/WRITE SCORE", ERROR_WIN);

      when INTERNAL =>
        CON_IO.PUT ("INTERNAL ERROR", ERROR_WIN);

      when FORMAT =>
        CON_IO.PUT ("ERROR. NUMBER REQUIRED (1 .. 50)", ERROR_WIN);
    end case;
    CON_IO.MOVE ( (2, 65), ERROR_WIN);
    CON_IO.PUT ("Hit a key", ERROR_WIN);
  end PUT_ERROR;

  procedure CLEAR_ERROR is
  begin
    CON_IO.SET_BACKGROUND (CON_IO.DEFAULT_BACKGROUND, ERROR_WIN);
    CON_IO.CLEAR (ERROR_WIN);
  end CLEAR_ERROR;

  -- get frame number
  procedure GET_NO_FRAME (NO : out SOK_TYPES.FRAME_RANGE; RESULT : out GET_RESULT_LIST) is

    STR : STRING (1..2) := (others => ' ');
    LAST : NATURAL;
    STAT : CON_IO.CURS_MVT;
    POS  : POSITIVE := 1;
    INS  : BOOLEAN := FALSE;
  begin
    CON_IO.SET_BACKGROUND (CON_IO.CYAN, GET_WIN);
    CON_IO.SET_FOREGROUND (CON_IO.BLACK, NAME => GET_WIN);
    CON_IO.CLEAR (GET_WIN);
    CON_IO.MOVE ( (01, 07), GET_WIN);
    CON_IO.PUT ("Enter frame no (" &
                NORMAL (SOK_TYPES.FRAME_RANGE'FIRST, 1) &
                " to " &
                NORMAL (SOK_TYPES.FRAME_RANGE'LAST, 2) &
                ") : ", GET_WIN);
    CON_IO.MOVE ( (02, 02), GET_WIN);
    CON_IO.PUT ("Enter to validate, Escape to give_up", GET_WIN);

    loop
      CON_IO.MOVE ( (01, 34), GET_WIN);
      CON_IO.PUT_THEN_GET (STR, LAST, STAT, POS, INS, GET_WIN,
       FOREGROUND => CON_IO.LIGHT_GRAY, BACKGROUND => CON_IO.BLACK,
       TIME_OUT => (DELAY_KIND => TIMERS.DELAY_SEC,
                    PERIOD     => CON_IO.NO_PERIOD,
                    DELAY_SECONDS => 1.0) );

      case STAT is
        when CON_IO.ESC =>
          RESULT := ESC;
          exit;

        when CON_IO.REFRESH =>
          RESULT := REFRESH;
          exit;

        when CON_IO.BREAK =>
          raise SOK_INPUT.BREAK_REQUESTED;

        when CON_IO.TIMEOUT =>
          SOK_TIME.DISP_TIME;

        when CON_IO.RET =>
          -- digit or space allowed
          for I in STR'RANGE loop
            if STR(I) not in '0' .. '9' and then STR(I) /= ' ' then
              raise FORMAT_ERROR;
            end if;
          end loop;
          -- not empty
          if LAST = 0 then
            RESULT := ESC;
            exit;
          end if;
          begin
            NO := SOK_TYPES.FRAME_RANGE'VALUE (STR (1..LAST));
            RESULT := SET;
            exit;
          exception
            when CONSTRAINT_ERROR =>
              raise FORMAT_ERROR;
          end;
        when others =>
          null;
        end case;
    end loop;

    CON_IO.SET_BACKGROUND (CON_IO.DEFAULT_BACKGROUND, GET_WIN);
    CON_IO.CLEAR (GET_WIN);

  exception
    when FORMAT_ERROR =>
      CON_IO.SET_BACKGROUND (CON_IO.DEFAULT_BACKGROUND, GET_WIN);
      CON_IO.CLEAR (GET_WIN);
      raise;
  end GET_NO_FRAME;



  procedure END_OF_PROGRAM is
  begin
    CON_IO.CLEAR;
  end END_OF_PROGRAM;


end SOK_DISPLAY;
