with NORMAL;
package body SCREEN is

  -------------------------------
  -- GLOBAL SCREEN DEFINITIONS --
  -------------------------------
  GLOBAL_WIN, SECRET_WIN, PROPAL_WIN, TRY_WIN,
   COLOR_WIN, HELP_WIN, MENU_WIN, LEVEL_WIN, EXIT_WIN : CON_IO.WINDOW;

  -- Fixed geometry
  PROPAL_COL_WIDTH : constant CON_IO.COL_RANGE :=  2;
  PROPAL_LAST_ROW  : constant CON_IO.ROW_RANGE := 22;
  PROPAL_FIRST_ROW : constant CON_IO.ROW_RANGE :=
   PROPAL_LAST_ROW - (CON_IO.ROW_RANGE(COMMON.MAX_NUMBER_PROPAL)-1) * 2;
  PROPAL_LAST_COL  : constant CON_IO.COL_RANGE := 15;
  TRY_FIRST_COL    : constant CON_IO.COL_RANGE := 22;
  COLOR_COL_WIDTH  : constant CON_IO.COL_RANGE :=  2;
  COLOR_FIRST_ROW  : constant CON_IO.ROW_RANGE :=  7;
  COLOR_LAST_ROW   : constant CON_IO.ROW_RANGE :=
   COLOR_FIRST_ROW + (CON_IO.ROW_RANGE(COMMON.MAX_NUMBER_COLOR)-1) * 2;
  COLOR_FIRST_COL  : constant CON_IO.COL_RANGE := 36;
  COLOR_LAST_COL   : constant CON_IO.COL_RANGE :=
   COLOR_FIRST_COL + COLOR_COL_WIDTH - 1;
  MENU_ROW : constant CON_IO.ROW_RANGE := 22;
  MENU_FIRST_COL : constant CON_IO.COL_RANGE := 46;
  MENU_LAST_COL : constant CON_IO.COL_RANGE := 56;
  LEVEL_FIRST_COL : constant CON_IO.COL_RANGE := 59;
  LEVEL_LAST_COL : constant CON_IO.COL_RANGE := 65;
  EXIT_FIRST_COL : constant CON_IO.COL_RANGE := 68;
  EXIT_LAST_COL : constant CON_IO.COL_RANGE := 76;

  -- Level dependant gemetry
  CURRENT_LEVEL : COMMON.LAST_LEVEL_RANGE;
  PROPAL_FIRST_COL : CON_IO.COL_RANGE;
  TRY_LAST_COL     : CON_IO.COL_RANGE;

  -- Color definitions
  COLOR_DEFINITION : constant array (COMMON.COLOR_RANGE) of
   CON_IO.EFFECTIVE_COLORS := (
       0 => CON_IO.BROWN,
       1 => CON_IO.BLUE,
       2 => CON_IO.GREEN,
       3 => CON_IO.CYAN,
       4 => CON_IO.RED,
       5 => CON_IO.MAGENTA,
       6 => CON_IO.LIGHT_GRAY,
       7 => CON_IO.ORANGE,
       8 => CON_IO.YELLOW);

  FOREGROUND_COLOR  : constant CON_IO.EFFECTIVE_COLORS := CON_IO.DARK_GRAY;
  BACKGROUND_COLOR  : constant CON_IO.EFFECTIVE_BASIC_COLORS :=
   COLOR_DEFINITION(0);

  -- When possible to try
  TRY_COLOR : constant CON_IO.EFFECTIVE_COLORS := CON_IO.WHITE;
  -- When click in try or menu window
  BACKGROUND_SELECT : constant CON_IO.EFFECTIVE_BASIC_COLORS :=
   CON_IO.LIGHT_GRAY;
  -- Used to answer
  OK_COLOR  : constant CON_IO.EFFECTIVE_COLORS := CON_IO.BLACK;
  NOK_COLOR : constant CON_IO.EFFECTIVE_COLORS := CON_IO.WHITE;

  PIN_INT : constant CON_IO.INT_CHAR := CHARACTER'POS('!');


  procedure SET_MOUSE_DEFAULT_COLOR is
  begin
    null;
  end SET_MOUSE_DEFAULT_COLOR;

  procedure SET_MOUSE_COLOR (COLOR : in COMMON.EFF_COLOR_RANGE) is
  begin
    null;
  end SET_MOUSE_COLOR;


  -- Square, in PROPAL_WIN for a propal & level
  function PROPAL_SQUARE (PROPAL : COMMON.PROPAL_RANGE;
                          LEVEL  : COMMON.LEVEL_RANGE) return CON_IO.SQUARE is
    LOWER_RIGHT : constant CON_IO.SQUARE :=
     CON_IO.GET_RELATIVE_LOWER_RIGHT (PROPAL_WIN);
  begin
    return (LOWER_RIGHT.ROW - (CON_IO.ROW_RANGE(PROPAL)-1) * 2,
            (CON_IO.COL_RANGE(LEVEL)-1) * (PROPAL_COL_WIDTH+1) );
  end PROPAL_SQUARE;




  -- init the screen, the windows, draw borders
  procedure INIT (LEVEL : in COMMON.LAST_LEVEL_RANGE) is
    SQUARE : CON_IO.SQUARE;
    use COMMON;
  begin
    if not CON_IO.IS_OPEN (GLOBAL_WIN) then
      CON_IO.RESET_TERM;
      -- open windows
      CON_IO.OPEN (GLOBAL_WIN, (1, 1), (23, 78));
      CON_IO.OPEN (COLOR_WIN,  (COLOR_FIRST_ROW,  COLOR_FIRST_COL),
                               (COLOR_LAST_ROW,   COLOR_LAST_COL) );
      CON_IO.OPEN (HELP_WIN,   (4, MENU_FIRST_COL),(MENU_ROW-3, 76) );
      CON_IO.OPEN (MENU_WIN,   (MENU_ROW,  MENU_FIRST_COL),
                               (MENU_ROW,  MENU_LAST_COL) );
      CON_IO.OPEN (LEVEL_WIN,  (MENU_ROW,  LEVEL_FIRST_COL),
                               (MENU_ROW,  LEVEL_LAST_COL) );
      CON_IO.OPEN (EXIT_WIN,   (MENU_ROW,  EXIT_FIRST_COL),
                               (MENU_ROW,  EXIT_LAST_COL) );
      -- Set default colors
      CON_IO.SET_FOREGROUND (FOREGROUND_COLOR, NAME => GLOBAL_WIN);
      CON_IO.SET_BACKGROUND (BACKGROUND_COLOR, NAME => GLOBAL_WIN);
      CON_IO.SET_FOREGROUND (FOREGROUND_COLOR, NAME => COLOR_WIN);
      CON_IO.SET_BACKGROUND (BACKGROUND_COLOR, NAME => COLOR_WIN);
      CON_IO.SET_FOREGROUND (FOREGROUND_COLOR, NAME => HELP_WIN);
      CON_IO.SET_BACKGROUND (BACKGROUND_COLOR, NAME => HELP_WIN);
      CON_IO.SET_FOREGROUND (FOREGROUND_COLOR, NAME => MENU_WIN);
      CON_IO.SET_BACKGROUND (BACKGROUND_COLOR, NAME => MENU_WIN);
      CON_IO.SET_FOREGROUND (FOREGROUND_COLOR, NAME => LEVEL_WIN);
      CON_IO.SET_BACKGROUND (BACKGROUND_COLOR, NAME => LEVEL_WIN);
      CON_IO.SET_FOREGROUND (FOREGROUND_COLOR, NAME => EXIT_WIN);
      CON_IO.SET_BACKGROUND (BACKGROUND_COLOR, NAME => EXIT_WIN);
    else
      CON_IO.CLOSE (SECRET_WIN);
      CON_IO.CLOSE (PROPAL_WIN);
      CON_IO.CLOSE (TRY_WIN);
    end if;


    -- compute level dependant geometry
    CURRENT_LEVEL := LEVEL;
    PROPAL_FIRST_COL := PROPAL_LAST_COL
     - (CON_IO.COL_RANGE(CURRENT_LEVEL)-1) * (PROPAL_COL_WIDTH+1)
     - (PROPAL_COL_WIDTH-1);
    TRY_LAST_COL := TRY_FIRST_COL + (CON_IO.COL_RANGE(CURRENT_LEVEL)-1);

    CON_IO.OPEN (SECRET_WIN, (1, PROPAL_FIRST_COL), (1, PROPAL_LAST_COL) );
    CON_IO.OPEN (PROPAL_WIN, (PROPAL_FIRST_ROW, PROPAL_FIRST_COL),
                             (PROPAL_LAST_ROW,  PROPAL_LAST_COL) );
    CON_IO.OPEN (TRY_WIN,    (PROPAL_FIRST_ROW, TRY_FIRST_COL),
                             (PROPAL_LAST_ROW,  TRY_LAST_COL) );
    CON_IO.SET_FOREGROUND (FOREGROUND_COLOR, NAME => SECRET_WIN);
    CON_IO.SET_BACKGROUND (BACKGROUND_COLOR, NAME => SECRET_WIN);
    CON_IO.SET_FOREGROUND (FOREGROUND_COLOR, NAME => PROPAL_WIN);
    CON_IO.SET_BACKGROUND (BACKGROUND_COLOR, NAME => PROPAL_WIN);
    CON_IO.SET_FOREGROUND (FOREGROUND_COLOR, NAME => TRY_WIN);
    CON_IO.SET_BACKGROUND (BACKGROUND_COLOR, NAME => TRY_WIN);

    -- Redraw and frames
    CON_IO.CLEAR (GLOBAL_WIN);
    CON_IO.FRAME (NAME => GLOBAL_WIN);
    CON_IO.FRAME (NAME => SECRET_WIN);
    CON_IO.FRAME (NAME => PROPAL_WIN);
    CON_IO.FRAME (NAME => TRY_WIN);
    CON_IO.FRAME (NAME => COLOR_WIN);
    CON_IO.FRAME (NAME => HELP_WIN);
    CON_IO.FRAME (NAME => LEVEL_WIN);
    CON_IO.FRAME (NAME => MENU_WIN);
    CON_IO.FRAME (NAME => EXIT_WIN);

    -- Draw lines in propal and try frames
    for J in COMMON.LEVEL_RANGE'FIRST .. LEVEL loop
      for I in COMMON.PROPAL_RANGE loop
        SQUARE := PROPAL_SQUARE (I, J);
        if J /= CURRENT_LEVEL then
          -- Draw | of propal
          CON_IO.MOVE (SQUARE.ROW, SQUARE.COL + PROPAL_COL_WIDTH, PROPAL_WIN);
          CON_IO.PUT_INT (25, NAME => PROPAL_WIN);
        end if;
        if I /= COMMON.PROPAL_RANGE'LAST then
          -- Draw -- of propal
          for K in 1 .. PROPAL_COL_WIDTH loop
            CON_IO.MOVE (SQUARE.ROW-1, SQUARE.COL+K-1, PROPAL_WIN);
            CON_IO.PUT_INT (18, NAME => PROPAL_WIN);
          end loop;
          if J /= CURRENT_LEVEL then
            -- Draw + of propal
            CON_IO.MOVE (SQUARE.ROW-1, SQUARE.COL+PROPAL_COL_WIDTH, PROPAL_WIN);
            CON_IO.PUT_INT (15, NAME => PROPAL_WIN);
          end if;

          -- Draw - of try
          CON_IO.MOVE (SQUARE.ROW-1, CON_IO.COL_RANGE(J)-1, TRY_WIN);
          CON_IO.PUT_INT (18, NAME => TRY_WIN);
        end if;

      end loop;

    end loop;

    -- Adapt secret, propal and try frames
    CON_IO.MOVE (0, PROPAL_FIRST_COL-1);
    CON_IO.PUT_INT (24, FOREGROUND => FOREGROUND_COLOR,
                        BACKGROUND => BACKGROUND_COLOR);
    CON_IO.MOVE (0, PROPAL_LAST_COL+1);
    CON_IO.PUT_INT (24, FOREGROUND => FOREGROUND_COLOR,
                        BACKGROUND => BACKGROUND_COLOR);

    for I in COMMON.PROPAL_RANGE'FIRST ..
             COMMON.PROPAL_RANGE'PRED(COMMON.PROPAL_RANGE'LAST) loop
      -- |- in propal
      CON_IO.MOVE (PROPAL_LAST_ROW - 1 - (CON_IO.ROW_RANGE(I)-1) * 2,
                   PROPAL_FIRST_COL - 1);
      CON_IO.PUT_INT (21, FOREGROUND => FOREGROUND_COLOR,
                          BACKGROUND => BACKGROUND_COLOR);
      -- -| in propal
      CON_IO.MOVE (PROPAL_LAST_ROW - 1 - (CON_IO.ROW_RANGE(I)-1) * 2,
                   PROPAL_LAST_COL + 1);
      CON_IO.PUT_INT (22, FOREGROUND => FOREGROUND_COLOR,
                          BACKGROUND => BACKGROUND_COLOR);
      -- |- in try
      CON_IO.MOVE (PROPAL_LAST_ROW - 1 - (CON_IO.ROW_RANGE(I)-1) * 2,
                   TRY_FIRST_COL - 1);
      CON_IO.PUT_INT (21, FOREGROUND => FOREGROUND_COLOR,
                          BACKGROUND => BACKGROUND_COLOR);
      -- -| in propal
      CON_IO.MOVE (PROPAL_LAST_ROW - 1 - (CON_IO.ROW_RANGE(I)-1) * 2,
                   TRY_LAST_COL + 1);
      CON_IO.PUT_INT (22, FOREGROUND => FOREGROUND_COLOR,
                          BACKGROUND => BACKGROUND_COLOR);
    end loop;

   for J in COMMON.LEVEL_RANGE'FIRST .. CURRENT_LEVEL - 1 loop
      -- T in propal
      CON_IO.MOVE (PROPAL_FIRST_ROW - 1,
                   PROPAL_FIRST_COL + PROPAL_COL_WIDTH +
                   (CON_IO.COL_RANGE(J)- 1) * (PROPAL_COL_WIDTH+1) );
      CON_IO.PUT_INT (24, FOREGROUND => FOREGROUND_COLOR,
                          BACKGROUND => BACKGROUND_COLOR);
      -- L in propal
      CON_IO.MOVE (PROPAL_LAST_ROW + 1,
                   PROPAL_FIRST_COL + PROPAL_COL_WIDTH +
                   (CON_IO.COL_RANGE(J)- 1) * (PROPAL_COL_WIDTH+1) );
      CON_IO.PUT_INT (23, FOREGROUND => FOREGROUND_COLOR,
                          BACKGROUND => BACKGROUND_COLOR);
    end loop;


    -- Draw propal numbers
    for I in COMMON.PROPAL_RANGE loop
      CON_IO.MOVE (PROPAL_LAST_ROW - (CON_IO.ROW_RANGE(I)-1) * 2,
                   PROPAL_LAST_COL + 3);
      CON_IO.PUT (NORMAL (INTEGER(I), 2), FOREGROUND => FOREGROUND_COLOR,
                                          BACKGROUND => BACKGROUND_COLOR);
    end loop;

    -- Draw colors
    for I in COMMON.EFF_COLOR_RANGE loop
      CON_IO.MOVE ((CON_IO.ROW_RANGE(I)-1) * 2, 0, COLOR_WIN);
      CON_IO.PUT_INT (PIN_INT, FOREGROUND => COLOR_DEFINITION(I),
                           NAME => COLOR_WIN);
      CON_IO.MOVE ((CON_IO.ROW_RANGE(I)-1) * 2, 1, COLOR_WIN);
      CON_IO.PUT_INT (PIN_INT, FOREGROUND => COLOR_DEFINITION(I),
                           NAME => COLOR_WIN);
      if I /= COMMON.EFF_COLOR_RANGE'LAST then
        CON_IO.MOVE ((CON_IO.ROW_RANGE(I)-1) * 2 + 1, 0, COLOR_WIN);
        CON_IO.PUT_INT (18, NAME => COLOR_WIN);
        CON_IO.MOVE ((CON_IO.ROW_RANGE(I)-1) * 2 + 1, 1, COLOR_WIN);
        CON_IO.PUT_INT (18, NAME => COLOR_WIN);

        CON_IO.MOVE (COLOR_FIRST_ROW + (CON_IO.ROW_RANGE(I)-1) * 2 + 1,
                     COLOR_FIRST_COL - 1);
        CON_IO.PUT_INT (21, FOREGROUND => FOREGROUND_COLOR,
                            BACKGROUND => BACKGROUND_COLOR);
        CON_IO.MOVE (COLOR_FIRST_ROW + (CON_IO.ROW_RANGE(I)-1) * 2 + 1,
                     COLOR_LAST_COL + 1);
        CON_IO.PUT_INT (22, FOREGROUND => FOREGROUND_COLOR,
                            BACKGROUND => BACKGROUND_COLOR);
      end if;
    end loop;

    -- Draw title
    CON_IO.MOVE (1, TRY_LAST_COL + 2, GLOBAL_WIN);
    CON_IO.PUT ("M A S T E R   M I N D", GLOBAL_WIN);

    -- Draw level
    for I in COMMON.LAST_LEVEL_RANGE loop
      PUT_LEVEL (I, SELECTED => FALSE);
    end loop;

    -- Draw EXIT
    CON_IO.MOVE (NAME => EXIT_WIN);
    CON_IO.PUT  (" E X I T ", NAME => EXIT_WIN, MOVE => FALSE);

    -- no try
    for I in COMMON.PROPAL_RANGE loop
      PUT_TRY (I, CANNOT_TRY);
    end loop;

  end INIT;

  procedure CLEAR is
  begin
    CON_IO.RESET_TERM;
    CON_IO.MOVE;
  end CLEAR;


  ------------
  -- PROPAL --
  ------------
  procedure PUT_DEFAULT_POS (
   PROPAL : in COMMON.PROPAL_RANGE;
   LEVEL  : in COMMON.LEVEL_RANGE;
   SHOW   : in BOOLEAN) is

    COLOR : CON_IO.EFFECTIVE_COLORS;
    INTS  : ARRAY (1 .. 2) of CON_IO.INT_CHAR;
    SQUARE : CON_IO.SQUARE;
    use COMMON;
  begin
    if LEVEL > CURRENT_LEVEL then
      raise CONSTRAINT_ERROR;
    end if;

    -- Set color and square in global
    if SHOW then
      COLOR := CON_IO.WHITE;
      INTS (1) := 21;
      INTS (2) := 22;
    else
      COLOR := FOREGROUND_COLOR;
      INTS (1) := 25;
      INTS (2) := 25;
    end if;
    SQUARE := PROPAL_SQUARE (PROPAL, LEVEL);
    SQUARE := CON_IO.TO_ABSOLUTE (SQUARE, PROPAL_WIN);
    SQUARE := CON_IO.TO_RELATIVE (SQUARE, GLOBAL_WIN);

    -- Draw frame of square
    CON_IO.MOVE (SQUARE.ROW, SQUARE.COL-1 , NAME => GLOBAL_WIN);
    CON_IO.PUT_INT (INTS(1), FOREGROUND => COLOR, NAME => GLOBAL_WIN);
    CON_IO.MOVE (SQUARE.ROW, SQUARE.COL+PROPAL_COL_WIDTH, NAME => GLOBAL_WIN);
    CON_IO.PUT_INT (INTS(2), FOREGROUND => COLOR, NAME => GLOBAL_WIN);
    for K in 1 .. PROPAL_COL_WIDTH loop
      CON_IO.MOVE (SQUARE.ROW - 1, SQUARE.COL + K - 1, NAME => GLOBAL_WIN);
      CON_IO.PUT_INT (18, FOREGROUND => COLOR, NAME => GLOBAL_WIN);
      CON_IO.MOVE (SQUARE.ROW + 1, SQUARE.COL + K - 1, NAME => GLOBAL_WIN);
      CON_IO.PUT_INT (18, FOREGROUND => COLOR, NAME => GLOBAL_WIN);
    end loop;

  end PUT_DEFAULT_POS;

  procedure PUT_TRY (
   PROPAL   : in COMMON.PROPAL_RANGE;
   TRY_STATE : in PUT_TRY_LIST) is
    SQUARE : CON_IO.SQUARE;
  begin
    SQUARE.ROW := PROPAL_SQUARE (PROPAL, COMMON.LEVEL_RANGE'FIRST).ROW;
    SQUARE.COL := 0;
    for I in COMMON.LEVEL_RANGE'FIRST .. CURRENT_LEVEL loop
      CON_IO.MOVE (SQUARE.ROW, SQUARE.COL+CON_IO.COL_RANGE(I)-1, TRY_WIN);
      case TRY_STATE is
        when CANNOT_TRY =>
          CON_IO.PUT ('X', NAME => TRY_WIN, MOVE => FALSE);
        when CAN_TRY =>
          CON_IO.PUT ('?', FOREGROUND => TRY_COLOR, NAME => TRY_WIN,
           MOVE => FALSE);
        when SELECTED =>
          CON_IO.PUT ('?', FOREGROUND => BACKGROUND_COLOR,
                           BACKGROUND => BACKGROUND_SELECT, NAME => TRY_WIN,
                           MOVE => FALSE);
      end case;
    end loop;

  end PUT_TRY;

  procedure PUT_COLOR (
   PROPAL : in COMMON.PROPAL_RANGE;
   LEVEL  : in COMMON.LEVEL_RANGE;
   COLOR  : in COMMON.COLOR_RANGE) is
    SQUARE : CON_IO.SQUARE;
    use COMMON;
  begin
    if LEVEL > CURRENT_LEVEL then
      raise CONSTRAINT_ERROR;
    end if;
    SQUARE := PROPAL_SQUARE (PROPAL, LEVEL);
    for I in 1 .. PROPAL_COL_WIDTH loop
      CON_IO.MOVE (SQUARE.ROW, SQUARE.COL+I-1, PROPAL_WIN);
      if COLOR /= COMMON.COLOR_RANGE'FIRST then
        CON_IO.PUT_INT (PIN_INT, FOREGROUND => COLOR_DEFINITION(COLOR),
         NAME => PROPAL_WIN);
      else
        CON_IO.PUT (' ', FOREGROUND => FOREGROUND_COLOR, MOVE => FALSE,
         NAME => PROPAL_WIN);
      end if;
    end loop;

  end PUT_COLOR;

  procedure PUT_ANSWER (
   PROPAL : in COMMON.PROPAL_RANGE;
   PLACED_OK, COLORS_OK : in NATURAL) is
    SQUARE : CON_IO.SQUARE;
    use COMMON;
  begin
    if COLORS_OK + PLACED_OK > NATURAL(CURRENT_LEVEL) then
      raise CONSTRAINT_ERROR;
    end if;

    SQUARE.ROW := PROPAL_SQUARE (PROPAL, COMMON.LEVEL_RANGE'FIRST).ROW;
    SQUARE.COL := 0;
    -- Clear
    for I in COMMON.LEVEL_RANGE'FIRST .. CURRENT_LEVEL loop
      CON_IO.MOVE (SQUARE.ROW, SQUARE.COL+CON_IO.COL_RANGE(I)-1, TRY_WIN);
      CON_IO.PUT (' ', MOVE => FALSE, NAME => TRY_WIN);
    end loop;
    -- Put
    for I in 1 .. PLACED_OK loop
      CON_IO.MOVE (SQUARE, TRY_WIN);
      CON_IO.PUT_INT (1, FOREGROUND => OK_COLOR, NAME => TRY_WIN);
      SQUARE.COL := SQUARE.COL + 1;
    end loop;
    for I in 1 .. COLORS_OK loop
      CON_IO.MOVE (SQUARE, TRY_WIN);
      CON_IO.PUT_INT (1, FOREGROUND => NOK_COLOR, NAME => TRY_WIN);
      SQUARE.COL := SQUARE.COL + 1;
    end loop;

  end PUT_ANSWER;

  ------------
  -- SECRET --
  ------------
  procedure PUT_SECRET_COLOR (
   LEVEL  : in COMMON.LEVEL_RANGE;
   COLOR  : in COMMON.COLOR_RANGE) is
    SQUARE : CON_IO.SQUARE;
  begin
    SQUARE.ROW := 0;
    SQUARE.COL := PROPAL_SQUARE (1, LEVEL).COL;
    for I in 1 .. PROPAL_COL_WIDTH loop
      CON_IO.MOVE (SQUARE.ROW, SQUARE.COL+I-1, SECRET_WIN);
      CON_IO.PUT_INT (PIN_INT, FOREGROUND => COLOR_DEFINITION(COLOR),
       NAME => SECRET_WIN);
    end loop;

  end PUT_SECRET_COLOR;

  ----------
  -- MENU --
  ----------
  procedure PUT_START_GIVEUP (START : in BOOLEAN; SELECTED : in BOOLEAN) is
    FORE : CON_IO.EFFECTIVE_COLORS;
    BACK : CON_IO.EFFECTIVE_BASIC_COLORS;
  begin
    if SELECTED then
      FORE := BACKGROUND_COLOR;
      BACK := BACKGROUND_SELECT;
    else
      FORE := FOREGROUND_COLOR;
      BACK := BACKGROUND_COLOR;
    end if;
    CON_IO.MOVE (NAME => MENU_WIN);
    if START then
      CON_IO.PUT (" S T A R T ",
       FOREGROUND => FORE,
       BACKGROUND => BACK,
       NAME => MENU_WIN,
       MOVE => FALSE);
    else
      CON_IO.PUT ("  GIVE UP  ",
       FOREGROUND => FORE,
       BACKGROUND => BACK,
       NAME => MENU_WIN,
       MOVE => FALSE);
    end if;

  end PUT_START_GIVEUP;

  -----------
  -- LEVEL --
  -----------
  procedure PUT_LEVEL (LEVEL_NO : in COMMON.LAST_LEVEL_RANGE;
   SELECTED : in BOOLEAN) is
    COL : constant CON_IO.ROW_RANGE :=
     CON_IO.ROW_RANGE (LEVEL_NO) -
     CON_IO.ROW_RANGE (COMMON.LAST_LEVEL_RANGE'FIRST);
    FORE : CON_IO.EFFECTIVE_COLORS;
    BACK : CON_IO.EFFECTIVE_BASIC_COLORS;
  begin
    if SELECTED then
      FORE := BACKGROUND_COLOR;
      BACK := BACKGROUND_SELECT;
    else
      FORE := FOREGROUND_COLOR;
      BACK := BACKGROUND_COLOR;
    end if;
    CON_IO.MOVE (0, COL * 2 + 1, NAME => LEVEL_WIN);
    CON_IO.PUT (NORMAL (INTEGER(LEVEL_NO), 1),
     FOREGROUND => FORE,
     BACKGROUND => BACK,
     NAME => LEVEL_WIN,
     MOVE => FALSE);
  end PUT_LEVEL;

  procedure PUT_CURRENT_LEVEL (LEVEL_NO : in COMMON.LAST_LEVEL_RANGE) is
    COL : constant CON_IO.ROW_RANGE :=
     CON_IO.ROW_RANGE (LEVEL_NO) -
     CON_IO.ROW_RANGE (COMMON.LAST_LEVEL_RANGE'FIRST);
  begin
    CON_IO.FRAME (NAME => LEVEL_WIN);
    CON_IO.MOVE (MENU_ROW - 2, LEVEL_FIRST_COL + 2 * COL,
     NAME => GLOBAL_WIN);
    CON_IO.PUT_INT (24, NAME => GLOBAL_WIN);
    CON_IO.MOVE (MENU_ROW,     LEVEL_FIRST_COL + 2 * COL,
     NAME => GLOBAL_WIN);
    CON_IO.PUT_INT (23, NAME => GLOBAL_WIN);
  end PUT_CURRENT_LEVEL;


  procedure PUT_EXIT (SELECTED : in BOOLEAN) is
  begin
    CON_IO.MOVE (NAME => EXIT_WIN);
    if SELECTED then
      CON_IO.PUT  (" E X I T ",
       NAME => EXIT_WIN,
       FOREGROUND => BACKGROUND_COLOR,
       BACKGROUND => BACKGROUND_SELECT,
       MOVE => FALSE);
    else
      CON_IO.PUT  (" E X I T ", NAME => EXIT_WIN, MOVE => FALSE);
    end if;
  end PUT_EXIT;

  ----------
  -- HELP --
  ----------
  procedure PUT_HELP (HELP : HELP_STATE) is
  begin
    CON_IO.CLEAR (NAME => HELP_WIN);
    case HELP is
      when RELEASED =>
        CON_IO.MOVE (NAME => HELP_WIN);
        CON_IO.PUT_LINE ("Select :",                      NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE (" -> A color to set",            NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     it in a proposition.",     NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE (" -> A proposition to clear",    NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     or move it.",              NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE (" -> A try to get answer",       NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     to a try.",                NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE (" -> A menu option :",           NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     Give-up.",                 NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     Exit.",                    NAME => HELP_WIN);
      when CLICK_COLOR =>
        CON_IO.MOVE (NAME => HELP_WIN);
        CON_IO.PUT_LINE ("Release on :",                  NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE (" -> A position to affect it",   NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     at this position.",        NAME => HELP_WIN);
      when CLICK_PROPAL =>
        CON_IO.MOVE (NAME => HELP_WIN);
        CON_IO.PUT_LINE ("Release on :",                  NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE (" -> an empty square to move",   NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     this color.",              NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE (" -> outside propositions",      NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     to clear this color.",     NAME => HELP_WIN);
      when CLICK_OTHER =>
        CON_IO.MOVE (NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN, NUMBER => 2);
        CON_IO.PUT_LINE ("Release on the same item",      NAME => HELP_WIN);
        CON_IO.PUT_LINE (" to validate.",                 NAME => HELP_WIN);
      when START =>
        CON_IO.MOVE (NAME => HELP_WIN);
        CON_IO.PUT_LINE ("Select :",                      NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE (" -> A menu option :",           NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     Start.",                   NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     Level (3 4 5).",           NAME => HELP_WIN);
        CON_IO.PUT_LINE ("     Exit.",                    NAME => HELP_WIN);
      when DISCARDED =>
        CON_IO.MOVE (NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN, NUMBER => 2);
        CON_IO.PUT_LINE ("WRONG CLICK POSITION",          NAME => HELP_WIN);
        CON_IO.NEW_LINE (NAME => HELP_WIN);
        CON_IO.PUT_LINE ("Release anywhere",              NAME => HELP_WIN);
        CON_IO.PUT_LINE (" and click again.",             NAME => HELP_WIN);
    end case;

  end PUT_HELP;

  -----------
  -- COLOR --
  -----------
  procedure PUT_SELECTED_COLOR (
   COLOR : in COMMON.EFF_COLOR_RANGE;
   SELECTED : in BOOLEAN) is
    FORE : CON_IO.EFFECTIVE_COLORS;
    INTS  : ARRAY (1 .. 2) of CON_IO.INT_CHAR;
    SQUARE : CON_IO.SQUARE;
    use COMMON;
  begin
    -- Set color and square in global
    if SELECTED then
      FORE := CON_IO.WHITE;
      INTS (1) := 21;
      INTS (2) := 22;
    else
      FORE := FOREGROUND_COLOR;
      INTS (1) := 25;
      INTS (2) := 25;
    end if;

    CON_IO.MOVE ((CON_IO.ROW_RANGE(COLOR)-1) * 2, 0, NAME => COLOR_WIN);
    SQUARE := CON_IO.POSITION (COLOR_WIN);
    SQUARE := CON_IO.TO_ABSOLUTE (SQUARE, COLOR_WIN);
    SQUARE := CON_IO.TO_RELATIVE (SQUARE, GLOBAL_WIN);

    -- Draw frame of square
    CON_IO.MOVE (SQUARE.ROW, SQUARE.COL-1, NAME => GLOBAL_WIN);
    CON_IO.PUT_INT (INTS(1), FOREGROUND => FORE, NAME => GLOBAL_WIN);
    CON_IO.MOVE (SQUARE.ROW, SQUARE.COL+2, NAME => GLOBAL_WIN);
    CON_IO.PUT_INT (INTS(2), FOREGROUND => FORE, NAME => GLOBAL_WIN);

    CON_IO.MOVE (SQUARE.ROW+1, SQUARE.COL, NAME => GLOBAL_WIN);
    CON_IO.PUT_INT (18, FOREGROUND => FORE, NAME => GLOBAL_WIN);
    CON_IO.MOVE (SQUARE.ROW+1, SQUARE.COL+1, NAME => GLOBAL_WIN);
    CON_IO.PUT_INT (18, FOREGROUND => FORE, NAME => GLOBAL_WIN);
    CON_IO.MOVE (SQUARE.ROW-1, SQUARE.COL, NAME => GLOBAL_WIN);
    CON_IO.PUT_INT (18, FOREGROUND => FORE, NAME => GLOBAL_WIN);
    CON_IO.MOVE (SQUARE.ROW-1, SQUARE.COL+1, NAME => GLOBAL_WIN);
    CON_IO.PUT_INT (18, FOREGROUND => FORE, NAME => GLOBAL_WIN);

  end PUT_SELECTED_COLOR;

  -----------
  -- MOUSE --
  -----------
  -- call before mouse selection
  procedure SHOW_MOUSE is
  begin
    null;
  end SHOW_MOUSE;

  -- call before any put
  procedure HIDE_MOUSE is
  begin
    null;
  end HIDE_MOUSE;


  ---------------
  -- SELECTION --
  ---------------
  function TO_PROPAL (ROW : CON_IO.ROW_RANGE) return COMMON.PROPAL_RANGE is
  begin
    return
     COMMON.PROPAL_RANGE (
      CON_IO.ROW_RANGE (COMMON.MAX_NUMBER_PROPAL) - (ROW / 2) );
  end TO_PROPAL;

  procedure GET_SELECTED (
   WHERE : in CON_IO.SQUARE;
   WHAT  : out SELECTION_REC) is
    SQUARE : CON_IO.SQUARE;
    use COMMON;
  begin
    WHAT := (SELECTION_KIND => NOTHING, SELECTION => NOTHING);

    begin
      SQUARE := CON_IO.TO_RELATIVE (WHERE, PROPAL_WIN);
      if SQUARE.ROW mod 2 /= 0 then
        WHAT.SELECTION := PROPAL;
        return;
      end if;
      if SQUARE.COL mod (PROPAL_COL_WIDTH+1) = PROPAL_COL_WIDTH then
        WHAT.SELECTION := PROPAL;
        return;
      end if;

      WHAT := (SELECTION_KIND => PROPAL,
               PROPAL_NO => TO_PROPAL (SQUARE.ROW),
               COLUMN_NO => COMMON.LEVEL_RANGE(
                             (SQUARE.COL / (PROPAL_COL_WIDTH+1))+1) );
    exception
      when CON_IO.INVALID_SQUARE => null;
    end;

    begin
      SQUARE := CON_IO.TO_RELATIVE (WHERE, TRY_WIN);
      if SQUARE.ROW mod 2 /= 0 then
        WHAT.SELECTION := TRY;
        return;
      end if;
      WHAT := (SELECTION_KIND => TRY,
               TRY_NO => TO_PROPAL (SQUARE.ROW) );
    exception
      when CON_IO.INVALID_SQUARE => null;
    end;

    begin
      SQUARE := CON_IO.TO_RELATIVE (WHERE, COLOR_WIN);
      if SQUARE.ROW mod 2 /= 0 then
        WHAT.SELECTION := COLOR;
        return;
      end if;
      WHAT := (
       SELECTION_KIND => COLOR,
       COLOR_NO => COMMON.EFF_COLOR_RANGE (1 + (SQUARE.ROW / 2)) );
    exception
      when CON_IO.INVALID_SQUARE => null;
    end;

    begin
      SQUARE := CON_IO.TO_RELATIVE (WHERE, MENU_WIN);
      WHAT := (SELECTION_KIND => MENU);
    exception
      when CON_IO.INVALID_SQUARE => null;
    end;

    begin
      SQUARE := CON_IO.TO_RELATIVE (WHERE, LEVEL_WIN);
      if (SQUARE.COL + 1) mod 2 /= 0 then
        WHAT.SELECTION := LEVEL;
        return;
      end if;
      WHAT := (
       SELECTION_KIND => LEVEL,
       LEVEL_NO => COMMON.LAST_LEVEL_RANGE (
        (SQUARE.COL - 1) / 2 + INTEGER (COMMON.LAST_LEVEL_RANGE'FIRST)) );
    exception
      when CON_IO.INVALID_SQUARE => null;
    end;

    begin
      SQUARE := CON_IO.TO_RELATIVE (WHERE, EXIT_WIN);
      WHAT := (SELECTION_KIND => EXIT_GAME);
    exception
      when CON_IO.INVALID_SQUARE => null;
    end;

  end GET_SELECTED;

end SCREEN;
