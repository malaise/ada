-- all the primitives to access the screen
with SYSTEM;
with TASK_MNG;
package body NAV_SCREEN is
  use CON_IO;

  -- the 8 needed windows
  W_TITLE, W_MASK, W_GET, W_RES, W_ACT, W_ERR, W_HELP, W_TIME : WINDOW;

  -- the left column of get ang result areas
  COL_GET : constant COL_RANGE := 29;
  COL_RES : constant COL_RANGE := 70;
  -- start column in mask and width of dots and arrows
  COL_LINE : constant COL_RANGE := 37;
  WIDTH_LINE : constant POSITIVE := 32;
  -- columns of actions
  COMP_WID : constant COL_RANGE :=  7;
  QUIT_WID : constant COL_RANGE :=  4;
  HELP_WID : constant COL_RANGE :=  9;
  CLEA_WID : constant COL_RANGE :=  5;
  ACT_OFF : constant := 5;

  -- background color of all get fields
  GET_BACK : constant EFFECTIVE_BASIC_COLORS := BLUE;
  -- foreground color of results
  RES_FORE : constant EFFECTIVE_COLORS := LIGHT_GREEN;

  -- delay max of a get (data or action) in seconds.
  DELTA_GET : CONSTANT duration := 0.5;
  -- number of deltas before clearing err messages
  TIME_OUT_GET : constant := 6;

  -- time displaying
  procedure SHOW_TIME is separate;

  package TIME_TASK_MNG is new TASK_MNG (CALL_BACK => SHOW_TIME);

  -- Clear all the screen
  procedure RESET is
  begin
    RESET_TERM;
  end RESET;

  -- To write the title
  procedure TITLE is
  begin
    TIME_TASK_MNG.START;
    MOVE ((0, 30), W_TITLE);
    PUT ("AERONAUTICAL NAVIGATION", W_TITLE, LIGHT_BLUE);

    MOVE ((2, 0), W_TITLE);
    PUT ("Keys: Enter, arrows, Ins, Del, Backspace, Home, End, Page Up,"
     & " Page Down", W_TITLE);
    MOVE ((3, 9), W_TITLE);
    PUT ("digits, '.', '?', '+', '-'", W_TITLE);

    MOVE ((4, 0), W_TITLE);
    PUT ("Format: speeds positives in knots or km/h (0.0 .. 999.9)",
     W_TITLE);
    MOVE ((5, 8), W_TITLE);
    PUT ("angles positives in degrees and minutes (0.00 .. 359.59)",
     W_TITLE);
    MOVE ((6, 8), W_TITLE);
    PUT ("drift in degrees and minutes (-90.00 .. +90.00)",
     W_TITLE);
    MOVE ((7, 8), W_TITLE);
    PUT ("? in a field to clear it", W_TITLE);

    TIME_TASK_MNG.SCHEDULE;

  end TITLE;

  -- To put the mask (all fixed text around) for the get the problem and
  --  for the put of the result
  procedure PUT_MASK is
    COL : COL_RANGE;
  begin
    MOVE ( (1, COL_GET), W_MASK); PUT ("Data", W_MASK);
    MOVE ( (1, COL_RES), W_MASK); PUT ("Results", W_MASK);
    MOVE ( ( 3,  0), W_MASK); PUT ("Wind : ", W_MASK);
    MOVE ( ( 6,  0), W_MASK); PUT ("Plane : ", W_MASK);
    MOVE ( ( 9,  0), W_MASK); PUT ("Trajectory : ", W_MASK);
    MOVE ( (12,  0), W_MASK); PUT ("Drift : ", W_MASK);
    MOVE ( ( 3, 14), W_MASK); PUT ("speed", W_MASK);
    MOVE ( ( 4, 14), W_MASK); PUT ("from", W_MASK);
    MOVE ( ( 6, 14), W_MASK); PUT ("air speed", W_MASK);
    MOVE ( ( 7, 14), W_MASK); PUT ("heading", W_MASK);
    MOVE ( ( 9, 14), W_MASK); PUT ("ground speed", W_MASK);
    MOVE ( (10, 14), W_MASK); PUT ("route", W_MASK);

    COL := 0;

    MOVE ( (0, COL), W_ACT);
    PUT ("Compute", W_ACT, BACKGROUND => DEFAULT_BACKGROUND);
    MOVE ( (0, COL + COMP_WID + 1), W_ACT); PUT (' ', W_ACT);
    COL := COL + COMP_WID + ACT_OFF;

    MOVE ( (0, COL), W_ACT);
    PUT ("Quit", W_ACT, BACKGROUND => DEFAULT_BACKGROUND);
    MOVE ( (0, COL + QUIT_WID + 1), W_ACT); PUT (' ', W_ACT);
    COL := COL + QUIT_WID + ACT_OFF;

    MOVE ( (0, COL), W_ACT);
    PUT ("Call help", W_ACT, BACKGROUND => DEFAULT_BACKGROUND);
    MOVE ( (0, COL + HELP_WID + 1), W_ACT); PUT (' ', W_ACT);
    COL := COL + HELP_WID + ACT_OFF;

    MOVE ( (0, COL), W_ACT);
    PUT ("Clear", W_ACT, BACKGROUND => DEFAULT_BACKGROUND);
    MOVE ( (0, COL + CLEA_WID + 1), W_ACT); PUT (' ', W_ACT);

  end PUT_MASK;

  -- row of a field in get and result areas
  function FLD_ROW (FIELD : NAV_DATA.T_LIST_DATA) return ROW_RANGE is
  begin
    case FIELD is
      when NAV_DATA.WIND_S => return 0;
      when NAV_DATA.WIND_A => return 1;
      when NAV_DATA.PLAN_S => return 3;
      when NAV_DATA.PLAN_A => return 4;
      when NAV_DATA.TRAJ_S => return 6;
      when NAV_DATA.TRAJ_A => return 7;
      when NAV_DATA.DRIFT  => return 9;
    end case;
  end FLD_ROW;

  -- COL of a field in get and result areas
  function FLD_COL (FIELD : NAV_DATA.T_LIST_DATA) return ROW_RANGE is
  begin
    case FIELD is
      when NAV_DATA.WIND_S | NAV_DATA.PLAN_S | NAV_DATA.TRAJ_S => return 1;
      when NAV_DATA.WIND_A | NAV_DATA.PLAN_A | NAV_DATA.TRAJ_A => return 1;
      when NAV_DATA.DRIFT  => return 0;
    end case;
  end FLD_COL;

  -- get a problem data field
  procedure GET (FIELD : in NAV_DATA.T_LIST_DATA; BLINK : in BOOLEAN := FALSE;
   STR : in out STRING; POS : in out POSITIVE; INSERT : in out BOOLEAN;
   NEXT : out MOVEMENT) is
    LAST : NATURAL;
    NXT : MOVEMENT;
  begin
    for I in 1 .. TIME_OUT_GET loop
      MOVE ( (FLD_ROW(FIELD), FLD_COL(FIELD)), W_GET);
      if BLINK then
        CON_IO.PUT_THEN_GET (STR, LAST, NXT, POS, INSERT, W_GET, RED,
         CON_IO.BLINK, TIME_OUT => DELTA_GET);
      else
        CON_IO.PUT_THEN_GET (STR, LAST, NXT, POS, INSERT, W_GET,
         TIME_OUT => DELTA_GET);
      end if;
      TIME_TASK_MNG.SCHEDULE;
      exit when NXT /= TIMEOUT;
    end loop;
    NEXT := NXT;
  end GET;

  -- put the formated field when successfully got
  procedure PUT (FIELD : in NAV_DATA.T_LIST_DATA; STR : in STRING;
   BLINK : in BOOLEAN := FALSE) is
  begin
    MOVE ( (FLD_ROW(FIELD), FLD_COL(FIELD)), W_GET);
    if BLINK then
      PUT (STR, W_GET, RED, CON_IO.BLINK);
    else
      PUT (STR, W_GET);
    end if;
  end PUT;

  -- put a field of the result
  procedure PUT_RESULT (FIELD : in NAV_DATA.T_LIST_DATA; STR : in STRING) is
  begin
    MOVE ( (FLD_ROW(FIELD), FLD_COL(FIELD)), W_RES);
    PUT (STR, W_RES);
  end PUT_RESULT;


  -- draw a line of dots between field in got area and it in result area
  procedure DOT (FIELD : in NAV_DATA.T_LIST_DATA) is
    DOTS : constant STRING (1 .. WIDTH_LINE) := (others => '.');
  begin
    -- move (+3 cause in w_mask and not in w_get nor w_put)
    MOVE ( (FLD_ROW(FIELD) + 3, COL_LINE), W_MASK);
    PUT (DOTS, W_MASK, RES_FORE);
  end DOT;

  -- draw an arrow between a clear field in got area and the result
  procedure ARROW (FIELD : in NAV_DATA.T_LIST_DATA) is
    MINUS : constant STRING (1 .. WIDTH_LINE-1) := (others => '-');
  begin
    MOVE ( (FLD_ROW(FIELD) + 3, COL_LINE), W_MASK);
    PUT (MINUS & '>', W_MASK, RES_FORE);
  end ARROW;

  -- clears a line of dots or an arrow
  procedure CLEAR_LINE (FIELD : in NAV_DATA.T_LIST_DATA) is
    SPACES : constant STRING (1 .. WIDTH_LINE) := (others => ' ');
  begin
    MOVE ( (FLD_ROW(FIELD) + 3, COL_LINE), W_MASK);
    PUT (SPACES, W_MASK);
  end CLEAR_LINE;

  function GET_ACTION return ACTION is
    STR : STRING (1 .. 0);
    LAST : NATURAL;
    STAT : CURS_MVT;
    POS : POSITIVE;
    INS : BOOLEAN;
    CUR_ACTION : ACTION;
    subtype OPERATION is ACTION range COMPUTE .. CLEAR;

    function ACT_COL (OPER : OPERATION) return COL_RANGE is
    begin
      case OPER is
        when COMPUTE =>
         return COMP_WID + 1;
        when QUIT    =>
         return COMP_WID + ACT_OFF + QUIT_WID + 1;
        when HELP    =>
         return COMP_WID + ACT_OFF + QUIT_WID + ACT_OFF + HELP_WID + 1;
        when CLEAR   =>
         return COMP_WID + ACT_OFF + QUIT_WID + ACT_OFF + HELP_WID
          + ACT_OFF + CLEA_WID + 1;
      end case;
    end ACT_COL;
  begin
    CUR_ACTION := COMPUTE;
    STAT := RIGHT;
    loop
      -- infinite get with GET_BACK on GET_BACK
      if STAT /= TIMEOUT then
        MOVE (0, ACT_COL(CUR_ACTION), W_ACT);
        PUT ('X', W_ACT, BACKGROUND => GET_BACK);
      end if;
      MOVE (0, ACT_COL(CUR_ACTION), W_ACT);
      GET (STR, LAST, STAT, POS, INS, W_ACT, TIME_OUT => DELTA_GET);
      TIME_TASK_MNG.SCHEDULE;
      if STAT /= TIMEOUT then
        PUT (' ', W_ACT, BACKGROUND => GET_BACK);
      end if;
      case STAT is
        when UP => return PREV;
        when DOWN | PGDOWN | PGUP => return NEXT;
        when RET => return CUR_ACTION;
        when ESC | TIMEOUT | FULL | MOUSE_BUTTON | BREAK => null;
        when LEFT | STAB =>
          if CUR_ACTION /= OPERATION'FIRST then
            CUR_ACTION := OPERATION'PRED (CUR_ACTION);
          else
            CUR_ACTION := OPERATION'LAST;
          end if;
        when RIGHT | TAB =>
          if CUR_ACTION /= OPERATION'LAST then
            CUR_ACTION := OPERATION'SUCC (CUR_ACTION);
          else
            CUR_ACTION := OPERATION'FIRST;
          end if;
        when REFRESH => 
          return REFRESH;
      end case;
    end loop;
  end GET_ACTION;

  -- displays the "wrong format" error message
  procedure ERR_FORMAT is
  begin
    MOVE ( (0, 0), W_ERR);
    PUT ("ERROR : Wrong input format or bad value.", W_ERR);
  end ERR_FORMAT;

  -- display an error adapted to the detected inconsistency of data
  --  (result of check)
  procedure ERR_CHECK (ERROR : in NAV_DATA.T_CONSISTENCY) is
  begin
    move ( (0, 0), W_ERR);
    PUT ("ERROR : ", W_ERR);
    case ERROR is
      when NAV_DATA.KNOWN_ERR =>
        PUT ("Only 3 fields must be unknown.", W_ERR);
      when NAV_DATA.ANGLE_ERR =>
        PUT ("The 3 known angles are not consistent.", W_ERR);
      when NAV_DATA.WIND_ERR =>
        PUT ("The wind must be fully known or fully unknown.", W_ERR);
      when NAV_DATA.TRAJ_ERR =>
        PUT("If the route is unknown, the ground speed must be unknown as well.",
         W_ERR);
      when NAV_DATA.DRIFT_ERR =>
        PUT ( "If the drift is known, the heading or the route must be unknown.",
         W_ERR);
      when NAV_DATA.VAL_ERR =>
        PUT ("The values are not compatible.", W_ERR);
      when NAV_DATA.OK => null;
    end case;
  end ERR_CHECK;

  procedure CLEAR_ERR is
  begin
    CON_IO.CLEAR (W_ERR);
  end CLEAR_ERR;


  -- Ask the operator wether he realy wants to quit
  function CONFIRM_QUIT return BOOLEAN is
    STR : STRING (1 .. 0);
    LAST : NATURAL;
    STAT : CURS_MVT;
    POS : POSITIVE;
    INS : BOOLEAN;
  begin
    PUT ("Confirm you want to quit by entering 'Return' : ", W_ERR);
    loop
      MOVE ( (0, 49), W_ERR);
      GET (STR, LAST, STAT, POS, INS, W_ERR, TIME_OUT => DELTA_GET);
      TIME_TASK_MNG.SCHEDULE;
      exit when STAT /= CON_IO.TIMEOUT;
    end loop;
    CLEAR (W_ERR);
    if STAT = RET then
      ABORT_CLOCK;
    end if;
    return STAT = RET;
  end CONFIRM_QUIT;

  -- displays the help screen
  procedure PUT_HELP is
    STR : STRING (1..0);
    LST : NATURAL;
    STAT : CON_IO.CURS_MVT;
    POS : POSITIVE;
    INS : BOOLEAN;
  begin
    CLEAR (W_HELP);
    MOVE ((0, 8), W_HELP);
    PUT ("heading and trailing spaces are ignored", W_HELP);
    MOVE ((1, 8), W_HELP);
    PUT ("decimal are optional for speeds and angles", W_HELP);
    MOVE ((2, 8), W_HELP);
    PUT ("+ is optional for the drift", W_HELP);
    MOVE ((4, 0), W_HELP);
    PUT ("Constraints: ", W_HELP);
    MOVE ((4, 13), W_HELP);
    PUT ("There must be 3 unknown fields", W_HELP);
    MOVE ((5, 13), W_HELP);
    PUT ("The 3 angles (wind, heading, route) must be different, and", W_HELP);
    MOVE ((6, 14), W_HELP);
    PUT ("heading and wind must be on different sides of the route", W_HELP);
    MOVE ((7, 13), W_HELP);
    PUT ("The wind must be fully known or fully unknown", W_HELP);
    MOVE ((8, 13), W_HELP);
    PUT ("If the route is unknown, the ground speed must be unknown as well", W_HELP);
    MOVE ((9, 13), W_HELP);
    PUT ("If the drift is known, the heading or the route must be unknown",
     W_HELP);
    MOVE ((10, 13), W_HELP);
    PUT ("The wind and the air speed must allow to follow the route",
     W_HELP);

    MOVE ((13, 0), W_HELP);
    PUT ("Enter Return to go back to the data ", W_HELP);
    loop
      MOVE ((13, 45), W_HELP);
      GET (STR, LST, STAT, POS, INS, W_HELP,
       DEFAULT_BACKGROUND, DEFAULT_BLINK_STAT, DEFAULT_BACKGROUND,
       DELTA_GET);
      TIME_TASK_MNG.SCHEDULE;
      if STAT = CON_IO.REFRESH then
        TITLE;
      end if;
      exit when STAT = CON_IO.RET or STAT = CON_IO.REFRESH;
    end loop;
    CLEAR (W_HELP);
  end PUT_HELP;

  procedure ABORT_CLOCK is
  begin
    MOVE ( (0, 0), W_ACT);
    TIME_TASK_MNG.ABORT_TASK;
  exception
    when TIME_TASK_MNG.TASK_ABORTED =>
      null;
  end ABORT_CLOCK;


begin -- NAV_SCREEN
  CON_IO.OPEN (W_TITLE, ( 0,  0), ( 7, 79));
  CON_IO.OPEN (W_MASK,  ( 8,  0), (22, 79));
  CON_IO.OPEN (W_HELP,  ( 8,  0), (22, 79));
  CON_IO.OPEN (W_GET,   (11, COL_GET), (20, COL_GET + 7));
  CON_IO.OPEN (W_RES,   (11, COL_RES), (20, COL_RES + 7));
  CON_IO.OPEN (W_ACT,   (22,  0), (22, 79));
  CON_IO.OPEN (W_ERR,   (24,  2), (24, 79));
  CON_IO.OPEN (W_TIME,  ( 0, 60), ( 0, 79));
  SET_BACKGROUND (GET_BACK, W_GET);
  SET_BACKGROUND (GET_BACK, W_ACT);
  SET_FOREGROUND (RES_FORE, NOT_BLINK, W_RES);
  SET_FOREGROUND (RED, NOT_BLINK, W_ERR);
  SET_FOREGROUND (LIGHT_BLUE, NAME => W_TIME);
end NAV_SCREEN;

