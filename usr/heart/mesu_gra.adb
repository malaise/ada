with AFPX, CON_IO, NORMAL, MATH, TEXT_HANDLER, DOS;
with MESU_DEF, STR_MNG, MESU_NAM, PERS_MNG, PERS_DEF, MESU_FIL;
use PERS_DEF;
package body MESU_GRA is

  -- X and Y first and last, in screen and reality
  X_FIRST : constant NATURAL := 0;
  -- To be computed: 4 * font_width
  XS_FIRST : CON_IO.GRAPHICS.X_RANGE := 43;
  -- To be computed: X_MAX
  XS_LAST  : CON_IO.GRAPHICS.X_RANGE;
  -- To be computed: last sample in time
  X_LAST  : NATURAL;

  Y_FIRST  : constant PERS_DEF.BPM_RANGE := 100;
  Y_LAST   : constant PERS_DEF.BPM_RANGE := PERS_DEF.BPM_RANGE'LAST;
  Y_STEP   : constant PERS_DEF.BPM_RANGE := 25;
  -- To be computed: 2 * font height
  YS_FIRST : CON_IO.GRAPHICS.Y_RANGE;
  -- To be computed: Y_MAX - MAX_NB_MESURE * font height
  YS_LAST  : CON_IO.GRAPHICS.Y_RANGE;

  -- Scale factors from reality to screen
  -- To be computed
  X_FACTOR : FLOAT;
  Y_FACTOR : FLOAT;

  -- Font offset: how much to lower Y to get char "centered" to Y axis 
  FONT_OFFSET_HEIGHT : CON_IO.GRAPHICS.Y_RANGE;

  -- What to know about a record
  type MESURE_CELL is record
    PERSON : PERS_DEF.PERSON_REC;
    MESURE : MESU_DEF.MESURE_REC;
    DROWN  : BOOLEAN;
  end record;

  -- The info stored about records
  subtype MESURE_RANGE is NATURAL range 0 .. MAX_NB_MESURE;
  MESURE_ARRAY : array (1 .. MAX_NB_MESURE) of MESURE_CELL;
  NB_MESURE : MESURE_RANGE;
  -- No of mesure of last TZ drawn
  PREV_TZ : MESURE_RANGE;


  -- From reality to screen
  function X_TO_SCREEN (X : in NATURAL) return CON_IO.GRAPHICS.X_RANGE is
  begin
    return CON_IO.GRAPHICS.X_RANGE(FLOAT(X - X_FIRST) * X_FACTOR) + XS_FIRST;
  end X_TO_SCREEN;

  function Y_TO_SCREEN (BPM : in PERS_DEF.BPM_RANGE) return CON_IO.GRAPHICS.Y_RANGE is
    use PERS_DEF;
  begin
    return CON_IO.GRAPHICS.Y_RANGE(FLOAT(BPM - Y_FIRST) * Y_FACTOR) + YS_FIRST;
  end Y_TO_SCREEN;

  -- From reality to screen
  function X_TO_SCREEN_SECURE (X : in NATURAL) return INTEGER is
  begin
    return INTEGER(FLOAT(X - X_FIRST) * X_FACTOR) + XS_FIRST;
  end X_TO_SCREEN_SECURE;

  function Y_TO_SCREEN_SECURE (BPM : in PERS_DEF.BPM_RANGE) return INTEGER is
    use PERS_DEF;
  begin
    return INTEGER(FLOAT(BPM - Y_FIRST) * Y_FACTOR) + YS_FIRST;
  end Y_TO_SCREEN_SECURE;

  function IN_SCREEN (X : INTEGER; Y : INTEGER)
  return BOOLEAN is
    use CON_IO.GRAPHICS;
  begin
    return     Y >= YS_FIRST
      and then Y <= YS_LAST
      and then X >= XS_FIRST
      and then X <= XS_LAST;
  end IN_SCREEN;

  procedure PIXEL (X : in INTEGER; Y : in INTEGER; IN_GRAPHIC : in BOOLEAN) is
    use CON_IO;
  begin
    if IN_GRAPHIC and then
    (       X < XS_FIRST or else X > XS_LAST
    or else Y < YS_FIRST or else Y > YS_LAST) then
      return;
    end if;
    CON_IO.GRAPHICS.DRAW_POINT (X, Y);
  exception
    when others =>
      null;
  end PIXEL;


  procedure DRAW_LINE (XA : in INTEGER; YA : in INTEGER;
                       XB : in INTEGER; YB : in INTEGER;
                       IN_GRAPHIC       : in BOOLEAN := FALSE;
                       DRAW_FIRST_POINT : in BOOLEAN := TRUE) is
    -- Y := A * X + B;
    A : MATH.REAL;
    B : MATH.REAL;
    X, X1, X2 : INTEGER;
    Y, Y1, Y2 : INTEGER;
    use MATH;
  begin
    if XA = XB then
      -- Vertical line. Must have YA <= YB
      if YA <= YB then
        for Y in YA .. YB loop
          PIXEL (XA, Y, IN_GRAPHIC);
        end loop;
      else
        for Y in reverse YA .. YB loop
          PIXEL (XA, Y, IN_GRAPHIC);
        end loop;
      end if;
    elsif YA = YB then
      -- Horizontal line
      if XA <= XB then
        for X in XA .. XB loop
          PIXEL (X, YA, IN_GRAPHIC);
        end loop;
      else
        for X in reverse XA .. XB loop
          PIXEL (X, YA, IN_GRAPHIC);
        end loop;
      end if;
    else
      -- Other lines
      if abs (FLOAT(XB - XA) / FLOAT (CON_IO.GRAPHICS.X_MAX -
                                      CON_IO.GRAPHICS.X_RANGE'FIRST))
      >  abs (FLOAT(YB - YA) / FLOAT (CON_IO.GRAPHICS.Y_MAX -
                                      CON_IO.GRAPHICS.Y_RANGE'FIRST) ) then
        if XA < XB then
          X1 := XA;
          Y1 := YA;
          X2 := XB;
          Y2 := YB;
        else
          X1 := XB;
          Y1 := YB;
          X2 := XA;
          Y2 := YA;
        end if;
        -- Y := A * X + B
        A := MATH.REAL (Y2 - Y1) / MATH.REAL (X2 - X1);
        B := MATH.REAL(Y1) - (A * MATH.REAL(X1));
        for X in X1 .. X2 loop
          Y := CON_IO.GRAPHICS.Y_RANGE(MATH.ROUND (A * MATH.REAL(X) + B));
          PIXEL (X, Y, IN_GRAPHIC);
        end loop;
      else
        if YA < YB then
          X1 := XA;
          Y1 := YA;
          X2 := XB;
          Y2 := YB;
        else
          X1 := XB;
          Y1 := YB;
          X2 := XA;
          Y2 := YA;
        end if;
        -- X := A * Y + B
        A := MATH.REAL (X2 - X1) / MATH.REAL (Y2 - Y1);
        B := MATH.REAL(X1) - (A * MATH.REAL(Y1));
        for Y in Y1 .. Y2 loop
          X := CON_IO.GRAPHICS.X_RANGE(MATH.ROUND (A * MATH.REAL(Y) + B));
          PIXEL (X, Y, IN_GRAPHIC);
        end loop;
      end if;
    end if;

    if not DRAW_FIRST_POINT then
      PIXEL (XA, YA, IN_GRAPHIC);
    end if;
  end DRAW_LINE;

  -- Graphic layout (help, scales, TZ)
  procedure DRAW_LAYOUT is
    HELP_COLOR  : constant CON_IO.EFFECTIVE_COLORS := CON_IO.BROWN;
    SCALE_COLOR : constant CON_IO.EFFECTIVE_COLORS := CON_IO.BLUE;
    -- Scale step on X in seconds
    SECS_SCALE_STEP : constant := 600;
    SECS : NATURAL;
    -- Scale step on Y in BPM
    BPM : PERS_DEF.BPM_RANGE;
    X : CON_IO.GRAPHICS.X_RANGE;
    Y : CON_IO.GRAPHICS.Y_RANGE;

    use CON_IO;

  begin
    -- Help
    MOVE (ROW_RANGE_LAST, 5);
    SET_FOREGROUND (HELP_COLOR);
    PUT ("Escape to quit, '1' to '" & NORMAL(NB_MESURE,1)
       & "' to draw/hide a record, T for Training Zones.");
    -- Axes of scale
    SET_FOREGROUND (SCALE_COLOR);
    GRAPHICS.DRAW_LINE (XS_FIRST, YS_FIRST, XS_LAST, YS_FIRST);
    GRAPHICS.DRAW_LINE (XS_FIRST, YS_FIRST, XS_FIRST, YS_LAST);
    -- Horizontal scale : one + each 10 mn (600 seconds)
    --                    Time in mn each 3 +
    for I in 0 .. X_LAST / SECS_SCALE_STEP loop
      SECS := I * SECS_SCALE_STEP;
      X := X_TO_SCREEN (SECS);
      GRAPHICS.DRAW_LINE (X, YS_FIRST - 2, X, YS_FIRST + 2);
      if I rem 3 = 0 or else I = X_LAST / SECS_SCALE_STEP then
        if X / GRAPHICS.FONT_WIDTH - 1 <= COL_RANGE'LAST - 3 then
          MOVE (ROW_RANGE_LAST - 1, X / GRAPHICS.FONT_WIDTH - 1);
        else
          MOVE (ROW_RANGE_LAST - 1, COL_RANGE'LAST - 3);
        end if;
        PUT (NORMAL (SECS / 60, 3));
      end if;
    end loop;
    -- Vertical scale : one + each 25 BPM
    --                  BPM for each +
    for I in Y_FIRST / Y_STEP .. Y_LAST / Y_STEP loop
      BPM := PERS_DEF.BPM_RANGE(I) * Y_STEP;
      Y := Y_TO_SCREEN (BPM);
      GRAPHICS.DRAW_LINE (XS_FIRST - 2, Y, XS_FIRST + 2, Y);
      GRAPHICS.PUT (NORMAL (INTEGER(BPM), 3),
                    1,
                    Y - FONT_OFFSET_HEIGHT);
    end loop;

  end DRAW_LAYOUT;

  procedure DRAW_TZ (SHOW : in BOOLEAN) is
    TZ_COLOR    : constant CON_IO.EFFECTIVE_COLORS := CON_IO.RED;
    BPM : PERS_DEF.BPM_RANGE;
    Y : CON_IO.GRAPHICS.Y_RANGE;
    MESURE_INDEX : MESURE_RANGE;
  begin
    CON_IO.SET_FOREGROUND (TZ_COLOR);
    if not SHOW then
      MESURE_INDEX := PREV_TZ;
    else
      -- First drawn mesure if any (else first mesure)
      MESURE_INDEX := 1;
      for MESU in 1 .. NB_MESURE loop
        if MESURE_ARRAY(MESU).DROWN then
          MESURE_INDEX := MESU;
          exit;
        end if;
      end loop;
      PREV_TZ := MESURE_INDEX;
    end if;

    for I in PERS_DEF.PERSON_TZ_ARRAY'RANGE loop
      BPM := MESURE_ARRAY(MESURE_INDEX).MESURE.TZ(I);
      if BPM >= Y_FIRST then
        Y := Y_TO_SCREEN(BPM);
        DRAW_LINE (XS_FIRST, Y, XS_LAST - 4 * CON_IO.GRAPHICS.FONT_WIDTH, Y);
        CON_IO.GRAPHICS.PUT (
                    NORMAL(INTEGER(BPM), 3),
                    CON_IO.GRAPHICS.X_MAX - (3 * CON_IO.GRAPHICS.FONT_WIDTH),
                    Y - FONT_OFFSET_HEIGHT);
      end if;
    end loop;
  end DRAW_TZ;

  -- Draw one record
  procedure DRAW_MESURE (NO : in MESURE_RANGE) is
    use CON_IO;
    COLORS : constant array (1 .. MAX_NB_MESURE) of CON_IO.EFFECTIVE_COLORS
           := (1 => CON_IO.LIGHT_GRAY,
               2 => CON_IO.CYAN,
               3 => CON_IO.LIGHT_BLUE,
               4 => CON_IO.LIGHT_GREEN,
               5 => CON_IO.ORANGE,
               6 => CON_IO.BLUE,
               7 => CON_IO.MAGENTA,
               8 => CON_IO.YELLOW,
               9 => CON_IO.WHITE);
    use PERS_DEF;
    SEC1, SEC2 : NATURAL;
    BPM1, BPM2 : PERS_DEF.BPM_RANGE;
    MESURE : MESU_DEF.MESURE_REC renames MESURE_ARRAY(NO).MESURE;
    TITLE_TXT : TEXT_HANDLER.TEXT (CON_IO.COL_RANGE'LAST);
  begin
    SET_FOREGROUND (COLORS(NO));
    -- Person and date
    MOVE (NO-1, 10);
    if MESURE.SAMPLES(1) = PERS_DEF.BPM_RANGE'FIRST or else
       MESURE.SAMPLES(2) = PERS_DEF.BPM_RANGE'FIRST then
      TEXT_HANDLER.SET (TITLE_TXT, "(*)");
    else
      TEXT_HANDLER.SET (TITLE_TXT, "   ");
    end if;
    TEXT_HANDLER.APPEND (TITLE_TXT,
           NORMAL(NO, 1) & ":"
         & MESURE_ARRAY(NO).PERSON.NAME & " "
         & MESURE_ARRAY(NO).PERSON.ACTIVITY & " "
         & STR_MNG.TO_PRINTED_STR(MESURE.DATE) & " "
         & MESURE.COMMENT);
    PUT (TEXT_HANDLER.VALUE (TITLE_TXT));

    if MESURE.SAMPLES(2) = PERS_DEF.BPM_RANGE'FIRST then
      return;
    end if;


    SEC1 := 0;
    BPM1 := MESURE.SAMPLES(1);

    PIXEL (X_TO_SCREEN_SECURE (SEC1), Y_TO_SCREEN_SECURE (BPM1), TRUE);
    for I in MESU_DEF.SAMPLE_NB_RANGE
    range 2 .. MESU_DEF.SAMPLE_NB_RANGE'LAST loop
      exit when MESURE.SAMPLES(I) = PERS_DEF.BPM_RANGE'FIRST;
      SEC2 := SEC1 + INTEGER(MESURE.SAMPLING_DELTA);
      BPM2 := MESURE.SAMPLES(I);
      -- Check in screen
      DRAW_LINE (X_TO_SCREEN_SECURE (SEC1), Y_TO_SCREEN_SECURE (BPM1),
                 X_TO_SCREEN_SECURE (SEC2), Y_TO_SCREEN_SECURE (BPM2),
                 IN_GRAPHIC => TRUE, DRAW_FIRST_POINT => FALSE);
      SEC1 := SEC2;
      BPM1 := BPM2;
    end loop;

  end DRAW_MESURE;

  -- The main
  procedure GRAPHIC (EXIT_PROGRAM : out BOOLEAN) is
    SAVED_POS : NATURAL;
    LINE      : AFPX.LINE_REC;
    FILE_NAME : MESU_NAM.FILE_NAME_STR;
    DATE_S    : MESU_NAM.FILE_DATE_STR;
    NO_S      : MESU_NAM.FILE_NO_STR;
    PID_S     : MESU_NAM.FILE_PID_STR;
    POS_PERS  : NATURAL;
    PERSON    : PERS_DEF.PERSON_REC;
    SAME_TZ   : BOOLEAN;
    TZ_DROWN  : BOOLEAN;
    KEY       : NATURAL;
    IS_CHAR   : BOOLEAN;
    CTRL      : BOOLEAN;
    SHIFT     : BOOLEAN;
    NO_MESURE : MESURE_RANGE;

    -- Check if same TZ
    procedure CHECK_SAME_TZ is
      FIRST_DROWN_MESURE : MESURE_RANGE := 0;
    begin
      SAME_TZ := TRUE;
      for MESU in 1 .. NB_MESURE loop
        if MESURE_ARRAY(MESU).DROWN then
          if FIRST_DROWN_MESURE = 0 then
            -- This one is the first drown mesure
            FIRST_DROWN_MESURE := MESU;
          else
            -- Compare this TZs to the ones of first drown mesure
            for I in PERS_DEF.PERSON_TZ_ARRAY'RANGE loop
              if MESURE_ARRAY(MESU).MESURE.TZ(I) /=
                 MESURE_ARRAY(FIRST_DROWN_MESURE).MESURE.TZ(I) then
                SAME_TZ := FALSE;
                return;
              end if;
            end loop;
          end if;
        end if;
      end loop;
    end CHECK_SAME_TZ;


    use AFPX.LINE_LIST_MNG;
    use PERS_DEF;
  begin
    -- Compute dynamic sizing
    XS_FIRST := 4 * CON_IO.GRAPHICS.FONT_WIDTH;
    XS_LAST  := CON_IO.GRAPHICS.X_MAX;
    YS_FIRST := CON_IO.GRAPHICS.FONT_HEIGHT * 2;
    YS_LAST := CON_IO.GRAPHICS.Y_MAX
             - MAX_NB_MESURE * CON_IO.GRAPHICS.FONT_HEIGHT;
              
    Y_FACTOR := FLOAT(YS_FIRST - YS_LAST) / FLOAT(Y_FIRST - Y_LAST);
    FONT_OFFSET_HEIGHT := CON_IO.GRAPHICS.FONT_HEIGHT / 3;

    -- List is not empty
    SAVED_POS := GET_POSITION (AFPX.LINE_LIST);

    NB_MESURE := 0;
    -- for each in list : store in array
    MOVE_TO (AFPX.LINE_LIST, NEXT, 0, FALSE);
    loop
      -- Get line, file_name, split
      READ (AFPX.LINE_LIST, LINE, CURRENT);
      STR_MNG.FORMAT_LIST_TO_MESURE (LINE, FILE_NAME);
      MESU_NAM.SPLIT_FILE_NAME (FILE_NAME, DATE_S, NO_S, PID_S);
      -- Get person
      PERS_MNG.SEARCH (PERS_DEF.THE_PERSONS, PERS_DEF.PID_RANGE'VALUE(PID_S),
                       POS_PERS);
      PERS_DEF.PERSON_LIST_MNG.READ (PERS_DEF.THE_PERSONS, PERSON,
                                     PERS_DEF.PERSON_LIST_MNG.CURRENT);
      -- Get mesure
      NB_MESURE := NB_MESURE + 1;
      MESURE_ARRAY(NB_MESURE).PERSON := PERSON;
      MESURE_ARRAY(NB_MESURE).MESURE := MESU_FIL.LOAD (FILE_NAME);
      MESURE_ARRAY(NB_MESURE).DROWN  := FALSE;

      -- Next line except if list empty or end of list
      exit when IS_EMPTY (AFPX.LINE_LIST) or else
                GET_POSITION (AFPX.LINE_LIST) = LIST_LENGTH (AFPX.LINE_LIST);

      MOVE_TO (AFPX.LINE_LIST);
    end loop;
    MOVE_TO (AFPX.LINE_LIST, NEXT, SAVED_POS - 1, FALSE);

    -- Compute last X
    X_LAST := 0;
    THE_RECORDS:
    for I in 1 .. NB_MESURE loop
      THIS_RECORD:
      for J in MESU_DEF.SAMPLE_NB_RANGE loop
        if MESURE_ARRAY(I).MESURE.SAMPLES(J) = PERS_DEF.BPM_RANGE'FIRST then
          -- No more sample for this record
          exit THIS_RECORD;
        elsif J * INTEGER(MESURE_ARRAY(I).MESURE.SAMPLING_DELTA) > X_LAST then
          -- Greatest X so far
          X_LAST := J * INTEGER(MESURE_ARRAY(I).MESURE.SAMPLING_DELTA);
        end if;
      end loop THIS_RECORD;
    end loop THE_RECORDS;

    -- Compute X Factor
    X_FACTOR := FLOAT(XS_FIRST - XS_LAST) / FLOAT(X_FIRST - X_LAST);

    -- Graphic mode
    CON_IO.RESET_TERM;
    CON_IO.SET_XOR_MODE(CON_IO.XOR_ON);

    DRAW_LAYOUT;
    TZ_DROWN := FALSE;

    MAIN_LOOP:
    loop
      -- GET key
      CON_IO.GET_KEY(KEY, IS_CHAR, CTRL, SHIFT);
      -- exit when Escape
      if not IS_CHAR and then KEY = 27 then
        EXIT_PROGRAM := FALSE;
        exit MAIN_LOOP;
      end if;
      if IS_CHAR and then (        KEY = CHARACTER'POS('T')
                           or else KEY = CHARACTER'POS('t')) then
        if TZ_DROWN then
          -- Hide TZs
          DRAW_TZ(FALSE);
          TZ_DROWN := FALSE;
        else
          CHECK_SAME_TZ;
          if SAME_TZ then
            -- Draw TZs
            DRAW_TZ(TRUE);
            TZ_DROWN := TRUE;
          end if;
        end if;
      end if;
      -- Draw if key in 1 .. 9 then
      if IS_CHAR and then KEY >= CHARACTER'POS('1')
                 and then KEY <= CHARACTER'POS('9') then
        NO_MESURE := KEY - CHARACTER'POS('1') + 1;
        if NO_MESURE <= NB_MESURE then
          if not MESURE_ARRAY(NO_MESURE).DROWN then
            MESURE_ARRAY(NO_MESURE).DROWN := TRUE;
            -- Drawing a new record : check if TZ to be hidden
            if TZ_DROWN then
              CHECK_SAME_TZ;
              if not SAME_TZ then
                -- This mesure has a TZ incompatible with the drown TZs
                -- Hide TZ
                DRAW_TZ(FALSE);
                TZ_DROWN := FALSE;
              end if;
            end if;
          else
            -- Hidding a record
            MESURE_ARRAY(NO_MESURE).DROWN := FALSE;
          end if;

          DRAW_MESURE (NO_MESURE);
        end if;
      end if;
    end loop MAIN_LOOP;

    -- Back to text mode
    CON_IO.RESET_TERM;
    CON_IO.SET_XOR_MODE(CON_IO.XOR_OFF);
  end GRAPHIC;

end MESU_GRA;
