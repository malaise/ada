with CALENDAR, TEXT_IO;
with BIG_CON_IO, NORMAL, X_MNG, UPPER_CHAR;
package body CURVE is
  use MY_MATH;

  package P_IO is new TEXT_IO.FLOAT_IO (T_COORDINATE);

  -- Find lowest and greatest X of points
  procedure X_BOUNDARIES (
   POINTS       : in T_THE_POINTS;
   X_MIN, X_MAX : out T_COORDINATE) is
    LOC_X_MIN, LOC_X_MAX : T_COORDINATE;
  begin
    if POINTS'LENGTH = 0 then
      LOC_X_MIN := T_COORDINATE'FIRST;
      LOC_X_MAX := T_COORDINATE'LAST;
    else
      LOC_X_MIN := T_COORDINATE'LAST;
      LOC_X_MAX := T_COORDINATE'FIRST;

      for I in POINTS'RANGE loop
        if POINTS(I).X < LOC_X_MIN then
          LOC_X_MIN := POINTS(I).X;
        end if;
        if POINTS(I).X > LOC_X_MAX then
          LOC_X_MAX := POINTS(I).X;
        end if;
      end loop;
    end if;

    X_MIN := LOC_X_MIN;
    X_MAX := LOC_X_MAX;
  end X_BOUNDARIES;

  -- Coordinate to string
  function COO_TO_STR (C : T_COORDINATE) return STRING is
    STR : STRING (1..13);
  begin
    P_IO.PUT (STR, C, 5, 3);
    return STR;
  end COO_TO_STR;

  -- Draw and redraw curves until escape
  procedure DRAW (BOUNDARIES : in T_BOUNDARIES;
                  POINTS : in T_THE_POINTS) is

    -- Miscellaneous drawings on screen : help, axes, scales, points
    type T_MISC_LIST is (M_HELP, M_AXES, M_SCALE, M_POINTS, M_CURVE);
    type T_MISC is array (T_MISC_LIST) of BOOLEAN;
    MISC : T_MISC := (others => FALSE);

    -- Possible ZOOM_NO values
    subtype ZOOM_NO_RANGE is NATURAL range 0 .. 9;

    -- Current and last stored Zoom window
    CURR_ZOOM_NO, LAST_ZOOM_NO : ZOOM_NO_RANGE;
    ZOOM_ARRAY : array (ZOOM_NO_RANGE) of T_BOUNDARIES;


    -- Result of DRAW_ONE
    DRAW_RESULT : BOOLEAN;

    -- Compute real position (x, y) to screen position (x, y)
    package CONVERT is

      -- Screen coordinates of the frame
      type T_SCREEN_BOUNDARIES is record
        X_MIN, X_MAX : BIG_CON_IO.GRAPHICS.X_RANGE;
        Y_MIN, Y_MAX : BIG_CON_IO.GRAPHICS.Y_RANGE;
      end record;

      -- Current limits (in pixels) of drawing
      function GET_SCREEN_BOUNDARIES return T_SCREEN_BOUNDARIES;

      -- Is point (pixels) inside the frame (raise OUT_OF_FRAME if not)
      procedure IN_FRAME (X_S, Y_S : in INTEGER);

      -- From real to screen and reverse
      function X_REAL_SCREEN (X_REAL : T_COORDINATE)
                             return BIG_CON_IO.GRAPHICS.X_RANGE;
      function Y_REAL_SCREEN (Y_REAL : T_COORDINATE)
                             return BIG_CON_IO.GRAPHICS.Y_RANGE;
      function X_SCREEN_REAL (X_SCREEN : BIG_CON_IO.GRAPHICS.X_RANGE)
               return T_COORDINATE;
      function Y_SCREEN_REAL (Y_SCREEN : BIG_CON_IO.GRAPHICS.Y_RANGE)
               return T_COORDINATE;

      EPSILON : constant T_COORDINATE := T_COORDINATE'EPSILON;
      
      -- Computes every conversion according to new boundaries
      procedure MAJ (BOUNDS : in T_BOUNDARIES);

      -- Raised by MAJ if Xmax-Xmin <= EPSILON
      --            or    Ymax-Ymin <= EPSILON
      MAJ_ERROR : exception;

    end CONVERT;


    package body CONVERT is

      -- Screen and real coordinates of frame
      REAL_BOUNDARIES : T_BOUNDARIES;
      SCREEN_BOUNDARIES : T_SCREEN_BOUNDARIES;

      -- Conversion from real to screen :  screen = real * factor + offset
      type T_CONVERSION is record
        OFFSET_X, OFFSET_Y : MY_MATH.REAL;
        FACTOR_X, FACTOR_Y : MY_MATH.REAL;
      end record;
      CONVERSION : T_CONVERSION;

      -- Raised if point is not in frame
      OUT_OF_FRAME : exception;

      -- Display / hide wait message
      procedure WAIT_MESSAGE is
        MSG : constant STRING := "COMPUTING. Please wait ...";
      begin
        BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.LIGHT_BLUE);
        BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST,
                     BIG_CON_IO.COL_RANGE_LAST - MSG'LENGTH);
        BIG_CON_IO.PUT (MSG);
        BIG_CON_IO.FLUSH;
      end WAIT_MESSAGE;

      -- Screen coordinates of the frame
      function GET_SCREEN_BOUNDARIES return T_SCREEN_BOUNDARIES is
      begin
        return SCREEN_BOUNDARIES;
      end GET_SCREEN_BOUNDARIES;

      -- Is point (pixels) inside the frame (raise OUT_OF_FRAME if not)
      procedure IN_FRAME (X_S, Y_S : in INTEGER) is
      begin
        if      X_S < SCREEN_BOUNDARIES.X_MIN
        or else X_S > SCREEN_BOUNDARIES.X_MAX then
          raise OUT_OF_FRAME;
        end if;
        if      Y_S < SCREEN_BOUNDARIES.Y_MIN
        or else Y_S > SCREEN_BOUNDARIES.Y_MAX then
          raise OUT_OF_FRAME;
        end if;
      end IN_FRAME;

      -- screen <-> real conversions
      function X_REAL_SCREEN (X_REAL : T_COORDINATE)
                             return BIG_CON_IO.GRAPHICS.X_RANGE is
        X_INT : INTEGER;
        X_SCR : BIG_CON_IO.GRAPHICS.X_RANGE;
      begin
        X_INT := INTEGER (CONVERSION.OFFSET_X + X_REAL * CONVERSION.FACTOR_X);
        X_SCR := X_INT;
        return X_SCR;
      exception
        when others => raise OUT_OF_FRAME;
      end X_REAL_SCREEN;

      function Y_REAL_SCREEN (Y_REAL : T_COORDINATE)
                             return BIG_CON_IO.GRAPHICS.Y_RANGE is
        Y_INT : INTEGER;
        Y_SCR : BIG_CON_IO.GRAPHICS.Y_RANGE;
      begin
        Y_INT := INTEGER (CONVERSION.OFFSET_Y + Y_REAL * CONVERSION.FACTOR_Y);
        Y_SCR := Y_INT;
        return Y_SCR;
      exception
        when others => raise OUT_OF_FRAME;
      end Y_REAL_SCREEN;

      function X_SCREEN_REAL (X_SCREEN : BIG_CON_IO.GRAPHICS.X_RANGE)
                             return T_COORDINATE is
      begin
        return (T_COORDINATE(X_SCREEN)-CONVERSION.OFFSET_X)
             / CONVERSION.FACTOR_X;
      end X_SCREEN_REAL;

      function Y_SCREEN_REAL (Y_SCREEN : BIG_CON_IO.GRAPHICS.Y_RANGE)
                             return T_COORDINATE is
      begin
        return (T_COORDINATE(Y_SCREEN)-CONVERSION.OFFSET_Y)
             / CONVERSION.FACTOR_Y;
      end Y_SCREEN_REAL;

      -- Compute real_boundaries, screen_boundaries and conversion
      -- from the points and new boundaries
      procedure MAJ (BOUNDS : in T_BOUNDARIES) is
        X_REAL, Y_REAL : T_COORDINATE;
      begin
        -- Display wait message
        WAIT_MESSAGE;

        if      BOUNDS.SCALE = FREE_SCREEN
        or else BOUNDS.SCALE = FREE_NORMED then
          -- Real boundaries are provided by the caller
          REAL_BOUNDARIES := BOUNDS;
        else
          -- Real y boundaries must be computed
          -- (x provided, y of points and curve for y)
          REAL_BOUNDARIES := (SCALE => FREE_SCREEN,
           X_MIN => BOUNDS.X_MIN,
           Y_MIN => MY_MATH.REAL'LAST,
           X_MAX => BOUNDS.X_MAX,
           Y_MAX => MY_MATH.REAL'FIRST);

          -- Find lowest and greatest Y of points in X_min .. X_max
          for I in POINTS'RANGE loop
            if POINTS(I).X
               in REAL_BOUNDARIES.X_MIN .. REAL_BOUNDARIES.X_MAX then
              if POINTS(I).Y < REAL_BOUNDARIES.Y_MIN then
                REAL_BOUNDARIES.Y_MIN := POINTS(I).Y;
              end if;
              if POINTS(I).Y > REAL_BOUNDARIES.Y_MAX then
                REAL_BOUNDARIES.Y_MAX := POINTS(I).Y;
              end if;
            end if;
          end loop;
        end if;

        -- Compute X conversion
        if REAL_BOUNDARIES.X_MAX - REAL_BOUNDARIES.X_MIN <= EPSILON then
          raise MAJ_ERROR;
        end if;
        CONVERSION.FACTOR_X :=
          MY_MATH.REAL (BIG_CON_IO.GRAPHICS.X_MAX - BIG_CON_IO.GRAPHICS.X_RANGE'FIRST)
          / (REAL_BOUNDARIES.X_MAX - REAL_BOUNDARIES.X_MIN);
        CONVERSION.OFFSET_X  := MY_MATH.REAL (BIG_CON_IO.GRAPHICS.X_RANGE'FIRST)
         - REAL_BOUNDARIES.X_MIN * CONVERSION.FACTOR_X;

        -- Now X scale is computed, we can compute curve and update Ys
        if BOUNDS.SCALE /= FREE_SCREEN and then
           BOUNDS.SCALE /= FREE_NORMED then
          -- Find lowest and greatest y of curve
          for X in BIG_CON_IO.GRAPHICS.X_RANGE'FIRST .. BIG_CON_IO.GRAPHICS.X_MAX loop
            X_REAL := X_SCREEN_REAL (X);
            Y_REAL := F (X_REAL);
            if Y_REAL < REAL_BOUNDARIES.Y_MIN then
              REAL_BOUNDARIES.Y_MIN := Y_REAL;
            end if;
            if Y_REAL > REAL_BOUNDARIES.Y_MAX then
              REAL_BOUNDARIES.Y_MAX := Y_REAL;
            end if;
          end loop;
        end if;

        -- Compute Y conversion
        if REAL_BOUNDARIES.Y_MAX - REAL_BOUNDARIES.Y_MIN <= EPSILON then
          raise MAJ_ERROR;
        end if;
        CONVERSION.FACTOR_Y :=
          MY_MATH.REAL (BIG_CON_IO.GRAPHICS.Y_MAX - BIG_CON_IO.GRAPHICS.Y_RANGE'FIRST)
          / (REAL_BOUNDARIES.Y_MAX - REAL_BOUNDARIES.Y_MIN);
        CONVERSION.OFFSET_Y  := MY_MATH.REAL (BIG_CON_IO.GRAPHICS.Y_RANGE'FIRST)
        - REAL_BOUNDARIES.Y_MIN * CONVERSION.FACTOR_Y;

        -- If Scale is normed, factors must be the same on X and Y
        -- (the lowest)
        if      BOUNDS.SCALE = FREE_NORMED
        or else BOUNDS.SCALE = CURVE_NORMED then
          if CONVERSION.FACTOR_X < CONVERSION.FACTOR_Y then
            CONVERSION.FACTOR_Y := CONVERSION.FACTOR_X;
          else
            CONVERSION.FACTOR_X := CONVERSION.FACTOR_Y;
          end if;
          -- Update conversion
          CONVERSION.OFFSET_X  := MY_MATH.REAL (BIG_CON_IO.GRAPHICS.X_RANGE'FIRST)
          - REAL_BOUNDARIES.X_MIN * CONVERSION.FACTOR_X;
          CONVERSION.OFFSET_Y  := MY_MATH.REAL (BIG_CON_IO.GRAPHICS.Y_RANGE'FIRST)
          - REAL_BOUNDARIES.Y_MIN * CONVERSION.FACTOR_Y;
        end if;

        -- Compute screen boundaries
        SCREEN_BOUNDARIES.X_MIN := X_REAL_SCREEN (REAL_BOUNDARIES.X_MIN);
        SCREEN_BOUNDARIES.X_MAX := X_REAL_SCREEN (REAL_BOUNDARIES.X_MAX);
        SCREEN_BOUNDARIES.Y_MIN := Y_REAL_SCREEN (REAL_BOUNDARIES.Y_MIN);
        SCREEN_BOUNDARIES.Y_MAX := Y_REAL_SCREEN (REAL_BOUNDARIES.Y_MAX);

        -- Hide wait message
        WAIT_MESSAGE;

      exception
        when others =>
          -- Hide wait message
          WAIT_MESSAGE;
          raise;
      end MAJ;

    end CONVERT;


    -- Draw a curve and miscellaneous drawings on request
    -- -> when zoom defines new scale, new bounds are stored
    --     and TRUE is retuned
    -- -> when new ZOOM number is selected, then TRUE is returned
    -- -> when Escape, FALSE is returned
    function DRAW_ONE return BOOLEAN is

      use CONVERT;

      -- Draw first, show/hide or update, for scales or help
      type DRAW_ACTION is (INIT, TOGGLE, UPDATE);

      -- Nothing to draw, new zoom mode, or update for zoom frame
      type DRAW_FRAME_ACTION is (NONE, TOGGLE, UPDATE, REDRAW);
      ZOOM_FRAME_ACTION : DRAW_FRAME_ACTION;

      -- Previous values of scale/frame (for update)
      -- Updated by drawing functions
      PREV_SCALE_BOUNDS : T_SCREEN_BOUNDARIES;
      PREV_FRAME_BOUNDS : T_SCREEN_BOUNDARIES;

      -- Zoom modes: init, drag, done
      -- To be set : CURR before calls to DRAW_HELP and DRAW_Z_FRAME (TOGGLE)
      --             PREV after  calls to DRAW_HELP and DRAW_Z_FRAME (TOGGLE)
      -- PREV_ZOOM_MODE is set to CUR_ZOOM_MODE by DRAW_Z_FRAME(TOGGLE)
      type ZOOM_MODE_LIST is (INIT, DRAG, DONE);
      CURR_ZOOM_MODE, PREV_ZOOM_MODE : ZOOM_MODE_LIST;

      -- Current screen boundaries
      SCREEN_BOUNDARIES : constant T_SCREEN_BOUNDARIES
                        := GET_SCREEN_BOUNDARIES;
      -- Current mouse pos/boundaries
      MOUSE_BOUNDS : T_SCREEN_BOUNDARIES;

      -- For waiting for an event
      STR  : STRING (1 .. 1);
      LAST : NATURAL;
      STAT : BIG_CON_IO.CURS_MVT;
      POS  : POSITIVE;
      INS  : BOOLEAN;
      -- Input command
      CHAR : CHARACTER;
      -- Mouse event
      MOUSE_EVENT : BIG_CON_IO.MOUSE_EVENT_REC(BIG_CON_IO.X_Y);
      -- Status (pos) at start / end of drag
      CLICKED_STATUS : BIG_CON_IO.MOUSE_EVENT_REC(BIG_CON_IO.X_Y);

      MVALID : BOOLEAN;
      MX : BIG_CON_IO.GRAPHICS.X_RANGE;
      MY : BIG_CON_IO.GRAPHICS.Y_RANGE;

      -- Set mouse position within screen boundaries
      -- Invert Y
      procedure SET_MOUSE_IN_FRAME(X : in out BIG_CON_IO.GRAPHICS.X_RANGE;
                                   Y : in out BIG_CON_IO.GRAPHICS.Y_RANGE) is
      begin
        if X < SCREEN_BOUNDARIES.X_MIN then
          X := SCREEN_BOUNDARIES.X_MIN;
        elsif X > SCREEN_BOUNDARIES.X_MAX then
          X := SCREEN_BOUNDARIES.X_MAX;
        end if;
        if Y < SCREEN_BOUNDARIES.Y_MIN then
          Y := SCREEN_BOUNDARIES.Y_MIN;
        elsif Y > SCREEN_BOUNDARIES.Y_MAX then
          Y := SCREEN_BOUNDARIES.Y_MAX;
        end if;
      end SET_MOUSE_IN_FRAME;

      procedure TOGGLE_HELP_MISC (MISC_INDEX : in T_MISC_LIST) is
      begin
        BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.MAGENTA);
        case MISC_INDEX is
          when M_HELP =>
            null;
          when M_AXES =>
            BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 5,
                       BIG_CON_IO.COL_RANGE_LAST - 7);
            BIG_CON_IO.PUT ("*");
          when M_POINTS =>
            BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 4,
                       BIG_CON_IO.COL_RANGE_LAST - 7);
            BIG_CON_IO.PUT ("*");
          when M_CURVE =>
            BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 3,
                       BIG_CON_IO.COL_RANGE_LAST - 7);
            BIG_CON_IO.PUT ("*");
          when M_SCALE =>
            BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 2,
                       BIG_CON_IO.COL_RANGE_LAST - 7);
            BIG_CON_IO.PUT ("*");
        end case;
      end TOGGLE_HELP_MISC;

      -- Draw help message
      procedure DRAW_HELP (ACTION : in DRAW_ACTION) is

        -- Dedicated message according to zoom mode (hide/show)
        procedure PUT_MODE (MODE : in ZOOM_MODE_LIST) is
        begin
          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 10,
                       BIG_CON_IO.COL_RANGE_LAST - 17);
          case MODE is
            when INIT =>
              BIG_CON_IO.PUT ("Point & click L");
            when DRAG =>
              BIG_CON_IO.PUT ("Drag L & release");
            when DONE =>
              BIG_CON_IO.PUT ("L or R click");
          end case;
        end PUT_MODE;

      begin
        -- Optimization : most frequent case (update and same mode or no help)
        if ACTION = UPDATE and then
           (CURR_ZOOM_MODE = PREV_ZOOM_MODE or else not MISC(M_HELP)) then
          return;
        end if;

        -- Help on zoom only if mouse
        BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.MAGENTA);

        -- Previous mode to hide : Something drawn and new thing different
        if MISC(M_HELP) and then
             ((ACTION = UPDATE and then
               CURR_ZOOM_MODE /= PREV_ZOOM_MODE) or else
              ACTION = TOGGLE)                   then
          PUT_MODE (PREV_ZOOM_MODE);
        end if;

        -- New mode to draw : toggle to new or update
        if (MISC(M_HELP) and then
            ACTION = UPDATE and then
            CURR_ZOOM_MODE /= PREV_ZOOM_MODE)          or else
           (not MISC(M_HELP) and then ACTION = TOGGLE) or else
           (ACTION = INIT)                             then
          PUT_MODE (CURR_ZOOM_MODE);
        end if;

        -- Global help
        if ACTION = TOGGLE or else ACTION = INIT then
          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 9,
                       BIG_CON_IO.COL_RANGE_LAST - 17);
          BIG_CON_IO.PUT ("Current ZOOM: " & NORMAL(CURR_ZOOM_NO, 1) );
          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 8,
                       BIG_CON_IO.COL_RANGE_LAST - 17);
          BIG_CON_IO.PUT ("0.." & NORMAL(LAST_ZOOM_NO, 1) & ": other ZOOM");

          -- if mouse not installed : color is set here
          BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.MAGENTA);

          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 7,
                       BIG_CON_IO.COL_RANGE_LAST - 9);
          BIG_CON_IO.PUT ("SWITCHES:");
          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 6,
                       BIG_CON_IO.COL_RANGE_LAST - 9);
          BIG_CON_IO.PUT ("H * Help");
          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 5,
                       BIG_CON_IO.COL_RANGE_LAST - 9);
          BIG_CON_IO.PUT ("A   Axes");
          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 4,
                       BIG_CON_IO.COL_RANGE_LAST - 9);
          BIG_CON_IO.PUT ("P   Points");
          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 3,
                       BIG_CON_IO.COL_RANGE_LAST - 9);
          BIG_CON_IO.PUT ("C   Curve");
          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 2,
                       BIG_CON_IO.COL_RANGE_LAST - 9);
          BIG_CON_IO.PUT ("S   Scales");
          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE_LAST - 1,
                       BIG_CON_IO.COL_RANGE_LAST - 9);
          BIG_CON_IO.PUT ("Esc Exit");
        end if;

        -- New help mode
        if ACTION = TOGGLE then
          MISC(M_HELP) := not MISC(M_HELP);
          for I in T_MISC_LIST loop
            if MISC (I) then
              TOGGLE_HELP_MISC (I);
            end if;
          end loop;
        end if;
      end DRAW_HELP;

      -- Draw all points
      procedure DRAW_POINTS is

        -- Draw a point knowing its real coordinates
        procedure DRAW_POINT (X, Y : in T_COORDINATE) is
          X_S : BIG_CON_IO.GRAPHICS.X_RANGE;
          Y_S : BIG_CON_IO.GRAPHICS.Y_RANGE;
          type PIX is record
            X, Y: INTEGER;
          end record;
          type T_POINT_PIXELS is array (POSITIVE range <>) of PIX;
          POINT_PIXELS : T_POINT_PIXELS (1..15) :=
               ( (-2, -2), (-2,  2), ( 2, -2), (  2,  2), (-1, -1),
                 (-1,  0), (-1,  1), ( 1, -1), (  1,  0), ( 1,  1),
                 ( 0, -2), ( 0, -1), ( 0,  0), (  0,  1), ( 0,  2) );
        begin
          X_S := X_REAL_SCREEN (X);
          Y_S := Y_REAL_SCREEN (Y);
          for I in POINT_PIXELS'RANGE loop
            begin
              IN_FRAME (X_S + POINT_PIXELS(I).X, Y_S + POINT_PIXELS(I).Y);
              BIG_CON_IO.GRAPHICS.DRAW_POINT (X_S + POINT_PIXELS(I).X,
                                          Y_S + POINT_PIXELS(I).Y);
            exception
              when others => null;
            end;
          end loop;
        exception
          when others => null;
        end DRAW_POINT;

      begin
        BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.RED);
        for I in POINTS'RANGE loop
          DRAW_POINT (POINTS(I).X, POINTS(I).Y);
        end loop;
        if MISC (M_HELP) then
          TOGGLE_HELP_MISC (M_POINTS);
        end if;
      end DRAW_POINTS;

      -- Draw an horizontal line (for frames and axes)
      procedure DRAW_X (X_MIN, X_MAX : in NATURAL; Y : in NATURAL) is
      begin
        BIG_CON_IO.GRAPHICS.DRAW_LINE (X_MIN, Y, X_MAX, Y);
      end DRAW_X;

      -- Draw a vertical line (for frames and axes)
      procedure DRAW_Y (X : in NATURAL; Y_MIN, Y_MAX : in NATURAL) is
      begin
        BIG_CON_IO.GRAPHICS.DRAW_LINE (X, Y_MIN, X, Y_MAX);
      end DRAW_Y;

      -- Draw axes of the curve
      procedure DRAW_AXES is
        X_0, Y_0 : NATURAL;
        INTERSEC : BOOLEAN := TRUE;
      begin
        BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.LIGHT_BLUE);
        -- Horizontal
        begin
          Y_0 := Y_REAL_SCREEN (0.0);
          IN_FRAME (0, Y_0);
          DRAW_X(SCREEN_BOUNDARIES.X_MIN+1, SCREEN_BOUNDARIES.X_MAX-1, Y_0);
        exception
          when others => INTERSEC := FALSE;
        end;
        -- Vertical
        begin
          X_0 := X_REAL_SCREEN (0.0);
          IN_FRAME (X_0, 0);
          DRAW_Y (X_0, SCREEN_BOUNDARIES.Y_MIN+1, SCREEN_BOUNDARIES.Y_MAX-1);
        exception
          when others => INTERSEC := FALSE;
        end;
        -- Re draw intersection of axes
        if INTERSEC then
          begin
            IN_FRAME (X_0, Y_0);
            BIG_CON_IO.GRAPHICS.DRAW_POINT (X_0, Y_0);
          exception
            when others => null;
          end;
        end if;
        if MISC (M_HELP) then
          TOGGLE_HELP_MISC (M_AXES);
        end if;

      end DRAW_AXES;

      -- Draw the curve
      procedure DRAW_CURVE is
        Y_S : INTEGER;
        X_R, Y_R : MY_MATH.REAL;

        -- Draw the frame around the curve
        procedure DRAW_FRAME is
        begin
          BIG_CON_IO.SET_XOR_MODE (BIG_CON_IO.XOR_OFF);
          BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.WHITE);
          DRAW_X (SCREEN_BOUNDARIES.X_MIN, SCREEN_BOUNDARIES.X_MAX,
                  SCREEN_BOUNDARIES.Y_MIN);
          DRAW_Y (SCREEN_BOUNDARIES.X_MAX, SCREEN_BOUNDARIES.Y_MIN,
                  SCREEN_BOUNDARIES.Y_MAX);
          DRAW_X (SCREEN_BOUNDARIES.X_MIN, SCREEN_BOUNDARIES.X_MAX,
                  SCREEN_BOUNDARIES.Y_MAX);
          DRAW_Y (SCREEN_BOUNDARIES.X_MIN, SCREEN_BOUNDARIES.Y_MIN,
                  SCREEN_BOUNDARIES.Y_MAX);
          BIG_CON_IO.SET_XOR_MODE (BIG_CON_IO.XOR_ON);
        end DRAW_FRAME;

      begin
        -- Draw frame
        DRAW_FRAME;
        BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.LIGHT_GREEN);
        -- Draw pixel for each possible X screen
        for X_S in BIG_CON_IO.GRAPHICS.X_RANGE
                 range SCREEN_BOUNDARIES.X_MIN .. SCREEN_BOUNDARIES.X_MAX loop
          begin
            -- Xscreen -> Xreal -> Yreal -> Yscreen
            X_R := X_SCREEN_REAL (X_S);
            Y_R := F (X_R);
            Y_S := Y_REAL_SCREEN (Y_R);
            IN_FRAME (X_S, Y_S);
            BIG_CON_IO.GRAPHICS.DRAW_POINT (X_S, Y_S);
          exception
            when others => null;
          end;
        end loop;
        if MISC (M_HELP) then
          TOGGLE_HELP_MISC (M_CURVE);
        end if;
        -- BIG_CON_IO.BELL(1);
      end DRAW_CURVE;


      -- Draw scales. Only external if no mouse
      --  Also mouse position / drag current limits if mouse
      procedure DRAW_SCALE (ACTION : in DRAW_ACTION;
                            ZOOM_BOUNDS : in T_SCREEN_BOUNDARIES) is
        type SCALE_POS is (X_MIN, X_MAX, Y_MIN, Y_MAX);

        -- Draw/hide one Zoom scale value
        procedure PUT_SCALE (SCALE : in T_COORDINATE; POS : in SCALE_POS) is
        begin
          case POS is
            when X_MIN =>
              BIG_CON_IO.MOVE (13, 1);
            when X_MAX =>
              BIG_CON_IO.MOVE (13, 66);
            when Y_MIN =>
              BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE'PRED(BIG_CON_IO.ROW_RANGE'PRED(
                            BIG_CON_IO.ROW_RANGE_LAST)), 30);
            when Y_MAX =>
              BIG_CON_IO.MOVE (2, 30);
          end case;
          BIG_CON_IO.PUT (COO_TO_STR(SCALE));
        end PUT_SCALE;
      begin
        -- Optimization : most frequent case
        -- update and same value or no scale
        if ACTION = UPDATE and then
           (ZOOM_BOUNDS = PREV_SCALE_BOUNDS or else not MISC(M_SCALE)) then
          return;
        end if;

        BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.CYAN);

        -- Previous scale to hide : Something drawn and new values different
        if MISC(M_SCALE) then
          if (ACTION = UPDATE and then ZOOM_BOUNDS /= PREV_SCALE_BOUNDS)
          or else ACTION = TOGGLE then
            PUT_SCALE (X_SCREEN_REAL(PREV_SCALE_BOUNDS.X_MIN), X_MIN);
            PUT_SCALE (X_SCREEN_REAL(PREV_SCALE_BOUNDS.X_MAX), X_MAX);
            PUT_SCALE (Y_SCREEN_REAL(PREV_SCALE_BOUNDS.Y_MIN), Y_MIN);
            PUT_SCALE (Y_SCREEN_REAL(PREV_SCALE_BOUNDS.Y_MAX), Y_MAX);
          end if;
        end if;

        -- New scale to draw : new values update or toggle to new
        if (MISC(M_SCALE) and then ACTION = UPDATE and then
                              ZOOM_BOUNDS /= PREV_SCALE_BOUNDS)
           or else (ACTION = TOGGLE and then not MISC(M_SCALE))
           or else (ACTION = INIT   and then     MISC(M_SCALE)) then
          PUT_SCALE (X_SCREEN_REAL(ZOOM_BOUNDS.X_MIN), X_MIN);
          PUT_SCALE (X_SCREEN_REAL(ZOOM_BOUNDS.X_MAX), X_MAX);
          PUT_SCALE (Y_SCREEN_REAL(ZOOM_BOUNDS.Y_MIN), Y_MIN);
          PUT_SCALE (Y_SCREEN_REAL(ZOOM_BOUNDS.Y_MAX), Y_MAX);
          PREV_SCALE_BOUNDS := ZOOM_BOUNDS;
        end if;

        if      ACTION = TOGGLE
        or else (ACTION = INIT and then MISC (M_SCALE)) then
          -- External scales
          BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.LIGHT_GRAY);
          BIG_CON_IO.MOVE (1, 30);
          BIG_CON_IO.PUT (COO_TO_STR(Y_SCREEN_REAL(SCREEN_BOUNDARIES.Y_MAX)));
          BIG_CON_IO.MOVE (BIG_CON_IO.ROW_RANGE'PRED(BIG_CON_IO.ROW_RANGE_LAST), 30);
          BIG_CON_IO.PUT (COO_TO_STR(Y_SCREEN_REAL(SCREEN_BOUNDARIES.Y_MIN)));
          BIG_CON_IO.MOVE (12, 1);
          BIG_CON_IO.PUT (COO_TO_STR(X_SCREEN_REAL(SCREEN_BOUNDARIES.X_MIN)));
          BIG_CON_IO.MOVE (12, 66);
          BIG_CON_IO.PUT (COO_TO_STR(X_SCREEN_REAL(SCREEN_BOUNDARIES.X_MAX)));
        end if;

        -- Toggle mode
        if ACTION = TOGGLE then
           MISC(M_SCALE) := not MISC(M_SCALE);
        end if;

        -- Update help
        if ACTION = INIT or else ACTION = TOGGLE then
          if MISC (M_HELP) then
            TOGGLE_HELP_MISC (M_SCALE);
          end if;
        end if;
      end DRAW_SCALE;

      -- Draw Z frame (when in drag)
      procedure DRAW_Z_FRAME (ACTION : in DRAW_FRAME_ACTION := UPDATE;
                              ZOOM_BOUNDS : in T_SCREEN_BOUNDARIES) is

        -- Draw/hide a zoom frame
        procedure PUT_FRAME (BOUNDS : in T_SCREEN_BOUNDARIES) is
        begin
          BIG_CON_IO.GRAPHICS.DRAW_RECTANGLE (
             BOUNDS.X_MIN,
             BOUNDS.Y_MIN,
             BOUNDS.X_MAX,
             BOUNDS.Y_MAX);
        end PUT_FRAME;

      begin
        -- Redraw (refresh when done)
        if ACTION = REDRAW then
          BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.CYAN);
          PUT_FRAME(PREV_FRAME_BOUNDS);
          return;
        end if;

        -- Optimization : most frequent case
        -- Update and same frame, or update and not in drag
        if ACTION = UPDATE
        and then (CURR_ZOOM_MODE /= DRAG
                      or else ZOOM_BOUNDS = PREV_FRAME_BOUNDS) then
          return;
        end if;
        if ACTION = NONE then
          return;
        end if;

        -- If action = update, then cur mode is drag and bounds are new
        BIG_CON_IO.SET_FOREGROUND (BIG_CON_IO.CYAN);

        -- Previous frame to hide : new drag or drag -> done or done -> init
        if ACTION = UPDATE
           or else CURR_ZOOM_MODE /= DRAG then
          PUT_FRAME (PREV_FRAME_BOUNDS);
        end if;

        -- New frame to draw : new drag or init -> drag or drag -> done
        if ACTION = UPDATE
        or else CURR_ZOOM_MODE /= INIT then
          PUT_FRAME(ZOOM_BOUNDS);
          PREV_FRAME_BOUNDS := ZOOM_BOUNDS;
        end if;
        -- Toggle zoom mode
        if ACTION = TOGGLE then
          PREV_ZOOM_MODE := CURR_ZOOM_MODE;
        end if;
      end DRAW_Z_FRAME;

      -- Exchange x_min and x_max if necessary. Same on Y.
      -- (For mouse scales when in drag)
      procedure SORT_BOUNDS (BOUNDS : in out T_SCREEN_BOUNDARIES) is
        TMP : NATURAL;
      begin
        if BOUNDS.X_MIN > BOUNDS.X_MAX then
          TMP := BOUNDS.X_MIN;
          BOUNDS.X_MIN := BOUNDS.X_MAX;
          BOUNDS.X_MAX := TMP;
        end if;
        if BOUNDS.Y_MIN > BOUNDS.Y_MAX then
          TMP := BOUNDS.Y_MIN;
          BOUNDS.Y_MIN := BOUNDS.Y_MAX;
          BOUNDS.Y_MAX := TMP;
        end if;
      end SORT_BOUNDS;

      procedure CANCEL_ZOOM is
      begin
        CURR_ZOOM_MODE := INIT;
        -- Reset zoom and hide zoom frame.
        DRAW_HELP (UPDATE);
        ZOOM_FRAME_ACTION := TOGGLE;
        BIG_CON_IO.ENABLE_MOTION_EVENTS (MISC(M_SCALE));
      end CANCEL_ZOOM;

    use BIG_CON_IO;
    begin -- DRAW_ONE

      -- Init context
      PREV_ZOOM_MODE := INIT;
      CURR_ZOOM_MODE := INIT;
      STAT := BIG_CON_IO.REFRESH;
      BIG_CON_IO.ENABLE_MOTION_EVENTS (MISC(M_SCALE));
      MISC(M_CURVE) := TRUE;

      BIG_CON_IO.GRAPHICS.GET_CURRENT_POINTER_POS (MVALID, MX, MY);
      if MVALID then
        SET_MOUSE_IN_FRAME(MX, MY);
        MOUSE_BOUNDS.X_MIN := MX;
        MOUSE_BOUNDS.X_MAX := MX;
        MOUSE_BOUNDS.Y_MIN := MY;
        MOUSE_BOUNDS.Y_MAX := MY;
        CLICKED_STATUS.X := MX;
        CLICKED_STATUS.Y := MY;
        PREV_FRAME_BOUNDS := MOUSE_BOUNDS;
        PREV_SCALE_BOUNDS := MOUSE_BOUNDS;
      end if;

      loop -- Main loop of mouse and keys actions

        if STAT = BIG_CON_IO.REFRESH and then CURR_ZOOM_MODE /= DRAG then
          -- Discard refresh when in drag
          -- Frozen mouse when done
          if CURR_ZOOM_MODE /= DONE then
            BIG_CON_IO.GRAPHICS.GET_CURRENT_POINTER_POS (MVALID, MX, MY);
            if MVALID then
              SET_MOUSE_IN_FRAME(MX, MY);
              MOUSE_BOUNDS.X_MIN := MX;
              MOUSE_BOUNDS.X_MAX := MX;
              MOUSE_BOUNDS.Y_MIN := MY;
              MOUSE_BOUNDS.Y_MAX := MY;
            end if;
          end if;
          -- Draw what has to be for initial/refresh
          BIG_CON_IO.CLEAR;
          if MISC(M_CURVE)  then DRAW_CURVE; end if;
          if MISC(M_AXES)   then DRAW_AXES; end if;
          if MISC(M_POINTS) then DRAW_POINTS; end if;
          if MISC(M_HELP)   then DRAW_HELP(INIT); end if;
          if MISC(M_SCALE)  then DRAW_SCALE(INIT, MOUSE_BOUNDS); end if;
          if CURR_ZOOM_MODE = DONE then
            -- MOUSE_BOUNDS not used
            DRAW_Z_FRAME (REDRAW, MOUSE_BOUNDS);
          end if;
        end if;

        -- Infinite wait
        BIG_CON_IO.GET (STR, LAST, STAT, POS, INS, ECHO => FALSE);

        case STAT is
          when BIG_CON_IO.BREAK =>
            -- End of curve
            return FALSE;
          when BIG_CON_IO.REFRESH =>
            -- Redraw at next loop;
            null;
          when BIG_CON_IO.TIMEOUT =>
            -- Should not occure: GET(INFINITE_TIME)
            null;
          when BIG_CON_IO.ESC =>
            if CURR_ZOOM_MODE = INIT then
              -- Initial zoom mode. Exit drawings.
              return FALSE;
            end if;
          when BIG_CON_IO.FULL =>
            -- KEY pressed
            CHAR := STR(1);
            if UPPER_CHAR(CHAR) = 'A' then
              -- Toggle axes
              DRAW_AXES;
              MISC(M_AXES) := not MISC(M_AXES);
            elsif UPPER_CHAR(CHAR) = 'S' then
              -- Toggle scales
              DRAW_SCALE (TOGGLE, MOUSE_BOUNDS);
              BIG_CON_IO.ENABLE_MOTION_EVENTS (
                 MISC(M_SCALE) or else CURR_ZOOM_MODE = DRAG);
            elsif UPPER_CHAR(CHAR) = 'P' then
              -- Toggle points
              DRAW_POINTS;
              MISC(M_POINTS) := not MISC(M_POINTS);
            elsif UPPER_CHAR(CHAR) = 'H' then
              -- Toggle help
              DRAW_HELP(TOGGLE);
            elsif UPPER_CHAR(CHAR) = 'C' then
              -- Toggle curve
              MISC(M_CURVE) := not MISC(M_CURVE);
              DRAW_CURVE;
            elsif CHAR >= '0' and then
                  CHAR <= CHARACTER'VAL(LAST_ZOOM_NO + CHARACTER'POS('0')) then
              -- New zoom level selection
              CURR_ZOOM_NO := CHARACTER'POS(CHAR) - CHARACTER'POS('0');
              -- Compute new conversions
              MAJ (ZOOM_ARRAY(CURR_ZOOM_NO));
              return TRUE;
            elsif CHAR = '+' then
              if CURR_ZOOM_NO /= ZOOM_NO_RANGE'LAST then
                CURR_ZOOM_NO := CURR_ZOOM_NO + 1;
                -- Compute new conversions
                MAJ (ZOOM_ARRAY(CURR_ZOOM_NO));
                return TRUE;
              else
                BIG_CON_IO.BELL(3);
              end if;
            elsif CHAR = '-' then
              if CURR_ZOOM_NO /= ZOOM_NO_RANGE'FIRST then
                CURR_ZOOM_NO := CURR_ZOOM_NO - 1;
                -- Compute new conversions
                MAJ (ZOOM_ARRAY(CURR_ZOOM_NO));
                return TRUE;
              else
                BIG_CON_IO.BELL(3);
              end if;
            else
              -- Invalid key
              BIG_CON_IO.BELL(3);
            end if;

          when BIG_CON_IO.MOUSE_BUTTON =>
            -- New button status
            BIG_CON_IO.GET_MOUSE_EVENT (MOUSE_EVENT, BIG_CON_IO.X_Y);
            SET_MOUSE_IN_FRAME(MOUSE_EVENT.X, MOUSE_EVENT.Y);
            -- Update scales and frame according to zoom mode
            if CURR_ZOOM_MODE = INIT then
              -- Before DRAG : follow cur pos
              MOUSE_BOUNDS.X_MIN := MOUSE_EVENT.X;
              MOUSE_BOUNDS.X_MAX := MOUSE_EVENT.X;
              MOUSE_BOUNDS.Y_MIN := MOUSE_EVENT.Y;
              MOUSE_BOUNDS.Y_MAX := MOUSE_EVENT.Y;
            elsif CURR_ZOOM_MODE = DRAG then
              -- Drag : clicked_pos and cur pos
              MOUSE_BOUNDS.X_MIN := MOUSE_EVENT.X;
              MOUSE_BOUNDS.X_MAX := CLICKED_STATUS.X;
              MOUSE_BOUNDS.Y_MIN := MOUSE_EVENT.Y;
              MOUSE_BOUNDS.Y_MAX := CLICKED_STATUS.Y;
              SORT_BOUNDS (MOUSE_BOUNDS);
              ZOOM_FRAME_ACTION := UPDATE;
            end if;

            case CURR_ZOOM_MODE is
              when INIT =>
                if MOUSE_EVENT.VALID
                and then MOUSE_EVENT.BUTTON = BIG_CON_IO.LEFT
                and then MOUSE_EVENT.STATUS = BIG_CON_IO.PRESSED then
                
                  CURR_ZOOM_MODE := DRAG;
                  DRAW_HELP (UPDATE);
                  -- Store what has to be done with zoom frame
                  CLICKED_STATUS := MOUSE_EVENT;
                  ZOOM_FRAME_ACTION := TOGGLE;
                  BIG_CON_IO.ENABLE_MOTION_EVENTS (TRUE);
                else
                  -- no change
                  ZOOM_FRAME_ACTION := NONE;
                end if;
              when DRAG =>
                if       MOUSE_EVENT.BUTTON = BIG_CON_IO.LEFT
                and then MOUSE_EVENT.STATUS = BIG_CON_IO.RELEASED then
                  -- release
                  if      MOUSE_EVENT.X = CLICKED_STATUS.X
                  or else MOUSE_EVENT.Y = CLICKED_STATUS.Y then
                    -- No zoom possible : Cancel
                    CANCEL_ZOOM;
                  else
                    -- Drag done
                    BIG_CON_IO.ENABLE_MOTION_EVENTS (FALSE);
                    CURR_ZOOM_MODE := DONE;
                    DRAW_HELP (UPDATE);
                    -- Store what has to be done with zoom frame
                    ZOOM_FRAME_ACTION := TOGGLE;
                  end if;
                end if;
              when DONE =>
                if MOUSE_EVENT.VALID
                and then MOUSE_EVENT.BUTTON = BIG_CON_IO.LEFT
                and then MOUSE_EVENT.STATUS = BIG_CON_IO.PRESSED then
                  -- Click left : Validate
                  -- Zoom status is DONE. Validate new scales in Curr_zoom_no+1
                  declare
                    ROOT_BOUNDS : T_BOUNDARIES
                                  renames ZOOM_ARRAY(ZOOM_NO_RANGE'FIRST);
                    NEW_BOUNDS :  T_BOUNDARIES;
                    NEW_ZOOM_NO : ZOOM_NO_RANGE;
                  begin

                    if CURR_ZOOM_NO /= ZOOM_NO_RANGE'LAST then
                      NEW_ZOOM_NO := CURR_ZOOM_NO + 1;
                    else
                      NEW_ZOOM_NO := CURR_ZOOM_NO;
                    end if;
                    -- New_bounds.scale mode is FREE_SCREEN or FREE_NORMED
                    --  according to initial scale type
                    if ROOT_BOUNDS.SCALE = CURVE_SCREEN or else
                       ROOT_BOUNDS.SCALE = FREE_SCREEN then
                      NEW_BOUNDS := (SCALE => FREE_SCREEN,
                         X_MIN => X_SCREEN_REAL(MOUSE_BOUNDS.X_MIN),
                         X_MAX => X_SCREEN_REAL(MOUSE_BOUNDS.X_MAX),
                         Y_MIN => Y_SCREEN_REAL(MOUSE_BOUNDS.Y_MIN),
                         Y_MAX => Y_SCREEN_REAL(MOUSE_BOUNDS.Y_MAX) );
                    else  -- NORMED
                      NEW_BOUNDS := (SCALE => FREE_NORMED,
                         X_MIN => X_SCREEN_REAL(MOUSE_BOUNDS.X_MIN),
                         X_MAX => X_SCREEN_REAL(MOUSE_BOUNDS.X_MAX),
                         Y_MIN => Y_SCREEN_REAL(MOUSE_BOUNDS.Y_MIN),
                         Y_MAX => Y_SCREEN_REAL(MOUSE_BOUNDS.Y_MAX) );
                    end if;
                    -- Compute new conversions
                    begin
                      MAJ (NEW_BOUNDS);
                      -- No exception, store it
                      CURR_ZOOM_NO := NEW_ZOOM_NO;
                      LAST_ZOOM_NO := CURR_ZOOM_NO;
                      ZOOM_ARRAY(CURR_ZOOM_NO) := NEW_BOUNDS;
                      BIG_CON_IO.ENABLE_MOTION_EVENTS (FALSE);
                      return TRUE;
                    exception
                      when others =>
                        CANCEL_ZOOM;
                        BIG_CON_IO.BELL(3);
                    end;
                  end;

                elsif MOUSE_EVENT.VALID
                and then MOUSE_EVENT.BUTTON = BIG_CON_IO.RIGHT
                and then MOUSE_EVENT.STATUS = BIG_CON_IO.PRESSED then
                  CANCEL_ZOOM;
                else
                  -- no change in init or done
                  ZOOM_FRAME_ACTION := NONE;
                end if;

            end case; -- Current zoom mode

            -- perform zoom frame drawing
            if MOUSE_EVENT.VALID
            or else MOUSE_EVENT.STATUS /= BIG_CON_IO.PRESSED then
              DRAW_Z_FRAME (ZOOM_FRAME_ACTION, MOUSE_BOUNDS);
            end if;
          when others =>
            -- Ret, arrows, Pg*...
            -- Invalid key
            BIG_CON_IO.BELL(3);
            
        end case; -- event

        -- perform scales drawing
        DRAW_SCALE (UPDATE, MOUSE_BOUNDS);

      end loop;

    end DRAW_ONE;

  begin -- DRAW
    -- Initialise graphics
    BIG_CON_IO.INIT;
    BIG_CON_IO.SET_FOREGROUND (BLINK_STAT => BIG_CON_IO.NOT_BLINK);

    BIG_CON_IO.SET_XOR_MODE (BIG_CON_IO.XOR_ON);
    BIG_CON_IO.SET_POINTER_SHAPE(BIG_CON_IO.CROSS);

    -- Initialise zooms storing
    ZOOM_ARRAY(ZOOM_NO_RANGE'FIRST) := BOUNDARIES;
    CURR_ZOOM_NO := ZOOM_NO_RANGE'FIRST;
    LAST_ZOOM_NO := ZOOM_NO_RANGE'FIRST;
    -- Compute first conversions
    CONVERT.MAJ(ZOOM_ARRAY(ZOOM_NO_RANGE'FIRST));

    loop
      begin
        DRAW_RESULT := DRAW_ONE;
      exception
        when others =>
          if CURR_ZOOM_NO = ZOOM_NO_RANGE'FIRST then
            -- Exception in first draw is fatal
            raise;
          else
            -- try to go back to previous (which should be ok).
            -- At least, we raise after 9 attempts!
            CURR_ZOOM_NO := CURR_ZOOM_NO - 1;
            CONVERT.MAJ(ZOOM_ARRAY(CURR_ZOOM_NO));
            DRAW_RESULT := TRUE;
            BIG_CON_IO.BELL(3);
          end if;
      end;

      if not DRAW_RESULT then
        -- Exit drawings
        BIG_CON_IO.DESTROY;
        exit;
      else
        -- New drawing : clear graphic
        BIG_CON_IO.RESET_TERM;
      end if;

    end loop;


  exception
    when others =>
      begin
        BIG_CON_IO.DESTROY;
      exception
        when others => null;
      end;
      raise;
  end DRAW;

  function INIT return BOOLEAN is
  begin
    BIG_CON_IO.INIT;
    return TRUE;
  exception
    when BIG_CON_IO.INIT_FAILURE =>
      return FALSE;
  end INIT;

  procedure DESTROY is
  begin
    BIG_CON_IO.DESTROY;
  exception
    when others =>
      null;
  end DESTROY;
end CURVE;

