with CON_IO, AFPX, CURVE, MATH;
with POINTS, SCREEN, SET_POINTS_LIST, DIALOG, RESOL, MENU21;
package body MENU2 is

  type RESTORE_LIST is (NONE, PARTIAL, LIST, FULL); 
  CURSOR_FIELD : AFPX.FIELD_RANGE;

  package CURVE_DATA is
    NB_POINTS : NATURAL;
    THE_POINTS : POINTS.P_T_THE_POINTS(1 .. POINTS.MAX_NUMBER);
    THE_DEGREE : RESOL.R_T_DEGREE;
    THE_SOLUTION : RESOL.VECTOR (1 .. RESOL.R_T_DEGREE'LAST + 1);
    THE_BOUNDS : CURVE.T_BOUNDARIES;
    THE_BOUNDS_SET : BOOLEAN;
    function CURVE_F_X (X : POINTS.P_T_COORDINATE)
                       return POINTS.P_T_COORDINATE;
    procedure CURVE_DRAW (BOUNDARIES : in CURVE.T_BOUNDARIES;
                          POINTS : in CURVE.T_THE_POINTS);
  end CURVE_DATA;

  task CURVE_TASK is
    -- Start computation, SOLUTION is computed on main stack
    entry START (SOLUTION : in RESOL.VECTOR; OK : out BOOLEAN);
    entry STOPPED;
  end;

  procedure ERROR (MSG : in SCREEN.S_ERROR_LIST) is
  begin
    SCREEN.ERROR(MSG);
    -- Restore screen
    AFPX.USE_DESCRIPTOR(3, FALSE);
    SCREEN.INIT_FOR_MAIN2 (CURSOR_FIELD);
  end ERROR;

  procedure DO_RESTORE (RESTORE : in RESTORE_LIST) is
    ACTIVATE_NO_CURVE : constant BOOLEAN := CURVED_STOPPED;
  begin
    case RESTORE is
      when NONE =>
        null;
      when PARTIAL =>
        AFPX.USE_DESCRIPTOR(3, FALSE);
        SCREEN.INIT_FOR_MAIN2 (CURSOR_FIELD);
        SCREEN.PUT_FILE;
      when LIST =>
        -- polynom display needs reset of list
        AFPX.USE_DESCRIPTOR(3, FALSE);
        SET_POINTS_LIST;
        SCREEN.INIT_FOR_MAIN2 (CURSOR_FIELD);
        SCREEN.PUT_FILE;
      when FULL =>
        AFPX.USE_DESCRIPTOR(3, TRUE);
        SET_POINTS_LIST;
        SCREEN.INIT_FOR_MAIN2 (CURSOR_FIELD);
        SCREEN.PUT_FILE;
    end case;
    -- Activate or not according to curve activity
    -- Back
    AFPX.SET_FIELD_ACTIVATION (SCREEN.EXIT_BUTTON_FLD, ACTIVATE_NO_CURVE);
    -- Set degree
    AFPX.SET_FIELD_ACTIVATION (22, ACTIVATE_NO_CURVE);
    -- Draw
    AFPX.SET_FIELD_ACTIVATION (31, ACTIVATE_NO_CURVE);
  end DO_RESTORE;

  function F_X (X : POINTS.P_T_COORDINATE;
                    POLYNOM : RESOL.VECTOR) return POINTS.P_T_COORDINATE is
     Y : POINTS.P_T_COORDINATE := 0.0;
     BUBBLE : POINTS.P_T_COORDINATE := 1.0;
     use MATH;
  begin
     -- Y = F(X) from vector
     for I in POLYNOM'RANGE loop
       Y := Y + POLYNOM(I) * BUBBLE;
       BUBBLE := BUBBLE * X;
     end loop;
     return Y;
   end F_X;

  procedure COMPUTE_XY (POINT : out POINTS.P_T_ONE_POINT;  OK : out BOOLEAN) is
   LP : POINTS.P_T_ONE_POINT;
   SET : BOOLEAN;
  begin
    SCREEN.PUT_TITLE(SCREEN.Y_F_X);
    -- Get X
    SET := FALSE;
    DIALOG.READ_COORDINATE (SCREEN.I_X, SET, LP.X);
    AFPX.SET_FIELD_ACTIVATION (SCREEN.GET_FLD, FALSE);
    if not SET then
      OK := FALSE;
      return;
    end if;

    -- Compute Y
    SCREEN.INFORM(SCREEN.I_WAIT);
    begin
      declare
        -- Resolution of problem
        SOLUTION : constant RESOL.VECTOR
                 := RESOL.R_RESOLUTION (POINTS.P_THE_POINTS);
      begin
        LP.Y := F_X(LP.X, SOLUTION);
      end;
    exception
      when others =>
        SCREEN.ERROR (SCREEN.E_RESOLUTION_PROBLEM);
        OK := FALSE;
    end;
    SCREEN.INFORM(SCREEN.I_CLEAR);
    POINT := LP;
  end COMPUTE_XY;

  package body CURVE_DATA is
    function CURVE_F_X (X : POINTS.P_T_COORDINATE)
                    return POINTS.P_T_COORDINATE is
    begin
      return F_X (X, THE_SOLUTION(1 .. THE_DEGREE + 1));
    end CURVE_F_X;
    procedure MY_DRAW is new CURVE.DRAW (CURVE_F_X);
    procedure CURVE_DRAW (BOUNDARIES : in CURVE.T_BOUNDARIES;
                          POINTS : in CURVE.T_THE_POINTS) is
    begin
      MY_DRAW (BOUNDARIES, POINTS);
    end CURVE_DRAW;
  end CURVE_DATA;

  task body CURVE_TASK is
    DRAW_IT : BOOLEAN;
    INIT_OK : BOOLEAN;
    use CURVE_DATA;
  begin
    loop
      select
        accept START (SOLUTION : in RESOL.VECTOR; OK : out BOOLEAN) do
          begin
            NB_POINTS := POINTS.P_NB;
            THE_POINTS(1 .. NB_POINTS) := POINTS.P_THE_POINTS;
            THE_DEGREE := RESOL.R_DEGREE;
            THE_SOLUTION (1 .. THE_DEGREE + 1) := SOLUTION;
            MENU21.GET_BOUNDS (THE_BOUNDS_SET, THE_BOUNDS);
            DRAW_IT := THE_BOUNDS_SET;
          exception
            when others =>
              DRAW_IT := FALSE;
          end;
          if DRAW_IT then
            DRAW_IT := CURVE.INIT;
          end if;
          OK := DRAW_IT;
        end START;
      or
        accept STOPPED do
          DRAW_IT := FALSE;
        end STOPPED;
      or
        terminate;
      end select;

      if DRAW_IT then
        begin
           CURVE_DRAW (THE_BOUNDS, THE_POINTS(1 .. NB_POINTS));
        exception
          when others =>
            -- Draw error
            null;
        end;
      end if;  
    end loop;
  end CURVE_TASK;

  function CURVED_STOPPED return BOOLEAN is
   OK : BOOLEAN;
  begin
    select
      CURVE_TASK.STOPPED;
      OK := TRUE;
    or
      delay 0.0;
      OK := FALSE;
    end select;
    return OK;
  end CURVED_STOPPED;

  procedure DRAW_CURVE is
    OK : BOOLEAN;
  begin
    SCREEN.PUT_TITLE(SCREEN.CURVE);
    SCREEN.INFORM(SCREEN.I_WAIT);
    CURVE_TASK.START (RESOL.R_RESOLUTION (POINTS.P_THE_POINTS), OK);
    SCREEN.INFORM(SCREEN.I_CLEAR);
    -- Accept started if start OK
    if not OK then
      SCREEN.ERROR (SCREEN.E_CURVE_PROBLEM);
    end if;
  exception
    when others =>
      SCREEN.ERROR (SCREEN.E_RESOLUTION_PROBLEM);
  end DRAW_CURVE;


  procedure MAIN_SCREEN (DATA_CHANGED : in BOOLEAN) is
    CURSOR_COL : CON_IO.COL_RANGE;
    REDISPLAY : BOOLEAN;
    PTG_RESULT : AFPX.RESULT_REC;
    RESTORE : RESTORE_LIST;

    use AFPX;

  begin
    AFPX.USE_DESCRIPTOR(3);
    -- Try to keep previous data
    if DATA_CHANGED then
      -- Or reset to max
      if POINTS.P_NB - 1 < RESOL.R_T_DEGREE'LAST then
        RESOL.R_SET_DEGREE(POINTS.P_NB - 1);
      else
        RESOL.R_SET_DEGREE(RESOL.R_T_DEGREE'LAST);
      end if;
      RESOL.R_POINTS_MODIFICATION;
    end if;
    SCREEN.INIT_FOR_MAIN2 (CURSOR_FIELD);
    SCREEN.PUT_FILE;

    -- Update Nb of points and save_status
    SCREEN.PUT_POINT_STATUS;

    CURSOR_COL := 0;
    REDISPLAY := FALSE;
    RESTORE := NONE;

    loop
      DO_RESTORE (RESTORE);


      AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
      REDISPLAY := FALSE;
      RESTORE := NONE;
      case PTG_RESULT.EVENT is
        when AFPX.KEYBOARD =>
          case PTG_RESULT.KEYBOARD_KEY is
            when AFPX.RETURN_KEY =>
              null;
            when AFPX.ESCAPE_KEY =>
              if not CURVED_STOPPED then
                SCREEN.ERROR (SCREEN.E_CURVE_ACTIVE);
                RESTORE := PARTIAL;
              else
                return;
              end if;
            when AFPX.BREAK_KEY =>
              null;
          end case;
        when AFPX.MOUSE_BUTTON =>
          case PTG_RESULT.FIELD_NO is
            when SCREEN.LIST_SCROLL_FLD_RANGE'FIRST ..
                 SCREEN.LIST_SCROLL_FLD_RANGE'LAST =>
              SCREEN.SCROLL(PTG_RESULT.FIELD_NO);
            when SCREEN.EXIT_BUTTON_FLD =>
              return;
            when 22 =>
              DIALOG.READ_DEGREE;
              RESTORE := PARTIAL;
            when 25 =>
              -- Display polynom
              SCREEN.PUT_TITLE(SCREEN.POLYNOM);
              SCREEN.INFORM(SCREEN.I_WAIT);
              -- Display
              begin
                DIALOG.PUT_POLYNOM (RESOL.R_RESOLUTION(POINTS.P_THE_POINTS));
              exception
                when others =>
                  SCREEN.ERROR (SCREEN.E_RESOLUTION_PROBLEM);
              end;
              RESTORE := LIST;
            when 27 =>
              -- Y=f(x)
              SCREEN.PUT_TITLE(SCREEN.Y_F_X);
              declare
                POINT : POINTS.P_T_ONE_POINT;
                OK : BOOLEAN;
              begin
                COMPUTE_XY (POINT, OK);
                if OK then 
                  DIALOG.PUT_YFX (POINT);
                end if;
              end;
              RESTORE := PARTIAL;
            when 29 =>
              -- Set boudaries
              MENU21.MAIN_SCREEN;
              RESTORE := FULL;
            when 31 =>
              -- Draw
              -- Set bounds if needed
              if not MENU21.BOUNDS_SET then
                MENU21.MAIN_SCREEN;
                RESTORE := FULL;
              else
                RESTORE := NONE;
              end if;
              if MENU21.BOUNDS_SET then
                -- Restore for wait/error
                DO_RESTORE(RESTORE);
                DRAW_CURVE;
              end if;
              RESTORE := FULL;
            when others =>
              null;
          end case; 
        when AFPX.REFRESH =>
          REDISPLAY := TRUE;
      end case;
    end loop;

  end MAIN_SCREEN;

end MENU2;
