with CON_IO, AFPX, CURVE;
with POINTS, SCREEN, SET_POINTS_LIST, DIALOG, RESOL, MENU21;
package body MENU2 is

  type RESTORE_LIST is (NONE, PARTIAL, LIST, FULL); 
  CURSOR_FIELD : AFPX.FIELD_RANGE;

  procedure ERROR (MSG : in SCREEN.S_ERROR_LIST) is
  begin
    SCREEN.ERROR(MSG);
    -- Restore screen
    AFPX.USE_DESCRIPTOR(3, FALSE);
    SCREEN.INIT_FOR_MAIN2 (CURSOR_FIELD);
  end ERROR;

  procedure DO_RESTORE (RESTORE : in RESTORE_LIST) is
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
  end DO_RESTORE;

  function F_X (X : POINTS.P_T_COORDINATE;
                    POLYNOM : RESOL.VECTOR) return POINTS.P_T_COORDINATE is
     Y : POINTS.P_T_COORDINATE := 0.0;
     BUBBLE : POINTS.P_T_COORDINATE := 1.0;
  begin
     -- Y = F(X) from vector
     for I in POLYNOM'RANGE loop
       Y := Y + POLYNOM(I)*BUBBLE;
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
    if not SET then
      OK := FALSE;
      return;
    end if;

    -- Compute Y
    SCREEN.INFORM(SCREEN.I_WAIT);
    declare
      -- Resolution of problem
      SOLUTION : constant RESOL.VECTOR
               := RESOL.R_RESOLUTION (POINTS.P_THE_POINTS);
    begin
      LP.Y := F_X(LP.X, SOLUTION);
    exception
      when others =>
        SCREEN.ERROR (SCREEN.E_RESOLUTION_PROBLEM);
        OK := FALSE;
    end;
    SCREEN.INFORM(SCREEN.I_CLEAR);
    POINT := LP;
  end COMPUTE_XY;

  procedure DRAW_CURVE is
    SET : BOOLEAN;
    THE_BOUNDS : CURVE.T_BOUNDARIES;
    THE_POINTS : constant POINTS.P_T_THE_POINTS := POINTS.P_THE_POINTS;
  begin
    SCREEN.PUT_TITLE(SCREEN.CURVE);
    SCREEN.INFORM (SCREEN.I_WAIT);
    MENU21.GET_BOUNDS (SET, THE_BOUNDS);
    if not SET then
      raise PROGRAM_ERROR;
    end if;
    declare
      -- Resolution of problem
      SOLUTION : constant RESOL.VECTOR
               := RESOL.R_RESOLUTION (POINTS.P_THE_POINTS);
      function MY_F_X (X : POINTS.P_T_COORDINATE) return POINTS.P_T_COORDINATE is
      begin
        return F_X (X, SOLUTION);
      end MY_F_X;
      procedure MY_DRAW is new CURVE.DRAW (MY_F_X);
    begin
      CON_IO.RESET_TERM;
      MY_DRAW (THE_BOUNDS, THE_POINTS);
    exception
      when others =>
        DO_RESTORE(FULL);
        SCREEN.ERROR (SCREEN.E_CURVE_PROBLEM);
    end;
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
      RESOL.R_SET_DEGREE(POINTS.P_NB - 1);
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
              return;
            when AFPX.BREAK_KEY =>
              null;
          end case;
        when AFPX.MOUSE_BUTTON =>
          case PTG_RESULT.FIELD_NO is
            when SCREEN.LIST_SCROLL_FLD_RANGE'FIRST ..
                 SCREEN.LIST_SCROLL_FLD_RANGE'LAST =>
              SCREEN.SCROLL(PTG_RESULT.FIELD_NO);
            when SCREEN.EXIT_BUTTON_FLD =>
              -- Back
              return;
            when 22 =>
              -- Get and set new degree
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
