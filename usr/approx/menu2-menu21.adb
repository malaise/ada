with CON_IO, AFPX, CURVE;
with POINTS, SCREEN, DIALOG, POINT_STR;
separate(MENU2)
package body MENU21 is

  type RESTORE_LIST is (NONE, PARTIAL); 
  CURSOR_FIELD : AFPX.FIELD_RANGE;

  THE_BOUNDS_SET : BOOLEAN := FALSE;
  THE_BOUNDS : CURVE.T_BOUNDARIES;

  procedure RESET_BOUNDS is
  begin
    THE_BOUNDS_SET := FALSE;
  end RESET_BOUNDS;

  function BOUNDS_SET return BOOLEAN is
  begin
    return THE_BOUNDS_SET;
  end BOUNDS_SET;

  procedure GET_BOUNDS (SET : out BOOLEAN; BOUNDS : out CURVE.T_BOUNDARIES) is
  begin
    SET := THE_BOUNDS_SET;
    BOUNDS := THE_BOUNDS;
  end GET_BOUNDS;


  procedure ERROR (MSG : in SCREEN.S_ERROR_LIST) is
  begin
    SCREEN.ERROR(MSG);
    -- Restore screen
    AFPX.USE_DESCRIPTOR(4, FALSE);
    SCREEN.INIT_FOR_MAIN21 (CURSOR_FIELD);
  end ERROR;

  procedure PUT_BOUNDS is
    use CURVE;
  begin
    -- Allow clear if bounds set
    AFPX.SET_FIELD_ACTIVATION (SCREEN.EXIT_BUTTON_FLD, THE_BOUNDS_SET);
    AFPX.CLEAR_FIELD (33);
    AFPX.CLEAR_FIELD (34);
    if not THE_BOUNDS_SET then
      AFPX.ENCODE_FIELD (33, (0,0), "Not set");
      return;
    end if;
    case THE_BOUNDS.SCALE is
      when CURVE.CURVE_SCREEN =>
        AFPX.ENCODE_FIELD (33, (0, 0), "Computed & fit screen");
      when CURVE.CURVE_NORMED =>
        AFPX.ENCODE_FIELD (33, (0, 0), "Computed & normed");
      when CURVE.FREE_SCREEN =>
        AFPX.ENCODE_FIELD (33, (0, 0), "Defined & fit screen");
      when CURVE.FREE_NORMED =>
        AFPX.ENCODE_FIELD (33, (0, 0), "Defined & normed");
    end case;
    AFPX.ENCODE_FIELD (34, (0, 0), "Xmin: "
         & POINT_STR.COORDINATE_IMAGE(THE_BOUNDS.X_MIN));
    AFPX.ENCODE_FIELD (34, (1, 0), "Xmax: "
         & POINT_STR.COORDINATE_IMAGE(THE_BOUNDS.X_MAX));
    if      THE_BOUNDS.SCALE = CURVE.FREE_SCREEN
    or else THE_BOUNDS.SCALE = CURVE.FREE_NORMED then
      AFPX.ENCODE_FIELD (34, (2, 0), "Ymin: "
           & POINT_STR.COORDINATE_IMAGE(THE_BOUNDS.Y_MIN));
      AFPX.ENCODE_FIELD (34, (3, 0), "Ymax: "
           & POINT_STR.COORDINATE_IMAGE(THE_BOUNDS.Y_MAX));
    end if;
  end PUT_BOUNDS;

  -- COMPUTE_X should be set only when scale is curve*
  procedure SET_BOUNDS (SCALE : in CURVE.T_SCALE;
                        COMPUTE_X : in BOOLEAN := FALSE) is
    LOC_BOUNDS : CURVE.T_BOUNDARIES(SCALE);
    SET : BOOLEAN;
    use CURVE;
  begin
    if COMPUTE_X and then
      (        SCALE = CURVE.FREE_SCREEN
       or else SCALE = CURVE.FREE_NORMED) then
      raise PROGRAM_ERROR;
    end if;
    SCREEN.PUT_TITLE (SCREEN.BOUNDARIES);
    if COMPUTE_X then
      CURVE.X_BOUNDARIES(POINTS.P_THE_POINTS,
                         LOC_BOUNDS.X_MIN, LOC_BOUNDS.X_MAX);
    else
      SET := FALSE;
      DIALOG.READ_COORDINATE(SCREEN.I_XMIN, SET, LOC_BOUNDS.X_MIN);
      if not SET then
        return;
      end if;
      SET := FALSE;
      DIALOG.READ_COORDINATE(SCREEN.I_XMAX, SET, LOC_BOUNDS.X_MAX);
      if not SET then
        return;
      end if;
      if      SCALE = CURVE.FREE_SCREEN
      or else SCALE = CURVE.FREE_NORMED then
      SET := FALSE;
        DIALOG.READ_COORDINATE(SCREEN.I_YMIN, SET, LOC_BOUNDS.Y_MIN);
        if not SET then
          return;
        end if;
        SET := FALSE;
        DIALOG.READ_COORDINATE(SCREEN.I_YMAX, SET, LOC_BOUNDS.Y_MAX);
        if not SET then
          return;
        end if;
      end if;
    end if;
    THE_BOUNDS := LOC_BOUNDS;
    THE_BOUNDS_SET := TRUE;
  end SET_BOUNDS;
    

  procedure MAIN_SCREEN is
    CURSOR_COL : CON_IO.COL_RANGE;
    REDISPLAY : BOOLEAN;
    PTG_RESULT : AFPX.RESULT_REC;
    RESTORE : RESTORE_LIST;
    ACTIVATE_NO_CURVE : BOOLEAN;

    use AFPX;

  begin
    AFPX.USE_DESCRIPTOR(4);

    CURSOR_COL := 0;
    REDISPLAY := FALSE;
    RESTORE := PARTIAL;

    loop
      -- Activate or not according to curve activity
      ACTIVATE_NO_CURVE := MENU2.CURVED_STOPPED;
      case RESTORE is
        when NONE =>
          null;
        when PARTIAL =>
          AFPX.USE_DESCRIPTOR(4, FALSE);
          SCREEN.INIT_FOR_MAIN21 (CURSOR_FIELD);
          SCREEN.PUT_FILE;
          PUT_BOUNDS;
      end case;
      -- Clear
      SCREEN.PUT_TITLE (SCREEN.BOUNDARIES, not ACTIVATE_NO_CURVE);
      AFPX.SET_FIELD_ACTIVATION (SCREEN.EXIT_BUTTON_FLD, ACTIVATE_NO_CURVE);
      AFPX.SET_FIELD_ACTIVATION (20, ACTIVATE_NO_CURVE);
      AFPX.SET_FIELD_ACTIVATION (21, ACTIVATE_NO_CURVE);
      AFPX.SET_FIELD_ACTIVATION (22, ACTIVATE_NO_CURVE);
      AFPX.SET_FIELD_ACTIVATION (24, ACTIVATE_NO_CURVE);
      AFPX.SET_FIELD_ACTIVATION (25, ACTIVATE_NO_CURVE);
      AFPX.SET_FIELD_ACTIVATION (26, ACTIVATE_NO_CURVE);
      AFPX.SET_FIELD_ACTIVATION (28, ACTIVATE_NO_CURVE);
      AFPX.SET_FIELD_ACTIVATION (29, ACTIVATE_NO_CURVE);
      AFPX.SET_FIELD_ACTIVATION (30, ACTIVATE_NO_CURVE);

      AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
      REDISPLAY := FALSE;
      RESTORE := NONE;
      case PTG_RESULT.EVENT is
        when AFPX.KEYBOARD =>
          case PTG_RESULT.KEYBOARD_KEY is
            when AFPX.RETURN_KEY =>
              return;
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
            when SCREEN.OK_BUTTON_FLD =>
              -- Back
              return;
            when SCREEN.EXIT_BUTTON_FLD =>
              -- Clear
              THE_BOUNDS_SET := FALSE;
              PUT_BOUNDS;
            when 21 =>
              -- Computed fit screen
              SET_BOUNDS (CURVE.CURVE_SCREEN, COMPUTE_X => TRUE);
              RESTORE := PARTIAL;
            when 22 =>
              -- Computed normed
              SET_BOUNDS (CURVE.CURVE_NORMED, COMPUTE_X => TRUE);
              RESTORE := PARTIAL;
            when 25 =>
              -- X set fit screen : Get Xmin & Xmax
              SET_BOUNDS (CURVE.CURVE_SCREEN);
              RESTORE := PARTIAL;
            when 26 =>
              -- X set normed : Get Xmin & Xmax
              SET_BOUNDS (CURVE.CURVE_NORMED);
              RESTORE := PARTIAL;
            when 29 =>
              -- Defined fit screen : Get Xmin, Xmax Ymin & Ymax
              SET_BOUNDS (CURVE.FREE_SCREEN);
              RESTORE := PARTIAL;
            when 30 =>
              -- Defined normed : Get Xmin, Xmax Ymin & Ymax
              SET_BOUNDS (CURVE.FREE_NORMED);
              RESTORE := PARTIAL;
            when others =>
              null;
          end case; 
        when AFPX.REFRESH =>
          REDISPLAY := TRUE;
      end case;
    end loop;

  end MAIN_SCREEN;

end MENU21;

