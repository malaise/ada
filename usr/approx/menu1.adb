with CON_IO, AFPX, DIRECTORY, TEXT_HANDLER, SELECT_FILE, NORMAL;
with POINTS, SCREEN, SET_POINTS_LIST, DIALOG, POINT_STR, MENU2;
package body MENU1 is

  type RESTORE_LIST is (NONE, PARTIAL, FULL); 
  CURSOR_FIELD : AFPX.FIELD_RANGE;
  FILE_NAME_TXT : TEXT_HANDLER.TEXT (DIRECTORY.MAX_DIR_NAME_LEN);

  procedure PUT_POINT_STATUS is
    -- Width of nb_point
    HEIGHT : AFPX.HEIGHT_RANGE;
    WIDTH  : AFPX.WIDTH_RANGE;
  begin
    AFPX.GET_FIELD_SIZE(16, HEIGHT, WIDTH);
    AFPX.ENCODE_FIELD(16, (0, 0), NORMAL(POINTS.P_NB, WIDTH));
    if POINTS.P_SAVED then
      AFPX.CLEAR_FIELD(18);
    else
      AFPX.RESET_FIELD(18);
    end if;
  end PUT_POINT_STATUS;

  function MY_SELECT_FILE is new SELECT_FILE(PUT_POINT_STATUS);

  procedure ENCODE_FILE_IN_GET (FILE_NAME : in STRING) is
  begin
    AFPX.SET_FIELD_ACTIVATION (SCREEN.GET_FLD, TRUE);
    AFPX.SET_FIELD_PROTECTION (SCREEN.GET_FLD, TRUE);
    AFPX.ENCODE_FIELD (SCREEN.GET_FLD, (0, 0),
       SCREEN.PROCUSTE(FILE_NAME, SCREEN.GET_GET_WIDTH));
  end ENCODE_FILE_IN_GET;
  

  procedure ERROR (MSG : in SCREEN.S_ERROR_LIST) is
  begin
    SCREEN.ERROR(MSG);
    -- Restore screen
    AFPX.USE_DESCRIPTOR(1, FALSE);
    SCREEN.INIT_FOR_MAIN1 (CURSOR_FIELD);
  end ERROR;

  function EXIT_PROG return BOOLEAN is
  begin
    SCREEN.PUT_TITLE(SCREEN.EXIT_APPROX);
    if DIALOG.CONFIRM_LOST then
      -- The end
      CON_IO.DESTROY;
      return TRUE;
    end if;
    return FALSE;
  end EXIT_PROG;

  -- Read a data file
  function READ_FILE (FILE_NAME : in FILE.F_T_FILE_NAME) return BOOLEAN is
  begin
    SCREEN.PUT_TITLE (SCREEN.READ_POINTS);
    ENCODE_FILE_IN_GET (FILE_NAME);
    if FILE.F_EXISTS(FILE_NAME) then
      SCREEN.PUT_TITLE (SCREEN.READ_POINTS);
      TEXT_HANDLER.SET (FILE_NAME_TXT, FILE_NAME);
      begin
        -- Get data in points and list
        POINTS.P_STORE (FILE.F_READ(FILE_NAME));
        POINTS.P_SAVED;
        SET_POINTS_LIST;
        SCREEN.PUT_TITLE (SCREEN.DATA);
        return TRUE;
      exception
        when FILE.F_ACCESS_ERROR | FILE.F_IO_ERROR =>
          -- Error reading. Prev data is lost :-(
          POINTS.P_CLEAR;
          SET_POINTS_LIST;
          TEXT_HANDLER.EMPTY (FILE_NAME_TXT);
          ERROR(SCREEN.E_IO_ERROR);
      end;
    else
      -- Error but prev data is kept
      ERROR(SCREEN.E_FILE_NOT_FOUND);
    end if;
    return FALSE;
  end READ_FILE;

  procedure LOAD_SAVE (LOAD : in BOOLEAN; RESTORE : out RESTORE_LIST) is
    TMP_FILE_NAME : TEXT_HANDLER.TEXT (DIRECTORY.MAX_DIR_NAME_LEN);
  begin
    RESTORE := NONE;
    -- Title
    if LOAD then
      SCREEN.PUT_TITLE(SCREEN.READ_POINTS);
    else
      SCREEN.PUT_TITLE(SCREEN.WRITE_POINTS);
    end if;
    if not LOAD and then POINTS.P_EMPTY then
      -- Error when saving no data
      ERROR (SCREEN.E_NO_DATA);
      RESTORE := PARTIAL;
      return;
    elsif LOAD and then not POINTS.P_SAVED then
      -- Confirm loss when loading and unsaved points
      RESTORE := PARTIAL;
      if not DIALOG.CONFIRM_LOST then
        return;
      end if;
    end if;
              
    -- Select file
    RESTORE := FULL;
    TEXT_HANDLER.SET (TMP_FILE_NAME,
                      MY_SELECT_FILE(2, TEXT_HANDLER.VALUE(FILE_NAME_TXT), LOAD));
    if TEXT_HANDLER.EMPTY (TMP_FILE_NAME) then
      -- Cancelled
     return;
    end if;

    -- Restore (for errors)
    AFPX.USE_DESCRIPTOR(1);
    SET_POINTS_LIST;
    SCREEN.INIT_FOR_MAIN1 (CURSOR_FIELD);
    SCREEN.PUT_FILE (TEXT_HANDLER.VALUE(FILE_NAME_TXT));
    ENCODE_FILE_IN_GET (TEXT_HANDLER.VALUE(TMP_FILE_NAME));
    RESTORE := PARTIAL;
    -- load or save
    if LOAD then
      if READ_FILE (TEXT_HANDLER.VALUE(TMP_FILE_NAME)) then
        -- Done,
        TEXT_HANDLER.SET (FILE_NAME_TXT, TMP_FILE_NAME);
        -- Else kept or lost
      end if;
    else
      if FILE.F_EXISTS(TEXT_HANDLER.VALUE(TMP_FILE_NAME))
      and then not SCREEN.CONFIRM(SCREEN.C_FILE_EXISTS, TRUE) then
        return;
      end if;
      begin
        FILE.F_WRITE(TEXT_HANDLER.VALUE(TMP_FILE_NAME), POINTS.P_THE_POINTS);
        POINTS.P_SAVED;
        TEXT_HANDLER.SET (FILE_NAME_TXT, TMP_FILE_NAME);
      exception
        when others =>
          ERROR (SCREEN.E_IO_ERROR);
      end;
    end if;  
  end LOAD_SAVE;

  procedure READ_POINT (SET : in out BOOLEAN; POINT : in out POINTS.P_T_ONE_POINT) is
    LP : POINTS.P_T_ONE_POINT;
    OK : BOOLEAN;
  begin
    OK := SET;
    LP := POINT;
    DIALOG.READ_COORDINATE(SCREEN.I_X, OK, LP.X);
    if not OK then
      SET := FALSE;
      return;
    end if;
    OK := SET;
    DIALOG.READ_COORDINATE(SCREEN.I_Y, OK, LP.Y);
    if not OK then
      SET := FALSE;
      return;
    end if;
    POINT := LP;
    SET := TRUE;
  end READ_POINT;

  procedure MAIN_SCREEN (INIT_FILE_NAME : in FILE.F_T_FILE_NAME) is
    CURSOR_COL : CON_IO.COL_RANGE;
    REDISPLAY : BOOLEAN;
    PTG_RESULT : AFPX.RESULT_REC;
    RESTORE : RESTORE_LIST;
    A_POINT : POINTS.P_T_ONE_POINT;
    POINT_SET : BOOLEAN;
    POINT_INDEX : POSITIVE;
    DATA_CHANGED : BOOLEAN;

    use AFPX;

  begin
    AFPX.USE_DESCRIPTOR(1);
    SCREEN.INIT_FOR_MAIN1 (CURSOR_FIELD);
    TEXT_HANDLER.EMPTY (FILE_NAME_TXT);
    SCREEN.PUT_FILE ("");

    -- Get field width

    -- File?
    if INIT_FILE_NAME /= "" then
      if READ_FILE (INIT_FILE_NAME) then
        TEXT_HANDLER.SET (FILE_NAME_TXT, INIT_FILE_NAME);
      end if;
      RESTORE := PARTIAL;
    else
      RESTORE := NONE;
    end if;

    -- Update Nb of points and save_status
    SCREEN.PUT_POINT_STATUS;

    CURSOR_COL := 0;
    REDISPLAY := FALSE;
    DATA_CHANGED := TRUE;
    loop
      case RESTORE is
        when NONE =>
          null;
        when PARTIAL =>
          AFPX.USE_DESCRIPTOR(1, FALSE);
          SCREEN.INIT_FOR_MAIN1 (CURSOR_FIELD);
          SCREEN.PUT_FILE (TEXT_HANDLER.VALUE(FILE_NAME_TXT));
        when FULL =>
          AFPX.USE_DESCRIPTOR(1);
          SET_POINTS_LIST;
          SCREEN.INIT_FOR_MAIN1 (CURSOR_FIELD);
          SCREEN.PUT_FILE (TEXT_HANDLER.VALUE(FILE_NAME_TXT));
      end case;

      -- Delete/modify/approximation/sort
      if POINTS.P_NB = 0 then
        AFPX.SET_FIELD_ACTIVATION (26, FALSE);
        AFPX.SET_FIELD_ACTIVATION (27, FALSE);
        AFPX.SET_FIELD_ACTIVATION (29, FALSE);
        AFPX.SET_FIELD_ACTIVATION (31, FALSE);
      else
        AFPX.SET_FIELD_ACTIVATION (26, TRUE);
        AFPX.SET_FIELD_ACTIVATION (27, TRUE);
        AFPX.SET_FIELD_ACTIVATION (29, TRUE);
        AFPX.SET_FIELD_ACTIVATION (31, TRUE);
      end if;

      AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
      REDISPLAY := FALSE;
      RESTORE := NONE;
      case PTG_RESULT.EVENT is
        when AFPX.KEYBOARD =>
          case PTG_RESULT.KEYBOARD_KEY is
            when AFPX.RETURN_KEY =>
              null;
            when AFPX.ESCAPE_KEY =>
              if EXIT_PROG then
                -- The end
                return;
              else
                RESTORE := PARTIAL;
              end if;
              SCREEN.PUT_TITLE(SCREEN.EXIT_APPROX);
              if DIALOG.CONFIRM_LOST then
                CON_IO.DESTROY;
                return;
              else
                RESTORE := PARTIAL;
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
              if EXIT_PROG then
                -- The end
                return;
              else
                RESTORE := PARTIAL;
              end if;
            when 21 | 22 =>
              LOAD_SAVE(PTG_RESULT.FIELD_NO = 21, RESTORE);
              DATA_CHANGED := TRUE;
            when 23 =>
              -- New points
              SCREEN.PUT_TITLE(SCREEN.NEW_POINTS);
              if DIALOG.CONFIRM_LOST then
                POINTS.P_CLEAR;
                SET_POINTS_LIST;
                TEXT_HANDLER.EMPTY(FILE_NAME_TXT);
                -- Update file_name, nb of points and save_status
                TEXT_HANDLER.EMPTY (FILE_NAME_TXT);
              end if;
              DATA_CHANGED := TRUE;
              RESTORE := PARTIAL;
            when 25 =>
              -- Add point
              AFPX.SET_FIELD_PROTECTION (AFPX.LIST_FIELD_NO, TRUE);
              SCREEN.PUT_TITLE(SCREEN.ADD_1);
              loop
                POINT_SET := FALSE;
                READ_POINT(POINT_SET, A_POINT);
                exit when not POINT_SET;
                POINTS.P_UPD_POINT (POINTS.ADD, 1, A_POINT);
                SET_POINTS_LIST;
                DATA_CHANGED := TRUE;
              end loop;
              AFPX.SET_FIELD_PROTECTION (AFPX.LIST_FIELD_NO, FALSE);
              RESTORE := PARTIAL;
            when 26 | 27 | AFPX.LIST_FIELD_NO =>
              -- Delete / modify a point
              AFPX.SET_FIELD_PROTECTION (AFPX.LIST_FIELD_NO, TRUE);
              -- Get index then point
              POINT_INDEX := AFPX.LINE_LIST_MNG.GET_POSITION (AFPX.LINE_LIST);
              A_POINT := POINTS.P_ONE_POINT(POINT_INDEX);
              if PTG_RESULT.FIELD_NO = 26 then
                -- Delete a points
                SCREEN.PUT_TITLE(SCREEN.SUPPRESS_1);
                AFPX.ENCODE_FIELD (SCREEN.GET_FLD, (0, 0),
                    POINT_STR.ENCODE_REC(A_POINT).STR(1 .. SCREEN.GET_GET_WIDTH));
                AFPX.SET_FIELD_ACTIVATION(SCREEN.GET_FLD, TRUE);
                if SCREEN.CONFIRM(SCREEN.C_DELETE_POINT, TRUE) then
                  POINTS.P_UPD_POINT (POINTS.REMOVE, POINT_INDEX, A_POINT);
                  SET_POINTS_LIST;
                  DATA_CHANGED := TRUE;
                end if;
              else
                -- Modify
                SCREEN.PUT_TITLE(SCREEN.MODIFY_1);
                POINT_SET := TRUE;
                READ_POINT(POINT_SET, A_POINT);
                if POINT_SET then
                  POINTS.P_UPD_POINT (POINTS.MODIFY, POINT_INDEX, A_POINT);
                  SET_POINTS_LIST;
                  DATA_CHANGED := TRUE;
                end if;
              end if;
              AFPX.SET_FIELD_PROTECTION (AFPX.LIST_FIELD_NO, FALSE);
              RESTORE := PARTIAL;
            when 29 =>
              -- approximation
              SCREEN.STORE_FILE;
              MENU2.MAIN_SCREEN(DATA_CHANGED);
              RESTORE := FULL;
              DATA_CHANGED := FALSE;
            when 31 =>
              -- Sort
              POINTS.P_SORT;
              SET_POINTS_LIST;
              DATA_CHANGED := TRUE;
            when others =>
              null;
          end case; 
        when AFPX.REFRESH =>
          REDISPLAY := TRUE;
      end case;
    end loop;

  end MAIN_SCREEN;

end MENU1;
