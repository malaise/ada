separate (sok_manager)

function SOK_MENU (ALLOW_WRITE : BOOLEAN) return MENU_RESULT_REC is

  CUR_ACTION : SOK_DISPLAY.MENU_ACTION_LIST := SOK_DISPLAY.RESET;
  KEY : SOK_INPUT.KEY_LIST;

  subtype MENU_ERROR_LIST is SOK_DISPLAY.ERROR_LIST
   range SOK_DISPLAY.NO_FRAME .. SOK_DISPLAY.FORMAT;
  procedure PUT_MENU_ERROR (ERROR : in MENU_ERROR_LIST) is
  begin
    SOK_DISPLAY.PUT_ERROR (ERROR);
    declare
      KEY : SOK_INPUT.KEY_LIST;
    begin
      KEY := SOK_INPUT.GET_KEY;
    exception
      when SOK_INPUT.BREAK_REQUESTED =>
        SOK_DISPLAY.CLEAR_ERROR;
        raise;
      when others =>
        SOK_DISPLAY.CLEAR_ERROR;
    end;
  end PUT_MENU_ERROR;

  procedure BACK is
  begin
    SOK_DISPLAY.CLEAR_MENU;
    SOK_DISPLAY.PUT_HELP (SOK_DISPLAY.FRAME);
  end BACK;

  procedure REDISPLAY is
  begin
    SOK_DISPLAY.PUT_FRAME (STATE.FRAME);
    if not ALLOW_WRITE then
      SET_BLINK (STATE.FRAME, TRUE);
    end if;
    SOK_DISPLAY.PUT_HELP (CUR_ACTION);
    SOK_DISPLAY.PUT_MENU (CUR_ACTION, ALLOW_WRITE);
    SOK_TIME.DISP_TIME;
  end REDISPLAY;

  use SOK_DISPLAY;
begin
  SOK_DISPLAY.CLEAR_MENU;
  SOK_DISPLAY.PUT_MENU (CUR_ACTION, ALLOW_WRITE);
  loop
    SOK_DISPLAY.PUT_HELP (CUR_ACTION);
    KEY := SOK_INPUT.GET_KEY;
    case KEY is
      when SOK_INPUT.RIGHT =>
        if CUR_ACTION /= SOK_DISPLAY.MENU_ACTION_LIST'LAST then
          CUR_ACTION := SOK_DISPLAY.MENU_ACTION_LIST'SUCC (CUR_ACTION);
        else
          CUR_ACTION := SOK_DISPLAY.MENU_ACTION_LIST'FIRST;
        end if;
        if CUR_ACTION = SOK_DISPLAY.WRITE and not ALLOW_WRITE then
          CUR_ACTION := SOK_DISPLAY.READ;
        end if;
        SOK_DISPLAY.UPDATE_MENU (CUR_ACTION);
      when SOK_INPUT.LEFT =>
        if CUR_ACTION /= SOK_DISPLAY.MENU_ACTION_LIST'FIRST then
          CUR_ACTION := SOK_DISPLAY.MENU_ACTION_LIST'PRED (CUR_ACTION);
        else
          CUR_ACTION := SOK_DISPLAY.MENU_ACTION_LIST'LAST;
        end if;
        if CUR_ACTION = SOK_DISPLAY.WRITE and not ALLOW_WRITE then
          CUR_ACTION := SOK_DISPLAY.BREAK;
        end if;
        SOK_DISPLAY.UPDATE_MENU (CUR_ACTION);
      when SOK_INPUT.UP | SOK_INPUT.DOWN | SOK_INPUT.UNDO =>
        null;
      when SOK_INPUT.NEXT =>
        -- validation
        SOK_DISPLAY.CLEAR_MENU;
        case CUR_ACTION is
          when SOK_DISPLAY.READ =>
            SOK_DISPLAY.PUT_MENU (CUR_ACTION, ALLOW_WRITE);
            begin
              SOK_FILE.RESTORE (STATE);
              BACK;
              return (RESULT => RESTART_FRAME, UPDATE_STATE => UPDATE_TIME);
            exception
              when SOK_FILE.FRAME_FILE_NOT_FOUND =>
                PUT_MENU_ERROR (NO_FRAME);
                BACK;
                return (RESULT => GO_ON);
              when SOK_FILE.ERROR_READING_FRAME =>
                PUT_MENU_ERROR (RESTORE);
                BACK;
                return (RESULT => GO_ON);
            end;
          when SOK_DISPLAY.WRITE =>
            SOK_DISPLAY.PUT_MENU (CUR_ACTION, ALLOW_WRITE);
            begin
              SOK_FILE.SAVE (STATE);
              BACK;
              return (RESULT => GO_ON);
            exception
              when SOK_FILE.ERROR_WRITING_FRAME =>
                PUT_MENU_ERROR (SAVE);
                BACK;
                return (RESULT => GO_ON);
            end;
          when SOK_DISPLAY.RESET =>
            BACK;
            return (RESULT => RESTART_FRAME, UPDATE_STATE => RESET_ALL);
          when SOK_DISPLAY.GET_NEW =>
            -- Loop while redisplay
            loop
              SOK_DISPLAY.PUT_MENU (CUR_ACTION, ALLOW_WRITE);
              declare
                GOT_NO : SOK_TYPES.FRAME_RANGE;
                RESULT : SOK_DISPLAY.GET_RESULT_LIST;
              begin
                SOK_DISPLAY.GET_NO_FRAME (GOT_NO, RESULT);
                case RESULT is
                  when SOK_DISPLAY.SET =>
                    BACK;
                    STATE.NO_FRAME := GOT_NO;
                    return (RESULT => RESTART_FRAME, UPDATE_STATE => RESET_ALL);
                  when SOK_DISPLAY.REFRESH =>
                    REDISPLAY;
                  when SOK_DISPLAY.ESC =>
                    exit;
                end case;
              exception
                when SOK_DISPLAY.FORMAT_ERROR =>
                  PUT_MENU_ERROR (FORMAT);
                  BACK;
                  SOK_DISPLAY.CLEAR_MENU;
                  SOK_DISPLAY.PUT_MENU (CUR_ACTION, ALLOW_WRITE);
              end;
            end loop;
          when SOK_DISPLAY.BREAK =>
            BACK;
            raise SOK_INPUT.BREAK_REQUESTED;
        end case;
      when SOK_INPUT.ESC =>
        if ALLOW_WRITE then
          SOK_DISPLAY.PUT_HELP (SOK_DISPLAY.FRAME);
        else
          SOK_DISPLAY.PUT_HELP (SOK_DISPLAY.DONE);
        end if;
        SOK_DISPLAY.CLEAR_MENU;
        return (RESULT => GO_ON);
      when SOK_INPUT.REFRESH =>
        REDISPLAY;
    end case;
  end loop;

end SOK_MENU;
