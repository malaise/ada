with DOS;
separate (ACTION)
procedure TREAT_CLICK is
begin

  if not PLAYING then

    case CUR_SELECTION.SELECTION_KIND is

      when SCREEN.MENU =>
        SCREEN.PUT_HELP (SCREEN.CLICK_OTHER);
        SCREEN.PUT_START_GIVEUP (START => TRUE, SELECTED => TRUE);
        LAST_CLICK := CUR_SELECTION;

      when SCREEN.LEVEL =>
        SCREEN.PUT_HELP (SCREEN.CLICK_OTHER);
        SCREEN.PUT_LEVEL (CUR_SELECTION.LEVEL_NO,
                          SELECTED => TRUE);
        LAST_CLICK := CUR_SELECTION;

      when SCREEN.EXIT_GAME =>
        SCREEN.PUT_HELP (SCREEN.CLICK_OTHER);
        SCREEN.PUT_EXIT (SELECTED => TRUE);
        LAST_CLICK := CUR_SELECTION;

      when others =>
        LAST_CLICK := (SELECTION_KIND => SCREEN.NOTHING,
                       SELECTION => SCREEN.NOTHING);
        SCREEN.PUT_HELP (SCREEN.DISCARDED);
        DOS.SOUND;
    end case;

  else

    case CUR_SELECTION.SELECTION_KIND is

      when SCREEN.MENU =>
        SCREEN.PUT_HELP (SCREEN.CLICK_OTHER);
        SCREEN.PUT_START_GIVEUP (START => FALSE, SELECTED => TRUE);
        LAST_CLICK := CUR_SELECTION;

      when SCREEN.EXIT_GAME =>
        SCREEN.PUT_HELP (SCREEN.CLICK_OTHER);
        SCREEN.PUT_EXIT (SELECTED => TRUE);
        LAST_CLICK := CUR_SELECTION;

      when SCREEN.TRY =>
        declare
          TRY_STATE : COMMON.TRY_LIST :=
           COMMON.GET_PROPAL_STATE (CUR_SELECTION.TRY_NO).TRY;
          use COMMON;
        begin
          -- Check that this propal is completed and not already answered
          if TRY_STATE = COMMON.CAN_TRY then
            SCREEN.PUT_HELP (SCREEN.CLICK_OTHER);
            SCREEN.PUT_TRY (PROPAL => CUR_SELECTION.TRY_NO,
                            TRY_STATE => SCREEN.SELECTED);
            LAST_CLICK := CUR_SELECTION;
          else
            SCREEN.PUT_HELP (SCREEN.DISCARDED);
            LAST_CLICK := (SELECTION_KIND => SCREEN.NOTHING,
                           SELECTION => SCREEN.TRY);
            DOS.SOUND;
          end if;
        end;

      when SCREEN.COLOR =>
        SCREEN.PUT_HELP (SCREEN.CLICK_COLOR);
        SCREEN.PUT_SELECTED_COLOR (COLOR => CUR_SELECTION.COLOR_NO,
                                   SELECTED => TRUE);
        SCREEN.SET_MOUSE_COLOR (COLOR => CUR_SELECTION.COLOR_NO);
        LAST_CLICK := CUR_SELECTION;

      when SCREEN.PROPAL =>
        declare
          PROPAL_STATE : COMMON.PROPAL_STATE_REC :=
           COMMON.GET_PROPAL_STATE (CUR_SELECTION.PROPAL_NO);
          use COMMON;
        begin
          -- Check that this propal is completed and not already answered
          if PROPAL_STATE.TRY /= COMMON.ANSWERED and then
             PROPAL_STATE.PROPAL_COLOR(CUR_SELECTION.COLUMN_NO)
              /= COMMON.COLOR_RANGE'FIRST then
            SCREEN.PUT_HELP (SCREEN.CLICK_PROPAL);
            -- Attempt to move a color in propal. Clear square
            SCREEN.PUT_DEFAULT_POS (CUR_SELECTION.PROPAL_NO,
                                    CUR_SELECTION.COLUMN_NO,
                                    SHOW => TRUE);
            SCREEN.PUT_COLOR(PROPAL => CUR_SELECTION.PROPAL_NO,
                             LEVEL  => CUR_SELECTION.COLUMN_NO,
                             COLOR  => COMMON.COLOR_RANGE'FIRST);
            SCREEN.SET_MOUSE_COLOR (
             COLOR => PROPAL_STATE.PROPAL_COLOR(CUR_SELECTION.COLUMN_NO));
            LAST_CLICK := CUR_SELECTION;
          else
            SCREEN.PUT_HELP (SCREEN.DISCARDED);
            LAST_CLICK := (SELECTION_KIND => SCREEN.NOTHING,
                           SELECTION => SCREEN.PROPAL);
            DOS.SOUND;
          end if;
        end;

      when others =>
        SCREEN.PUT_HELP (SCREEN.DISCARDED);
        LAST_CLICK := (SELECTION_KIND => SCREEN.NOTHING,
                       SELECTION => SCREEN.NOTHING);
        DOS.SOUND;

    end case;

  end if;


end TREAT_CLICK;
