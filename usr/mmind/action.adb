with CALENDAR;
with CON_IO, RND;
with COMMON, SCREEN, RESPONSE;
package body ACTION is


  LEVEL : COMMON.LAST_LEVEL_RANGE;

  PLAYING : BOOLEAN;

  CUR_SELECTION : SCREEN.SELECTION_REC;
  LAST_CLICK : SCREEN.SELECTION_REC;

  FIRST_FREE : COMMON.PROPAL_RANGE;

  procedure INIT is
  begin
    CON_IO.INIT;
    RND.RANDOMIZE;
    LEVEL := COMMON.GET_LEVEL;
  end INIT;

  procedure END_ACTION is
  begin
    null;
  end END_ACTION;

  procedure UPDATE_TRY (PROPAL : in COMMON.PROPAL_RANGE) is
    PROP_STATE : COMMON.PROPAL_STATE_REC := COMMON.GET_PROPAL_STATE (PROPAL);
    use COMMON;
  begin
    for I in COMMON.LEVEL_RANGE
     range COMMON.LEVEL_RANGE'FIRST .. LEVEL loop
      if PROP_STATE.PROPAL_COLOR(I) = 0 then
        COMMON.SET_TRY_STATE (PROPAL, COMMON.NOT_SET);
        SCREEN.PUT_TRY (PROPAL, SCREEN.CANNOT_TRY);
        return;
      end if;
    end loop;
    COMMON.SET_TRY_STATE (PROPAL, COMMON.CAN_TRY);
    SCREEN.PUT_TRY (PROPAL, SCREEN.CAN_TRY);
  end UPDATE_TRY;


  procedure TREAT_CLICK is separate;

  procedure TREAT_RELEASE (GO_ON, EXIT_GAME : out BOOLEAN) is separate;


  -- True if start again, False if exit
  function PLAY return BOOLEAN is
    CLICKED : BOOLEAN := FALSE;
    GO_ON, EXIT_GAME : BOOLEAN;
  begin
    -- Start new game - playing
    LEVEL := COMMON.GET_LEVEL;
    COMMON.RESET_STATE;
    SCREEN.INIT (LEVEL);
    SCREEN.SET_MOUSE_DEFAULT_COLOR;

    FIRST_FREE := COMMON.PROPAL_RANGE'FIRST;
    RESPONSE.NEW_CODE;

    PLAYING := TRUE;
    SCREEN.PUT_START_GIVEUP (START => FALSE, SELECTED => FALSE);
    SCREEN.PUT_HELP (SCREEN.RELEASED);
    SCREEN.PUT_CURRENT_LEVEL (LEVEL);


    MAIN:
    loop

      declare
        STR : STRING (1 .. 0);
        LAST : NATURAL;
        STAT : CON_IO.CURS_MVT;
        POS : POSITIVE;
        INS : BOOLEAN;
        MOUSE_STATUS : CON_IO.MOUSE_EVENT_REC;
        use SCREEN, CON_IO;
      begin
        WAIT_EVENT:
        loop
          CON_IO.GET (STR, LAST, STAT, POS, INS, ECHO => FALSE);
          if STAT = CON_IO.MOUSE_BUTTON then
            CON_IO.GET_MOUSE_EVENT (MOUSE_STATUS);
            if MOUSE_STATUS.BUTTON = CON_IO.LEFT then
              -- exit on new event
              exit WAIT_EVENT when CLICKED
                 xor (MOUSE_STATUS.STATUS = CON_IO.PRESSED);
            end if;
          elsif STAT = CON_IO.REFRESH then
            SCREEN.INIT (LEVEL);
            -- Put colors
            declare
              PROPAL : COMMON.PROPAL_STATE_REC(LEVEL);
              PLACED_OK, COLORS_OK : NATURAL;
              use COMMON;
            begin
              for I in COMMON.PROPAL_RANGE loop
                PROPAL := COMMON.GET_PROPAL_STATE(I);
                for J in 1 .. LEVEL loop
                  SCREEN.PUT_COLOR (I, J, PROPAL.PROPAL_COLOR(J));
                end loop;
                if PROPAL.TRY = COMMON.CAN_TRY then
                  SCREEN.PUT_TRY (I, SCREEN.CAN_TRY);
                elsif PROPAL.TRY = COMMON.ANSWERED then
                  COMMON.GET_ANSWER (I, PLACED_OK, COLORS_OK);
                  SCREEN.PUT_ANSWER (I, PLACED_OK, COLORS_OK);
                end if;
              end loop;
            end;
            if PLAYING then
              SCREEN.PUT_START_GIVEUP (START => FALSE, SELECTED => FALSE);
              SCREEN.PUT_HELP (SCREEN.RELEASED);
              SCREEN.PUT_CURRENT_LEVEL (LEVEL);
            else
              declare
                CODE : RESPONSE.COLOR_REC(LEVEL);
              begin
                CODE := RESPONSE.GET_CODE;
                for J in 1 .. LEVEL loop
                  SCREEN.PUT_SECRET_COLOR(J, CODE.COLOR(J));
                end loop;
              end;
              SCREEN.PUT_START_GIVEUP (START => TRUE, SELECTED => FALSE);
              SCREEN.PUT_HELP (SCREEN.START);
              SCREEN.PUT_CURRENT_LEVEL (COMMON.GET_STORED_LEVEL);
            end if;
          elsif STAT = CON_IO.BREAK then
            SCREEN.CLEAR;
            END_ACTION;
            return FALSE;
          end if;
        end loop WAIT_EVENT;
        CLICKED := not CLICKED;
        SCREEN.GET_SELECTED ( WHERE => (ROW => MOUSE_STATUS.ROW,
                                        COL => MOUSE_STATUS.COL),
                              WHAT => CUR_SELECTION);
      end;


      if CLICKED then
        TREAT_CLICK;
      else
        TREAT_RELEASE (GO_ON, EXIT_GAME);
        exit MAIN when not GO_ON;
      end if;

    end loop MAIN;

    if EXIT_GAME then
      SCREEN.CLEAR;
      END_ACTION;
    end if;

    return not EXIT_GAME;
  exception
    when others =>
      CON_IO.MOVE;
      return EXIT_GAME;
  end PLAY;

end ACTION;
