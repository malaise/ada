with MY_IO, CON_IO;
with DOS;
separate (ACTION)
procedure TREAT_RELEASE (GO_ON, EXIT_GAME : out BOOLEAN) is

  procedure PUT_SECRET is
    SECRET : RESPONSE.COLOR_REC := RESPONSE.GET_CODE;
  begin
    for I in COMMON.LEVEL_RANGE
     range COMMON.LEVEL_RANGE'FIRST .. LEVEL loop
      SCREEN.PUT_SECRET_COLOR(I, SECRET.COLOR(I));
    end loop;
  end PUT_SECRET;

  use COMMON, SCREEN;

begin
  -- cancel click
  case LAST_CLICK.SELECTION_KIND is
    when SCREEN.MENU =>
      if PLAYING then
        SCREEN.PUT_START_GIVEUP (START => FALSE, SELECTED => FALSE);
        -- DEBUG
        -- if CUR_SELECTION.SELECTION_KIND = SCREEN.EXIT_GAME then
        -- PUT_SECRET;
        -- end if;
      else
        SCREEN.PUT_START_GIVEUP (START => TRUE, SELECTED => FALSE);
      end if;
    when SCREEN.LEVEL =>
      SCREEN.PUT_LEVEL (LAST_CLICK.LEVEL_NO, SELECTED => FALSE);
    when SCREEN.EXIT_GAME =>
      SCREEN.PUT_EXIT (SELECTED => FALSE);
    when SCREEN.TRY =>
      SCREEN.PUT_TRY (PROPAL => LAST_CLICK.TRY_NO,
                      TRY_STATE => SCREEN.CAN_TRY);
    when SCREEN.COLOR =>
      SCREEN.PUT_SELECTED_COLOR (COLOR => LAST_CLICK.COLOR_NO,
                                 SELECTED => FALSE);
      SCREEN.SET_MOUSE_DEFAULT_COLOR;
    when SCREEN.PROPAL =>
      SCREEN.PUT_DEFAULT_POS (LAST_CLICK.PROPAL_NO,
                              LAST_CLICK.COLUMN_NO,
                              SHOW => FALSE);
      SCREEN.SET_MOUSE_DEFAULT_COLOR;
    when others=>
      null;
  end case;

  -- treat release
  if LAST_CLICK.SELECTION_KIND = CUR_SELECTION.SELECTION_KIND then
    case CUR_SELECTION.SELECTION_KIND is
      when SCREEN.EXIT_GAME =>
        GO_ON := FALSE;
        EXIT_GAME := TRUE;

      when SCREEN.LEVEL =>
        COMMON.SET_LEVEL (CUR_SELECTION.LEVEL_NO);
        SCREEN.PUT_CURRENT_LEVEL (CUR_SELECTION.LEVEL_NO);
        GO_ON := TRUE;
        EXIT_GAME := FALSE;

      when SCREEN.MENU =>
        if PLAYING then
          -- give up
          PUT_SECRET;
          SCREEN.PUT_START_GIVEUP (START => TRUE, SELECTED => FALSE);
          GO_ON := TRUE;
          EXIT_GAME := FALSE;
        else
          -- restart
          SCREEN.PUT_START_GIVEUP (START => FALSE, SELECTED => FALSE);
          GO_ON := FALSE;
          EXIT_GAME := FALSE;
        end if;
        PLAYING := not PLAYING;

      when TRY =>
        if LAST_CLICK.TRY_NO = CUR_SELECTION.TRY_NO then
          -- valid try (already tested on click)
          declare
            FREE_STATE : COMMON.PROPAL_STATE_REC;
            COLOR : RESPONSE.COLOR_REC (LEVEL);
            RESP : RESPONSE.RESPONSE_REC;
          begin
            if CUR_SELECTION.TRY_NO /= FIRST_FREE then
              declare
                PROP_STATE : COMMON.PROPAL_STATE_REC;
              begin
                FREE_STATE := COMMON.GET_PROPAL_STATE (CUR_SELECTION.TRY_NO);
                PROP_STATE := COMMON.GET_PROPAL_STATE (FIRST_FREE);
                -- Switch propals
                COMMON.SET_PROPAL_STATE (
                 PROPAL => CUR_SELECTION.TRY_NO,
                 STATE  => PROP_STATE);
                COMMON.SET_PROPAL_STATE (
                 PROPAL => FIRST_FREE,
                 STATE  => FREE_STATE);
                -- update screen of propals and try
                for I in COMMON.LEVEL_RANGE
                 range COMMON.LEVEL_RANGE'FIRST .. LEVEL loop
                  SCREEN.PUT_COLOR (
                   PROPAL => CUR_SELECTION.TRY_NO,
                   LEVEL => I,
                   COLOR => PROP_STATE.PROPAL_COLOR(I) );
                  SCREEN.PUT_COLOR (
                   PROPAL => FIRST_FREE,
                   LEVEL => I,
                   COLOR => FREE_STATE.PROPAL_COLOR(I) );
                end loop;
                -- answered impossible because of NEXT_FREE
                if PROP_STATE.TRY = COMMON.CAN_TRY then
                  SCREEN.PUT_TRY (CUR_SELECTION.TRY_NO, CAN_TRY);
                else
                  SCREEN.PUT_TRY (CUR_SELECTION.TRY_NO, SCREEN.CANNOT_TRY);
                end if;
              end;
            else
              FREE_STATE := COMMON.GET_PROPAL_STATE (FIRST_FREE);
            end if;

            -- build color rec and update screen
            for I in COMMON.LEVEL_RANGE
             range COMMON.LEVEL_RANGE'FIRST .. LEVEL loop
              COLOR.COLOR(I) := FREE_STATE.PROPAL_COLOR(I);
            end loop;

            -- respond
            RESP := RESPONSE.RESPOND (COLOR);
            SCREEN.PUT_ANSWER (
             PROPAL => FIRST_FREE,
             PLACED_OK => RESP.PLACED_OK,
             COLORS_OK => RESP.COLORS_OK);

            -- answered
            COMMON.SET_TRY_STATE (FIRST_FREE, COMMON.ANSWERED);
            COMMON.SET_ANSWER (FIRST_FREE, RESP.PLACED_OK, RESP.COLORS_OK);

            -- Check end of game
            if FIRST_FREE = COMMON.MAX_NUMBER_PROPAL or else
               RESP.PLACED_OK = NATURAL (LEVEL) then
              PLAYING := FALSE;
              SCREEN.PUT_START_GIVEUP (START => TRUE, SELECTED => FALSE);
              PUT_SECRET;
            else
              FIRST_FREE := COMMON.PROPAL_RANGE'SUCC(FIRST_FREE);
            end if;

          end;
        else
          DOS.SOUND;
        end if;
        GO_ON := TRUE;
        EXIT_GAME := FALSE;
      when COLOR =>
        DOS.SOUND;
        GO_ON := TRUE;
        EXIT_GAME := FALSE;
      when PROPAL =>
        -- Move color in propal
        declare
          PROP_STATE : COMMON.PROPAL_STATE_REC;
          PREV_STATE : COMMON.PROPAL_STATE_REC;
          MOVED_COLOR : COMMON.EFF_COLOR_RANGE;
        begin
          PROP_STATE := COMMON.GET_PROPAL_STATE (CUR_SELECTION.PROPAL_NO);
          PREV_STATE := COMMON.GET_PROPAL_STATE (LAST_CLICK.PROPAL_NO);
          MOVED_COLOR := PREV_STATE.PROPAL_COLOR(LAST_CLICK.COLUMN_NO);
          if PROP_STATE.PROPAL_COLOR(CUR_SELECTION.COLUMN_NO) =
           COMMON.COLOR_RANGE'FIRST then
            -- Dest is free : move color
            COMMON.SET_COLOR (LAST_CLICK.PROPAL_NO,
                              LAST_CLICK.COLUMN_NO,
                              COMMON.COLOR_RANGE'FIRST);
            UPDATE_TRY (LAST_CLICK.PROPAL_NO);

            COMMON.SET_COLOR (CUR_SELECTION.PROPAL_NO,
                              CUR_SELECTION.COLUMN_NO,
                              MOVED_COLOR);
            SCREEN.PUT_COLOR (CUR_SELECTION.PROPAL_NO,
                              CUR_SELECTION.COLUMN_NO,
                              MOVED_COLOR);
            UPDATE_TRY (CUR_SELECTION.PROPAL_NO);
          else
            -- Dest is used : restore color of clicked square
            SCREEN.PUT_COLOR (LAST_CLICK.PROPAL_NO,
                              LAST_CLICK.COLUMN_NO,
                              MOVED_COLOR);
            if LAST_CLICK.PROPAL_NO /= CUR_SELECTION.PROPAL_NO
            and then LAST_CLICK.COLUMN_NO /= CUR_SELECTION.COLUMN_NO then
              DOS.SOUND;
            end if;
          end if;
        end;
        GO_ON := TRUE;
        EXIT_GAME := FALSE;
      when NOTHING =>
        GO_ON := TRUE;
        EXIT_GAME := FALSE;
    end case;

  elsif LAST_CLICK.SELECTION_KIND = COLOR and then
        CUR_SELECTION.SELECTION_KIND = PROPAL then
    -- check that propal is valid
    if CUR_SELECTION.PROPAL_NO >= FIRST_FREE then
      COMMON.SET_COLOR (PROPAL => CUR_SELECTION.PROPAL_NO,
                        LEVEL  => CUR_SELECTION.COLUMN_NO,
                        COLOR  => LAST_CLICK.COLOR_NO);
      SCREEN.PUT_COLOR(PROPAL => CUR_SELECTION.PROPAL_NO,
                       LEVEL  => CUR_SELECTION.COLUMN_NO,
                       COLOR  => LAST_CLICK.COLOR_NO);
      UPDATE_TRY (CUR_SELECTION.PROPAL_NO);

    else
      DOS.SOUND;
    end if;
    GO_ON := TRUE;
    EXIT_GAME := FALSE;

  elsif LAST_CLICK.SELECTION_KIND = PROPAL then
    if CUR_SELECTION.SELECTION_KIND = SCREEN.NOTHING and then
       CUR_SELECTION.SELECTION = SCREEN.PROPAL then
      -- Restore color
      declare
        PREV_STATE : COMMON.PROPAL_STATE_REC;
        MOVED_COLOR : COMMON.EFF_COLOR_RANGE;
      begin
        PREV_STATE := COMMON.GET_PROPAL_STATE (LAST_CLICK.PROPAL_NO);
        MOVED_COLOR := PREV_STATE.PROPAL_COLOR(LAST_CLICK.COLUMN_NO);
        SCREEN.PUT_COLOR (LAST_CLICK.PROPAL_NO,
                          LAST_CLICK.COLUMN_NO,
                          MOVED_COLOR);
        DOS.SOUND;
      end;
    else
      -- Remove a color from propal (already cleared)
      COMMON.SET_COLOR (LAST_CLICK.PROPAL_NO,
                        LAST_CLICK.COLUMN_NO,
                        COMMON.COLOR_RANGE'FIRST);
      UPDATE_TRY (LAST_CLICK.PROPAL_NO);
    end if;
    GO_ON := TRUE;
    EXIT_GAME := FALSE;
  else
    if LAST_CLICK.SELECTION_KIND /= SCREEN.NOTHING then
      DOS.SOUND;
    end if;
    GO_ON := TRUE;
    EXIT_GAME := FALSE;
  end if;

  if PLAYING then
    SCREEN.PUT_HELP (SCREEN.RELEASED);
  else
    SCREEN.PUT_HELP (SCREEN.START);
  end if;
end TREAT_RELEASE;
