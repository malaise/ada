with AFPX, CON_IO, NORMAL;
use AFPX;

with COMMON;
package body SCREEN is

  -- All the bars
  subtype INDEX_RANGE is POSITIVE range 1 .. 16;

  -- Status of one / all bars
  type STATUS_LIST is (FREE, SELECTED, REMOVED);
  type STATUS_TAB is array (INDEX_RANGE) of STATUS_LIST;
  STATUS : STATUS_TAB;


  HUMAN_SCORE : NATURAL;
  MACHINE_SCORE : NATURAL;

  function FIRST_INDEX_OF_ROW (ROW : COMMON.ROW_RANGE) return INDEX_RANGE is
    INDEX : INDEX_RANGE := INDEX_RANGE'FIRST;
  begin
    for I in 2 .. ROW loop
      INDEX := INDEX + COMMON.BAR_PER_ROW(I - 1);
    end loop;
    return INDEX;
  end FIRST_INDEX_OF_ROW;

  function LAST_INDEX_OF_ROW (ROW : COMMON.ROW_RANGE) return INDEX_RANGE is
  begin
    return FIRST_INDEX_OF_ROW (ROW) + COMMON.BAR_PER_ROW (ROW) - 1;
  end LAST_INDEX_OF_ROW;

  function INTRO return COMMON.GAME_LIST is
    -- For put then get
    CURSOR_FIELD : FIELD_RANGE := FIELD_RANGE'FIRST;
    CURSOR_COL : CON_IO.COL_RANGE := CON_IO.COL_RANGE'FIRST;
    RESULT : RESULT_REC;
  begin
    USE_DESCRIPTOR (1);
    loop
      PUT_THEN_GET(CURSOR_FIELD, CURSOR_COL, RESULT, TRUE);
      exit when RESULT.EVENT = AFPX.MOUSE_BUTTON;
    end loop;
    if RESULT.FIELD_NO = 3 then
      return COMMON.NIM;
    elsif RESULT.FIELD_NO = 4 then
      return COMMON.MARIENBAD;
    elsif RESULT.FIELD_NO = 5 then
      raise EXIT_REQUESTED;
    end if;
    -- To avoid warning
    return COMMON.NIM;
  end INTRO;

  procedure RESET (GAME : in COMMON.GAME_LIST) is
    use COMMON;
  begin
    USE_DESCRIPTOR (2);
    SCORE (HUMAN_SCORE, MACHINE_SCORE);
    if GAME = COMMON.NIM then
      ENCODE_FIELD (20, (0,0), "   Nim   ");
      ENCODE_FIELD (22, (1,1), "Play Marienbad");
    else
      ENCODE_FIELD (20, (0,0), "Marienbad");
      ENCODE_FIELD (22, (1,1), "   Play Nim");
    end if;
    STATUS := (others => FREE);
    for I in FIELD_RANGE'(1) .. 16 loop
      SET_FIELD_ACTIVATION (I, TRUE); 
      SET_FIELD_COLORS (I, BACKGROUND => CON_IO.GREEN);
    end loop;
    SET_FIELD_ACTIVATION (22, FALSE);
  end RESET;

  procedure PLAY is
    -- For put then get
    CURSOR_FIELD : FIELD_RANGE := FIELD_RANGE'FIRST;
    CURSOR_COL : CON_IO.COL_RANGE := CON_IO.COL_RANGE'FIRST;
    RESULT : RESULT_REC;
    REDISPLAY : BOOLEAN;

    -- The current selection
    SELECTION_INDEX : INDEX_RANGE;
    NB_SELECTED : NATURAL;
    ROW_SELECTED : COMMON.ROW_RANGE;

    -- Get the row of an index
    function ROW_OF_INDEX (INDEX : INDEX_RANGE) return COMMON.ROW_RANGE is
    begin
      case INDEX is
        when 01 .. 07 => return 1;
        when 08 .. 12 => return 2;
        when 13 .. 15 => return 3;
        when 16       => return 4;
      end case;
    end ROW_OF_INDEX;

  begin
    NB_SELECTED := 0;
    ROW_SELECTED := 1;
    REDISPLAY := FALSE;
    loop
      -- Activate play
      SET_FIELD_ACTIVATION (17, NB_SELECTED /= 0);
      PUT_THEN_GET(CURSOR_FIELD, CURSOR_COL, RESULT, REDISPLAY);
      if RESULT.EVENT = AFPX.MOUSE_BUTTON then
        case RESULT.FIELD_NO is
          when 1 .. 16 =>
            SELECTION_INDEX := INDEX_RANGE(RESULT.FIELD_NO);
  
            -- First selection
            if NB_SELECTED = 0 then
              ROW_SELECTED := ROW_OF_INDEX(SELECTION_INDEX);
            end if;

            -- Take selection / unselect
            if ROW_OF_INDEX(SELECTION_INDEX) = ROW_SELECTED then
              if STATUS(SELECTION_INDEX) = SELECTED then
                STATUS(SELECTION_INDEX)  := FREE;
                SET_FIELD_COLORS (RESULT.FIELD_NO, BACKGROUND => CON_IO.GREEN);
                NB_SELECTED := NB_SELECTED - 1;
              else
                STATUS(SELECTION_INDEX)  := SELECTED;
                SET_FIELD_COLORS (RESULT.FIELD_NO, BACKGROUND => CON_IO.RED);
                NB_SELECTED := NB_SELECTED + 1;
              end if;
            end if;
          
          when 17 =>
            -- Take selection
            for I in FIRST_INDEX_OF_ROW(ROW_SELECTED) .. LAST_INDEX_OF_ROW(ROW_SELECTED) loop
              if STATUS(I) = SELECTED then
                STATUS(I) := REMOVED;
                SET_FIELD_ACTIVATION (FIELD_RANGE(I), FALSE); 
              end if;
            end loop;
            exit;
          when 18 =>
            raise EXIT_REQUESTED;
          when others =>
            null; 
        end case;
        REDISPLAY := FALSE;
      else
        REDISPLAY := TRUE;
      end if;
    end loop;

  end PLAY;

  function CONTENT (ROW : COMMON.ROW_RANGE) return COMMON.FULL_BAR_RANGE is
    J : NATURAL := 0;
    RES : COMMON.FULL_BAR_RANGE := 0;
  begin
    for I in reverse FIRST_INDEX_OF_ROW(ROW) .. LAST_INDEX_OF_ROW(ROW) loop
      if STATUS(I) = FREE then
        RES := RES + 2 ** J;
      end if;
    end loop;
    return RES;
  end CONTENT;


  procedure UPDATE (ROW : in COMMON.ROW_RANGE; BARS : in COMMON.FULL_BAR_RANGE;
                    RESULT : in COMPUTE.RESULT_LIST; CHANGE_GAME : out BOOLEAN) is
    NB_TO_REMOVE, J : NATURAL;
    -- For put then get
    CURSOR_FIELD : FIELD_RANGE := FIELD_RANGE'FIRST;
    CURSOR_COL : CON_IO.COL_RANGE := CON_IO.COL_RANGE'FIRST;
    PTG_RESULT : RESULT_REC;
    use COMPUTE;
  begin
    CHANGE_GAME := FALSE;
    -- Current content
    NB_TO_REMOVE := 0;
    for I in FIRST_INDEX_OF_ROW(ROW) .. LAST_INDEX_OF_ROW(ROW) loop
      if STATUS(I) = FREE then
        NB_TO_REMOVE := NB_TO_REMOVE + 1;
      end if;
    end loop;
    -- Nb to remove
    NB_TO_REMOVE := NB_TO_REMOVE - BARS;

    -- Select
    j := NB_TO_REMOVE;
    for I in FIRST_INDEX_OF_ROW(ROW) .. LAST_INDEX_OF_ROW(ROW) loop
      if STATUS(I) = FREE then
        STATUS(I)  := SELECTED;
        SET_FIELD_COLORS (FIELD_RANGE(I), BACKGROUND => CON_IO.RED);
        J := J - 1;
        exit when J = 0;
      end if;
    end loop;

    -- Display
    if RESULT in COMPUTE.PLAYED_RESULT_LIST then
      SET_FIELD_ACTIVATION (17, FALSE);
      PUT;
      delay 0.5;
    end if;

    -- Remove
    j := NB_TO_REMOVE;
    for I in FIRST_INDEX_OF_ROW(ROW) .. LAST_INDEX_OF_ROW(ROW) loop
      if STATUS(I) = SELECTED then
        STATUS(I) := REMOVED;
        SET_FIELD_COLORS (FIELD_RANGE(I), BACKGROUND => CON_IO.RED);
        SET_FIELD_ACTIVATION (FIELD_RANGE(I), FALSE);
        J := J - 1;
        exit when J = 0;
      end if;
    end loop;

    -- Validate
    if RESULT /= COMPUTE.PLAYED then
      if RESULT = COMPUTE.PLAYED_AND_WON or else RESULT = COMPUTE.WON then
        ENCODE_FIELD(21, (0, 34), "I win :-)");
      else
        ENCODE_FIELD(21, (0, 33), "You win :-(");
      end if;
      SET_FIELD_ACTIVATION (17, TRUE);
      ENCODE_FIELD (17, (1, 1), "P l a y");
      SET_FIELD_ACTIVATION (22, TRUE);
      loop
        PUT_THEN_GET(CURSOR_FIELD, CURSOR_COL, PTG_RESULT, TRUE);
        if PTG_RESULT.EVENT = AFPX.MOUSE_BUTTON and then PTG_RESULT.FIELD_NO = 22 then
          CHANGE_GAME := TRUE;
        end if;
        exit when PTG_RESULT.EVENT = AFPX.MOUSE_BUTTON
                  and then (PTG_RESULT.FIELD_NO = 17 or else PTG_RESULT.FIELD_NO = 22);
        if PTG_RESULT.EVENT = AFPX.MOUSE_BUTTON and then PTG_RESULT.FIELD_NO = 18 then
          raise EXIT_REQUESTED;
        end if;
      end loop;
      RESET_FIELD (17);
      RESET_FIELD (21);
      SET_FIELD_ACTIVATION (22, FALSE);
    end if;
  end UPDATE;

  procedure SCORE (HUMAN, MACHINE : in NATURAL) is
  begin
    HUMAN_SCORE := HUMAN;
    MACHINE_SCORE := MACHINE;
    ENCODE_FIELD (19, (0,  6), NORMAL(HUMAN_SCORE, 3));
    ENCODE_FIELD (19, (0, 17), NORMAL(MACHINE_SCORE, 3));
  end SCORE;
  

end SCREEN;
