with SOK_DISPLAY;

-- Movement manager of SOKOBAN
package body SOK_MOVEMENT is


  function NEW_COORDINATE (
   POSITION : SOK_TYPES.COORDINATE_REC;
   MOVEMENT : MOVEMENT_LIST) return SOK_TYPES.COORDINATE_REC is
    NEW_POSITION : SOK_TYPES.COORDINATE_REC := POSITION;
  begin
    case MOVEMENT is
      when SOK_INPUT.UP =>
        NEW_POSITION.ROW :=
         SOK_TYPES.ROW_RANGE'PRED(NEW_POSITION.ROW);
      when SOK_INPUT.DOWN =>
        NEW_POSITION.ROW :=
         SOK_TYPES.ROW_RANGE'SUCC(NEW_POSITION.ROW);
      when SOK_INPUT.LEFT =>
        NEW_POSITION.COL :=
         SOK_TYPES.COL_RANGE'PRED(NEW_POSITION.COL);
      when SOK_INPUT.RIGHT =>
        NEW_POSITION.COL :=
         SOK_TYPES.COL_RANGE'SUCC(NEW_POSITION.COL);
    end case;
    return NEW_POSITION;
  exception
    when others => raise ILLEGAL_MOVEMENT;
  end NEW_COORDINATE;



  -- try to do a movement
  -- Give frame, current position and movement to try
  procedure DO_MOVEMENT (
   FRAME    : in out SOK_TYPES.FRAME_TAB;
   POSITION : in out SOK_TYPES.COORDINATE_REC;
   MOVEMENT : in MOVEMENT_LIST;
   RESULT   : out RESULT_LIST) is

   CUR_MAN_SQUARE   : SOK_TYPES.SQUARE_REC;

   NEW_MAN_POSITION : SOK_TYPES.COORDINATE_REC;
   NEW_MAN_SQUARE   : SOK_TYPES.SQUARE_REC;

   NEW_BOX_POSITION : SOK_TYPES.COORDINATE_REC;
   NEW_BOX_SQUARE   : SOK_TYPES.SQUARE_REC;

   LOC_RESULT : RESULT_LIST;

   use SOK_TYPES;
  begin
    -- check content of current position
    CUR_MAN_SQUARE := FRAME (POSITION.ROW, POSITION.COL);
    if CUR_MAN_SQUARE.PATTERN = SOK_TYPES.WALL or else
     CUR_MAN_SQUARE.CONTENT /= SOK_TYPES.MAN then
      -- on a wall or no man !!
      raise ILLEGAL_MOVEMENT;
    end if;

    -- set new position
    NEW_MAN_POSITION := NEW_COORDINATE (POSITION, MOVEMENT);
    NEW_MAN_SQUARE := FRAME (NEW_MAN_POSITION.ROW, NEW_MAN_POSITION.COL);

    -- check content of new man position
    case NEW_MAN_SQUARE.PATTERN is
      when SOK_TYPES.WALL =>
        LOC_RESULT := REFUSED;

      when SOK_TYPES.FREE | SOK_TYPES.TARGET =>
        case NEW_MAN_SQUARE.CONTENT is
          when SOK_TYPES.MAN =>
            -- another man !!
            raise ILLEGAL_MOVEMENT;
          when SOK_TYPES.NOTHING =>
            -- simple man movement
            LOC_RESULT := DONE;
          when SOK_TYPES.BOX =>
            -- a push
            NEW_BOX_POSITION := NEW_COORDINATE (NEW_MAN_POSITION, MOVEMENT);
            NEW_BOX_SQUARE :=
             FRAME (NEW_BOX_POSITION.ROW, NEW_BOX_POSITION.COL);

            -- check content of new box position
            case NEW_BOX_SQUARE.PATTERN is
              when SOK_TYPES.WALL =>
                LOC_RESULT := REFUSED;

              when SOK_TYPES.FREE | SOK_TYPES.TARGET =>
                case NEW_BOX_SQUARE.CONTENT is
                  when SOK_TYPES.MAN =>
                    -- another man !!
                    raise ILLEGAL_MOVEMENT;
                  when SOK_TYPES.BOX =>
                    -- impossible to push 2 boxes
                    LOC_RESULT := REFUSED;
                  when SOK_TYPES.NOTHING =>
                    -- push
                    if NEW_MAN_SQUARE.PATTERN = NEW_BOX_SQUARE.PATTERN then
                      -- a push from free to free or from target to target
                      LOC_RESULT := BOX_MOVED;
                    elsif NEW_MAN_SQUARE.PATTERN = SOK_TYPES.FREE then
                      -- a push from free to target
                      LOC_RESULT := BOX_OK_MORE;
                    else
                      -- a push from target to free
                      LOC_RESULT := BOX_OK_LESS;
                    end if;
                end case;
            end case;
        end case;
    end case;


    -- update old and new man position
    case LOC_RESULT is
      when REFUSED =>
        null;
      when DONE | BOX_MOVED | BOX_OK_MORE | BOX_OK_LESS =>
        FRAME (POSITION.ROW, POSITION.COL).CONTENT := SOK_TYPES.NOTHING;
        SOK_DISPLAY.PUT_SQUARE (
         SQUARE     => FRAME (POSITION.ROW, POSITION.COL),
         COORDINATE => POSITION);
        POSITION := NEW_MAN_POSITION;

        FRAME (NEW_MAN_POSITION.ROW, NEW_MAN_POSITION.COL).CONTENT :=
         SOK_TYPES.MAN;
        SOK_DISPLAY.PUT_SQUARE (
         SQUARE     => FRAME (NEW_MAN_POSITION.ROW, NEW_MAN_POSITION.COL),
         COORDINATE => NEW_MAN_POSITION);
    end case;

    -- update new box position
    case LOC_RESULT is
      when REFUSED | DONE =>
        null;
      when BOX_MOVED | BOX_OK_MORE | BOX_OK_LESS =>
        FRAME (NEW_BOX_POSITION.ROW, NEW_BOX_POSITION.COL).CONTENT :=
         SOK_TYPES.BOX;
        SOK_DISPLAY.PUT_SQUARE (
         SQUARE     => FRAME (NEW_BOX_POSITION.ROW, NEW_BOX_POSITION.COL),
         COORDINATE => NEW_BOX_POSITION);
    end case;

    -- Done
    RESULT := LOC_RESULT;
  end DO_MOVEMENT;



  -- to undo a movement
  -- give frame, current position and movement which
  -- moved to current position
  procedure UNDO_MOVEMENT (
   FRAME      : in out SOK_TYPES.FRAME_TAB;
   SAVED_DATA : in SAVED_DATA_REC;
   RESULT        : out UNDO_RESULT_LIST;
   PREV_POSITION : out SOK_TYPES.COORDINATE_REC) is

    CUR_MAN_SQUARE   : SOK_TYPES.SQUARE_REC;

    OLD_MAN_POSITION : SOK_TYPES.COORDINATE_REC;
    OLD_MAN_SQUARE   : SOK_TYPES.SQUARE_REC;

    CUR_BOX_POSITION : SOK_TYPES.COORDINATE_REC;
    CUR_BOX_SQUARE   : SOK_TYPES.SQUARE_REC;

    LOC_RESULT : UNDO_RESULT_LIST;

    UNDO_MOVEMENT : MOVEMENT_LIST;

    use SOK_TYPES;
  begin
    -- check content of current position
    CUR_MAN_SQUARE := FRAME (SAVED_DATA.POS_ORIG.ROW,
                             SAVED_DATA.POS_ORIG.COL);
    if CUR_MAN_SQUARE.PATTERN = SOK_TYPES.WALL or else
     CUR_MAN_SQUARE.CONTENT /= SOK_TYPES.MAN then
      -- on a wall or no man !!
      raise ILLEGAL_MOVEMENT;
    end if;

    -- set undo movement
    case SAVED_DATA.MOVEMENT is
      when SOK_INPUT.UP    => UNDO_MOVEMENT := SOK_INPUT.DOWN;
      when SOK_INPUT.DOWN  => UNDO_MOVEMENT := SOK_INPUT.UP;
      when SOK_INPUT.RIGHT => UNDO_MOVEMENT := SOK_INPUT.LEFT;
      when SOK_INPUT.LEFT  => UNDO_MOVEMENT := SOK_INPUT.RIGHT;
    end case;

    -- set previous man position
    OLD_MAN_POSITION := NEW_COORDINATE (SAVED_DATA.POS_ORIG, UNDO_MOVEMENT);
    OLD_MAN_SQUARE := FRAME (OLD_MAN_POSITION.ROW, OLD_MAN_POSITION.COL);

    -- check content of old man position
    case OLD_MAN_SQUARE.PATTERN is
      when SOK_TYPES.WALL =>
        raise ILLEGAL_MOVEMENT;

      when SOK_TYPES.FREE | SOK_TYPES.TARGET =>
        case OLD_MAN_SQUARE.CONTENT is
          when SOK_TYPES.MAN | SOK_TYPES.BOX =>
            -- another man, or a box at old man position !!
            raise ILLEGAL_MOVEMENT;
          when SOK_TYPES.NOTHING =>
            -- OK check if also box has to be moved
            case SAVED_DATA.RESULT is
              when DONE =>
                LOC_RESULT := DONE;
              when BOX_MOVED =>
                -- set current box position
                CUR_BOX_POSITION := NEW_COORDINATE (SAVED_DATA.POS_ORIG,
                                                    SAVED_DATA.MOVEMENT);
                CUR_BOX_SQUARE := FRAME (CUR_BOX_POSITION.ROW,
                                         CUR_BOX_POSITION.COL);

                -- check content of current box position
                case CUR_BOX_SQUARE.PATTERN is
                  when SOK_TYPES.WALL =>
                    raise ILLEGAL_MOVEMENT;

                  when SOK_TYPES.FREE | SOK_TYPES.TARGET =>
                    case CUR_BOX_SQUARE.CONTENT is
                      when SOK_TYPES.MAN | SOK_TYPES.NOTHING =>
                        -- another man or nothing !!
                        raise ILLEGAL_MOVEMENT;
                      when SOK_TYPES.BOX =>
                        -- OK, pop box
                        if CUR_MAN_SQUARE.PATTERN = CUR_BOX_SQUARE.PATTERN then
                          -- a pop from free to free or from target to target
                          LOC_RESULT := BOX_MOVED;
                        elsif CUR_MAN_SQUARE.PATTERN = SOK_TYPES.FREE then
                          -- a pop from target to free
                          LOC_RESULT := BOX_OK_LESS;
                        else
                          -- a pop from free to target
                          LOC_RESULT := BOX_OK_MORE;
                        end if;
                    end case;
                end case;
            end case;
        end case;
    end case;

    -- update old box position, and position content
    case LOC_RESULT is
      when DONE =>
        -- original man becomes empty
        FRAME (SAVED_DATA.POS_ORIG.ROW, SAVED_DATA.POS_ORIG.COL).CONTENT :=
               SOK_TYPES.NOTHING;
      when BOX_MOVED | BOX_OK_MORE | BOX_OK_LESS =>
        -- original man position receives box
        FRAME (SAVED_DATA.POS_ORIG.ROW, SAVED_DATA.POS_ORIG.COL).CONTENT :=
               SOK_TYPES.BOX;
        -- original box posoition becomes empty. Show
        FRAME (CUR_BOX_POSITION.ROW, CUR_BOX_POSITION.COL).CONTENT :=
               SOK_TYPES.NOTHING;
        SOK_DISPLAY.PUT_SQUARE (
         SQUARE     => FRAME (CUR_BOX_POSITION.ROW, CUR_BOX_POSITION.COL),
         COORDINATE => CUR_BOX_POSITION);
    end case;


    -- update current and old man position
    -- show original man position
    SOK_DISPLAY.PUT_SQUARE (
     SQUARE     => FRAME (SAVED_DATA.POS_ORIG.ROW, SAVED_DATA.POS_ORIG.COL),
     COORDINATE => SAVED_DATA.POS_ORIG);
    -- set and show current man position
    FRAME (OLD_MAN_POSITION.ROW, OLD_MAN_POSITION.COL).CONTENT :=
     SOK_TYPES.MAN;
    SOK_DISPLAY.PUT_SQUARE (
     SQUARE     => FRAME (OLD_MAN_POSITION.ROW, OLD_MAN_POSITION.COL),
     COORDINATE => OLD_MAN_POSITION);

    -- Done
    PREV_POSITION := OLD_MAN_POSITION;
    RESULT := LOC_RESULT;
 end UNDO_MOVEMENT;

end SOK_MOVEMENT;