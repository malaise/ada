with CALENDAR;    -- for time

with DOS;         -- for sound
with CON_IO;      -- for reset_term

with SOK_DISPLAY;
with SOK_INPUT;
with SOK_MOVEMENT;
with SOK_SAVE;
with SOK_TIME;
package body SOK_MANAGER is


  -- internal state of a frame
  type STATE_REC is record
    FRAME        : SOK_TYPES.FRAME_TAB;
    NO_FRAME     : SOK_TYPES.FRAME_RANGE;
    POSITION     : SOK_TYPES.COORDINATE_REC;
    NBRE_TARGETS : NATURAL;
    BOX_OK       : NATURAL;
    MOVES        : NATURAL;
    PUSHES       : NATURAL;
    SCORE        : SOK_TYPES.SCORE_REC;
  end record;
  STATE : STATE_REC;


  -- Menu return : go on with same frame or a new frame
  --  if new frame, reset_all state (read) or update time (restored)
  type MENU_RESULT_LIST is (GO_ON, RESTART_FRAME);
  type UPDATE_STATE_LIST is (RESET_ALL, UPDATE_TIME);
  type MENU_RESULT_REC (RESULT : MENU_RESULT_LIST := GO_ON) is record
    case RESULT is
      when GO_ON     => null;
      when RESTART_FRAME => UPDATE_STATE : UPDATE_STATE_LIST;
    end case;
  end record;

  -- frames reading, saving and restoring.
  package SOK_FILE is

    -- to read a new frame
    procedure READ (NO_FRAME : in  SOK_TYPES.FRAME_RANGE;
                    FRAME    : out SOK_TYPES.FRAME_TAB);
    DATA_FILE_NOT_FOUND, ERROR_READING_DATA : exception;

    -- save a frame and recover a frame, with saved movements
    procedure SAVE (STATE : in STATE_REC);
    ERROR_WRITING_FRAME : exception;

    procedure RESTORE (STATE : out STATE_REC);
    FRAME_FILE_NOT_FOUND, ERROR_READING_FRAME : exception;

    -- Initialise scores, read/update score
    procedure INIT_SCORES;
    function READ_SCORE (NO : SOK_TYPES.FRAME_RANGE) return SOK_TYPES.SCORE_REC;
    procedure WRITE_SCORE (NO : in SOK_TYPES.FRAME_RANGE;
                           SCORE : in SOK_TYPES.SCORE_REC);
    SCORE_IO_ERROR : exception;
    

  end SOK_FILE;

  procedure PLAY_FRAME (UPDATE_STATE : out UPDATE_STATE_LIST);

  INTERNAL_ERROR : exception;

  package body SOK_FILE is separate;


  procedure PLAY_GAME (FIRST_FRAME : in SOK_TYPES.FRAME_RANGE) is
    FRAME_RESULT : UPDATE_STATE_LIST;
    FOUND : BOOLEAN;

    procedure END_OF_PROGRAM is
    begin
      -- End of game
      SOK_INPUT.END_OF_PROGRAM;

      SOK_DISPLAY.END_OF_PROGRAM;
    end END_OF_PROGRAM;

    use SOK_TYPES;

  begin
    begin

      -- init for first frame
      begin
        SOK_DISPLAY.INIT;
      exception
        when others =>
        raise SOK_INPUT.BREAK_REQUESTED;
      end;
      STATE.NO_FRAME := FIRST_FRAME;
      FRAME_RESULT := RESET_ALL;
      begin
        SOK_FILE.INIT_SCORES;
      exception
        when SOK_FILE.SCORE_IO_ERROR =>
          SOK_DISPLAY.PUT_ERROR (SOK_DISPLAY.INIT_SCORE);
          raise;
      end;

      loop
        case FRAME_RESULT is
          when RESET_ALL =>
            -- Read frame
            begin
              SOK_FILE.READ (STATE.NO_FRAME, STATE.FRAME);
            exception
              when SOK_FILE.DATA_FILE_NOT_FOUND =>
                SOK_DISPLAY.PUT_ERROR (SOK_DISPLAY.NO_DATA);
                raise;
              when SOK_FILE.ERROR_READING_DATA =>
                SOK_DISPLAY.PUT_ERROR (SOK_DISPLAY.READ);
                raise;
            end;

            -- Read score
            begin
              STATE.SCORE := SOK_FILE.READ_SCORE(STATE.NO_FRAME);
            exception
              when SOK_FILE.SCORE_IO_ERROR =>
                SOK_DISPLAY.PUT_ERROR (SOK_DISPLAY.SCORE_IO);
                raise;
            end;

            -- Init state
            STATE.MOVES        := 0;
            STATE.PUSHES       := 0;

            -- clear saved movements
            SOK_SAVE.RESET;

            -- Init time and start;
            SOK_TIME.RESET_TIME;
            SOK_TIME.START_TIME;

            -- find man starting position and complete state
            FOUND := FALSE;
            STATE.NBRE_TARGETS := 0;
            STATE.BOX_OK       := 0;
            for I in SOK_TYPES.ROW_RANGE loop
              for J in SOK_TYPES.COL_RANGE loop
                if STATE.FRAME (I, J).PATTERN /= SOK_TYPES.WALL and then
                   STATE.FRAME (I, J).CONTENT = SOK_TYPES.MAN then
                   if FOUND then
                   -- man already found !!
                    raise INTERNAL_ERROR;
                  else
                    FOUND := TRUE;
                    STATE.POSITION := (ROW => I, COL => J);
                  end if;
                end if;

                if STATE.FRAME (I, J).PATTERN = SOK_TYPES.TARGET then
                  STATE.NBRE_TARGETS := STATE.NBRE_TARGETS + 1;
                  if STATE.FRAME(I, J).CONTENT = SOK_TYPES.BOX then
                    STATE.BOX_OK := STATE.BOX_OK + 1;
                  end if;
                end if;
              end loop;
            end loop;
            if not FOUND then
              -- No man !!
              raise INTERNAL_ERROR;
            end if;

          when UPDATE_TIME =>
            -- Init of time already done in sok_file
            SOK_TIME.START_TIME;
        end case;

        -- display
        SOK_DISPLAY.PUT_FRAME (STATE.FRAME);
        SOK_DISPLAY.PUT_HELP (SOK_DISPLAY.FRAME);
        SOK_DISPLAY.PUT_SCORE (STATE.SCORE);

        SOK_DISPLAY.PUT_LINE (STATE.MOVES, STATE.PUSHES, STATE.BOX_OK,
                              STATE.NBRE_TARGETS, STATE.NO_FRAME);

        PLAY_FRAME (FRAME_RESULT);
      end loop;

    exception
      when INTERNAL_ERROR =>
        SOK_DISPLAY.PUT_ERROR (SOK_DISPLAY.INTERNAL);
        raise;
    end;

  exception
    when SOK_INPUT.BREAK_REQUESTED =>
      END_OF_PROGRAM;
    when others =>
      SOK_INPUT.PAUSE;
      SOK_DISPLAY.CLEAR_ERROR;
      END_OF_PROGRAM;
  end PLAY_GAME;

  procedure SET_BLINK (
   FRAME : in SOK_TYPES.FRAME_TAB;
   BLINK : BOOLEAN) is
   use SOK_TYPES;
  begin
    for I in SOK_TYPES.ROW_RANGE loop
      for J in SOK_TYPES.COL_RANGE loop
        if FRAME (I, J).PATTERN = SOK_TYPES.TARGET and then
           FRAME (I, J).CONTENT = SOK_TYPES.BOX then
          SOK_DISPLAY.PUT_SQUARE (
           SQUARE     => FRAME (I, J),
           COORDINATE => (ROW=>I, COL=>J),
           BLINK      => BLINK);
        end if;
      end loop;
    end loop;
  end SET_BLINK;

  -- if return is GO_ON, nothing to do
  -- if return is RESTART_FRAME, part of state has been set
  --  if RESET_ALL, only NO_FRAME is set (read)
  --  if UPDATE_TIME, only time has to be updated (restore)
  function SOK_MENU (ALLOW_WRITE : BOOLEAN) return MENU_RESULT_REC is separate;


  procedure PLAY_FRAME (UPDATE_STATE : out UPDATE_STATE_LIST) is

    KEY : SOK_INPUT.KEY_LIST;

    SAVE_SCORE : BOOLEAN;
    DISP_SCORE : SOK_TYPES.SCORE_REC;

    RESULT : SOK_MOVEMENT.RESULT_LIST;

    SAVED_POS : SOK_TYPES.COORDINATE_REC;

    POPED_DATA : SOK_MOVEMENT.SAVED_DATA_REC;

    MENU_RESULT : MENU_RESULT_REC;

    use SOK_TYPES, SOK_INPUT;

  begin
    -- Score to display
    DISP_SCORE := STATE.SCORE;

    -- movements
    loop
      KEY := SOK_INPUT.GET_KEY;

      if KEY = SOK_INPUT.ESC then
        -- menu
        MENU_RESULT := SOK_MENU (ALLOW_WRITE => TRUE);
        case MENU_RESULT.RESULT is
          when GO_ON =>
            KEY := SOK_INPUT.ESC;
            SOK_DISPLAY.PUT_LINE (STATE.MOVES, STATE.PUSHES, STATE.BOX_OK,
                                  STATE.NBRE_TARGETS, STATE.NO_FRAME);
            SOK_DISPLAY.PUT_SCORE (DISP_SCORE);
          when RESTART_FRAME =>
            UPDATE_STATE := MENU_RESULT.UPDATE_STATE;
            return;
        end case;
      end if;

      if KEY = SOK_INPUT.REFRESH then
        SOK_DISPLAY.PUT_FRAME (STATE.FRAME);
        SOK_DISPLAY.PUT_HELP (SOK_DISPLAY.FRAME);
        SOK_DISPLAY.PUT_LINE (STATE.MOVES, STATE.PUSHES, STATE.BOX_OK,
                              STATE.NBRE_TARGETS, STATE.NO_FRAME);
        SOK_DISPLAY.PUT_SCORE (DISP_SCORE);
      end if;

      if KEY in SOK_MOVEMENT.MOVEMENT_LIST then
        -- movement
        SOK_MOVEMENT.DO_MOVEMENT (STATE.FRAME, STATE.POSITION, KEY, RESULT);
        case RESULT is
          when SOK_MOVEMENT.REFUSED =>
            null;
          when SOK_MOVEMENT.DONE =>
            -- movement without box moving
            STATE.MOVES := STATE.MOVES + 1;
          when SOK_MOVEMENT.BOX_MOVED =>
            -- Same number of box OK
            STATE.MOVES  := STATE.MOVES  + 1;
            STATE.PUSHES := STATE.PUSHES + 1;
          when SOK_MOVEMENT.BOX_OK_MORE =>
            -- One more box OK
            STATE.MOVES  := STATE.MOVES  + 1;
            STATE.PUSHES := STATE.PUSHES + 1;
            STATE.BOX_OK := STATE.BOX_OK + 1;
            if STATE.BOX_OK = STATE.NBRE_TARGETS then
              -- frame finished
              SOK_TIME.STOP_TIME;
              SET_BLINK(STATE.FRAME, TRUE);
              SOK_DISPLAY.PUT_HELP (SOK_DISPLAY.DONE);
              SOK_DISPLAY.PUT_LINE (STATE.MOVES, STATE.PUSHES, STATE.BOX_OK,
                                    STATE.NBRE_TARGETS, STATE.NO_FRAME);

              -- Update and save score if needed
              SAVE_SCORE := FALSE;
              if not STATE.SCORE.SET then
                STATE.SCORE.SET := TRUE;
                SOK_TIME.GET_TIME (STATE.SCORE.DAY, STATE.SCORE.DUR);
                STATE.SCORE.MOVES := STATE.MOVES;
                STATE.SCORE.PUSHES := STATE.PUSHES;
                SAVE_SCORE := TRUE;
              else
                declare
                  DAY : NATURAL;
                  DUR : DURATION;
                begin
                  SOK_TIME.GET_TIME(DAY, DUR);
                  if (DAY < STATE.SCORE.DAY )
                  or else (DAY = STATE.SCORE.DAY
                     and then DUR < STATE.SCORE.DUR) then
                    STATE.SCORE.DAY := DAY;
                    STATE.SCORE.DUR := DUR;
                    SAVE_SCORE := TRUE;
                  end if;
                end;
                if STATE.MOVES < STATE.SCORE.MOVES then
                  STATE.SCORE.MOVES := STATE.MOVES;
                  SAVE_SCORE := TRUE;
                end if;
                if STATE.PUSHES < STATE.SCORE.PUSHES then
                  STATE.SCORE.PUSHES := STATE.PUSHES;
                  SAVE_SCORE := TRUE;
                end if;
              end if;
              if SAVE_SCORE then
                begin
                  SOK_FILE.WRITE_SCORE (STATE.NO_FRAME, STATE.SCORE);
                exception
                  when SOK_FILE.SCORE_IO_ERROR =>
                    SOK_DISPLAY.PUT_ERROR (SOK_DISPLAY.INIT_SCORE);
                    raise;
                end;
              end if;

              -- wait input to go on
              loop
                KEY := SOK_INPUT.GET_KEY;
                if KEY = SOK_INPUT.ESC then
                  -- menu
                  MENU_RESULT := SOK_MENU (ALLOW_WRITE => FALSE);
                  case MENU_RESULT.RESULT is
                    when GO_ON =>
                      -- Esc in menu
                      KEY := SOK_INPUT.ESC;
                      SOK_DISPLAY.PUT_LINE (STATE.MOVES, STATE.PUSHES, STATE.BOX_OK,
                                            STATE.NBRE_TARGETS, STATE.NO_FRAME);
                      SOK_DISPLAY.PUT_SCORE (DISP_SCORE);
                    when RESTART_FRAME =>
                      UPDATE_STATE := MENU_RESULT.UPDATE_STATE;
                      return;
                  end case;
                elsif KEY = SOK_INPUT.UNDO then
                  DOS.SOUND;
                elsif KEY = SOK_INPUT.REFRESH then
                  -- Refresh
                  SOK_DISPLAY.PUT_FRAME (STATE.FRAME);
                  SET_BLINK(STATE.FRAME, TRUE);
                  SOK_DISPLAY.PUT_HELP (SOK_DISPLAY.DONE);
                  SOK_DISPLAY.PUT_LINE (STATE.MOVES, STATE.PUSHES, STATE.BOX_OK,
                                        STATE.NBRE_TARGETS, STATE.NO_FRAME);
                  SOK_DISPLAY.PUT_SCORE (DISP_SCORE);
                end if;
                exit when KEY = SOK_INPUT.NEXT;
              end loop;
              SET_BLINK(STATE.FRAME, FALSE);
              if STATE.NO_FRAME /= SOK_TYPES.FRAME_RANGE'LAST then
                STATE.NO_FRAME := SOK_TYPES.FRAME_RANGE'SUCC(STATE.NO_FRAME);
              else
                STATE.NO_FRAME := SOK_TYPES.FRAME_RANGE'FIRST;
              end if;
              -- restart with new frame
              UPDATE_STATE := RESET_ALL;
              return;
            end if;
          when SOK_MOVEMENT.BOX_OK_LESS =>
            -- One less box OK
            STATE.MOVES  := STATE.MOVES  + 1;
            STATE.PUSHES := STATE.PUSHES + 1;
            STATE.BOX_OK := STATE.BOX_OK - 1;
        end case;

        -- save movement
        case RESULT is
          when SOK_MOVEMENT.REFUSED =>
            null;
          when SOK_MOVEMENT.DONE =>
            SOK_SAVE.PUSH (
             (POS_ORIG => STATE.POSITION,
              MOVEMENT => KEY,
              RESULT   => SOK_MOVEMENT.DONE) );
          when SOK_MOVEMENT.BOX_MOVED | SOK_MOVEMENT.BOX_OK_MORE |
               SOK_MOVEMENT.BOX_OK_LESS =>
            SOK_SAVE.PUSH (
             (POS_ORIG => STATE.POSITION,
              MOVEMENT => KEY,
              RESULT   => SOK_MOVEMENT.BOX_MOVED) );
         end case;

      elsif KEY = SOK_INPUT.UNDO then
        begin
          -- try to pop movement
          POPED_DATA := SOK_SAVE.POP;
          SOK_MOVEMENT.UNDO_MOVEMENT (STATE.FRAME, POPED_DATA,
                                      RESULT, SAVED_POS);
          case RESULT is
            when SOK_MOVEMENT.REFUSED =>
              -- impossible;
              null;
            when SOK_MOVEMENT.DONE =>
              -- movement without box moving
              STATE.MOVES := STATE.MOVES - 1;
            when SOK_MOVEMENT.BOX_MOVED =>
              -- Same number of box OK
              STATE.MOVES  := STATE.MOVES  - 1;
              STATE.PUSHES := STATE.PUSHES - 1;
            when SOK_MOVEMENT.BOX_OK_MORE =>
              -- One more box OK
              STATE.MOVES  := STATE.MOVES  - 1;
              STATE.PUSHES := STATE.PUSHES - 1;
              STATE.BOX_OK := STATE.BOX_OK + 1;
            when SOK_MOVEMENT.BOX_OK_LESS =>
              -- One less box OK
              STATE.MOVES  := STATE.MOVES  - 1;
              STATE.PUSHES := STATE.PUSHES - 1;
              STATE.BOX_OK := STATE.BOX_OK - 1;
          end case;
          STATE.POSITION := SAVED_POS;
        exception
          when SOK_SAVE.NO_MORE_SAVED_MOVEMENTS =>
            DOS.SOUND;
        end;

      end if; -- move or undo

      if SOK_MOVEMENT."/=" (RESULT, SOK_MOVEMENT.REFUSED) then
        SOK_DISPLAY.PUT_LINE (STATE.MOVES, STATE.PUSHES, STATE.BOX_OK,
                              STATE.NBRE_TARGETS, STATE.NO_FRAME);
      end if;


    end loop;
  end PLAY_FRAME;





end SOK_MANAGER;
