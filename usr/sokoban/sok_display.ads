with CALENDAR;
with SOK_TYPES;
-- displaying of sokoban
package SOK_DISPLAY is

  -- to init screen
  procedure INIT;

  -- puts all the frame
  procedure PUT_FRAME (FRAME : in SOK_TYPES.FRAME_TAB);

  -- puts a square
  procedure PUT_SQUARE (SQUARE     : in SOK_TYPES.SQUARE_REC;
                        COORDINATE : in SOK_TYPES.COORDINATE_REC;
                        BLINK      : in BOOLEAN := FALSE);

  -- puts the down line
  procedure PUT_LINE (MOVES : in NATURAL; PUSHES : in NATURAL;
                      BOXES_IN : in NATURAL; NB_BOXES : in POSITIVE;
                      FRAME : in SOK_TYPES.FRAME_RANGE);

  -- puts the score
  procedure PUT_SCORE (SCORE : in SOK_TYPES.SCORE_REC);

  -- puts the elapsed time
  procedure PUT_TIME (DAY : in NATURAL; TIME : in CALENDAR.DAY_DURATION);

  -- list of possible actions
  type ACTION_LIST is (FRAME, DONE, WRITE, READ, RESET, GET_NEW, BREAK);

  -- help window
  procedure PUT_HELP (HELP : in ACTION_LIST);


  subtype MENU_ACTION_LIST is ACTION_LIST range WRITE .. BREAK;
  -- put extra menu with initial selection
  procedure PUT_MENU (
   INIT_ACTION : in MENU_ACTION_LIST;
   ALLOW_WRITE : in BOOLEAN);
  procedure CLEAR_MENU;

  -- new action selected
  procedure UPDATE_MENU (NEW_ACTION : in MENU_ACTION_LIST);


  -- Errors
  type ERROR_LIST is (NO_DATA, READ, NO_FRAME, RESTORE, SAVE,
                      FORMAT, SCORE_IO, INTERNAL, INIT_SCORE);
  procedure PUT_ERROR (ERROR : in ERROR_LIST);
  procedure CLEAR_ERROR;

  -- clear screen
  procedure END_OF_PROGRAM;

  -- get frame number
  type GET_RESULT_LIST is (SET, ESC, REFRESH);
  procedure GET_NO_FRAME (NO : out SOK_TYPES.FRAME_RANGE; RESULT : out GET_RESULT_LIST);
  FORMAT_ERROR : exception;

end SOK_DISPLAY;
