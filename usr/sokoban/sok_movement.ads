with SOK_TYPES;
with SOK_INPUT;

-- Movement manager of SOKOBAN
package SOK_MOVEMENT is


  -- Man movement
  subtype MOVEMENT_LIST is SOK_INPUT.KEY_LIST
   range SOK_INPUT.LEFT .. SOK_INPUT.DOWN;

  -- result of a movement try
  type RESULT_LIST is
   (REFUSED, DONE, BOX_MOVED, BOX_OK_MORE, BOX_OK_LESS);

  -- result of a movement to save
  subtype SAVED_RESULT_LIST is RESULT_LIST range DONE .. BOX_MOVED;

  -- what to save and give back to undo
  type SAVED_DATA_REC is record
    -- position of man before movement
    POS_ORIG  : SOK_TYPES.COORDINATE_REC;
    -- direction of movement
    MOVEMENT  : MOVEMENT_LIST;
    -- result
    RESULT   : SAVED_RESULT_LIST;
  end record;

  -- result of an undone movement
  subtype UNDO_RESULT_LIST is RESULT_LIST range DONE .. BOX_OK_LESS;


  -- try to do a movement
  -- Give frame, current position and movement to try
  procedure DO_MOVEMENT (
   FRAME    : in out SOK_TYPES.FRAME_TAB;
   POSITION : in out SOK_TYPES.COORDINATE_REC;
   MOVEMENT : in MOVEMENT_LIST;
   RESULT   : out RESULT_LIST);


  -- to undo a movement
  -- give frame, current position and movement which
  -- moved to current position
  -- indicate also if a box was moved
  procedure UNDO_MOVEMENT (
   FRAME      : in out SOK_TYPES.FRAME_TAB;
   SAVED_DATA : in SAVED_DATA_REC;
   RESULT        : out UNDO_RESULT_LIST;
   PREV_POSITION : out SOK_TYPES.COORDINATE_REC);


  -- raised on DO_MOVEMENT if movement is not coherent
  -- or     on UNDO_MOVEMENT if it is not coherent
  ILLEGAL_MOVEMENT : exception;

end SOK_MOVEMENT;