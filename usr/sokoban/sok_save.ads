with SOK_TYPES;
with SOK_MOVEMENT;

-- salvage of movements for undo, save/restore ...
package SOK_SAVE is
  -- Max nbre of saved movements
  NBRE_SAVE : constant := 2000; -- 64 Kb

  -- when new frame, reset stack
  procedure RESET;

  -- circular buffer of saved movements for undo
  procedure PUSH (MAN_MOVEMENT : in SOK_MOVEMENT.SAVED_DATA_REC);
  function POP  return SOK_MOVEMENT.SAVED_DATA_REC;


  -- look first pushed or next pushed
  type LOOK_REF_LIST is (FIRST, NEXT);
  -- look first will return first pushed movemtny and so on ...
  function LOOK (REF : LOOK_REF_LIST) return SOK_MOVEMENT.SAVED_DATA_REC;


  -- on pop if circular buffer is empty
  -- on look if no more movement to look
  NO_MORE_SAVED_MOVEMENTS : exception;

end SOK_SAVE;