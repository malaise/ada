with QUEUES;

-- salvage of movements for undo, save/restore ...
package body SOK_SAVE is

  package MOVEMENT_LIFO is new QUEUES.LIFO (
   SIZE => NBRE_SAVE,
   ITEM => SOK_MOVEMENT.SAVED_DATA_REC);

  INDEX_SAVE : POSITIVE range 1 .. NBRE_SAVE;

  -- when new frame, reset stack and photo
  procedure RESET is
    MOVEMENT : SOK_MOVEMENT.SAVED_DATA_REC;
  begin
    loop
      begin
        MOVEMENT := POP;
      exception
        when NO_MORE_SAVED_MOVEMENTS =>
          exit;
      end;
    end loop;
  end RESET;


  -- circular buffer of saved movements for undo
  procedure PUSH (MAN_MOVEMENT : in SOK_MOVEMENT.SAVED_DATA_REC) is
  begin
    MOVEMENT_LIFO.PUSH (MAN_MOVEMENT);
  exception
    when MOVEMENT_LIFO.LIFO_FULL =>
      -- lifo is full : make a space and retry
      MOVEMENT_LIFO.DISCARD_LAST;
      MOVEMENT_LIFO.PUSH (MAN_MOVEMENT);
  end PUSH;



  function POP return SOK_MOVEMENT.SAVED_DATA_REC is
    MOVEMENT : SOK_MOVEMENT.SAVED_DATA_REC;
  begin
    MOVEMENT_LIFO.POP (MOVEMENT);
    return MOVEMENT;
  exception
    when MOVEMENT_LIFO.LIFO_EMPTY =>
      raise NO_MORE_SAVED_MOVEMENTS;
  end POP;

  -- look first pushed or next pushed
  -- type LOOK_REF_LIST is (FIRST, NEXT);
  function LOOK (REF : LOOK_REF_LIST) return SOK_MOVEMENT.SAVED_DATA_REC is
    MOVEMENT : SOK_MOVEMENT.SAVED_DATA_REC;
  begin
    if REF = FIRST then
      INDEX_SAVE := 1;
    elsif INDEX_SAVE = NBRE_SAVE then
      raise NO_MORE_SAVED_MOVEMENTS;
    else
      INDEX_SAVE := INDEX_SAVE + 1;
    end if;

    MOVEMENT_LIFO.LOOK_LAST (MOVEMENT, INDEX_SAVE);
    return MOVEMENT;
  exception
    when MOVEMENT_LIFO.LIFO_EMPTY | MOVEMENT_LIFO.LIFO_NOT =>
      raise NO_MORE_SAVED_MOVEMENTS;
  end LOOK;

end SOK_SAVE;