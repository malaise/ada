with AFPX, NULL_PROCEDURE;
generic
  -- Any initialisation to do after descriptor activation
  with procedure INIT_PROCEDURE;
  -- Any action to do when a fd_event has been received (see Afpx Fd_event)
  with procedure FD_CALLBACK is NULL_PROCEDURE;

function SELECT_FILE (DESCRIPTOR : AFPX.DESCRIPTOR_RANGE;
                      CURRENT_FILE : STRING;
                      FOR_READ : BOOLEAN) return STRING;


