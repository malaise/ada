with AFPX;
generic
  -- Any initialisation to do after descriptor activation
  with procedure INIT_PROCEDURE;
function SELECT_FILE (DESCRIPTOR : AFPX.DESCRIPTOR_RANGE;
                      CURRENT_FILE : STRING;
                      FOR_READ : BOOLEAN) return STRING;


