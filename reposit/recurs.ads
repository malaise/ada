-- executes the procedure DO_IN_DIR in the current directory and
--  in each directory of the sub-tree

-- Displays "==> COMPLETE_PATH <==" when entering each dir, or not
-- Do action in current directory also, or not
-- Stop after one level of subdirectories or not
-- Do action only in leaves (directory with no subdirectory)
-- Stops on error (DO_IN_DIR returning FALSE), or not
generic
  with function DO_IN_DIR return BOOLEAN;
procedure RECURS (
 NAME_OF_DIR : in BOOLEAN := TRUE;
 IN_CURRENT : in BOOLEAN := TRUE;
 FIRST_LEVEL_ONLY : in BOOLEAN := FALSE;
 LEAVES_ONLY : in BOOLEAN := FALSE;
 STOP_ON_ERROR : in BOOLEAN := TRUE);

