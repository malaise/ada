with MESU_DEF;
with MESU_NAM;
with PERS_DEF;
package MESU_FIL is


  -- Load a mesure from file.
  -- Pid and date of the mesure are extracted from file name
  -- Wild cards are not allowed
  function LOAD (FILE_NAME : MESU_NAM.FILE_NAME_STR)
  return MESU_DEF.MESURE_REC;

  -- Save a mesure in a file
  -- Pid and date of the mesure are used to build the file name
  procedure SAVE (FILE_NO : in MESU_NAM.FILE_NO_STR;
                  MESURE  : in MESU_DEF.MESURE_REC);

  -- Delete a mesure file
  -- Pid and date of the mesure are used to build the file name
  procedure DELETE (FILE_NAME : in MESU_NAM.FILE_NAME_STR);

  -- File not found, IO error
  IO_ERROR : exception;

  FILE_NAME_ERROR : exception renames MESU_NAM.FILE_NAME_ERROR;

  -- May be raised by load
  FILE_NOT_FOUND_ERROR : exception;

end MESU_FIL;