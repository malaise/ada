package PERS_FIL is

  -- Load the list from the file. (Erasing the current list)
  -- Current pos set to first item (if not empty)
  procedure LOAD;

  -- Save the list to file. (List not affected)
  procedure SAVE;

  -- Error on file open, create, read, write
  IO_ERROR : exception;
  -- Error on list write
  FULL_LIST_ERROR : exception;
  -- Fatal internal error
  PERS_FIL_INTERNAL_ERROR : exception;

end PERS_FIL;

