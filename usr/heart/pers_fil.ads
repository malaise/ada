package Pers_Fil is

  -- Load the list from the file. (Erasing the current list)
  -- Current pos set to first item (if not empty)
  procedure Load;

  -- Save the list to file. (List not affected)
  procedure Save;

  -- Error on file open, create, read, write
  Io_Error : exception;
  -- Error on list write
  Full_List_Error : exception;
  -- Fatal internal error
  Pers_Fil_Internal_Error : exception;

end Pers_Fil;

