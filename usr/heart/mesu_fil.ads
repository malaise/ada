with Mesu_Def;
with Mesu_Nam;
with Pers_Def;
package Mesu_Fil is


  -- Load a mesure from file.
  -- Pid and date of the mesure are extracted from file name
  -- Wild cards are not allowed
  function Load (File_Name : Mesu_Nam.File_Name_Str)
  return Mesu_Def.Mesure_Rec;

  -- Save a mesure in a file
  -- Pid and date of the mesure are used to build the file name
  procedure Save (File_No : in Mesu_Nam.File_No_Str;
                  Mesure  : in Mesu_Def.Mesure_Rec);

  -- Delete a mesure file
  -- Pid and date of the mesure are used to build the file name
  procedure Delete (File_Name : in Mesu_Nam.File_Name_Str);

  -- File not found, IO error
  Io_Error : exception;

  File_Name_Error : exception renames Mesu_Nam.File_Name_Error;

  -- May be raised by load
  File_Not_Found_Error : exception;

end Mesu_Fil;