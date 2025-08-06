with Mesu_Def;
with Mesu_Nam;
package Mesu_Fil is

  -- File format supported by Load
  -- Binary: same as in Mesu_Def.Mesure_Rec but without pid nor date
  -- Text: 3 for Sampling delta, 20 for comment, 6x3 Bmps, Nx3 Mesures

  -- Load a mesure from file.
  -- Pid and date of the mesure are extracted from file name
  -- Wild cards are not allowed
  function Load (File_Name : Mesu_Nam.File_Name_Str)
  return Mesu_Def.Mesure_Rec;

  -- Save a mesure in a file (Text format)
  -- Pid and date of the mesure are used to build the file name
  procedure Save (File_No : in Mesu_Nam.File_No_Str;
                  Mesure  : in Mesu_Def.Mesure_Rec);

  -- Delete a mesure file
  -- Pid and date of the mesure are used to build the file name
  procedure Delete (File_Name : in Mesu_Nam.File_Name_Str);

  -- File not found, Io error
  Io_Error : exception;

  File_Name_Error : exception renames Mesu_Nam.File_Name_Error;

  -- May be raised by load
  File_Not_Found_Error : exception;

end Mesu_Fil;

