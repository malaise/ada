
with Mesu_Nam;
-- Edition, Creation, deletion of mesure
package Mesu_Edi is

  -- Edit a mesure.
  -- If date or person changes, then the file name may be affected.
  -- If File_Name is empty as input, then it is a creation and file_name
  --  is affected
  -- If File_Name is set to empty, the edition has been canceled
  procedure Edit (File_Name : in out Mesu_Nam.File_Name_Str);

  -- Clone a mesure: create a new file name an edit it
  procedure Clone (File_Name : in out Mesu_Nam.File_Name_Str);

  -- Delete a mesure.
  -- If File_Name is set to empty, the edition has been canceled
  procedure Delete (File_Name : in out Mesu_Nam.File_Name_Str);

end Mesu_Edi;

