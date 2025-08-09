
with Mesu_Nam;
-- Edition, Creation, deletion of mesure
package Mesu_Edi is

  -- Edit a mesure.
  -- If date, time or person changes, then File_Name may be affected,
  --  the new mesure file is created but the previous file is NOT deleted
  -- If File_Name is empty as input, then it is a creation and File_Name
  --  is affected, and the mesure file is created
  -- If File_Name is set to empty, the edition has been canceled
  Empty_File_Name : Mesu_Nam.File_Name_Str := (others => ' ');
  procedure Edit (File_Name : in out Mesu_Nam.File_Name_Str);

  -- Clone a mesure: create a new file from File_Name (no Bpms) and edit it
  -- If File_Name is set to empty, the cloning has been canceled
  procedure Clone (File_Name : in out Mesu_Nam.File_Name_Str);

  -- Delete a mesure.
  -- NOTE: The mesure file is NOT deleted yet
  -- If File_Name is set to empty, the deletion has been canceled
  procedure Delete (File_Name : in out Mesu_Nam.File_Name_Str);

end Mesu_Edi;

