
with MESU_NAM;
-- Edition, Creation, deletion of mesure
package MESU_EDI is

  -- Edit a mesure.
  -- If date or person changes, then the file name may be affected.
  -- If FILE_NAME is empty as input, then it is a creation and file_name
  --  is affected
  -- If EXIT_PROGRAM then FILE_NAME is not significant
  -- If FILE_NAME is set to empty, the edition has been canceled
  procedure EDIT (FILE_NAME : in out MESU_NAM.FILE_NAME_STR;
                  EXIT_PROGRAM : out BOOLEAN);

  -- Delete a mesure.
  -- If EXIT_PROGRAM then FILE_NAME is not significant
  -- If FILE_NAME is set to empty, the edition has been canceled
  procedure DELETE (FILE_NAME : in out MESU_NAM.FILE_NAME_STR;
                    EXIT_PROGRAM : out BOOLEAN);

end MESU_EDI;
