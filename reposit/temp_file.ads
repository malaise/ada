-- Creates a temporary file (empty and closed) in Temp_Dir
-- Returns the file name (Temp_Dir & "/Temp_File.xxx")
package Temp_File is
  function Create (Temp_Dir : String) return String;
  -- If directory is not accessible (does not exists or is not user RWX)
  Invalid_Dir: exception;
  -- If none of the .000 to .999 file could be created
  No_More_Temp : exception;
end Temp_File;

