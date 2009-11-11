-- Manage the file listing file names
package File_Mng is

  -- Open the file of files
  -- May raise Open_Error
  Open_Error : exception;
  Stdin : constant String := "-";
  procedure Open (File_Name : in String);

  -- Get next file name from file
  -- May raise End_Error or Io_Error (and closes file)
  End_Error : exception;
  Io_Error : exception;
  function Get_Next_File return String;

end File_Mng;

