-- Input and output file management
with Text_Line, Text_Char;
package Files is

  -- Open both files
  procedure Open (Spec_File_Name : in String);
  In_Error, Out_Error : exception;

  -- Get file descriptors
  function In_File return Text_Char.File_Type;
  function Out_File return Text_Line.File_Type;

  -- Close files
  -- If not success, body file is erased
  procedure Close (Success : in Boolean);

end Files;
  
