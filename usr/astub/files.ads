-- Input and output file management
with Text_Line, Text_Char;
package Files is

  -- Open both files
  procedure Open (Spec_File_Name : in String);
  In_Error, Out_Error : exception;

  -- File descriptor of inputs
  In_File : Text_Char.File_Type;
  -- File descriptor of outputs
  Out_File : Text_Line.File_Type;

  -- What to do with result file
  -- Keep it (success)
  -- Remove_If_Not_Keep (error)
  -- Remove (success empty)
  type Result_Action_List is (Keep, Remove_If_Not_Keep, Remove);
  -- Close files
  procedure Close (Action : in Result_Action_List);

end Files;
  
