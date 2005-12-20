package Substit is

  Std_In_Out : constant String := "-";

  -- Process one file (stdin -> stdout if File_Name is Std_In_Out)
  -- Make a backup file, display substitutions on option
  -- Return the number of substitutions
  function Do_One_File (File_Name : String;
                        Backup    : Boolean;
                        Verbose   : Boolean) return Natural;
  -- Error handled and traced by Do_One
  Substit_Error : exception;

end Substit;

