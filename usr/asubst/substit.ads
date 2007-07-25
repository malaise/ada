package Substit is

  Std_In_Out : constant String := "-";

  subtype Long_Long_Natural is Long_Long_Integer
                                range 0 .. Long_Long_Integer'Last;

  -- Process one file (stdin -> stdout if File_Name is Std_In_Out)
  -- Make a backup file, display substitutions, and test mode on option
  -- Return the number of substitutions
  function Do_One_File (File_Name : String;
                        Max_Subst : Long_Long_Natural;
                        Backup    : Boolean;
                        Verbose   : Boolean;
                        Grep      : Boolean;
                        Test      : Boolean) return Long_Long_Natural;
  -- Error handled and traced by Do_One
  Substit_Error : exception;

end Substit;

