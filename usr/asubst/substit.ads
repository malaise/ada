with Num_Match;
package Substit is

  Std_In_Out : constant String := "-";

  package Subst_Match is new Num_Match (Integer);
  subtype Long_Long_Natural is Subst_Match.Natural_Type;

  -- Process one file (stdin -> stdout if File_Name is Std_In_Out)
  -- Make a backup file, display substitutions, and test mode on option
  -- Return the number of substitutions
  function Do_One_File (File_Name   : String;
                        Tmp_Dir     : String;
                        Delimiter   : String;
                        Match_Range : String;
                        Backup      : Boolean;
                        Verbose     : Boolean;
                        Grep        : Boolean;
                        Line_Nb     : Boolean;
                        Test        : Boolean) return Natural;
  -- Error handled and traced by Do_One
  Substit_Error : exception;

end Substit;

