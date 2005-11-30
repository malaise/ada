package Substit is

  Std_In_Out : constant String := "-";

  -- Process one file (stdin -> stdout if File_Name is Std_In_Out)
  procedure Do_One_File (File_Name : in String; Nb_Pattern : in Positive);
  -- Error handled and traced by Do_One
  File_Error : exception;

end Substit;

