with Mesu_Def;
package Mesu_Nam is

  -- The result. File name (or file template)
  subtype File_Name_Str is String (1 .. 12);

  Wild_Char : constant Character := '?';

  -- "YYyyMmDd" or "????????"
  subtype File_Date_Str is Mesu_Def.Date_Str;
  Wild_Date_Str : constant File_Date_Str := (others => Wild_Char);

  -- from "00" to "99" or "??"
  subtype File_No_Str is String (1 .. 2);
  Wild_No_Str : constant File_No_Str := (others => Wild_Char);

  -- from "000" to "999" or "???"
  subtype File_Pid_Str is String (1 .. 3);
  Wild_Pid_Str : constant File_Pid_Str := (others => Wild_Char);


  -- Build a file name (or a template if some '?')
  -- May raise File_Name_Error if some fields have wrong format
  --  or date is not valid
  function Build_File_Name (Date : File_Date_Str := Wild_Date_Str;
                            No   : File_No_Str   := Wild_No_Str;
                            Pid  : File_Pid_Str  := Wild_Pid_Str)
   return File_Name_Str;

  -- Check wether fields are valid
  function Valid_File_Def (Date : File_Date_Str := Wild_Date_Str;
                           No   : File_No_Str   := Wild_No_Str;
                           Pid  : File_Pid_Str  := Wild_Pid_Str)
   return Boolean;

  -- Split a file name (or a template)
  -- May raise File_Name_Error if some fields have wrong format
  --  or date is not valid
  procedure Split_File_Name (File_Name : in File_Name_Str;
                             Date      : out File_Date_Str;
                             No        : out File_No_Str;
                             Pid       : out File_Pid_Str);

  -- Check wether fields are valid
  function Valid_File_Name (File_Name : File_Name_Str) return Boolean;

  -- Find first file_no_str available for given date and pid
  -- May return Wild_No_Str if no more_slot available
  -- May raise File_Name_Error if date ir pid has wild
  function Find_Slot (Date : File_Date_Str;
                      Pid  : File_Pid_Str) return File_No_Str;


  File_Name_Error : exception;

end Mesu_Nam;

