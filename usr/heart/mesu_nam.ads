with Mesu_Def;
package Mesu_Nam is

  -- The result. File name (or file template) YYyyMmDdHhMM.Pid
  subtype File_Name_Str is String (1 .. 16);

  Wild_Char : constant Character := '?';

  -- "YYyyMmDd" or "????????"
  subtype File_Date_Str is Mesu_Def.Date_Str;
  Wild_Date_Str : constant File_Date_Str := (others => Wild_Char);

  subtype File_Time_Str is Mesu_Def.Time_Str;
  Wild_Time_Str : constant File_Time_Str := (others => Wild_Char);

  -- from "000" to "999" or "???"
  subtype File_Pid_Str is String (1 .. 3);
  Wild_Pid_Str : constant File_Pid_Str := (others => Wild_Char);


  -- Build a file name (or a template if some '?')
  -- May raise File_Name_Error if some fields have wrong format
  --  or date is not valid
  function Build_File_Name (Date : File_Date_Str := Wild_Date_Str;
                            Time : File_Time_Str := Wild_Time_Str;
                            Pid  : File_Pid_Str  := Wild_Pid_Str)
   return File_Name_Str;

  -- Build a file name from a Mesure
  -- May raise File_Name_Error if some fields have wrong format
  --  or date is not valid
  function Build_File_Name (Mesure : Mesu_Def.Mesure_Rec) return File_Name_Str;

  -- Check wether fields are valid
  function Valid_File_Def (Date : File_Date_Str := Wild_Date_Str;
                           Time : File_Time_Str := Wild_Time_Str;
                           Pid  : File_Pid_Str  := Wild_Pid_Str;
                           Allow_Wild : Boolean := True)
   return Boolean;

  -- Split a file name (or a template)
  -- May raise File_Name_Error if some fields have wrong format
  --  or date is not valid
  procedure Split_File_Name (File_Name : in File_Name_Str;
                             Date      : out File_Date_Str;
                             Time      : out File_Time_Str;
                             Pid       : out File_Pid_Str);

  -- Check wether fields are valid
  function Valid_File_Name (File_Name : File_Name_Str) return Boolean;

  -- Find first file available for given date and pid
  -- By default '????" the first avilable
  -- Otherwise the given time if available, or next minute if available
  -- May return Wild_time_Str otherwise
  -- May raise File_Name_Error if date or pid has wild
  function Find_Slot (Date : File_Date_Str;
                      Time : File_Time_Str;
                      Pid  : File_Pid_Str) return File_Time_Str;


  File_Name_Error : exception;

end Mesu_Nam;

