-- Opens a file and reads lines
-- Parses words separated by space or tab
with As.U.Utils;
with Text_Line;
generic

  -- If this comment string is set, then only significant lines
  -- (not empty nor starting with comment nor with comment as first word)
  -- are loaded
  Comment : in String := "";

package Get_Line is

  subtype Word_Txt is As.U.Asu_Us;
  subtype Word_Range is Positive;
  subtype Word_Count is Natural;
  subtype Line_Array is As.U.Utils.Asu_Array;
  subtype Line_Txt is As.U.Asu_Us;

  Name_Error : exception;
  Status_Error : exception renames Text_Line.Status_Error;
  Io_Error : exception renames Text_Line.Io_Error;
  End_Error : exception;

  -- Opens the file.
  -- Loads the first line
  procedure Open (File_Name : in String);

  -- Closes the file
  procedure Close;

  -- Loads next line of file
  procedure Read_Next_Line;

  --------------------------------------------------
  -- As soon as a line is loaded the following
  -- features are available without parsing the line
  --------------------------------------------------

  -- Current line number (not parsed)
  subtype Count is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
  subtype Positive_Count is Count range 1 .. Count'Last;
  function Get_Line_No return Positive_Count;

  -- Get the whole line (not parsed)
  procedure Get_Whole_Line (Line : in out Line_Txt);

  -- Get the first significant word of the line (not parsed)
  function Get_First_Word return String;

  ------------------------------------------------------------------
  -- The two following features trigger a parsing of the loaded line
  ------------------------------------------------------------------

  -- Number of words in currently loaded line
  function Get_Word_Number return Word_Count;

  -- Words of the currently loaded line
  function Get_Words return Line_Array;
  procedure Get_Words (Line : in out As.U.Utils.Asu_Ua.Unbounded_Array);

end Get_Line;

