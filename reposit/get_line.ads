-- Opens a file and reads lines
-- Parses words separated by space or tab
with Text_Io;
with Text_Handler;

generic

  -- These two are used while parsing the line
  Max_Word_Len : in Positive;
  Max_Word_Nb  : in Positive;
  -- Only this one is used while loading a line
  Max_Line_Len : in Positive;
  -- If this comment string is set, then only significant lines
  -- (not empty nor starting with comment nor with comment as first word)
  -- are loaded
  Comment : in String := "";

package Get_Line is

  subtype Word_Txt is Text_Handler.Text(Max_Len => Max_Word_Len);
  subtype Word_Range is Positive range 1 .. Max_Word_Nb;
  subtype Word_Count is Natural  range 0 .. Max_Word_Nb;
  type Line_Array is array (Word_Range) of Word_Txt;
  subtype Line_Txt is Text_Handler.Text(Max_Len => Max_Line_Len);

  -- Opens the file. Exceptions are the one of Text_Io.Open (In_File)
  -- Loads the first line
  procedure Open (File_Name : in String);

  -- Closes the file
  -- Exceptions are the one of Text_Io.Close
  procedure Close;

  -- The following features may raise
  Not_Open : exception;

  -- Loads next line of file
  procedure Read_Next_Line;
  No_More_Line   : exception;
  Line_Too_Long  : exception;

  --------------------------------------------------
  -- As soon as a line is loaded the following
  -- features are available without parsing the line
  --------------------------------------------------

  -- Current line number (not parsed)
  function Get_Line_No return Text_Io.Positive_Count;

  -- Get the whole line (not parsed)
  procedure Get_Whole_Line (Line : in out Line_Txt);

  -- Get the first significant word of the line (not parsed)
  function Get_First_Word return String;

  --------------------------------------------------
  -- The two following features trigger a parsing
  --  of the loaded line and may raise
  --------------------------------------------------
  Too_Many_Words : exception;
  Word_Too_Long  : exception;

  -- Number of words in currently loaded line
  function Get_Word_Number return Word_Count;

  -- Words of the currently loaded line
  procedure Get_Words (Line : in out Line_Array);

end Get_Line;

