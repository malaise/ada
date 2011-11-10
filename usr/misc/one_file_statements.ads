-- Compute (and optionally put) the number of Ada statements of file
--  the number of comments and the numbers of lines

package One_File_Statements is

  -- If File_Name is empty,
  --  if Put_It put formated total so far and reset it
  --  else put total of statements so far and reset it
  -- Else
  --   add numbers of this file to total
  --   if Put_It put formated numbers for this file
  procedure Print_Statements_Of_File (
             File_Name : String;
             Put_It : in Boolean := True);

end One_File_Statements;

