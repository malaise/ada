-- Compute (and optionally put) the number of Ada statements of file
--  the number of comments and the numbers of lines

package One_File_Statements is

  -- Java comments ("//" or /*.. */) instead of Ada ("--")
  Java_Syntax : Boolean := False;

  -- If File_Name is empty,
  --  if Summary put formated total so far and reset it
  --  else put total of statements so far and reset it
  -- Else
  --   add numbers of this file to total
  --   if Summary put formated numbers for this file
  procedure Print_Statements_Of_File (
             File_Name : String;
             Summary : in Boolean := True);

end One_File_Statements;

