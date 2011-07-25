-- Compute (and optionally put) the number of Ada statements of file
package One_File_Statements is

  -- If File_Name is empty,
  --  if Put_it put formated total so far and reset it
  --  else put total total so far and reset it
  -- Else
  --   add nb of statements of this file to total
  --   if Put_it put formated nb for this file
  procedure Print_Statements_Of_File (
             File_Name : String;
             Put_It : in Boolean := True);

end One_File_Statements;
