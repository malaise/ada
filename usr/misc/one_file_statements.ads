-- Compute (and optionally put) the number of Ada/Java statements of file
--  the percent number of comments and the numbers of lines

package One_File_Statements is

  -- Put header (for Statements_Of_File with Summary)
  procedure Put_Header;

  -- Compute statements for this file
  --  if Java comments, then use ("//" or /*.. */) instead of Ada ("--")
  --  add numbers of this file to total
  --  if Summary, then put formated 3 numbers for this file
  procedure Statements_Of_File (
             File_Name : String;
             Java_Syntax : Boolean := False;
             Summary : in Boolean := True);

  -- Put and reset the total so far
  --  if Summary, then put the formated total (3 values)
  --  else put the unformated total of statements so far
  procedure Put_Total (Summary : in Boolean);

end One_File_Statements;

