-- Compute (and optionally put) the number of Ada/Java statements of file
--  the percent number of comments and the numbers of lines

package One_File_Statements is

  Default_Width : constant := 50;
  -- Put header (for Statements_Of_File with Summary) for a given
  -- width for file names
  procedure Put_Header (Width : in Positive := Default_Width);

  -- Compute statements for this file
  --  if Java comments, then use ("//" or /*.. */) instead of Ada ("--")
  --  add numbers of this file to total
  --  if Summary, then put formated 3 numbers for this file
  procedure Statements_Of_File (
             File_Name   : in String;
             Java_Syntax : in Boolean := False;
             Summary     : in Boolean := True;
             Width       : in Positive := Default_Width);

  -- Put and reset the total so far
  --  if Summary, then put the formated total (3 values) with a given width
  --    for file name
  --  else put the unformated total of statements so far
  procedure Put_Total (Summary : in Boolean;
                       Width   : in Positive := Default_Width);

end One_File_Statements;

