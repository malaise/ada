-- For each file provided as argument format and put the number of Ada
--   statements of this file, then put the formated total
-- If -s, only put the total not formated
with Argument;
with One_File_Statements;

procedure Astat is
  First : Positive := 1;
  Put_It : Boolean := True;
begin

  if Argument.Get_Nbre_Arg >= 1
  and then Argument.Get_Parameter(1) = "-s" then
    First := 2;
    Put_It := False;
  end if;

  -- Store all files
  for Arg in First .. Argument.Get_Nbre_Arg loop
    -- One stat on each file
    One_File_Statements.Print_Statements_Of_File(Argument.Get_Parameter(Arg),
                                                 Put_It);
  end loop;

  -- Total
  One_File_Statements.Print_Statements_Of_File("", Put_It);

end Astat;
