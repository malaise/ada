-- For each file provided as argument format and put the number of Ada
--   statements of this file, then put the formated total
-- If -s, only put the total not formated
with Argument;
with One_File_Statements;

procedure Astat is
  First : Positive := 1;
  Put_It : Boolean := True;
begin

  for I in 1 .. Argument.Get_Nbre_Arg loop
    if Argument.Get_Parameter(I) = "--silent"
    or else Argument.Get_Parameter(I) = "-s" then
      Put_It := False;
      First := First + 1;
    elsif Argument.Get_Parameter(I) = "--java"
    or else Argument.Get_Parameter(I) = "-j" then
      One_File_Statements.Java_Syntax := True;
      First := First + 1;
    else
      exit;
    end if;
  end loop;

  -- Store all files
  for Arg in First .. Argument.Get_Nbre_Arg loop
    -- One stat on each file
    One_File_Statements.Print_Statements_Of_File(Argument.Get_Parameter(Arg),
                                                 Put_It);
  end loop;

  -- Total
  One_File_Statements.Print_Statements_Of_File("", Put_It);

end Astat;

