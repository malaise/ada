-- For each file provided as argument format and put the number of Ada
--   statements of this file, then put the formated total:
--   statements, then % of comments then number of lines
-- If -s, only put the total number of statments, not formated
with Argument, Basic_Proc;
with One_File_Statements;

procedure Astat is
  First : Positive := 1;
  Java_Syntax : Boolean := False;
  Details : Boolean := True;
begin
  if Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter (1) = "--help"
            or else Argument.Get_Parameter (1) = "-h") then
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
      & " [ -s | --silent ] [ -j | --java] { <file_name> }");
    return;
  end if;

  for I in 1 .. Argument.Get_Nbre_Arg loop
    if Argument.Get_Parameter (I) = "--silent"
    or else Argument.Get_Parameter (I) = "-s" then
      Details := False;
      First := First + 1;
    elsif Argument.Get_Parameter (I) = "--java"
    or else Argument.Get_Parameter (I) = "-j" then
      Java_Syntax := True;
      First := First + 1;
    else
      exit;
    end if;
  end loop;

  -- Put header of list
  if Details then
    One_File_Statements.Put_Header;
  end if;

  -- Store all files
  for Arg in First .. Argument.Get_Nbre_Arg loop
    -- One stat on each file
    One_File_Statements.Statements_Of_File (Argument.Get_Parameter (Arg),
                                            Java_Syntax, Details);
  end loop;

  -- Total
  One_File_Statements.Put_Total (Details);

end Astat;

