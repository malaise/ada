with Ada.Exceptions;
with Argument, Sys_Calls;
with Search_Pattern, Replace_pattern, Substit;
procedure Asubst is
  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
               & " <find_pattern> <replace_pattern> [ { <file> } ]");
    Sys_Calls.Put_Line_Error (
     " <find_pattern> ::= <regex>   |   { [ <regex> ] \n }");
    Sys_Calls.Put_Line_Error (
     " <replace_pattern> ::= string with \n \t or \&");
    Sys_Calls.Put_Line_Error (
     "  \n for New line, \t for (horiz) tab, \& for found string");
    Sys_Calls.Set_Error_Exit_Code;
  end Usage;

  Nb_Patterns : Positive;
  Ok : Boolean;
begin
  -- Check nb of arguments
  if Argument.Get_Nbre_Arg < 2 then
    Usage;
    return;
  end if;

  -- Parse both patterns
  begin
    Nb_Patterns := Search_Pattern.Parse (
         Argument.Get_Parameter (Occurence => 1));
  exception
    when Search_Pattern.Parse_Error =>
      Usage;
      return;
  end;
  Replace_Pattern.Parse (
         Argument.Get_Parameter (Occurence => 2));

  -- Process files
  Ok := True;
  if Argument.Get_Nbre_Arg = 2 then
    begin
      Substit.Do_One_File (Substit.Std_In_Out, Nb_Patterns);
    exception
      when Substit.File_Error =>
        Ok := False;
      when Error:Others =>
        
        Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                  & " Exception " & Ada.Exceptions.Exception_Name (Error)
                  & " while processing stdin to stdout.");
        Ok := False;
    end;
  else
    for I in 3 .. Argument.Get_Nbre_Arg loop
      begin
        Substit.Do_One_File (Argument.Get_Parameter (Occurence => I),
                             Nb_Patterns);
      exception
        when Substit.File_Error =>
          Ok := False;
        when Error:Others =>
          Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                    & " Exception " & Ada.Exceptions.Exception_Name (Error)
                    & " while processing file "
                    & Argument.Get_Parameter (Occurence => I) & ".");
          Ok := False;
      end;
    end loop;
  end if;

  if not Ok then
    Sys_Calls.Set_Error_Exit_Code;
  end if;
end Asubst;

