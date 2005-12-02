with Ada.Exceptions;
with Argument, Sys_Calls;
with Search_Pattern, Replace_Pattern, Substit;
procedure Asubst is

  Version : constant String  := "V1.2";
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

  Ok : Boolean;
begin
  -- Check nb of arguments
  if Argument.Get_Nbre_Arg = 1 then
    if Argument.Get_Parameter = "-v"
    or else Argument.Get_Parameter = "--version" then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & " " & Version);
      Sys_Calls.Set_Error_Exit_Code;
    else
      Usage;
    end if;
    return;
  elsif Argument.Get_Nbre_Arg < 2 then
    Usage;
    return;
  end if;

  -- Parse both patterns
  begin
    Search_Pattern.Parse (
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
      Substit.Do_One_File (Substit.Std_In_Out);
    exception
      when Substit.Substit_Error =>
        Ok := False;
      when Error:others =>
        
        Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                  & " Exception " & Ada.Exceptions.Exception_Name (Error)
                  & " while processing stdin to stdout.");
        Ok := False;
    end;
  else
    for I in 3 .. Argument.Get_Nbre_Arg loop
      begin
        Substit.Do_One_File (Argument.Get_Parameter (Occurence => I));
      exception
        when Substit.Substit_Error =>
          Ok := False;
        when Error:others =>
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

