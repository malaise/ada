with Ada.Exceptions;
with Argument, Sys_Calls;
with Search_Pattern, Replace_Pattern, Substit;
procedure Asubst is

  Version : constant String  := "V1.4";
  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
               & " [ <option> ] <find_pattern> <replace_pattern> [ { <file> } ]");
    Sys_Calls.Put_Line_Error (
     "  <option> ::= -b | -i | -s | --");
    Sys_Calls.Put_Line_Error (
     "  -b for basic regex, -i for case insensitive match,");
    Sys_Calls.Put_Line_Error (
     "  -s for backup of original file, -- to stop options.");
    Sys_Calls.Put_Line_Error (
     "  <find_pattern> ::= <regex> | <multiple_regex>");
    Sys_Calls.Put_Line_Error (
     "  <multiple_regex> ::= { [ <regex> ] \n } [ <regex> ]");
    Sys_Calls.Put_Line_Error (
     "  <replace_pattern> ::= string with \n (new line), \t (tab)");
    Sys_Calls.Put_Line_Error (
     "                                 or \& (the matching string).");
    Sys_Calls.Put_Line_Error (
     "  A <regex> does not contain ""\n"".");
    Sys_Calls.Put_Line_Error (
     "  A single <regex> is applied several times per line and can contain '^' or '$'.");
    Sys_Calls.Put_Line_Error (
     "  Each <regex> of <multiple_regex> applied to one line (once).");
    Sys_Calls.Put_Line_Error (
     "  The <multiple_regex> cannot have ""\n^"" or ""$\n"".");
    Sys_Calls.Put_Line_Error (
     "  Regex shall not be ambiguous, so be careful with '*'.");
    Sys_Calls.Put_Line_Error (
     "  More generally, regex are powerfull (see ""man 7 regex"") and automatic substitution");
    Sys_Calls.Put_Line_Error (
     "   can be dangerous, so use """ & Argument.Get_Program_Name & """ with caution.");
    Sys_Calls.Set_Error_Exit_Code;
  end Usage;


  -- Option management, start of patterns
  Extended : Boolean := True;
  Case_Sensitive : Boolean := True;
  Backup : Boolean := False;
  Start : Positive;
  
  -- Overall result
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

  -- Parse options
  Start := 1;
  for I in 1 .. 4 loop
    if Argument.Get_Parameter (Occurence => I) = "--" then
      -- Force end of options
      Start := I + 1;
      exit;
    elsif Argument.Get_Parameter (Occurence => I) = "-b" then
      -- Basic regex
      Extended := False;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-i" then
      -- Case insensitive match
      Case_Sensitive := False;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-s" then
      -- Make backup
      Backup := True;
      Start := I + 1;
    else
      -- Not an option
      exit;
    end if;
  end loop;

  -- Parse both patterns
  begin
    Search_Pattern.Parse (
         Argument.Get_Parameter (Occurence => Start),
         Extended, Case_Sensitive);
    Start := Start + 1;
  exception
    when Search_Pattern.Parse_Error =>
      Usage;
      return;
  end;
  Replace_Pattern.Parse (
         Argument.Get_Parameter (Occurence => Start));
  Start := Start + 1;

  -- Process files
  Ok := True;
  if Argument.Get_Nbre_Arg < Start then
    if Backup then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                & " ERROR. Cannot make backup if no file name.");
      Ok := False;
      begin
        Substit.Do_One_File (Substit.Std_In_Out, False);
      exception
        when Substit.Substit_Error =>
          Ok := False;
        when Error:others =>
          Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                    & " EXCEPTION " & Ada.Exceptions.Exception_Name (Error)
                    & " while processing stdin to stdout.");
          Ok := False;
      end;
    end if;
  else
    for I in Start .. Argument.Get_Nbre_Arg loop
      begin
        Substit.Do_One_File (Argument.Get_Parameter (Occurence => I), Backup);
      exception
        when Substit.Substit_Error =>
          Ok := False;
        when Error:others =>
          Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                    & " EXCEPTION " & Ada.Exceptions.Exception_Name (Error)
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

