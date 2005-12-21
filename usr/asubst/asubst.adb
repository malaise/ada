with Ada.Exceptions, Ada.Text_Io;
with Argument, Sys_Calls;
with Search_Pattern, Replace_Pattern, Substit;
procedure Asubst is

  Version : constant String  := "V2.8";

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
               & " [ { <option> } ] <find_pattern> <replace_pattern> [ { <file> } ]");
    Sys_Calls.Put_Line_Error (
     "or:    " & Argument.Get_Program_Name & " -h | --help | -v | --version");
    Sys_Calls.Put_Line_Error (
     "  Substitutes pattern in files, or from stdin to stdout if no file.");
  end Usage;

  procedure Help (New_Line : Boolean := True) is
  begin
    Usage;
    Sys_Calls.Put_Line_Error (
     "  <option> ::= -b | -i | -s | --");
    Sys_Calls.Put_Line_Error (
     "    -b or --basic for basic regex,");
    Sys_Calls.Put_Line_Error (
     "    -i or --ignorecase for case insensitive match,");
    Sys_Calls.Put_Line_Error (
     "    -s or --save for backup of original file,");
    Sys_Calls.Put_Line_Error (
     "    -q or --quiet for no printout,");
    Sys_Calls.Put_Line_Error (
     "    -n or --number for print number of substitutions,");
    Sys_Calls.Put_Line_Error (
     "    -v or --verbose for print each substitution,");
    Sys_Calls.Put_Line_Error (
     "    -- to stop options.");
    Sys_Calls.Put_Line_Error (
     "  <find_pattern> ::= <regex> | <multiple_regex>");
    Sys_Calls.Put_Line_Error (
     "    <multiple_regex> ::= { [ <regex> ] \n } [ <regex> ]");
    Sys_Calls.Put_Line_Error (
     "    A <regex> can contain ""\t"" (tab), ""\s"" (space) or ""\xIJ"" (hexa byte value)");
    Sys_Calls.Put_Line_Error (
     "    but can't contain ""\n"" (""\n"" matches the new_line character and is the");
    Sys_Calls.Put_Line_Error (
     "    delimiter of regexes).");
    Sys_Calls.Put_Line_Error (
     "    A single <regex> applies several times per line and can contain '^' or '$'.");
    Sys_Calls.Put_Line_Error (
     "    Each <regex> of <multiple_regex> applies to one line (once).");
    Sys_Calls.Put_Line_Error (
     "    The <multiple_regex> cannot have ""\n^"" or ""$\n"".");
    Sys_Calls.Put_Line_Error (
     "  <replace_pattern> is a string with ""\n"" (new_line), ""\t"" (tab), ""\s"" (space),");
    Sys_Calls.Put_Line_Error (
     "    ""\xIJ"" (hexa byte value), ""\RIJ"" or ""\rIJ"" (IJ in hexa, replaced by the");
    Sys_Calls.Put_Line_Error (
     "    string of the input text matching the IJth regex if \R, or matching the Jth");
    Sys_Calls.Put_Line_Error (
     "    substring of the Ith regex if \r), ""\U"" (start UPPERCASE conversion),");
    Sys_Calls.Put_Line_Error (
     "    ""\l"" (lowercase), ""\m"" (Mixed_Case), ""\c"" (stop case conversion). Any new");
    Sys_Calls.Put_Line_Error (
     "    conv replaces previous, case conv applies after (sub)string replacement.");
    Sys_Calls.Put_Line_Error (
     "    ""\R01"" <-> 1st <regex>, ""\R00"" <-> the <multiple_regex>, ""\ri0"" == ""\R0i"".");
    Sys_Calls.Put_Line_Error (
     "  Warning: regex are powerfull (see ""man 7 regex"") and automatic substitution");
    Sys_Calls.Put_Line_Error (
     "    can be dangerous, so use " & Argument.Get_Program_Name & " with caution:");
    Sys_Calls.Put_Line_Error (
     "    test pattern with ""echo string | " &  Argument.Get_Program_Name & " <search_pattern> <replace_patern>""");
    Sys_Calls.Put_Line_Error (
     "    and use -s option if unsure.");
    Sys_Calls.Set_Error_Exit_Code;
  end Help;

  procedure Error is
  begin
    Usage;
    Sys_Calls.Set_Error_Exit_Code;
  end Error;

  -- Option management, start of patterns
  Extended : Boolean := True;
  Case_Sensitive : Boolean := True;
  Backup : Boolean := False;
  type Verbose_List is (Quiet, File_Name, Subst_Nb, Verbose);
  Verbosity : Verbose_List := File_Name;
  -- Start index (in nb args) of patterns
  Start : Positive;
  -- Overall result
  Ok : Boolean;
  Nb_Subst : Natural;

begin
  -- Check nb of arguments
  if Argument.Get_Nbre_Arg = 1 then
    if Argument.Get_Parameter = "-v"
    or else Argument.Get_Parameter = "--version" then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & " " & Version);
      Sys_Calls.Set_Error_Exit_Code;
    elsif Argument.Get_Parameter = "-h"
    or else Argument.Get_Parameter = "--help" then
      Help;
    else
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
      Error;
    end if;
    return;
  elsif Argument.Get_Nbre_Arg < 2 then
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
    Error;
    return;
  end if;

  -- Parse options
  Start := 1;
  for I in 1 .. 4 loop
    if Argument.Get_Parameter (Occurence => I) = "--" then
      -- Force end of options
      Start := I + 1;
      exit;
    elsif Argument.Get_Parameter (Occurence => I) = "-b"
    or else Argument.Get_Parameter (Occurence => I) = "--basic" then
      -- Basic regex
      Extended := False;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-i"
    or else Argument.Get_Parameter (Occurence => I) = "--ignorecase" then
      -- Case insensitive match
      Case_Sensitive := False;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-s"
    or else Argument.Get_Parameter (Occurence => I) = "--save" then
      -- Make backup
      Backup := True;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-q"
    or else Argument.Get_Parameter (Occurence => I) = "--quiet" then
      -- Make backup
      Verbosity := Quiet;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-n"
    or else Argument.Get_Parameter (Occurence => I) = "--number" then
      -- Make backup
      Verbosity := Subst_Nb;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-v"
    or else Argument.Get_Parameter (Occurence => I) = "--verbose" then
      -- Make backup
      Verbosity := Verbose;
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
      Error;
      return;
  end;
  begin
    Replace_Pattern.Parse (
           Argument.Get_Parameter (Occurence => Start));
    Start := Start + 1;
  exception
    when Replace_Pattern.Parse_Error =>
      Error;
      return;
  end;

  -- Process files
  Ok := True;
  if Argument.Get_Nbre_Arg < Start then
    if Backup then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                & ": ERROR. Cannot make backup if no file name.");
      Ok := False;
    else
      begin
        Nb_Subst := Substit.Do_One_File (Substit.Std_In_Out, False, False);
      exception
        when Substit.Substit_Error =>
          Ok := False;
        when Error:others =>
          Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                    & ": EXCEPTION: " & Ada.Exceptions.Exception_Name (Error)
                    & " while processing stdin to stdout.");
          Ok := False;
      end;
    end if;
  else
    for I in Start .. Argument.Get_Nbre_Arg loop
      begin
        if Verbosity = Verbose then
          -- Put file name
          Ada.Text_Io.Put_Line (Argument.Get_Parameter (Occurence => I));
        end if;
        Nb_Subst := Substit.Do_One_File (
                      Argument.Get_Parameter (Occurence => I),
                      Backup, Verbosity = Verbose);
        if Verbosity = File_Name and then Nb_Subst /= 0 then
          -- Put file name if substitution occured
          Ada.Text_Io.Put_Line (Argument.Get_Parameter (Occurence => I));
        elsif Verbosity >= Subst_Nb then
          -- Put file name and nb of substitutions
          Ada.Text_Io.Put_Line (Argument.Get_Parameter (Occurence => I)
                              & Nb_Subst'Img);
        end if;
      exception
        when Substit.Substit_Error =>
          Ok := False;
        when Error:others =>
          Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                    & ": EXCEPTION: " & Ada.Exceptions.Exception_Name (Error)
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

