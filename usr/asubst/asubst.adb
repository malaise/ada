with Ada.Exceptions, Ada.Text_Io;
with Environ, Argument, Sys_Calls;
with Search_Pattern, Replace_Pattern, Substit, File_Mng, Debug;
procedure Asubst is

  Version : constant String  := "V2.13";

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
               & " [ { <option> } ] <find_pattern> <replace_string> [ { <file> } ]");
    Sys_Calls.Put_Line_Error (
     "or:    " & Argument.Get_Program_Name & " -h | --help | -v | --version");
    Sys_Calls.Put_Line_Error (
     "  Substitutes pattern in files, or from stdin to stdout if no file.");
  end Usage;

  procedure Help (New_Line : Boolean := True) is
  begin
    Usage;
    Sys_Calls.Put_Line_Error (
     "  <option> ::= -b | -f |  -i | -m <max> | -n | -q | -s | -u | -v | --");
    Sys_Calls.Put_Line_Error (
     "    -a or --ascii for pure ASCII processing,");
    Sys_Calls.Put_Line_Error (
     "    -b or --basic for basic regex,");
    Sys_Calls.Put_Line_Error (
     "    -f or --file to indicate that <file> will be a list of file names,");
    Sys_Calls.Put_Line_Error (
     "    -i or --ignorecase for case insensitive match,");
    Sys_Calls.Put_Line_Error (
     "    -m <max> or --max=<max> for stop processing file after <max> substitutions,");
    Sys_Calls.Put_Line_Error (
     "    -n or --number for print number of substitutions,");
    Sys_Calls.Put_Line_Error (
     "    -q or --quiet for no printout,");
    Sys_Calls.Put_Line_Error (
     "    -s or --save for backup of original file,");
    Sys_Calls.Put_Line_Error (
     "    -u or --utf8 for processing utf-8 sequences,");
    Sys_Calls.Put_Line_Error (
     "    -v or --verbose for print each substitution,");
    Sys_Calls.Put_Line_Error (
     "    -- to stop options.");
    Sys_Calls.Put_Line_Error (
     "  Also set ASUBST_UTF8 env variable for utf-8 processing by default.");
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
     "  <replace_string> is a string with ""\n"" (new_line), ""\t"" (tab), ""\s"" (space),");
    Sys_Calls.Put_Line_Error (
     "    ""\xIJ"" (hexa byte value), ""\RIJ"" or ""\rIJ"" (IJ in hexa, replaced by the");
    Sys_Calls.Put_Line_Error (
     "    string of the input text matching the IJth regex if \R, or matching the Jth");
    Sys_Calls.Put_Line_Error (
     "    substring of the Ith regex if \r), ""\u"" (start UPPERCASE conversion),");
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
     "    test pattern with ""echo string | " &  Argument.Get_Program_Name & " <search_pattern> <replace_string>""");
    Sys_Calls.Put_Line_Error (
     "    and use -s option if unsure.");
    Sys_Calls.Set_Error_Exit_Code;
  end Help;

  procedure Error is
  begin
    Usage;
    Sys_Calls.Set_Error_Exit_Code;
  end Error;

  -- Env management
  Utf8_Var_Name : constant String := "ASUBST_UTF8";
  -- Option management
  Extended : Boolean := True;
  File_Of_Files : Boolean := False;
  Case_Sensitive : Boolean := True;
  Max : Substit.Long_Long_Natural := 0;
  type Verbose_List is (Quiet, Put_File_Name, Put_Subst_Nb, Verbose);
  Verbosity : Verbose_List := Put_File_Name;
  Backup : Boolean := False;
  Utf8 : Boolean := False;
  -- Start index (in nb args) of patterns
  Start : Positive;
  -- Overall result
  Ok : Boolean;
  -- Nb subst per file
  Nb_Subst : Substit.Long_Long_Natural;

  -- Process one file
  procedure Do_One_File (File_Name : in String) is
  begin
    if Verbosity = Verbose then
      -- Put file name
      Ada.Text_Io.Put_Line (File_Name);
    end if;
    Nb_Subst := Substit.Do_One_File (
                  File_Name,
                  Max, Backup, Verbosity = Verbose);
    if Verbosity = Put_File_Name and then Nb_Subst /= 0 then
      -- Put file name if substitution occured
      Ada.Text_Io.Put_Line (File_Name);
    elsif Verbosity >= Put_Subst_Nb then
      -- Put file name and nb of substitutions
      Ada.Text_Io.Put_Line (File_Name & Nb_Subst'Img);
    end if;
  exception
    when Substit.Substit_Error =>
      Ok := False;
    when Error:others =>
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                & ": EXCEPTION: " & Ada.Exceptions.Exception_Name (Error)
                & " while processing file "
                & File_Name & ".");
      Ok := False;
  end Do_One_File;

begin
  -- Set default UTF8 processing if env variable set
  Utf8 := Environ.Is_Yes (Utf8_Var_Name);

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
    elsif Argument.Get_Parameter (Occurence => I) = "-a"
    or else Argument.Get_Parameter (Occurence => I) = "--ascii" then
      -- Force ASCII processing even if ENV was set
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option ascii");
      end if;
      Utf8 := False;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-b"
    or else Argument.Get_Parameter (Occurence => I) = "--basic" then
      -- Basic regex
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option basic regex");
      end if;
      Extended := False;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-f"
    or else Argument.Get_Parameter (Occurence => I) = "--file" then
      -- The file will be a list of files
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option file of files");
      end if;
      File_Of_Files := True;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-i"
    or else Argument.Get_Parameter (Occurence => I) = "--ignorecase" then
      -- Case insensitive match
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option ignore case");
      end if;
      Case_Sensitive := False;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-m"
    or else (Argument.Get_Parameter (Occurence => I)'Length > 6
     and then Argument.Get_Parameter (Occurence => I)(1 .. 6) = "--max=" ) then
      -- Stop each file after <max> substitutions
      begin
        if Argument.Get_Parameter (Occurence => I) = "-m" then
          -- -m <max>
          Max := Substit.Long_Long_Natural'Value (
            Argument.Get_Parameter (Occurence => I + 1));
          Start := I + 2;
        else
          -- --max=<max>
          declare
            Str : constant String := Argument.Get_Parameter (Occurence => I);
          begin
            Max := Substit.Long_Long_Natural'Value (Str (7 .. Str'Last));
          end;
          Start := I + 1;
        end if;
      exception
        when others =>
          Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
             & ": Syntax ERROR. Invalid specification of max subtitutions.");
          Error;
      end;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option max =" & Max'Img);
      end if;
    elsif Argument.Get_Parameter (Occurence => I) = "-n"
    or else Argument.Get_Parameter (Occurence => I) = "--number" then
      -- Put number of substitutions
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option put numbers");
      end if;
      Verbosity := Put_Subst_Nb;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-q"
    or else Argument.Get_Parameter (Occurence => I) = "--quiet" then
      -- Quiet mode
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option quiet");
      end if;
      Verbosity := Quiet;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-s"
    or else Argument.Get_Parameter (Occurence => I) = "--save" then
      -- Make backup
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option make backup");
      end if;
      Backup := True;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-u"
    or else Argument.Get_Parameter (Occurence => I) = "--utf8" then
      -- Process utf-8 sequences
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option utf8");
      end if;
      Utf8 := True;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I) = "-v"
    or else Argument.Get_Parameter (Occurence => I) = "--verbose" then
      -- Verbose put each substit
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option verbose");
      end if;
      Verbosity := Verbose;
      Start := I + 1;
    elsif Argument.Get_Parameter (Occurence => I)(1) = '-' then
      -- Not a valid option
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                              & ": Syntax ERROR. Invalid option "
                              & Argument.Get_Parameter (Occurence => I) & ".");
      Error;
      return;
    else
      -- Not an option
      exit;
    end if;
  end loop;

  -- Parse both patterns
  if Argument.Get_Nbre_Arg < Start + 1 then
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
    Error;
  end if;
  begin
    Search_Pattern.Parse (
         Argument.Get_Parameter (Occurence => Start),
         Extended, Case_Sensitive, Utf8);
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

  -- One file argument if file of files
  if File_Of_Files then
    if Argument.Get_Nbre_Arg /= Start then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                     & ": Syntax ERROR. One file (only) must be supplied"
                     & "  with -f or --file option.");
      Error;
      return;
    end if;
  end if;

  -- Process files
  Ok := True;
  if Argument.Get_Nbre_Arg < Start then
    -- No file: stdin -> stdout
    if Backup then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                & ": ERROR. Cannot make backup if no file name.");
      Ok := False;
    else
      begin
        Nb_Subst := Substit.Do_One_File (Substit.Std_In_Out, Max, False, False);
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
  elsif File_Of_Files then
    -- File of files: open it
    begin
      File_Mng.Open (Argument.Get_Parameter (Occurence => Start));
    exception
      when File_Mng.Open_Error =>
        Ok := False;
    end;
    -- Process files up to the end of file of files
    if Ok then
      loop
        begin
          Do_One_File (File_Mng.Get_Next_File);
        exception
          when File_Mng.End_Error =>
            exit;
          when File_Mng.Io_Error =>
            Ok := False;
            exit;
        end;
      end loop;
    end if;
  else
    -- Files are arguments
    for I in Start .. Argument.Get_Nbre_Arg loop
      Do_One_File (Argument.Get_Parameter (Occurence => I));
    end loop;
  end if;

  if not Ok then
    Sys_Calls.Set_Error_Exit_Code;
  end if;
end Asubst;

