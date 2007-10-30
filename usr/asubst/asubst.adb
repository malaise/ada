with Ada.Exceptions, Ada.Text_Io;
with Environ, Argument, Sys_Calls, Language;
with Search_Pattern, Replace_Pattern, Substit, File_Mng, Debug, Mixed_Str;
procedure Asubst is

  Version : constant String  := "V3_8";

  -- Exit codes
  Ok_Exit_Code : constant Natural := 0;
  No_Subst_Exit_Code : constant Natural := 1;
  Error_Exit_Code : constant Natural := 2;

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
               & " [ { <option> } ] <find_pattern> <replace_string> [ { <file> } ]");
    Sys_Calls.Put_Line_Error (
     "or:    " & Argument.Get_Program_Name & " -h | --help | -V | --version");
    Sys_Calls.Put_Line_Error (
     "  Substitutes pattern in files, or from stdin to stdout if no file.");
  end Usage;

  procedure Help (New_Line : Boolean := True) is
  begin
    Usage;
    Sys_Calls.Put_Line_Error (
     "  <option> ::= -a | -b | -f | -g | -i | -m <max> | -n | -q | -s | -t | -u | -v | -x | --");
    Sys_Calls.Put_Line_Error (
     "    -a or --ascii for pure ASCII processing,");
    Sys_Calls.Put_Line_Error (
     "    -b or --basic for basic regex,");
    Sys_Calls.Put_Line_Error (
     "    -f or --file to indicate that <file> will be a list of file names,");
    Sys_Calls.Put_Line_Error (
     "    -g or --grep to print matching text as grep would do (no subst),");
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
     "    -t or --test for test, substitutions not performed,");
    Sys_Calls.Put_Line_Error (
     "    -u or --utf8 for processing utf-8 sequences,");
    Sys_Calls.Put_Line_Error (
     "    -v or --verbose for print each substitution,");
    Sys_Calls.Put_Line_Error (
     "    -x or --noregex for <find_pattern> being considered as a single string,");
    Sys_Calls.Put_Line_Error (
     "    -- to stop options.");
    Sys_Calls.Put_Line_Error (
     "  Set env LANG to something containig UTF-8, or set ASUBST_UTF8 to Y for utf-8");
    Sys_Calls.Put_Line_Error (
     "   processing by default. (Processing mode can still be modified by -u or -a.)");
    Sys_Calls.Put_Line_Error (
     "  <find_pattern> ::= <regex> | <multiple_regex>");
    Sys_Calls.Put_Line_Error (
     "    <multiple_regex> ::= { [ <regex> ] \n } [ <regex> ]");
    Sys_Calls.Put_Line_Error (
     "    A <regex> can contain ""\t"" (tab), ""\s"" (space) or ""\xIJ"" (any hexa byte).");
    Sys_Calls.Put_Line_Error (
     "    A <regex> can't contain ""\n"" (""\n"" matches the new_line character");
    Sys_Calls.Put_Line_Error (
     "    and is the delimiter of regexes).");
    Sys_Calls.Put_Line_Error (
     "    The following shortcuts are provided for use in regex within brakets:");
    Sys_Calls.Put_Line_Error (
     "     \M [:alnum:]   \A [:alpha:]   \B [:blank:]   \C [:cntrl:]");
    Sys_Calls.Put_Line_Error (
     "     \D [:digit:]   \G [:graph:]   \L [:lower:]   \P [:print:]");
    Sys_Calls.Put_Line_Error (
     "     \T [:punct:]   \S [:space:]   \U [:upper:]   \X [:xdigit:]");
    Sys_Calls.Put_Line_Error (
     "    A <regex> can contain '^' or '$'. If not, it applies several times per line.");
    Sys_Calls.Put_Line_Error (
     "    Each <regex> of <multiple_regex> applies to one line (once).");
    Sys_Calls.Put_Line_Error (
     "    The <multiple_regex> cannot have ""\n^"" or ""$\n"".");
    Sys_Calls.Put_Line_Error (
     "    In noregex mode, only ""\t"", ""\s"", ""\xIJ"" and ""\n"" are interpreted,");
    Sys_Calls.Put_Line_Error (
     "    ""\n"" is forbidden, and ""\x00"" is allowed (forbidden in a regex).");
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
    Sys_Calls.Set_Exit_Code (Error_Exit_Code);
  end Help;

  procedure Error is
  begin
    Usage;
    Sys_Calls.Set_Exit_Code (Error_Exit_Code);
  end Error;

  Utf8_Var_Name : constant String := "ASUBST_UTF8";
  -- Option management
  Extended : Boolean := True;
  File_Of_Files : Boolean := False;
  Case_Sensitive : Boolean := True;
  Max : Substit.Long_Long_Natural := 0;
  type Verbose_List is (Quiet, Put_File_Name, Put_Subst_Nb, Verbose);
  Verbosity : Verbose_List := Put_File_Name;
  Grep : Boolean := False;
  Backup : Boolean := False;
  Is_Regex : Boolean := True;
  Test : Boolean := False;
  -- No of argument
  N_Arg : Positive;
  -- Start index (in nb args) of patterns
  Start : Positive;
  -- Overall result to summarize error and if any subst/search done
  Ok : Boolean;
  Found : Boolean;
  -- Nb subst per file
  Nb_Subst : Substit.Long_Long_Natural;
  -- Language
  Lang : Language.Language_List
       := Language.Get_Env;
  use type Language.Language_List;

  -- Process one file
  procedure Do_One_File (File_Name : in String) is
  begin
    if Verbosity = Verbose then
      -- Put file name
      Ada.Text_Io.Put_Line (File_Name);
    end if;
    Nb_Subst := Substit.Do_One_File (
                  File_Name,
                  Max, Backup, Verbosity = Verbose, Grep, Test);
    if Nb_Subst /= 0 then
      Found := True;
    end if;
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
  -- Superseed by ASUBST_UTF8 variable if set
  if Environ.Is_Yes (Utf8_Var_Name) then
    Lang := Language.Lang_Utf_8;
  elsif Environ.Is_No (Utf8_Var_Name) then
    Lang := Language.Lang_C;
  end if;

  -- Check nb of arguments
  if Argument.Get_Nbre_Arg = 1 then
    if Argument.Get_Parameter = "-V"
    or else Argument.Get_Parameter = "--version" then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & " " & Version);
      Sys_Calls.Set_Exit_Code (Error_Exit_Code);
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
  N_Arg := 1;
  loop
    if Argument.Get_Parameter (Occurence => N_Arg) = "--" then
      -- Force end of options
      Start := N_Arg + 1;
      exit;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-a"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--ascii" then
      -- Force ASCII processing even if ENV was set
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option ascii");
      end if;
      Lang := Language.Lang_C;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-b"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--basic" then
      -- Basic regex
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option basic regex");
      end if;
      Extended := False;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-f"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--file" then
      -- The file will be a list of files
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option file of files");
      end if;
      File_Of_Files := True;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-g"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--grep" then
      -- Put matching text like grep would do
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option grep display");
      end if;
      Grep := True;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-i"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--ignorecase" then
      -- Case insensitive match
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option ignore case");
      end if;
      Case_Sensitive := False;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-m"
    or else (Argument.Get_Parameter (Occurence => N_Arg)'Length > 6
     and then Argument.Get_Parameter (Occurence => N_Arg)(1 .. 6)
                                         = "--max=" ) then
      -- Stop each file after <max> substitutions
      begin
        if Argument.Get_Parameter (Occurence => N_Arg) = "-m" then
          -- -m <max>
          Max := Substit.Long_Long_Natural'Value (
            Argument.Get_Parameter (Occurence => N_Arg + 1));
          Start := N_Arg + 2;
        else
          -- --max=<max>
          declare
            Str : constant String := Argument.Get_Parameter (Occurence => N_Arg);
          begin
            Max := Substit.Long_Long_Natural'Value (Str (7 .. Str'Last));
          end;
          Start := N_Arg + 1;
        end if;
      exception
        when others =>
          Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
             & ": Syntax ERROR. Invalid specification of max subtitutions.");
          Error;
          return;
      end;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option max =" & Max'Img);
      end if;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-n"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--number" then
      -- Put number of substitutions
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option put numbers");
      end if;
      Verbosity := Put_Subst_Nb;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-q"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--quiet" then
      -- Quiet mode
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option quiet");
      end if;
      Verbosity := Quiet;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-s"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--save" then
      -- Make backup
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option make backup");
      end if;
      Backup := True;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-t"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--test" then
      -- Test mode
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option test");
      end if;
      Test := True;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-u"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--utf8" then
      -- Process utf-8 sequences
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option utf8");
      end if;
      Lang := Language.Lang_Utf_8;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-v"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--verbose" then
      -- Verbose put each substit
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option verbose");
      end if;
      Verbosity := Verbose;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) = "-x"
    or else Argument.Get_Parameter (Occurence => N_Arg) = "--noregex" then
      -- Find pattern is not a regex
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Option noregex");
      end if;
      Is_Regex := False;
      Start := N_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => N_Arg) /= ""
    and then Argument.Get_Parameter (Occurence => N_Arg)(1) = '-' then
      -- Not a valid option
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                      & ": Syntax ERROR. Invalid option "
                      & Argument.Get_Parameter (Occurence => N_Arg) & ".");
      Error;
      return;
    else
      -- Not an option
      exit;
    end if;
    N_Arg := N_Arg + 1;
    exit when N_Arg > Argument.Get_Nbre_Arg;
  end loop;

  -- Set language (for regexp)
  Language.Set_Language (Lang);
  if Debug.Set then
    Sys_Calls.Put_Line_Error ("Regex assumes language to be "
       & Mixed_Str (Language.Language_List'Image(
                Language.Get_Language)));
  end if;

  -- Parse both patterns
  if Argument.Get_Nbre_Arg < Start + 1 then
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
    Error;
    return;
  end if;
  begin
    Search_Pattern.Parse (
         Argument.Get_Parameter (Occurence => Start),
         Extended, Case_Sensitive, Is_Regex);
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

  -- Dependancies
  -- Grep => Test, not verbose, not backup
  if Grep then
    Test := True;
    Verbosity := Quiet;
    Backup := False;
  end if;

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
  Found := False;
  if Argument.Get_Nbre_Arg < Start then
    -- No file: stdin -> stdout
    if Backup then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                & ": ERROR. Cannot make backup when no file name.");
      Ok := False;
    else
      begin
        Nb_Subst := Substit.Do_One_File (
            File_Name => Substit.Std_In_Out,
            Max_Subst => Max,
            Backup    => False,
            Verbose   => False,
            Grep      => Grep,
            Test      => Test);
        if Nb_Subst /= 0 then
          Found := True;
        end if;
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
    Sys_Calls.Set_Exit_Code (Error_Exit_Code);
  else
    if not Found then
      Sys_Calls.Set_Exit_Code (No_Subst_Exit_Code);
    end if;
  end if;
end Asubst;

