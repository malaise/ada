with Ada.Exceptions;
with As.U, Environ, Argument, Argument_Parser, Basic_Proc, Language, Mixed_Str,
     Text_Line, Regular_Expressions;
with Search_Pattern, Replace_Pattern, Substit, File_Mng, Debug;
procedure Asubst is

  Version : constant String  := "V13.4";

  -- Exit codes
  Ok_Exit_Code : constant Natural := 0;
  No_Subst_Exit_Code : constant Natural := 1;
  Error_Exit_Code : constant Natural := 2;
  Terminate_Exit_Code : constant Natural := 3;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
               & " [ { <option> } ] <find_pattern> <replace_string> [ { <file> } ]");
    Basic_Proc.Put_Line_Error (
     "or:    " & Argument.Get_Program_Name & " -h | --help | -V | --version");
    Basic_Proc.Put_Line_Error (
     "  Substitutes pattern in files, or from stdin to stdout if no file.");
  end Usage;

  procedure Help is
  begin
    Usage;
    Basic_Proc.Put_Line_Error (
     "  <option> ::= -a | -D <string> | -d | -e <pattern> | -f | -g | -i");
    Basic_Proc.Put_Line_Error (
     "             | -l | -m <range> | -n | -p | -q | -s | -t | -u | -v | -x | --");
    Basic_Proc.Put_Line_Error (
     "    -a or --ascii for pure ASCII processing,");
    Basic_Proc.Put_Line_Error (
     "    -D <string> or --delimiter=<string> for a delimiter other than '\n',");
    Basic_Proc.Put_Line_Error (
     "    -d or --dotall for allow '.' to match '\n', when -D is set,");
    Basic_Proc.Put_Line_Error (
     "    -e <pattern> or --exclude=<pattern> for skip text matching <pattern>,");
    Basic_Proc.Put_Line_Error (
     "    -F <file> or --file_list=<file> to provide a file list of file names,");
    Basic_Proc.Put_Line_Error (
     "    -f or --file for display file name in grep mode,");
    Basic_Proc.Put_Line_Error (
     "    -g or --grep to print matching text (as grep would do) or substitution,");
    Basic_Proc.Put_Line_Error (
     "    -i or --ignorecase for case insensitive match (of search and exclusion),");
    Basic_Proc.Put_Line_Error (
     "    -l or --line for display line number in grep mode,");
    Basic_Proc.Put_Line_Error (
     "    -m <range> or --match=<range> for substitution of only <range> matches,");
    Basic_Proc.Put_Line_Error (
     "    -n or --number for print number of substitutions,");
    Basic_Proc.Put_Line_Error (
     "    -p <dir> or --tmp=<dir> for directory of temporary files,");
    Basic_Proc.Put_Line_Error (
     "    -q or --quiet for no printout,");
    Basic_Proc.Put_Line_Error (
     "    -s or --save for backup of original file,");
    Basic_Proc.Put_Line_Error (
     "    -t or --test for test, substitutions not performed,");
    Basic_Proc.Put_Line_Error (
     "    -u or --utf8 for processing utf-8 sequences,");
    Basic_Proc.Put_Line_Error (
     "    -v or --verbose for print each substitution,");
    Basic_Proc.Put_Line_Error (
     "    -x or --noregex for <find_pattern> being considered as string(s),");
    Basic_Proc.Put_Line_Error (
     "    -- to stop options.");
    Basic_Proc.Put_Line_Error (
     "  Set env LANG to something containig UTF-8, or set ASUBST_UTF8 to Y for utf-8");
    Basic_Proc.Put_Line_Error (
     "   processing by default. (Processing mode can still be modified by -u or -a.)");

    Basic_Proc.Put_Line_Error (
     "  <find_pattern> ::= <regex> | <multiple_regex>");
    Basic_Proc.Put_Line_Error (
     "    <multiple_regex> ::= { [ <regex> ] \n } [ <regex> ]");
    Basic_Proc.Put_Line_Error (
     "    A <regex> can contain ""\t"" (tab), ""\s"" (space), ""\xIJ"" (any hexa byte),");
    Basic_Proc.Put_Line_Error (
     "     or ""\I"" (I from 1 to 9, a back reference to a matching substring).");
    Basic_Proc.Put_Line_Error (
     "    A <regex> can't contain ""\n"" (""\n"" matches the new_line character");
    Basic_Proc.Put_Line_Error (
     "    and is the delimiter of regexes).");
    Basic_Proc.Put_Line_Error (
     "    The following shortcuts are provided for use in regex within brakets:");
    Basic_Proc.Put_Line_Error (
     "     \M [:alnum:]   \A [:alpha:]   \B [:blank:]   \C [:cntrl:]");
    Basic_Proc.Put_Line_Error (
     "     \D [:digit:]   \G [:graph:]   \L [:lower:]   \P [:print:]");
    Basic_Proc.Put_Line_Error (
     "     \T [:punct:]   \S [:space:]   \U [:upper:]   \X [:xdigit:]");
    Basic_Proc.Put_Line_Error (
     "    A <regex> can contain '^' or '$'. If not, it applies several times per line.");
    Basic_Proc.Put_Line_Error (
     "    Each <regex> of <multiple_regex> applies to one line (once).");
    Basic_Proc.Put_Line_Error (
     "    In noregex mode, only ""\t"", ""\s"", ""\xIJ"" and ""\n"" are interpreted");
    Basic_Proc.Put_Line_Error (
     "    and ""\x00"" is allowed (forbidden in a regex).");

    Basic_Proc.Put_Line_Error (
     "  <replace_string> is a string with the following specific sequences:");
    Basic_Proc.Put_Line_Error (
     "    ""\n"" (new_line), ""\t"" (tab), ""\s"" (space), ""\xIJ"" (hexa byte value).");
    Basic_Proc.Put_Line_Error (
     "    ""\iIJ<text>"" to replace by <text> if the Jth substring of the Ith regex");
    Basic_Proc.Put_Line_Error (
     "      matches. ""\iIJ"" can be followed by one or several ""\aIJ"" (and then) and");
    Basic_Proc.Put_Line_Error (
     "      ""\oIJ"" (or else). <text> ends when encountering another ""\iIJ"" (elsif),");
    Basic_Proc.Put_Line_Error (
     "      a ""\e"" (else) or a ""\f"" (end if). Ex: \i11\o12\a13OK\eNOK\f.");
    Basic_Proc.Put_Line_Error (
     "      The logic is if... elsif... elsif... else... endif.");
    Basic_Proc.Put_Line_Error (
     "    ""\RIJ"" (IJ in hexa) to replace by the input text matching the IJth regex,");
    Basic_Proc.Put_Line_Error (
     "    ""\rIJ"" to replace by the text matching the Jth substring of the Ith regex.");
    Basic_Proc.Put_Line_Error (
     "    ""\K""<shell command>""\k"", within which ""\RIJ"" and ""\rIJ"" are first replaced,");
    Basic_Proc.Put_Line_Error (
     "      then the command is launched (and must exit with 0), then the command");
    Basic_Proc.Put_Line_Error (
     "      directive is replaced by the command output.");
    Basic_Proc.Put_Line_Error (
     "    ""\P""<file path>""\p"", within which ""\RIJ"" and ""\rIJ"" are first replaced,");
    Basic_Proc.Put_Line_Error (
     "      then the content of the file is inserted as is.");
    Basic_Proc.Put_Line_Error (
     "    ""\u"" (start UPPERCASE conversion), ""\l"" (lowercase), ""\m"" (Mixed_Case),");
    Basic_Proc.Put_Line_Error (
     "      ""\c"" (stop case conversion). Any new conversion replaces previous.");
    Basic_Proc.Put_Line_Error (
     "    Conditions apply first, then replacement and command, then case conversion.");
    Basic_Proc.Put_Line_Error (
     "    ""\R01"" <-> 1st <regex>, ""\R00"" <-> all <regex>, ""\ri0"" == ""\R0i"".");
    Basic_Proc.Put_Line_Error (
     "    Like back references, substrs are numbered in order of opening parentheses.");
    Basic_Proc.Put_Line_Error (
     "    Note that ""\r0i"" and ""\i0i"" are is forbidden.");

    Basic_Proc.Put_Line_Error (
     "  <exclude_pattern> if set must have the same number of regex as <find_pattern>.");
    Basic_Proc.Put_Line_Error (
     "    Text matching <find_pattern> will be discarded if it also matches");
    Basic_Proc.Put_Line_Error (
     "    <exclude_pattern>.");
    Basic_Proc.Put_Line_Error (
     "  If a specific delimiter is set, it is used to read chunks of input text (whole");
    Basic_Proc.Put_Line_Error (
     "    flow if delimiter is empty). The <find_pattern> must be a simple <regex>");
    Basic_Proc.Put_Line_Error (
     "    (no '^' or '$', but '\n' is allowed), and applies to each chunk.");
    Basic_Proc.Put_Line_Error (
     "    Option -d (--dotall) can be usefull in this case.");
    Basic_Proc.Put_Line_Error (
     "    This allows multi-row processing.");
    Basic_Proc.Put_Line_Error (
     "  In grep mode, if <replace_string> is not empty then it is interpreted and put");
    Basic_Proc.Put_Line_Error (
     "    (ex: ""\R01"" for the matching text), otherwise the full line of the matching");
    Basic_Proc.Put_Line_Error (
     "    text is put (as grep would do), possibly with file name (if ""-f"") and with");
    Basic_Proc.Put_Line_Error (
     "    line number (if ""-fl"").");
    Basic_Proc.Put_Line_Error (
     "  Exit code is 0 if some matching was found, 1 if no match and 2 on error.");
    Basic_Proc.Put_Line_Error (
     "  Warning: regex are powerfull (see ""man 3 pcre"" and ""man 1 perlre"") and");
    Basic_Proc.Put_Line_Error (
     "    automatic substitution can be dangerous, so use " & Argument.Get_Program_Name & " with caution:");
    Basic_Proc.Put_Line_Error (
     "    test pattern with ""echo string | " &  Argument.Get_Program_Name & " <search_pattern> <replace_string>""");
    Basic_Proc.Put_Line_Error (
     "    and use -s or -tv option if unsure.");
    Basic_Proc.Set_Exit_Code (Error_Exit_Code);
  end Help;

  procedure Error is
  begin
    Usage;
    Basic_Proc.Set_Exit_Code (Error_Exit_Code);
  end Error;

  -- For getenv
  Utf8_Var_Name : constant String := "ASUBST_UTF8";

  -- The keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => ('a', As.U.Tus ("ascii"), False, False),
   02 => ('D', As.U.Tus ("delimiter"), False, True),
   03 => ('d', As.U.Tus ("dotall"), False, False),
   04 => ('e', As.U.Tus ("exclude"), False, True),
   05 => ('F', As.U.Tus ("file_list"), False, True),
   06 => ('g', As.U.Tus ("grep"), False, False),
   07 => ('h', As.U.Tus ("help"), False, False),
   08 => ('i', As.U.Tus ("ignorecase"), False, False),
   09 => ('l', As.U.Tus ("line"), False, False),
   10 => ('m', As.U.Tus ("match"), False, True),
   11 => ('n', As.U.Tus ("number"), False, False),
   12 => ('q', As.U.Tus ("quiet"), False, False),
   13 => ('s', As.U.Tus ("save"), False, False),
   14 => ('t', As.U.Tus ("test"), False, False),
   15 => ('u', As.U.Tus ("utf8"), False, False),
   16 => ('v', As.U.Tus ("verbose"), False, False),
   17 => ('V', As.U.Tus ("version"), False, False),
   18 => ('x', As.U.Tus ("noregex"), False, False),
   19 => ('p', As.U.Tus ("tmp"), False, True),
   20 => ('f', As.U.Tus ("file"), False, False)
   );
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  No_Key_Index : constant Argument_Parser.The_Keys_Index
               := Argument_Parser.No_Key_Index;

  -- Option management
  Dot_All : Boolean := False;
  Exclude : As.U.Asu_Us;
  File_Of_Files : Boolean := False;
  Case_Sensitive : Boolean := True;
  Match_Range : As.U.Asu_Us;
  Tmp_Dir : As.U.Asu_Us;
  type Verbose_List is (Quiet, Put_File_Name, Put_Subst_Nb, Verbose);
  Verbosity : Verbose_List := Put_File_Name;
  Grep : Boolean := False;
  Grep_Line_Nb : Boolean := False;
  Grep_File_Name : Boolean := False;
  Backup : Boolean := False;
  Is_Regex : Boolean := True;
  Test : Boolean := False;
  Delimiter : As.U.Asu_Us := As.U.Tus (Text_Line.Line_Feed_Str);
  -- Overall result to summarize error and if any subst/search done
  Ok : Boolean;
  Found : Boolean;
  -- Nb subst per file
  Nb_Subst : Substit.Long_Long_Natural;
  -- Language
  Lang : Language.Language_List
       := Language.Get_Env;
  use type Language.Language_List;

  -- Check that there are not several (conflictual) verbosity levels
  function Check_Verbose return Boolean is
  begin
    if Verbosity /= Put_File_Name then
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
          & ": Syntax ERROR. Options 'n', 'q' and 'v' are mutually exclusive.");
      Error;
      return False;
    else
      return True;
    end if;
  end Check_Verbose;

  -- Process one file
  procedure Do_One_File (File_Name : in String) is
  begin
    if Verbosity = Verbose then
      -- Put file name
      Basic_Proc.Put_Line_Output (File_Name);
    end if;
    Nb_Subst := Substit.Do_One_File (
                  File_Name,
                  Tmp_Dir.Image,
                  Delimiter.Image,
                  Match_Range.Image,
                  Backup, Verbosity = Verbose, Grep,
                  Grep_Line_Nb, Grep_File_Name, Test);
    if Nb_Subst /= 0 then
      Found := True;
    end if;
    if Verbosity = Put_File_Name and then Nb_Subst /= 0 then
      -- Put file name if substitution occured
      Basic_Proc.Put_Line_Output (File_Name);
    elsif Verbosity >= Put_Subst_Nb then
      -- Put file name and nb of substitutions
      Basic_Proc.Put_Line_Output (File_Name & Nb_Subst'Img);
    end if;
  exception
    when Substit.Substit_Error =>
      Ok := False;
    when Replace_Pattern.Terminate_Request =>
      raise;
    when Error:others =>
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
                & ": EXCEPTION: " & Ada.Exceptions.Exception_Name (Error)
                & " while processing file "
                & File_Name & ".");
      Ok := False;
  end Do_One_File;

begin
  -- Superseed by ASUBST_UTF8 variable if set
  if Environ.Is_Yes (Utf8_Var_Name) then
    Lang := Language.Lang_Utf_8;
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Environ set to UTF-8");
    end if;
  elsif Environ.Is_No (Utf8_Var_Name) then
    Lang := Language.Lang_C;
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Environ set to NO UTF-8");
    end if;
  end if;

  -- Parse keys and options
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR. "
      & Arg_Dscr.Get_Error & ".");
    Error;
    return;
  end if;

  -- Check version and help, must be alone
  if Arg_Dscr.Is_Set (17) then
    -- Version
    if Argument.Get_Nbre_Arg /= 1 then
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
      Error;
    else
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & " " & Version);
      Basic_Proc.Put_Line_Error ("PCRE "
                               & Regular_Expressions.Get_Pcre_Version);
      Basic_Proc.Set_Exit_Code (Error_Exit_Code);
    end if;
    return;
  elsif Arg_Dscr.Is_Set (07) then
    -- Help
    if  Argument.Get_Nbre_Arg /= 1 then
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
      Error;
    else
      Help;
    end if;
    return;
  elsif Arg_Dscr.Get_Nb_Occurences (No_Key_Index) < 2
  or else Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    -- There must be at least Search and Replace strings
    -- They must be after options
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
    Error;
    return;
  end if;

  -- Parse options
  if Arg_Dscr.Is_Set (01) then
    -- Force ASCII processing even if ENV was set
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option ascii");
    end if;
    Lang := Language.Lang_C;
  end if;
  if Arg_Dscr.Is_Set (02) then
    -- Specific delimiter instead of '\n'
    begin
      Delimiter := As.U.Tus (Arg_Dscr.Get_Option (02));
      if Delimiter.Length > Text_Line.Max_Line_Feed_Len then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
           & ": Syntax ERROR. Invalid delimiter.");
        Error;
        return;
    end;
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option delimiter = " & Delimiter.Image);
    end if;
  end if;
  if Arg_Dscr.Is_Set (03) then
    -- Allow dot to match all characters
    if not Arg_Dscr.Is_Set (02) then
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
         & ": Syntax ERROR. Option -d requires option -D.");
      Error;
    end if;
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option dot all");
    end if;
    Dot_All := True;
  end if;
  if Arg_Dscr.Is_Set (04) then
    -- Exclude text matching exclude_regexp
    begin
      Exclude := As.U.Tus (Arg_Dscr.Get_Option (04));
      if Exclude.Length = 0 then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
           & ": Syntax ERROR. Invalid exclude_pattern.");
        Error;
        return;
    end;
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option exclude = " & Exclude.Image);
    end if;
  end if;
  if Arg_Dscr.Is_Set (05) then
    -- The file will be a list of files
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option file of files");
    end if;
    File_Of_Files := True;
  end if;
  if Arg_Dscr.Is_Set (06) then
    -- Put matching text like grep would do
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option grep mode");
    end if;
    Grep := True;
  end if;
  if Arg_Dscr.Is_Set (08) then
    -- Case insensitive match
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option ignore case");
    end if;
    Case_Sensitive := False;
  end if;
  if Arg_Dscr.Is_Set (09) then
    -- Put line no
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option line no");
    end if;
    Grep_Line_Nb := True;
  end if;
  if Arg_Dscr.Is_Set (10) then
    -- Substit only occurences that match criteria
    declare
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
    begin
      Match_Range := As.U.Tus (Arg_Dscr.Get_Option (10));
      Dummy := Substit.Subst_Match.Matches (0, Match_Range.Image);
    exception
      when others =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
           & ": Syntax ERROR. Invalid specification of matching range.");
        Error;
        return;
    end;
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option match =" & Match_Range.Image);
    end if;
  else
    -- No criteria
    Match_Range := As.U.Tus ("-");
  end if;
  if Arg_Dscr.Is_Set (11) then
    -- Put number of substitutions
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option put numbers");
    end if;
    if not Check_Verbose then
      return;
    end if;
    Verbosity := Put_Subst_Nb;
  end if;
  if Arg_Dscr.Is_Set (12) then
    -- Quiet mode
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option quiet");
    end if;
    if not Check_Verbose then
      return;
    end if;
    Verbosity := Quiet;
  end if;
  if Arg_Dscr.Is_Set (13) then
    -- Make backup
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option make backup");
    end if;
    Backup := True;
  end if;
  if Arg_Dscr.Is_Set (14) then
    -- Test mode
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option test");
    end if;
    Test := True;
  end if;
  if Arg_Dscr.Is_Set (15) then
    if Arg_Dscr.Is_Set (1) then
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
         & ": Syntax ERROR. Incompatible options -a and -u.");
      Error;
      return;
    end if;
    -- Process utf-8 sequences
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option utf8");
    end if;
    Lang := Language.Lang_Utf_8;
  end if;
  if Arg_Dscr.Is_Set (16) then
    -- Verbose put each substit
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option verbose");
    end if;
    if not Check_Verbose then
      return;
    end if;
    Verbosity := Verbose;
  end if;
  if Arg_Dscr.Is_Set (18) then
    -- Find pattern is not a regex
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option noregex");
    end if;
    Is_Regex := False;
  end if;
  if Arg_Dscr.Is_Set (19) then
    -- Tmp_Dir for temporary files
    begin
      Tmp_Dir := As.U.Tus (Arg_Dscr.Get_Option (19));
      if Tmp_Dir.Length = 0 then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
           & ": Syntax ERROR. Invalid tmp dir.");
        Error;
        return;
    end;
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option tmp_dir = " & Tmp_Dir.Image);
    end if;
  end if;
  if Arg_Dscr.Is_Set (20) then
    -- Put file name
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option file name");
    end if;
    Grep_File_Name := True;
  end if;

  -- Set language (for regexp)
  Language.Set_Language (Lang);
  if Debug.Set then
    Basic_Proc.Put_Line_Error ("Regex assumes language to be "
       & Mixed_Str (Language.Language_List'Image(
                Language.Get_Language)));
  end if;

  begin
    Search_Pattern.Parse (
         Arg_Dscr.Get_Option (No_Key_Index, 1),
         Exclude.Image,
         Delimiter.Image,
         Case_Sensitive, Is_Regex, Dot_All);
  exception
    when Search_Pattern.Parse_Error =>
      Error;
      return;
  end;
  begin
    Replace_Pattern.Parse (
           Arg_Dscr.Get_Option (No_Key_Index, 2));
  exception
    when Replace_Pattern.Parse_Error =>
      Error;
      return;
  end;

  -- Dependancies
  -- Grep => Test, not verbose, not backup
  if Grep then
    if Test or else Verbosity /= Put_File_Name or else Backup then
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
        & ": Syntax ERROR. Grep mode imposes quiet, test and no-backup.");
      Error;
      return;
    end if;
    Test := True;
    Verbosity := Quiet;
    Backup := False;
  end if;
  -- File_Name => Grep
  if Grep_File_Name and then not Grep then
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
      & ": Syntax ERROR. File_name option is allowed in grep mode only.");
    Error;
    return;
  end if;
  -- Line_Nb => Grep and File_Name
  if Grep_Line_Nb and then not Grep_File_Name then
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
      & ": Syntax ERROR. Line_number option requires file_name (and grep mode).");
    Error;
    return;
  end if;
  -- Grep AND File_Name => empty Replace_Pattern
  if Grep and then Grep_File_Name
  and then Arg_Dscr.Get_Option (No_Key_Index, 2) /= "" then
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
      & ": Syntax ERROR. Grep with file_name implies empty replace_string.");
    Error;
    return;
  end if;

  -- One file argument if file of files
  if File_Of_Files
  and then Arg_Dscr.Get_Nb_Occurences (No_Key_Index) /= 2 then
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
                   & ": Syntax ERROR. No file accepted"
                   & " when -F or --file_list option is set.");
    Error;
    return;
  end if;

  -- Display search pattern and replace string
  if Debug.Set then
    Basic_Proc.Put_Line_Output ("Search pattern: >"
       & Arg_Dscr.Get_Option (No_Key_Index, 1) & "<");
    if Exclude.Image /= "" then
      Basic_Proc.Put_Line_Output ("Exclude pattern: >" & Exclude.Image & "<");
    end if;
    if not Grep then
      Basic_Proc.Put_Line_Output ("Replace string: >"
         & Arg_Dscr.Get_Option (No_Key_Index, 2) & "<");
    end if;
  end if;

  -- Process files
  Ok := True;
  Found := False;
  if File_Of_Files then
    -- File of files: open it
    begin
      File_Mng.Open (Arg_Dscr.Get_Option (05, 1));
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
  elsif Arg_Dscr.Get_Nb_Occurences (No_Key_Index) = 2 then
    -- No file: stdin -> stdout
    if Backup then
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
                & ": ERROR. Cannot make backup when no file name.");
      Ok := False;
    else
      begin
        Nb_Subst := Substit.Do_One_File (
            File_Name      => Substit.Std_In_Out,
            Tmp_Dir        => Tmp_Dir.Image,
            Delimiter      => Delimiter.Image,
            Match_Range    => Match_Range.Image,
            Backup         => False,
            Verbose        => False,
            Grep           => Grep,
            Grep_Line_Nb   => Grep_Line_Nb,
            Grep_File_Name => Grep_File_Name,
            Test          => Test);
        if Nb_Subst /= 0 then
          Found := True;
        end if;
      exception
        when Substit.Substit_Error =>
          Ok := False;
        when Replace_Pattern.Terminate_Request =>
          raise;
        when Error:others =>
          Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
                    & ": EXCEPTION: " & Ada.Exceptions.Exception_Name (Error)
                    & " while processing stdin to stdout.");
          Ok := False;
      end;
    end if;
  else
    -- Files are arguments
    for I in 3 .. Arg_Dscr.Get_Nb_Occurences (No_Key_Index) loop
      Do_One_File (Arg_Dscr.Get_Option (No_Key_Index, I));
    end loop;
  end if;

  -- Clean argument parser memory
  Arg_Dscr.Reset;

  if not Ok then
    Basic_Proc.Set_Exit_Code (Error_Exit_Code);
  else
    if not Found then
      Basic_Proc.Set_Exit_Code (No_Subst_Exit_Code);
    else
      Basic_Proc.Set_Exit_Code (Ok_Exit_Code);
    end if;
  end if;
exception
  when Replace_Pattern.Terminate_Request =>
    Basic_Proc.Set_Exit_Code (Terminate_Exit_Code);
end Asubst;

