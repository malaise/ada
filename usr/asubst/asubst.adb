with Ada.Exceptions;
with As.U.Utils, Environ, Argument, Argument_Parser, Basic_Proc, Language,
     Mixed_Str, Text_Line, Regular_Expressions;
with Search_Pattern, Replace_Pattern, Substit, File_Mng, Debug;
procedure Asubst is

  Version : constant String  := "V17.1";

  -- Exit codes
  Ok_Exit_Code : constant Natural := 0;
  No_Subst_Exit_Code : constant Natural := 1;
  Error_Exit_Code : constant Natural := 2;
  Terminate_Exit_Code : constant Natural := 3;

  -- The keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => (False, 'a', As.U.Tus ("ascii"),       False),
   02 => (True,  'D', As.U.Tus ("delimiter"),   False, False, As.U.Tus ("string")),
   03 => (False, 'd', As.U.Tus ("dotall"),      False),
   04 => (True,  'e', As.U.Tus ("exclude"),     False, True, As.U.Tus ("pattern")),
   05 => (True,  'F', As.U.Tus ("file_list"),   False, True, As.U.Tus ("file")),
   06 => (False, 'f', As.U.Tus ("file"),        False),
   07 => (False, 'g', As.U.Tus ("grep"),        False),
   08 => (False, 'i', As.U.Tus ("ignorecase"),  False),
   09 => (False, 'I', As.U.Tus ("invertmatch"), False),
   10 => (False, 'L', As.U.Tus ("list"),        False),
   11 => (False, 'l', As.U.Tus ("line"),        False),
   12 => (True,  'm', As.U.Tus ("match"),       False, False, As.U.Tus ("range")),
   13 => (False, 'n', As.U.Tus ("number"),      False),
   14 => (True,  'p', As.U.Tus ("tmp"),         False, True, As.U.Tus ("dir")),
   15 => (False, 'q', As.U.Tus ("quiet"),       False),
   16 => (False, 's', As.U.Tus ("save"),        False),
   17 => (False, 't', As.U.Tus ("test"),        False),
   18 => (False, 'u', As.U.Tus ("utf8"),        False),
   19 => (False, 'v', As.U.Tus ("verbose"),     False),
   20 => (False, 'x', As.U.Tus ("noregex"),     False),
   21 => (False, 'h', As.U.Tus ("help"),        False),
   22 => (False, 'V', As.U.Tus ("version"),     False)
   );
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  No_Key_Index : constant Argument_Parser.The_Keys_Index
               := Argument_Parser.No_Key_Index;
  Help_Key : constant Argument_Parser.The_Keys_Range := 21;
  Vers_Key : constant Argument_Parser.The_Keys_Range := 22;

  -- Help (short and long)
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
               & " [ { <option> } ] <find_pattern> <replace_string> [ { <file> } ]");
    Basic_Proc.Put_Line_Error (
     "or:    " & Argument.Get_Program_Name
         & " " & Argument_Parser.Image (Keys(Help_Key))
       & " | " & Argument_Parser.Image (Keys(Help_Key)));
    Basic_Proc.Put_Line_Error (
     "  Substitutes pattern in files, or from stdin to stdout if no file.");
  end Usage;

  Helps : constant As.U.Utils.Asu_Array (1 .. 20) := (
    01 => As.U.Tus ("for pure ASCII processing"),
    02 => As.U.Tus ("for a delimiter other than '\n'"),
    03 => As.U.Tus ("for allow '.' to match '\n', when -D is set"),
    04 => As.U.Tus ("for skip text matching <pattern>"),
    05 => As.U.Tus ("to provide a file list of file names"),
    06 => As.U.Tus ("for display file name in grep mode"),
    07 => As.U.Tus ("to print matching text (as grep would do) or substitution"),
    08 => As.U.Tus ("for case insensitive match (of search and exclusion)"),
    09 => As.U.Tus ("to invert matching (as grep -v would do)"),
    10 => As.U.Tus ("to print matching files (as grep -l would do)"),
    11 => As.U.Tus ("for display line number in grep mode"),
    12 => As.U.Tus ("for substitution of only <range> matches"),
    13 => As.U.Tus ("for print number of substitutions"),
    14 => As.U.Tus ("for directory of temporary files"),
    15 => As.U.Tus ("for no printout"),
    16 => As.U.Tus ("for backup of original file"),
    17 => As.U.Tus ("for test, substitutions not performed"),
    18 => As.U.Tus ("for processing utf-8 sequences"),
    19 => As.U.Tus ("for print each substitution"),
    20 => As.U.Tus ("for <find_pattern> being considered as string(s)")
   );

  procedure Help is
  begin
    Usage;
    Basic_Proc.Put_Line_Error (
     "  <option> ::= -a | -D <string> | -d | -e <pattern> | -F | -f | -g | -i");
    Basic_Proc.Put_Line_Error (
     "             | -l | -m <range> | -n | -p | -q | -s | -t | -u | -v | -x | --");
    for I in Helps'Range loop

      Basic_Proc.Put_Line_Error (
       "    " & Argument_Parser.Image (Keys(I)) & " " & Helps(I).Image & ",");
    end loop;

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
     "     ""\I"" (I from 1 to 9, a back reference to a matching substring),");
    Basic_Proc.Put_Line_Error (
     "     ""\RIJ"" (IJ in hexa), replaced by the input text matching the IJth regex,");
    Basic_Proc.Put_Line_Error (
     "     ""\rIJ"", replaced by the text matching the Jth substring of the Ith regex.");
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
     "    A <regex> can't contain the delimiter (default ""\n"").");
    Basic_Proc.Put_Line_Error (
     "    Each <regex> of <multiple_regex> applies to one line (once).");
    Basic_Proc.Put_Line_Error (
     "    In noregex mode, only ""\t"", ""\s"", ""\xIJ"" and ""\n"" are interpreted");
    Basic_Proc.Put_Line_Error (
     "     and ""\x00"" is allowed (forbidden in a regex).");

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
     "    text is put (as grep would do), possibly with file name (if ""-gf"") and with");
    Basic_Proc.Put_Line_Error (
     "    line number (if ""-gfl"").");
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
  Grep_List : Boolean := False;
  Grep_Invert : Boolean := False;
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
  -- Stdin and Stdout
  Std_In_Out : constant String := "-";
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
    File : As.U.Asu_Us;
    Bkp : Boolean;
  begin
    -- File and backup option: Stdin -> "-" and False
    File := As.U.Tus (File_Name);
    Bkp := Backup;
    if File_Name = "" or else File_Name = Std_In_Out then
      File := As.U.Tus (Substit.Std_In_Out);
      Bkp := False;
    end if;
    if Verbosity = Verbose then
      -- Put file name
      Basic_Proc.Put_Line_Output (File.Image);
    end if;
    Nb_Subst := Substit.Do_One_File (
                  File_Name      => File.Image,
                  Tmp_Dir        => Tmp_Dir.Image,
                  Delimiter      => Delimiter.Image,
                  Match_Range    => Match_Range.Image,
                  Backup         => Bkp,
                  Verbose        => Verbosity = Verbose,
                  Grep           => Grep,
                  Grep_List      => Grep_List,
                  Grep_File_Name => Grep_File_Name,
                  Grep_Line_Nb   => Grep_Line_Nb,
                  Grep_Invert    => Grep_Invert,
                  Test           => Test);
    if Nb_Subst /= 0 then
      Found := True;
    end if;
    if Verbosity = Put_File_Name and then Nb_Subst /= 0 then
      -- Put file name if substitution occured
      Basic_Proc.Put_Line_Output (File.Image);
    elsif Verbosity >= Put_Subst_Nb then
      -- Put file name and nb of substitutions
      Basic_Proc.Put_Line_Output (File.Image & Nb_Subst'Img);
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
                & File.Image & ".");
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
  if Arg_Dscr.Is_Set (Vers_Key) then
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
  elsif Arg_Dscr.Is_Set (Help_Key) then
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
    -- Put file name
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option file name");
    end if;
    Grep_File_Name := True;
  end if;
  if Arg_Dscr.Is_Set (07) then
    -- Put matching text like grep would do
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option grep mode");
    end if;
    Grep := True;
    Grep_List := False;
  end if;
  if Arg_Dscr.Is_Set (08) then
    -- Case insensitive match
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option ignore case");
    end if;
    Case_Sensitive := False;
  end if;
  if Arg_Dscr.Is_Set (09) then
    -- Invert grep matching
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option invert matching");
    end if;
    Grep_Invert := True;
  end if;
  if Arg_Dscr.Is_Set (10) then
    -- Put matching file like grep -l would do
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option file mode");
    end if;
    if Arg_Dscr.Is_Set (6) then
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
         & ": Syntax ERROR. Incompatible options -L and -f.");
      Error;
      return;
    end if;
    Grep := True;
    Grep_List := True;
  end if;
  if Arg_Dscr.Is_Set (11) then
    -- Put line no
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option line no");
    end if;
    Grep_Line_Nb := True;
  end if;
  if Arg_Dscr.Is_Set (12) then
    -- Substit only occurences that match criteria
    declare
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
    begin
      Match_Range := As.U.Tus (Arg_Dscr.Get_Option (12));
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
  if Arg_Dscr.Is_Set (13) then
    -- Put number of substitutions
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option put numbers");
    end if;
    if not Check_Verbose then
      return;
    end if;
    Verbosity := Put_Subst_Nb;
  end if;
  if Arg_Dscr.Is_Set (14) then
    -- Tmp_Dir for temporary files
    begin
      Tmp_Dir := As.U.Tus (Arg_Dscr.Get_Option (14));
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
  if Arg_Dscr.Is_Set (15) then
    -- Quiet mode
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option quiet");
    end if;
    if not Check_Verbose then
      return;
    end if;
    Verbosity := Quiet;
  end if;
  if Arg_Dscr.Is_Set (16) then
    -- Make backup
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option make backup");
    end if;
    Backup := True;
  end if;
  if Arg_Dscr.Is_Set (17) then
    -- Test mode
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option test");
    end if;
    Test := True;
  end if;
  if Arg_Dscr.Is_Set (18) then
    -- Process utf-8 sequences
    if Arg_Dscr.Is_Set (1) then
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
         & ": Syntax ERROR. Incompatible options -a and -u.");
      Error;
      return;
    end if;
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option utf8");
    end if;
    Lang := Language.Lang_Utf_8;
  end if;
  if Arg_Dscr.Is_Set (19) then
    -- Verbose put each substit
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option verbose");
    end if;
    if not Check_Verbose then
      return;
    end if;
    Verbosity := Verbose;
  end if;
  if Arg_Dscr.Is_Set (20) then
    -- Find pattern is not a regex
    if Debug.Set then
      Basic_Proc.Put_Line_Error ("Option noregex");
    end if;
    Is_Regex := False;
  end if;

  -- Set language (for regexp)
  Language.Set_Language (Lang);
  if Debug.Set then
    Basic_Proc.Put_Line_Error ("Regex assumes language to be "
       & Mixed_Str (Language.Language_List'Image(
                Language.Get_Language)));
  end if;

  -- Parse (including checks) search pattern and replace string
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
        & ": Syntax ERROR. Grep mode imposes quiet, test and no backup.");
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
  -- Invert => Grep
  if Grep_Invert and then not Grep then
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
      & ": Syntax ERROR. Invert_match option requires grep mode.");
    Error;
    return;
  end if;
  -- Grep AND (File_Name or List or Invert) => empty Replace_Pattern
  if Grep and then (Grep_File_Name or else Grep_List or else Grep_Invert)
  and then Arg_Dscr.Get_Option (No_Key_Index, 2) /= "" then
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
      & ": Syntax ERROR. Grep with file, list or invertmatch implies empty replace string.");
    Error;
    return;
  end if;
  -- Grep Invert => Single regexp
  if Grep and then Grep_Invert
  and then Search_Pattern.Number /= 1 then
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
      & ": Syntax ERROR. Grep with invertmatch implies single search pattern.");
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
    Basic_Proc.Put_Line_Error ("Search pattern: >"
       & Arg_Dscr.Get_Option (No_Key_Index, 1) & "<");
    if Exclude.Image /= "" then
      Basic_Proc.Put_Line_Output ("Exclude pattern: >" & Exclude.Image & "<");
    end if;
    if not Grep then
      Basic_Proc.Put_Line_Error ("Replace string: >"
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
          declare
            File : constant String := File_Mng.Get_Next_File;
          begin
            if File = Std_In_Out
            and then Arg_Dscr.Get_Option (05, 1) = File_Mng.Stdin then
              Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
                  & ": ERROR. Cannot use Stdin flow when file list is Stdin."
                  & " Skipping.");
              Ok := False;
            end if;
            Do_One_File (File);
          end;
        exception
          when File_Mng.End_Error =>
            exit;
          when File_Mng.Io_Error =>
            Ok := False;
            exit;
        end;
        end loop;
    end if;
  elsif Arg_Dscr.Get_Nb_Occurences (No_Key_Index) = 2
  or else Arg_Dscr.Get_Option (No_Key_Index, 3) = Std_In_Out then
    -- No file or "-": stdin -> stdout
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
            Grep_List      => Grep_List,
            Grep_File_Name => Grep_File_Name,
            Grep_Line_Nb   => Grep_Line_Nb,
            Grep_Invert    => Grep_Invert,
            Test           => Test);
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

