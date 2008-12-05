with Ada.Exceptions, Ada.Text_Io, Ada.Strings.Unbounded;
with Environ, Argument, Argument_Parser, Sys_Calls, Language, Mixed_Str, Text_Line;
with Search_Pattern, Replace_Pattern, Substit, File_Mng, Debug;
procedure Asubst is

  Version : constant String  := "V7.5";

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
     "  <option> ::= -a | -D <string> | -d | -e <pattern> | -f | -g | -i");
    Sys_Calls.Put_Line_Error (
     "             | -l | -m <max> | -n | -p | -q | -s | -t | -u | -v | -x | --");
    Sys_Calls.Put_Line_Error (
     "    -a or --ascii for pure ASCII processing,");
    Sys_Calls.Put_Line_Error (
     "    -D <string> or --delimiter=<string> for a delimiter other than '\n',");
    Sys_Calls.Put_Line_Error (
     "    -d or --display for display find and exclude patterns and replace_string,");
    Sys_Calls.Put_Line_Error (
     "    -e <pattern> or --exclude=<pattern> for skip text matching <pattern>,");
    Sys_Calls.Put_Line_Error (
     "    -f or --file to indicate that <file> will be a list of file names,");
    Sys_Calls.Put_Line_Error (
     "    -g or --grep to print matching text as grep would do (no subst),");
    Sys_Calls.Put_Line_Error (
     "    -i or --ignorecase for case insensitive match,");
    Sys_Calls.Put_Line_Error (
     "    -l or --line for display line number in grep mode,");
    Sys_Calls.Put_Line_Error (
     "    -m <max> or --max=<max> for stop processing file after <max> substitutions,");
    Sys_Calls.Put_Line_Error (
     "    -n or --number for print number of substitutions,");
    Sys_Calls.Put_Line_Error (
     "    -p <dir> or --tmp=<dir> for directory of temporary files,");
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
     "    -x or --noregex for <find_pattern> being considered as string(s),");
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
     "    A <regex> can contain ""\t"" (tab), ""\s"" (space), ""\xIJ"" (any hexa byte),");
    Sys_Calls.Put_Line_Error (
     "     or ""\I"" (I from 1 to 9, a back reference to a matching substring).");
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
     "    In noregex mode, only ""\t"", ""\s"", ""\xIJ"" and ""\n"" are interpreted");
    Sys_Calls.Put_Line_Error (
     "    and ""\x00"" is allowed (forbidden in a regex).");

    Sys_Calls.Put_Line_Error (
     "  <replace_string> is a string with the following specific sequences:");
    Sys_Calls.Put_Line_Error (
     "    ""\n"" (new_line), ""\t"" (tab), ""\s"" (space), ""\xIJ"" (hexa byte value).");
    Sys_Calls.Put_Line_Error (
     "    ""\iIJ<text>"" to replace by <text> if the Jth substring of the Ith regex");
    Sys_Calls.Put_Line_Error (
     "      matches. ""\iIJ"" can be followed by one or several""\aIJ"" (and then) and");
    Sys_Calls.Put_Line_Error (
     "      ""\oIJ"" (or else). <text> ends when encountering another ""\iIJ"" (elsif),");
    Sys_Calls.Put_Line_Error (
     "      a ""\e"" (else) or a ""\f"" (end if). Ex: \i11\o12\a13OK\eNOK\f.");
    Sys_Calls.Put_Line_Error (
     "      The logic is if... elsif... elsif... else... endif.");
    Sys_Calls.Put_Line_Error (
     "    ""\RIJ"" (IJ in hexa) to replace by the input text matching the IJth regex,");
    Sys_Calls.Put_Line_Error (
     "    ""\rIJ"" to replace by the text matching the Jth substring of the Ith regex.");
    Sys_Calls.Put_Line_Error (
     "    ""\u"" (start UPPERCASE conversion), ""\l"" (lowercase), ""\m"" (Mixed_Case),");
    Sys_Calls.Put_Line_Error (
     "      ""\c"" (stop case conversion). Any new conversion replaces previous.");
    Sys_Calls.Put_Line_Error (
     "    Conditions apply first, then replacement, then case conversion.");
    Sys_Calls.Put_Line_Error (
     "    ""\R01"" <-> 1st <regex>, ""\R00"" <-> all <regex>, ""\ri0"" == ""\R0i"".");
    Sys_Calls.Put_Line_Error (
     "    Like back references, substrs are numbered in order of opening parentheses.");
    Sys_Calls.Put_Line_Error (
     "    Note that ""\r0i"" and ""\i0i"" are is forbidden.");

    Sys_Calls.Put_Line_Error (
     "  If set <exclude_pattern> must have the same number of regex as <find_pattern>.");
    Sys_Calls.Put_Line_Error (
     "  Text matching <find_pattern> will be discarded if it also matches");
    Sys_Calls.Put_Line_Error (
     "    <exclude_pattern>.");
    Sys_Calls.Put_Line_Error (
     "  If a specific delimiter is set, it is used to read chunks of input text (whole");
    Sys_Calls.Put_Line_Error (
     "   flow if delimiter is empty). The <find_pattern> must be a simple <regex>");
    Sys_Calls.Put_Line_Error (
    "    (no '^' or '$', but '\n' is allowed), and applies to each chunk.");
    Sys_Calls.Put_Line_Error (
    "    This allows multi-row processing.");

    Sys_Calls.Put_Line_Error (
     "  Warning: regex are powerfull (see ""man 3 pcre"" and ""man 1 perlre"") and");
    Sys_Calls.Put_Line_Error (
     "    automatic substitution can be dangerous, so use " & Argument.Get_Program_Name & " with caution:");
    Sys_Calls.Put_Line_Error (
     "    test pattern with ""echo string | " &  Argument.Get_Program_Name & " <search_pattern> <replace_string>""");
    Sys_Calls.Put_Line_Error (
     "    and use -s or -tv option if unsure.");
    Sys_Calls.Set_Exit_Code (Error_Exit_Code);
  end Help;

  procedure Error is
  begin
    Usage;
    Sys_Calls.Set_Exit_Code (Error_Exit_Code);
  end Error;

  package Asu renames Argument_Parser.Asu;
  function Asu_Tus (Source : in String) return Argument_Parser.Asu_Us
                   renames Asu.To_Unbounded_String;

  -- For getenv
  Utf8_Var_Name : constant String := "ASUBST_UTF8";
  -- The keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => ('a', Asu_Tus ("ascii"), False, False),
   02 => ('D', Asu_Tus ("delimiter"), False, True),
   03 => ('d', Asu_Tus ("display"), False, False),
   04 => ('e', Asu_Tus ("exclude"), False, True),
   05 => ('f', Asu_Tus ("file"), False, False),
   06 => ('g', Asu_Tus ("grep"), False, False),
   07 => ('h', Asu_Tus ("help"), False, False),
   08 => ('i', Asu_Tus ("ignorecase"), False, False),
   09 => ('l', Asu_Tus ("line"), False, False),
   10 => ('m', Asu_Tus ("max"), False, True),
   11 => ('n', Asu_Tus ("number"), False, False),
   12 => ('q', Asu_Tus ("quiet"), False, False),
   13 => ('s', Asu_Tus ("save"), False, False),
   14 => ('t', Asu_Tus ("test"), False, False),
   15 => ('u', Asu_Tus ("utf8"), False, False),
   16 => ('v', Asu_Tus ("verbose"), False, False),
   17 => ('V', Asu_Tus ("version"), False, False),
   18 => ('x', Asu_Tus ("noregex"), False, False),
   19 => ('p', Asu_Tus ("tmp"), False, True)
   );
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  No_Key_Index : constant Argument_Parser.The_Keys_Index
               := Argument_Parser.No_Key_Index;

  -- Option management
  Display : Boolean := False;
  Exclude : Ada.Strings.Unbounded.Unbounded_String;
  File_Of_Files : Boolean := False;
  Case_Sensitive : Boolean := True;
  Max : Substit.Long_Long_Natural := 0;
  Tmp_Dir : Ada.Strings.Unbounded.Unbounded_String;
  type Verbose_List is (Quiet, Put_File_Name, Put_Subst_Nb, Verbose);
  Verbosity : Verbose_List := Put_File_Name;
  Grep : Boolean := False;
  Line_Nb : Boolean := False;
  Backup : Boolean := False;
  Is_Regex : Boolean := True;
  Test : Boolean := False;
  Delimiter : Ada.Strings.Unbounded.Unbounded_String
            := Ada.Strings.Unbounded.To_Unbounded_String
               (Text_Line.Line_Feed_Str);
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
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
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
      Ada.Text_Io.Put_Line (File_Name);
    end if;
    Nb_Subst := Substit.Do_One_File (
                  File_Name,
                  Asu.To_String (Tmp_Dir),
                  Asu.To_String (Delimiter),
                  Max, Backup, Verbosity = Verbose, Grep, Line_Nb, Test);
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
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Environ set to UTF-8");
    end if;
  elsif Environ.Is_No (Utf8_Var_Name) then
    Lang := Language.Lang_C;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Environ set to NO UTF-8");
    end if;
  end if;

  -- Parse keys and options
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR. "
      & Arg_Dscr.Get_Error & ".");
    Error;
    return;
  end if;

  -- Check version and help, must be alone
  if Arg_Dscr.Is_Set (17) then
    -- Version
    if Argument.Get_Nbre_Arg /= 1 then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
      Error;
    else
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & " " & Version);
      Sys_Calls.Set_Exit_Code (Error_Exit_Code);
    end if;
    return;
  elsif Arg_Dscr.Is_Set (07) then
    -- Help
    if  Argument.Get_Nbre_Arg /= 1 then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
      Error;
    else
      Help;
    end if;
    return;
  elsif Arg_Dscr.Get_Nb_Occurences (No_Key_Index) < 2
  or else Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    -- There must be at least Search and Replace strings
    -- They must be after options
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
    Error;
    return;
  end if;

  -- Parse options
  if Arg_Dscr.Is_Set (01) then
    -- Force ASCII processing even if ENV was set
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option ascii");
    end if;
    Lang := Language.Lang_C;
  end if;
  if Arg_Dscr.Is_Set (02) then
    -- Specific delimiter instead of '\n'
    begin
      Delimiter := Ada.Strings.Unbounded.To_Unbounded_String
                 (Arg_Dscr.Get_Option (02));
      if Ada.Strings.Unbounded.Length (Delimiter)
         > Text_Line.Max_Line_Feed_Len then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
           & ": Syntax ERROR. Invalid delimiter.");
        Error;
        return;
    end;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option delimiter = "
          & Ada.Strings.Unbounded.To_String (Delimiter));
    end if;
  end if;
  if Arg_Dscr.Is_Set (03) then
    -- Display patterns
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option display patterns");
    end if;
    Display := True;
  end if;
  if Arg_Dscr.Is_Set (04) then
    -- Exclude text matching exclude_regexp
    begin
      Exclude := Ada.Strings.Unbounded.To_Unbounded_String
                 (Arg_Dscr.Get_Option (04));
      if Ada.Strings.Unbounded.Length(Exclude) = 0 then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
           & ": Syntax ERROR. Invalid exclude_pattern.");
        Error;
        return;
    end;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option exclude = "
          & Ada.Strings.Unbounded.To_String (Exclude));
    end if;
  end if;
  if Arg_Dscr.Is_Set (05) then
    -- The file will be a list of files
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option file of files");
    end if;
    File_Of_Files := True;
  end if;
  if Arg_Dscr.Is_Set (06) then
    -- Put matching text like grep would do
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option grep display");
    end if;
    Grep := True;
  end if;
  if Arg_Dscr.Is_Set (08) then
    -- Case insensitive match
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option ignore case");
    end if;
    Case_Sensitive := False;
  end if;
  if Arg_Dscr.Is_Set (09) then
    -- Put line no
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option line no");
    end if;
    Line_Nb := True;
  end if;
  if Arg_Dscr.Is_Set (10) then
    -- Stop each file after <max> substitutions
    begin
      Max := Substit.Long_Long_Natural'Value (Arg_Dscr.Get_Option (07));
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
  end if;
  if Arg_Dscr.Is_Set (11) then
    -- Put number of substitutions
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option put numbers");
    end if;
    if not Check_Verbose then
      return;
    end if;
    Verbosity := Put_Subst_Nb;
  end if;
  if Arg_Dscr.Is_Set (12) then
    -- Quiet mode
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option quiet");
    end if;
    if not Check_Verbose then
      return;
    end if;
    Verbosity := Quiet;
  end if;
  if Arg_Dscr.Is_Set (13) then
    -- Make backup
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option make backup");
    end if;
    Backup := True;
  end if;
  if Arg_Dscr.Is_Set (14) then
    -- Test mode
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option test");
    end if;
    Test := True;
  end if;
  if Arg_Dscr.Is_Set (15) then
    -- Process utf-8 sequences
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option utf8");
    end if;
    Lang := Language.Lang_Utf_8;
  end if;
  if Arg_Dscr.Is_Set (16) then
    -- Verbose put each substit
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option verbose");
    end if;
    if not Check_Verbose then
      return;
    end if;
    Verbosity := Verbose;
  end if;
  if Arg_Dscr.Is_Set (18) then
    -- Find pattern is not a regex
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option noregex");
    end if;
    Is_Regex := False;
  end if;
  if Arg_Dscr.Is_Set (19) then
    -- Tmp_Dir for temporary files
    begin
      Tmp_Dir := Ada.Strings.Unbounded.To_Unbounded_String
                 (Arg_Dscr.Get_Option (19));
      if Ada.Strings.Unbounded.Length(Tmp_Dir) = 0 then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
           & ": Syntax ERROR. Invalid tmp dir.");
        Error;
        return;
    end;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Option tmp_dir = "
          & Ada.Strings.Unbounded.To_String (Tmp_Dir));
    end if;
  end if;

  -- Set language (for regexp)
  Language.Set_Language (Lang);
  if Debug.Set then
    Sys_Calls.Put_Line_Error ("Regex assumes language to be "
       & Mixed_Str (Language.Language_List'Image(
                Language.Get_Language)));
  end if;

  begin
    Search_Pattern.Parse (
         Arg_Dscr.Get_Option (No_Key_Index, 1),
         Ada.Strings.Unbounded.To_String (Exclude),
         Ada.Strings.Unbounded.To_String (Delimiter),
         Case_Sensitive, Is_Regex);
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
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
        & ": Syntax ERROR. Grep mode imposes quiet, test and no-backup.");
      Error;
      return;
    end if;
    Test := True;
    Verbosity := Quiet;
    Backup := False;
  end if;
  -- Line_Nb => Grep
  if Line_Nb and then not Grep then
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
      & ": Syntax ERROR. Line_nb mode is allowed in grep mode only.");
    Error;
    return;
  end if;
  -- Grep AND Line_Nb => empty Replace_Pattern
  if Grep and then Line_Nb
  and then Arg_Dscr.Get_Option (No_Key_Index, 2) /= "" then
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
      & ": Syntax ERROR. Grep with line_nb implies empty replace_string.");
    Error;
    return;
  end if;

  -- One file argument if file of files
  if File_Of_Files
  and then Arg_Dscr.Get_Nb_Occurences (No_Key_Index) /= 3 then
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                   & ": Syntax ERROR. One file (only) must be supplied"
                   & "  with -f or --file option.");
    Error;
    return;
  end if;

  -- Display search pattern and replace string
  if Display then
    Ada.Text_Io.Put_Line ("Search pattern: >"
       & Arg_Dscr.Get_Option (No_Key_Index, 1) & "<");
    if Asu.To_String (Exclude) /= "" then
      Ada.Text_Io.Put_Line ("Exclude pattern: >"
         & Asu.To_String (Exclude) & "<");
    end if;
    if not Grep then
      Ada.Text_Io.Put_Line ("Replace string: >"
         & Arg_Dscr.Get_Option (No_Key_Index, 2) & "<");
    end if;
  end if;

  -- Process files
  Ok := True;
  Found := False;
  if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) = 2 then
    -- No file: stdin -> stdout
    if Backup then
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                & ": ERROR. Cannot make backup when no file name.");
      Ok := False;
    else
      begin
        Nb_Subst := Substit.Do_One_File (
            File_Name => Substit.Std_In_Out,
            Tmp_Dir   => Asu.To_String (Tmp_Dir),
            Delimiter => Asu.To_String (Delimiter),
            Max_Subst => Max,
            Backup    => False,
            Verbose   => False,
            Grep      => Grep,
            Line_Nb   => Line_Nb,
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
      File_Mng.Open (Arg_Dscr.Get_Option (No_Key_Index, 3));
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
    for I in 3 .. Arg_Dscr.Get_Nb_Occurences (No_Key_Index) loop
      Do_One_File (Arg_Dscr.Get_Option (No_Key_Index, I));
    end loop;
  end if;

  -- Clean argument parser memory
  Arg_Dscr.Reset;

  if not Ok then
    Sys_Calls.Set_Exit_Code (Error_Exit_Code);
  else
    if not Found then
      Sys_Calls.Set_Exit_Code (No_Subst_Exit_Code);
    end if;
  end if;
end Asubst;

