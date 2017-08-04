with As.U, Basic_Proc, Argument, Argument_Parser, Str_Util, Trilean;
with Entities, Output, Targets, Lister, Exit_Code;
procedure Als is
  Version : constant String  := "V19.2";

  -- The keys and descriptor of parsed keys
  Nkc : constant Character := Argument_Parser.No_Key_Char;
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => (False, 'a', As.U.Tus ("all"),          False),
   02 => (False, 'A', As.U.Tus ("All"),          False),
   03 => (False, 'l', As.U.Tus ("list"),         False),
   04 => (False, '1', As.U.Tus ("1row"),         False),
   05 => (False, 'D', As.U.Tus ("directories"),  False),
   06 => (False, 'r', As.U.Tus ("reverse"),      False),
   07 => (False, 'R', As.U.Tus ("recursive"),    False),
   08 => (False, 's', As.U.Tus ("size"),         False),
   09 => (False, 't', As.U.Tus ("time"),         False),
   10 => (False, 'M', As.U.Tus ("merge"),        False),
   11 => (True,  'd', As.U.Tus ("date"),         True,  True, As.U.Tus ("date_comp><date")),
   12 => (False, 'h', As.U.Tus ("help"), False),
   13 => (False, 'v', As.U.Tus ("version"),      False),
   14 => (False, 'L', As.U.Tus ("links"),        False),
   15 => (False, 'F', As.U.Tus ("files"),        False),
   16 => (True,  'm', As.U.Tus ("match"),        True,  True, As.U.Tus ("criteria")),
   17 => (True,  'e', As.U.Tus ("exclude"),      True,  True, As.U.Tus ("criteria")),
   18 => (True,  Nkc, As.U.Tus ("match_dir"),    True,  True, As.U.Tus ("criteria")),
   19 => (True,  Nkc, As.U.Tus ("exclude_dir"),  True,  True, As.U.Tus ("criteria")),
   20 => (True,  'S', As.U.Tus ("separator"),    False, True, As.U.Asu_Null),
   21 => (False, 'T', As.U.Tus ("total"),        False),
   22 => (True,  'n', As.U.Tus ("newer"),        False, True, As.U.Tus ("date")),
   23 => (False, 'c', As.U.Tus ("classify"), False),
   24 => (True,  Nkc, As.U.Tus ("depth"),        False, True, As.U.Tus ("positive")),
   25 => (False, 'H', As.U.Tus ("human"),        False),
   26 => (False, 'N', As.U.Tus ("no_sort"),      False),
   27 => (False, 'p', As.U.Tus ("path"),         False),
   28 => (False, 'B', As.U.Tus ("broken_links"), False),
   29 => (False, Nkc, As.U.Tus ("follow_links"), False),
   30 => (False, Nkc, As.U.Tus ("date_iso"),     False),
   31 => (False, Nkc, As.U.Tus ("skip_dirs"),    False),
   32 => (False, 'O', As.U.Tus ("others"),       False),
   33 => (True,  Nkc, As.U.Tus ("dir_name"),     False, True, As.U.Tus ("always|never")),
   34 => (False, 'U', As.U.Tus ("utc"),          False),
   35 => (False, Nkc, As.U.Tus ("len_alpha"),    False),
   36 => (False, 'q', As.U.Tus ("quiet"),        False),
   37 => (True,  Nkc, As.U.Tus ("discard_dir"),  True,  True, As.U.Tus ("criteria")),
   38 => (False, 'b', As.U.Tus ("basename"),     False),
   39 => (False, Nkc, As.U.Tus ("nodir"),        False),
   40 => (True,  Nkc, As.U.Tus ("access"),       False, True, As.U.Tus ("rights")) );
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Usage
  function Key_Img (I : Argument_Parser.The_Keys_Range) return String is
    (Str_Util.Procuste (Argument_Parser.Image(Keys(I)), 19));

  procedure Usage is
    use Basic_Proc;
  begin
    Put_Line_Error ("Usage:" & Argument.Get_Program_Name
      & " [ { <option> } ] [ { <file_or_dir_name> } ]");
    Put_Line_Error ("   or: " & Argument.Get_Program_Name
      & " " & Argument_Parser.Image(Keys(12))
      & " | " & Argument_Parser.Image(Keys(13)));

    Put_Line_Error ("Which entries to show:");
    Put_Line_Error ("  " & Key_Img(01) & "// Show all entries including hidden (starting with '.')");
    Put_Line_Error ("  " & Key_Img(02) & "// Show all entries except ""."" and ""..""");
    Put_Line_Error ("  " & Key_Img(05) & "// Show only directories");
    Put_Line_Error ("  " & Key_Img(14) & "// Show only symbolic links");
    Put_Line_Error ("  " & Key_Img(15) & "// Show only regular files");
    Put_Line_Error ("  " & Key_Img(32) & "// Show other entries (device, pipe, socket...)");
    Put_Line_Error ("  " & Key_Img(28) & "// Show only broken symbolic links");
    Put_Line_Error ("  " & Key_Img(07) & "// Scan directories recursively");
    Put_Line_Error ("  " & Key_Img(24) & "// Scan only to given depth (needs ""-R"")");
    Put_Line_Error ("  " & Key_Img(31) & "// Skip directories from arguments");
    Put_Line_Error ("  " & Key_Img(40) & "// Show only file with the given access for current user");
    Put_Line_Error ("                     //   ex: ""rw*"" read and write, exec or not");
    Put_Line_Error ("                     //       ""r-x"" read, not write, exec");

    Put_Line_Error ("  <match_name>   ::= " & Argument_Parser.Image(Keys(16)));
    Put_Line_Error ("    <criteria>   ::= <templates> | @<regex>");
    Put_Line_Error ("    <templates>  ::= <template> [ { ,<template> } ]");
    Put_Line_Error ("                     // Keep only files which name matches the criteria");
    Put_Line_Error ("                     //  (one template or the regular expression)");
    Put_Line_Error ("  <exclude_name> ::= " & Argument_Parser.Image(Keys(17)));
    Put_Line_Error ("                     // Exclude files that match the criteria");
    Put_Line_Error ("  <match_dir>    ::= " & Argument_Parser.Image(Keys(18)));
    Put_Line_Error ("                     // Show only directories that match the criteria");
    Put_Line_Error ("  <exclude_dir>  ::= " & Argument_Parser.Image(Keys(19)));
    Put_Line_Error ("                     // Don't show directories that match the criteria");
    Put_Line_Error ("  <discard_dir>  ::= " & Argument_Parser.Image(Keys(37)));
    Put_Line_Error ("                     // Discard directories that match the criteria");
    Put_Line_Error ("  <date_spec> [ <date_spec> ]");
    Put_Line_Error ("    <date_spec> ::= " & Argument_Parser.Image(Keys(11)));
    Put_Line_Error ("    <date_comp> ::= eq | lt | le | gt | ge");
    Put_Line_Error ("    <date>      ::= yyyy-mm-ddThh:mm:ss | yyyy-mm-dd | Thh:mm:ss");
    Put_Line_Error ("                  | <positive><duration>");
    Put_Line_Error ("    <duration>  ::= Y | M | D | h | m | s");
    Put_Line_Error ("                     // Keep files that match the date specification");
    Put_Line_Error ("                     //  (before, after or equal to a given date or delay)");
    Put_Line_Error ("    " & Argument_Parser.Image(Keys(22)) & " is a shortcut to ""-d ge<date>""");
    Put_Line_Error ("How to show each entry (file or dir):");
    Put_Line_Error ("  " & Key_Img(03) & "// Show rights, owner, size, modif date, symlink target");
    Put_Line_Error ("  " & Key_Img(04) & "// One name per line");
    Put_Line_Error ("  " & Key_Img(23) & "// Append '/' to dirs, '@' to symlinks");
    Put_Line_Error ("  " & Key_Img(25) & "// Show sizes in friendly format (e.g. 1K, 2G)");
    Put_Line_Error ("  " & Key_Img(27) & "// Show full path of entries");
    Put_Line_Error ("  <separator> ::= " & Argument_Parser.Image(Keys(20)));
    Put_Line_Error ("                     // Insert <string> between each entry");
    Put_Line_Error ("  " & Key_Img(29) & "// Show final target of symlinks");
    Put_Line_Error ("  " & Key_Img(30) & "// Show date in strict ISO format (<date>T<time>)");
    Put_Line_Error ("  " & Key_Img(38) & "// In Merge mode, show only basename of each entry");
    Put_Line_Error ("How to organize entry list:");
    Put_Line_Error ("  " & Key_Img(08) & "// Sort by decreasing size (see also ""-r"")");
    Put_Line_Error ("  " & Key_Img(09) & "// Sort by decreasing time (see also ""-r"")");
    Put_Line_Error ("  " & Key_Img(35) & "// Sort by increasing name length (see also ""-r"")");
    Put_Line_Error ("  " & Key_Img(06) & "// Sort (by name, size, time or len) in reverse order");
    Put_Line_Error ("  " & Key_Img(26) & "// Keep same order as in the directory structure");
    Put_Line_Error ("  " & Key_Img(10) & "// Show a global list of entries (without dir names)");
    Put_Line_Error ("  " & Argument_Parser.Image(Keys(33)));
    Put_Line_Error ("                     // Show names of directories (default=when not empty)");
    Put_Line_Error ("  " & Key_Img(39) & "// Do not show names of directories (--dir_name=never)");
    Put_Line_Error ("  " & Key_Img(21) & "// Also show number and total size of listed entries");
    Put_Line_Error ("  " & Key_Img(34) & "// Use UTC i.o. local time for date spec and output");
    Put_Line_Error ("  " & Key_Img(36) & "// Do not show entries");
    Put_Line_Error ("Exits with 0 if a result, 1 if none and 2 on error.");
  end Usage;
  Error_Exception : exception;
  procedure Error (Msg : in String := "") is
  begin
    Basic_Proc.Put_Error (Argument.Get_Program_Name & ": Syntax ERROR.");
    if Msg /= "" then
      Basic_Proc.Put_Line_Error (" " & Msg & ".");
    else
      Basic_Proc.New_Line_Error;
    end if;
    Usage;
    raise Error_Exception;
  end Error;

  -- Option management
  List_Dots, List_Roots_And_Dots : Boolean;
  Dots : Entities.Dots_Kind_List;
  Long_List : Boolean;
  Follow_Links : Boolean;
  Human : Boolean;
  One_Row : Boolean;
  List_Only_Dirs : Boolean;
  List_Only_Files : Boolean;
  List_Only_Links : Lister.Link_Criteria_List;
  Sort_Reverse : Boolean;
  Recursive : Boolean;
  Sort_By_Size : Boolean;
  Sort_By_Time : Boolean;
  Sort_By_Len : Boolean;
  No_Sorting : Boolean;
  Merge_Lists : Boolean;
  Date1, Date2 : Entities.Date_Spec_Rec;
  Separator : As.U.Asu_Us;
  Put_Total : Boolean;
  Classify : Boolean;
  Depth : Natural;
  Full_Path : Boolean;
  Date_Iso : Boolean;
  Skip_Dirs : Boolean;
  List_Only_Others : Boolean;
  Dir_Name : Trilean.Trilean;
  Utc : Boolean;
  Quiet : Boolean;
  Basename : Boolean;
  Access_Rights : Lister.Access_Rights;

  -- Parse a date argument
  function Parse_Date (Str : String) return Entities.Date_Spec_Rec is separate;

  -- Set a file or dir, match or exclusion criteria, or dir discard criteria
  type Call_Access is access procedure (Template : in String;
                                        Regex    : in Boolean);
  procedure Set_Criteria (Name     : in String;
                          Criteria : in String;
                          Call     : in Call_Access) is separate;
  use type Entities.Date_Oper_List;
begin

  -- Parse keys and options
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error (Arg_Dscr.Get_Error);
  end if;

  if Arg_Dscr.Is_Set (12) then
    -- Help
    if Argument.Get_Nbre_Arg /= 1 then
     Error;
    else
      Usage;
      raise Error_Exception;
    end if;
  elsif Arg_Dscr.Is_Set (13) then
    -- Version
    if Argument.Get_Nbre_Arg /= 1 then
      Error;
    else
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & " " & Version);
      raise Error_Exception;
    end if;
  end if;

  -- Any path/file spec must be after options
  -- Only empty arguments are allowed to be embedded
  for I in 1 .. Arg_Dscr.Get_Nb_Embedded_Arguments loop
    declare
      -- Index of this non_key
      Arg_Pos : constant Positive
              := Arg_Dscr.Get_Position (Argument_Parser.No_Key_Index, I);
    begin
      if Argument.Get_Parameter (Arg_Pos, Argument.Any_Arg) /= "" then
        Error;
      end if;
    end;
  end loop;

  -- Parse options
  List_Roots_And_Dots := Arg_Dscr.Is_Set (01);
  List_Dots := Arg_Dscr.Is_Set (02);
  if List_Roots_And_Dots then
    Dots := Entities.Basic_Dots_Roots;
  elsif List_Dots then
    Dots := Entities.Basic_Dots;
  else
    Dots := Entities.Basic;
  end if;
  Long_List := Arg_Dscr.Is_Set (03);
  Follow_Links := Arg_Dscr.Is_Set (29);
  One_Row := Arg_Dscr.Is_Set (04);
  List_Only_Dirs := Arg_Dscr.Is_Set (05);
  Sort_Reverse := Arg_Dscr.Is_Set (06);
  Recursive := Arg_Dscr.Is_Set (07);
  Sort_By_Size := Arg_Dscr.Is_Set (08);
  Sort_By_Time := Arg_Dscr.Is_Set (09);
  Merge_Lists := Arg_Dscr.Is_Set (10);
  List_Only_Files := Arg_Dscr.Is_Set (15);
  Classify := Arg_Dscr.Is_Set (23);
  Human := Arg_Dscr.Is_Set (25);
  No_Sorting := Arg_Dscr.Is_Set (26);
  Full_Path := Arg_Dscr.Is_Set (27);
  Date_Iso := Arg_Dscr.Is_Set (30);
  List_Only_Others := Arg_Dscr.Is_Set (32);
  Utc := Arg_Dscr.Is_Set (34);
  Sort_By_Len := Arg_Dscr.Is_Set (35);
  Depth := 0;
  Quiet := Arg_Dscr.Is_Set (36);

  -- Access rights
  Access_Rights := (others => Trilean.Other);
  if Arg_Dscr.Is_Set (40) then
    declare
      -- Using a String raises Constraint_Error when reading it,
      -- with GNAT GPL 2016
       Str : constant As.U.Asu_Us := As.U.Tus (Arg_Dscr.Get_Option(40, 1));
      procedure Set (I : in Lister.Rights_List; Key : in Character) is
         Char : constant Character
              := Str.Element(Lister.Rights_List'Pos(I) + 1);
        use Trilean;
      begin
        -- key ('r', 'w', or 'x') => needs access, '-' => needs no access
        -- '*' => no criteria
        if    Char = Key then Access_Rights(I) := True;
        elsif Char = '-' then Access_Rights(I) := False;
        elsif Char /= '*' then raise Constraint_Error;
        end if;
      end Set;
    begin
      if Str.Length /= 3 then
        raise Constraint_Error;
      end if;
      Set (Lister.Read,  'r');
      Set (Lister.Write, 'w');
      Set (Lister.Exec,  'x');
    exception
      when Constraint_Error =>
        Error ("Invalid specification of access rights");
    end;
  end if;

  -- Check sorting
  if          (Sort_By_Time and then Sort_By_Size)
      or else (Sort_By_Time and then Sort_By_Len)
      or else (Sort_By_Size and then Sort_By_Len) then
    Error ("-s (--size), -t (--time) and --alpha_len are mutually exclusive");
  end if;
  if No_Sorting and then
      (Sort_By_Size or else Sort_By_Time
       or else Sort_By_Len or else Sort_Reverse) then
    Error ("-n (--no_sort) is exclusive with other sorting options");
  end if;

  -- Check dates
  if Arg_Dscr.Is_Set (11) and then Arg_Dscr.Is_Set (22) then
    Error ("-d (--date) and -n (--new) are mutially exclusive");
  end if;
  if Arg_Dscr.Get_Nb_Occurences (11) > 2 then
    Error ("At most two dates can be specified");
  elsif Arg_Dscr.Get_Nb_Occurences (11) /= 0 then
    Date1 := Parse_Date (Arg_Dscr.Get_Option(11, 1));
    if Arg_Dscr.Get_Nb_Occurences (11) = 2 then
      Date2 := Parse_Date (Arg_Dscr.Get_Option(11, 2));
    else
      Date2.Oper := Entities.None;
    end if;
  end if;
  if Arg_Dscr.Get_Nb_Occurences (11) = 2 then
    if Date1.Oper = Entities.Equal or else Date2.Oper = Entities.Equal then
      Error ("With two dates, none can be ""eq""");
    end if;
    if (Date1.Oper in Entities.Less_Oper_List
           and then Date2.Oper in Entities.Less_Oper_List)
    or else (Date1.Oper in Entities.Greater_Oper_List
             and then Date2.Oper in Entities.Greater_Oper_List) then
      Error ("With two dates, one must be ""lt"" or ""le"" "
             & "and the other ""gt or ""ge""");
    end if;
  end if;
  -- Parse option newer
  if Arg_Dscr.Get_Nb_Occurences (22) /= 0 then
    Date1 := Parse_Date ("ge" & Arg_Dscr.Get_Option(22, 1));
    Date2.Oper := Entities.None;
  end if;

  -- Some other simple options
  if Arg_Dscr.Is_Set (14) then
    List_Only_Links := Lister.All_Links;
  elsif Arg_Dscr.Is_Set (28) then
    List_Only_Links := Lister.Broken_Links;
  else
    List_Only_Links := Lister.No_Link;
  end if;

  -- Add match template if any
  for I in 1 .. Arg_Dscr.Get_Nb_Occurences (16) loop
    begin
      Set_Criteria ("match", Arg_Dscr.Get_Option (16, I),
                    Lister.Add_Match'Access);
    exception
      when Lister.Invalid_Template =>
        Error ("Invalid match template " & Arg_Dscr.Get_Option (16, I));
    end;
  end loop;
  -- Add exclude template if any
  for I in 1 .. Arg_Dscr.Get_Nb_Occurences (17) loop
    begin
      Set_Criteria ("exclude", Arg_Dscr.Get_Option (17, I),
                    Lister.Add_Exclude'Access);
    exception
      when Lister.Invalid_Template =>
        Error ("Invalid exclude template " & Arg_Dscr.Get_Option (17, I));
    end;
  end loop;
  -- Add dir_match if any
  for I in 1 .. Arg_Dscr.Get_Nb_Occurences (18) loop
    begin
      Set_Criteria ("match dir", Arg_Dscr.Get_Option (18, I),
                    Lister.Add_Dir_Match'Access);
    exception
      when Lister.Invalid_Template =>
        Error ("Invalid dir_match " & Arg_Dscr.Get_Option (18, I));
    end;
  end loop;
  -- Add dir_exclude if any
  for I in 1 .. Arg_Dscr.Get_Nb_Occurences (19) loop
    begin
      Set_Criteria ("exclude dir",
                    Arg_Dscr.Get_Option (19, I), Lister.Add_Dir_Exclude'Access);
    exception
      when Lister.Invalid_Template =>
        Error ("Invalid dir_match " & Arg_Dscr.Get_Option (19, I));
    end;
  end loop;
  -- Add dir_discard if any
  for I in 1 .. Arg_Dscr.Get_Nb_Occurences (37) loop
    begin
      Set_Criteria ("discard dir", Arg_Dscr.Get_Option (37, I),
                    Lister.Add_Dir_Discard'Access);
    exception
      when Lister.Invalid_Template =>
        Error ("Invalid dir_match " & Arg_Dscr.Get_Option (37, I));
    end;
  end loop;

  -- Separator
  if Arg_Dscr.Is_Set (20) then
    if Arg_Dscr.Get_Option (20) = "" then
      Error ("Empty separator");
    end if;
    Separator := As.U.Tus (Arg_Dscr.Get_Option (20));
    if Long_List or else One_Row or else Classify then
      Error ("Option incompatible with separator");
    end if;
  end if;

  -- Put dir names
  if Arg_Dscr.Is_Set (33) then
    if Arg_Dscr.Is_Set (39) then
      Error ("Invalid dir_name and nodir options");
    end if;
    if Arg_Dscr.Get_Option (33) = "always" then
      Dir_Name := Trilean.True;
    elsif Arg_Dscr.Get_Option (33) = "never" then
      Dir_Name := Trilean.False;
    else
      Error ("Invalid ""dir_name"" option");
    end if;
  elsif Arg_Dscr.Is_Set (39) then
    Dir_Name := Trilean.False;
  else
    -- Default names of non empty dirs
    Dir_Name := Trilean.Other;
  end if;

  -- Only basename (in Merge)
  Basename := Arg_Dscr.Is_Set (38);

  -- Put total size
  Put_Total := Arg_Dscr.Is_Set (21);

  -- Set output criteria
  declare
    Sort_Kind : Output.Sort_Kind_List;
    Format_Kind : Output.Format_Kind_List;
  begin
    if No_Sorting then
      Sort_Kind := Output.None;
    elsif Sort_By_Time then
      -- Time is higher criteria than size
      Sort_Kind := Output.Time;
    elsif Sort_By_Size then
      Sort_Kind := Output.Size;
    elsif Sort_By_Len then
      Sort_Kind := Output.Len;
    else
      Sort_Kind := Output.Alpha;
    end if;
    -- Output format style
    if Long_List then
      if Human then
        Format_Kind := Output.Long_Human;
      else
        Format_Kind := Output.Long;
      end if;
    elsif One_Row or else Merge_Lists then
      Format_Kind := Output.One_Row;
    else
      Format_Kind := Output.Simple;
    end if;
    Output.Set_Style (Sort_Kind, Sort_Reverse, Format_Kind,
                      Merge_Lists and then not Basename,
                      Full_Path, Classify, Date_Iso, Quiet, Separator);
  end;

  -- Depth
  if Arg_Dscr.Is_Set (24) then
    if not Recursive then
      Error ("--depth option requires -R (--recursive)");
    end if;
    if Arg_Dscr.Get_Option (24) = "" then
      Error ("No depth provided");
    end if;
    begin
      Depth := Natural'Value (Arg_Dscr.Get_Option (24));
      if Depth not in Positive'Range then
        raise Constraint_Error;
      end if;
    exception
      when others =>
         Error ("Invalid depth");
    end;
  end if;

  -- Skip dirs
  if Arg_Dscr.Is_Set (31) then
    if Recursive then
      Error ("Options recursive and skip_dirs are mutually exclusive");
    end if;
    Skip_Dirs := True;
  end if;

  -- Set selection criteria in Lister, activate Total computation
  Lister.Set_Criteria (List_Only_Dirs, List_Only_Files,
                       Access_Rights,
                       List_Only_Links, List_Only_Others,
                       Follow_Links, Date1, Date2, Utc);
  if Put_Total then
    Lister.Activate_Total;
  end if;

  -- List each target
  if Targets.List (Dots, Recursive, Depth, Merge_Lists, Skip_Dirs, Dir_Name,
                   Follow_Links, Arg_Dscr) then
    Exit_Code.Update (Exit_Code.Found);
  else
    Exit_Code.Update (Exit_Code.Empty);
  end if;

  if Put_Total then
    Output.Put_Line_Size (Lister.Get_Number, Lister.Get_Total);
  end if;

exception
  when Error_Exception =>
    Exit_Code.Update (Exit_Code.Error);
end Als;

