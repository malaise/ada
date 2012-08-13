with Ada.Calendar;
with As.U, Basic_Proc, Argument, Argument_Parser;
with Entities, Output, Targets, Lister, Exit_Code, String_Mng;
procedure Als is
  Version : constant String  := "V9.1";

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
   12 => (False, Nkc, As.U.Tus ("help"), False),
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
   25 => (False, 'h', As.U.Tus ("human"),        False),
   26 => (False, 'N', As.U.Tus ("no_sort"),      False),
   27 => (False, 'p', As.U.Tus ("path"),         False),
   28 => (False, 'B', As.U.Tus ("broken_links"), False),
   29 => (False, Nkc, As.U.Tus ("follow_links"), False),
   30 => (False, Nkc, As.U.Tus ("date_iso"),     False),
   31 => (False, Nkc, As.U.Tus ("skip_dirs"),    False),
   32 => (False, 'O', As.U.Tus ("others"),       False),
   33 => (False, Nkc, As.U.Tus ("no_name"),      False),
   34 => (False, 'U', As.U.Tus ("utc"),          False) );
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Usage
  function Key_Img (I : Argument_Parser.The_Keys_Range) return String is
  begin
    return String_Mng.Procuste (Argument_Parser.Image(Keys(I)), 19);
  end Key_Img;

  procedure Usage is
    use Basic_Proc;
  begin
    Put_Line_Error ("Usage:" & Argument.Get_Program_Name
      & " [ { <option> } ] [ { <file_or_dir_spec> } ]");
    Put_Line_Error ("   or: " & Argument.Get_Program_Name
      & Argument_Parser.Image(Keys(12))
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
    Put_Line_Error ("  " & Key_Img(33) & "// Do not show directory names");
    Put_Line_Error ("  <match_name>   ::= " & Argument_Parser.Image(Keys(16)));
    Put_Line_Error ("    <criteria>     ::= <templates> | @<regex>");
    Put_Line_Error ("    <templates>    ::= <template> [ { ,<template> } ]");
    Put_Line_Error ("                     // Keep only files that match the ciriteria (one template");
    Put_Line_Error ("                     //  or the regular expression)");
    Put_Line_Error ("  <exclude_name> ::= " & Argument_Parser.Image(Keys(17)));
    Put_Line_Error ("                     // Exclude files that match the ciriteria");
    Put_Line_Error ("  <match_dir>    ::= " & Argument_Parser.Image(Keys(18)));
    Put_Line_Error ("                     // Scan only directories that match the criteria");
    Put_Line_Error ("  <exclude_dir>  ::= " & Argument_Parser.Image(Keys(19)));
    Put_Line_Error ("                     // Don't scan directories that match the criteria");
    Put_Line_Error ("  <date_spec> [ <date_spec> ]");
    Put_Line_Error ("    <date_spec> ::= " & Argument_Parser.Image(Keys(11)));
    Put_Line_Error ("    <date_comp> ::= eq | lt | le | gt | ge");
    Put_Line_Error ("    <date>      ::= yyyy-mm-ddThh:mm:ss | yyyy-mm-dd | Thh:mm:ss");
    Put_Line_Error ("                  | <positive><duration>");
    Put_Line_Error ("    <duration>  ::= Y | M | D | h | m | s");
    Put_Line_Error ("                     // Keep files that match the date specification");
    Put_Line_Error ("                     //  (before, after or equal to a given date or delay)");
    Put_Line_Error ("    " & Argument_Parser.Image(Keys((22))) & " is a shortcut to ""-d ge<date>""");
    Put_Line_Error ("How to show each entry (file or dir):");
    Put_Line_Error ("  " & Key_Img(03) & "// Show rights, owner, size, date, symlink target");
    Put_Line_Error ("  " & Key_Img(04) & "// One name per line");
    Put_Line_Error ("  " & Key_Img(23) & "// Append '/' to dirs, '@' to symlinks");
    Put_Line_Error ("  " & Key_Img(25) & "// Show sizes in friendly format (e.g. 1K, 2G)");
    Put_Line_Error ("  " & Key_Img(27) & "// Show full path of entries");
    Put_Line_Error ("  <separator> ::= " & Argument_Parser.Image(Keys(20)));
    Put_Line_Error ("                     // Insert <string> beween each entry");
    Put_Line_Error ("  " & Key_Img(29) & "// Show final target of symlinks");
    Put_Line_Error ("  " & Key_Img(30) & "// Show date in strict ISO format (<date>T<time>)");
    Put_Line_Error ("How to organize entry list:");
    Put_Line_Error ("  " & Key_Img(08) & "// Sort by decrescent size (see also ""-r"")");
    Put_Line_Error ("  " & Key_Img(09) & "// Sort by decrescent time (see also ""-r"")");
    Put_Line_Error ("  " & Key_Img(06) & "// Sort (by name, size or time) in reverse order");
    Put_Line_Error ("  " & Key_Img(26) & "// Keep same order as in the directory structure");
    Put_Line_Error ("  " & Key_Img(10) & "// Show a global list of entries (without dir names)");
    Put_Line_Error ("  " & Key_Img(12) & "// Also show total size of listed entries");
    Put_Line_Error ("  " & Key_Img(34) & "// Use UTC i.o. local time for date spec and output");
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
  No_Name : Boolean;
  Utc : Boolean;

  -- Parse a date argument
  function Parse_Date (Str : String) return Entities.Date_Spec_Rec is separate;

  -- Set a file or dir, match or exclusion criteria
  type Call_Access is access procedure (Template : in String;
                                        Regex    : in Boolean);
  procedure Set_Criteria (Criteria : in String;
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
  No_Name := Arg_Dscr.Is_Set (33);
  Utc := Arg_Dscr.Is_Set (34);
  Depth := 0;

  -- Check sorting
  if Sort_By_Time and then Sort_By_Size then
    Error ("-s (--size) and -t (--time) are mutually exclusive");
  end if;
  if No_Sorting and then
  (Sort_By_Size or else Sort_By_Time or else Sort_Reverse) then
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
    if (Date1.Oper <= Entities.Less_Or_Equal
           and then Date2.Oper <= Entities.Less_Or_Equal)
    or else (Date1.Oper >= Entities.Greater_Than
             and then Date2.Oper >= Entities.Greater_Than) then
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
      Set_Criteria (Arg_Dscr.Get_Option (16, I), Lister.Add_Match'Access);
    exception
      when Lister.Invalid_Template =>
        Error ("Invalid match template " & Arg_Dscr.Get_Option (16, I));
    end;
  end loop;
  -- Add exclude template if any
  for I in 1 .. Arg_Dscr.Get_Nb_Occurences (17) loop
    begin
      Set_Criteria (Arg_Dscr.Get_Option (17, I), Lister.Add_Exclude'Access);
    exception
      when Lister.Invalid_Template =>
        Error ("Invalid exclude template " & Arg_Dscr.Get_Option (17, I));
    end;
  end loop;
  -- Add dir_match if any
  for I in 1 .. Arg_Dscr.Get_Nb_Occurences (18) loop
    begin
      Set_Criteria (Arg_Dscr.Get_Option (18, I), Lister.Add_Dir_Match'Access);
    exception
      when Lister.Invalid_Template =>
        Error ("Invalid dir_match " & Arg_Dscr.Get_Option (18, I));
    end;
  end loop;
  -- Add dir_exclude if any
  for I in 1 .. Arg_Dscr.Get_Nb_Occurences (19) loop
    begin
      Set_Criteria (Arg_Dscr.Get_Option (19, I), Lister.Add_Dir_Exclude'Access);
    exception
      when Lister.Invalid_Template =>
        Error ("Invalid dir_match " & Arg_Dscr.Get_Option (19, I));
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
    Output.Set_Style (Sort_Kind, Sort_Reverse, Format_Kind, Merge_Lists,
                      Full_Path, Classify, Date_Iso, Separator);
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
                       List_Only_Links, List_Only_Others,
                       Follow_Links, Date1, Date2, Utc);
  if Put_Total then
    Lister.Activate_Total;
  end if;

  -- List each target
  if Targets.List (Dots, Recursive, Depth, Merge_Lists, Skip_Dirs, not No_Name,
                   Arg_Dscr) then
    Exit_Code.Update (Exit_Code.Found);
  else
    Exit_Code.Update (Exit_Code.Empty);
  end if;

  if Put_Total then
    Output.Put_Size (Lister.Get_Total);
    Output.New_Line;
  end if;

exception
  when Error_Exception =>
    Exit_Code.Update (Exit_Code.Error);
end Als;

