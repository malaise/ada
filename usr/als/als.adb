with Ada.Calendar;
with As.U, Basic_Proc, Argument, Argument_Parser;
with Entities, Output, Targets, Lister;
procedure Als is
  Version : constant String  := "V6.2";

  -- Exit codes
  Found_Exit_Code : constant Natural := 0;
  Empty_Exit_Code : constant Natural := 1;
  Error_Exit_Code : constant Natural := 2;

  -- Usage
  procedure Usage is
    use Basic_Proc;
  begin
    Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " [ { <option> } ] [ { <file_or_dir_spec> } ]");
    Put_Line_Error ("How to show each entry (file or dir):");
    Put_Line_Error ("  -l (--list)        // Show rights, owner, size, date, symlink target");
    Put_Line_Error ("  -1 (--1row)        // One name per line");
    Put_Line_Error ("  -c (--classify)    // Append '/' to dirs, '@' to symlinks");
    Put_Line_Error ("  -h (--human)       // Show sizes in friendly format (e.g. 1K, 2G)");
    Put_Line_Error ("  -f (--full_path)   // Show full path of entries");
    Put_Line_Error ("  <separator> ::= -S <string> | --separator=<string>");
    Put_Line_Error ("                     // Insert <string> beween each entry");
    Put_Line_Error ("  --follow_links     // Show final target of symlinks");
    Put_Line_Error ("  --date_iso         // Show date in strick ISO format (<date>T<time>)");
    Put_Line_Error ("Which entries to show:");
    Put_Line_Error ("  -a (--all)         // Show all entries including hidden (starting with '.')");
    Put_Line_Error ("  -A (--All)         // Show all entries except ""."" and ""..""");
    Put_Line_Error ("  -D (--directories) // Show only directories");
    Put_Line_Error ("  -L (--links)       // Show only symbolic links");
    Put_Line_Error ("  -F (--files)       // Show only regular files");
    Put_Line_Error ("  -B (--broken_links)// Show only broken symbolic links");
    Put_Line_Error ("  -R (--recursive)   // Scan directories recursively");
    Put_Line_Error ("  --depth=<positive> // Scan only to given depth (needs ""-R"")");
    Put_Line_Error ("  <match_name> ::= -m <criteria> | --match=<criteria>");
    Put_Line_Error ("    <criteria> ::= <templates> | @<regex>");
    Put_Line_Error ("    <templates> ::= <template> [ { ,<template> } ]");
    Put_Line_Error ("                     // Keep only files that match the ciriteria (one template");
    Put_Line_Error ("                     //  or the regular expression)");
    Put_Line_Error ("  <exclude_name> ::= -e <criteria> | --exclude=<criteria>");
    Put_Line_Error ("                     // Exclude files that match the ciriteria");
    Put_Line_Error ("  <match_dir> ::= --match_dir=<criteria>");
    Put_Line_Error ("                     // Scan only directories that match the criteria");
    Put_Line_Error ("  <exclude_dir> ::= --exclude_dir=<criteria>");
    Put_Line_Error ("                     // Don't scan directories that match the criteria");
    Put_Line_Error ("  <date_spec> [ <date_spec> ]");
    Put_Line_Error ("    <date_spec> ::= -d <date_comp><date> | --date=<date_comp><date>");
    Put_Line_Error ("    <date_comp> ::= eq | lt | le | gt | ge");
    Put_Line_Error ("    <date>      ::= yyyy-mm-ddThh:mm:ss | yyyy-mm-dd | Thh:mm:ss");
    Put_Line_Error ("                  | <positive><duration>");
    Put_Line_Error ("    <duration>  ::= Y | M | D | h | m");
    Put_Line_Error ("                     // Keep files that match the date specification");
    Put_Line_Error ("                     //  (before, after or equal to a given date or delay)");
    Put_Line_Error ("    -n <date> (--newer=<date>) is a shortcut to ""-d ge<date>""");
    Put_Line_Error ("How to organize entry list:");
    Put_Line_Error ("  -s (--size)        // Sort by decrescent size (see also ""-r"")");
    Put_Line_Error ("  -t (--time)        // Sort by decrescent time (see also ""-r"")");
    Put_Line_Error ("  -r (--reverse)     // Sort (by name, size or time) in reverse order");
    Put_Line_Error ("  -N (--no_sort)     // Keep order same as in the directory structure");
    Put_Line_Error ("  -M (--merge)       // Show a global list of entries (without dir names)");
    Put_Line_Error ("  -T (--total)       // Also show total size of listed entries");
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

  -- The keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => ('a', As.U.Tus ("all"), False, False),
   02 => ('A', As.U.Tus ("All"), False, False),
   03 => ('l', As.U.Tus ("list"), False, False),
   04 => ('1', As.U.Tus ("1row"), False, False),
   05 => ('D', As.U.Tus ("directories"), False, False),
   06 => ('r', As.U.Tus ("reverse"), False, False),
   07 => ('R', As.U.Tus ("recursive"), False, False),
   08 => ('s', As.U.Tus ("size"), False, False),
   09 => ('t', As.U.Tus ("time"), False, False),
   10 => ('M', As.U.Tus ("merge"), False, False),
   11 => ('d', As.U.Tus ("date"), True, True),
   12 => (Argument_Parser.No_Key_Char, As.U.Tus ("help"), False, False),
   13 => ('v', As.U.Tus ("version"), False, False),
   14 => ('L', As.U.Tus ("links"), False, False),
   15 => ('F', As.U.Tus ("files"), False, False),
   16 => ('m', As.U.Tus ("match"), True, True),
   17 => ('e', As.U.Tus ("exclude"), True, True),
   18 => (Argument_Parser.No_Key_Char, As.U.Tus ("match_dir"), True, True),
   19 => (Argument_Parser.No_Key_Char, As.U.Tus ("exclude_dir"), True, True),
   20 => ('S', As.U.Tus ("separator"), False, True),
   21 => ('T', As.U.Tus ("total"), False, False),
   22 => ('n', As.U.Tus ("newer"), False, True),
   23 => ('c', As.U.Tus ("classify"), False, False),
   24 => (Argument_Parser.No_Key_Char, As.U.Tus ("depth"), False, True),
   25 => ('h', As.U.Tus ("human"), False, False),
   26 => ('N', As.U.Tus ("no_sort"), False, False),
   27 => ('f', As.U.Tus ("full_path"), False, False),
   28 => ('B', As.U.Tus ("broken_links"), False, False),
   29 => (Argument_Parser.No_Key_Char, As.U.Tus ("follow_links"), False, False),
   30 => (Argument_Parser.No_Key_Char, As.U.Tus ("date_iso"), False, False) );
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
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
      return;
    end if;
  elsif Arg_Dscr.Is_Set (13) then
    -- Version
    if Argument.Get_Nbre_Arg /= 1 then
     Error;
    else
      Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & " " & Version);
      return;
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
  Classify := Arg_Dscr.Is_Set (23);
  Human := Arg_Dscr.Is_Set (25);
  No_Sorting := Arg_Dscr.Is_Set (26);
  Full_Path := Arg_Dscr.Is_Set (27);
  Date_Iso := Arg_Dscr.Is_Set (30);
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
      Error ("With two dates, one must be ""lt"" or ""le"" and the other ""gt or ""ge""");
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
  List_Only_Files := Arg_Dscr.Is_Set (15);

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

  -- Set selection criteria in Lister, activate Total computation
  Lister.Set_Criteria (List_Only_Dirs, List_Only_Files, List_Only_Links,
                       Follow_Links, Date1, Date2);
  if Put_Total then
    Lister.Activate_Total;
  end if;

  -- List each target
  if Targets.List (Dots, Recursive, Depth, Merge_Lists, Arg_Dscr) then
    Basic_Proc.Set_Exit_Code (Found_Exit_Code);
  else
    Basic_Proc.Set_Exit_Code (Empty_Exit_Code);
  end if;

  if Put_Total then
    Output.Put_Size (Lister.Get_Total);
    Output.New_Line;
  end if;

exception
  when Error_Exception =>
    Basic_Proc.Set_Exit_Code (Error_Exit_Code);
end Als;

