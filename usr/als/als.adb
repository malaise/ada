with Ada.Calendar, Ada.Strings.Unbounded;
with Basic_Proc, Argument, Argument_Parser;
with Entities, Output, Targets, Lister;
procedure Als is
  Version : constant String  := "V2.10";

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
    Put_Line_Error (" <option> ::= -a (--all) | -A (--All)");
    Put_Line_Error ("            | -l (--list) | -1 (--1row) | -c (--classify) | <separator>");
    Put_Line_Error ("            | -D (--directories) | -L (--links) | -F (--files)");
    Put_Line_Error ("            | [ { <match_name> } ] | [ { <exclude_name> } ]");
    Put_Line_Error ("            | [ { <match_dir> } ] | [ { <exclude_dir> } ]");
    Put_Line_Error ("            | <date_spec> [ <date_spec> ]");
    Put_Line_Error ("            | -s (--size) | -t (--time) | -r (--reverse)");
    Put_Line_Error ("            | -R (--recursive) | -M (--merge) | -T (--total)");
    Put_Line_Error ("            | -n <date> (--newer=<date>)");
    Put_Line_Error (" <separator>     ::= -S <string> | --separator=<string>");
    Put_Line_Error (" <match_name>    ::= -m <criteria> | --match=<criteria>");
    Put_Line_Error (" <exclude_name>  ::= -e <criteria> | --exclude=<criteria>");
    Put_Line_Error (" <match_dir>     ::= --match_dir=<criteria>");
    Put_Line_Error (" <exclude_dir>   ::= --exclude_dir=<criteria>");
    Put_Line_Error (" <criteria>      ::= <templates> | @<regex>");
    Put_Line_Error (" <templates>     ::= <template> [ { ,<template> } ]");
    Put_Line_Error (" <date_spec>     ::= -d <date_comp><date> | --date=<date_comp><date>");
    Put_Line_Error (" <date_comp>     ::= eq | lt | le | gt | ge");
    Put_Line_Error (" <date>          ::= yyyy/mm/dd-hh:mm  |  hh:mm  |  <positive><duration>");
    Put_Line_Error (" <duration>      ::= Y | M | D | h | m");
    Put_Line_Error (" -n <date>       ::= -RMt -d ge<date>");
    Put_Line_Error ("exclude_name excludes the entries from the output list");
    Put_Line_Error ("  while exclude_dir excludes directories from the recursive scan.");
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

  package Asu renames Argument_Parser.Asu;
  function Asu_Tus (Source : in String) return Argument_Parser.Asu_Us
                   renames Asu.To_Unbounded_String;

  -- The keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => ('a', Asu_Tus ("all"), False, False),
   02 => ('A', Asu_Tus ("All"), False, False),
   03 => ('l', Asu_Tus ("list"), False, False),
   04 => ('1', Asu_Tus ("1row"), False, False),
   05 => ('D', Asu_Tus ("directories"), False, False),
   06 => ('r', Asu_Tus ("reverse"), False, False),
   07 => ('R', Asu_Tus ("recursive"), False, False),
   08 => ('s', Asu_Tus ("size"), False, False),
   09 => ('t', Asu_Tus ("time"), False, False),
   10 => ('M', Asu_Tus ("merge"), False, False),
   11 => ('d', Asu_Tus ("date"), True, True),
   12 => ('h', Asu_Tus ("help"), False, False),
   13 => ('v', Asu_Tus ("version"), False, False),
   14 => ('L', Asu_Tus ("links"), False, False),
   15 => ('F', Asu_Tus ("files"), False, False),
   16 => ('m', Asu_Tus ("match"), True, True),
   17 => ('e', Asu_Tus ("exclude"), True, True),
   18 => (Argument_Parser.No_Key_Char, Asu_Tus ("match_dir"), True, True),
   19 => (Argument_Parser.No_Key_Char, Asu_Tus ("exclude_dir"), True, True),
   20 => ('S', Asu_Tus ("separator"), False, True),
   21 => ('T', Asu_Tus ("total"), False, False),
   22 => ('n', Asu_Tus ("newer"), False, True),
   23 => ('c', Asu_Tus ("classify"), False, False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  No_Key_Index : constant Argument_Parser.The_Keys_Index
               := Argument_Parser.No_Key_Index;
  -- Option management
  List_Dots, List_Roots_And_Dots : Boolean;
  Dots : Entities.Dots_Kind_List;
  Long_List : Boolean;
  One_Row : Boolean;
  List_Only_Dirs : Boolean;
  List_Only_Links : Boolean;
  List_Only_Files : Boolean;
  Sort_Reverse : Boolean;
  Recursive : Boolean;
  Sort_By_Size : Boolean;
  Sort_By_Time : Boolean;
  Merge_Lists : Boolean;
  Date1, Date2 : Entities.Date_Spec_Rec;
  Separator : Ada.Strings.Unbounded.Unbounded_String;
  Put_Total : Boolean;
  Classify : Boolean;

  -- Parse a date argument
  function Parse_Date (Str : String) return Entities.Date_Spec_Rec is separate;

  -- Set a file or dir, match or exclusion criteria
  type Call_Access is access procedure (Template : in String;
                                        Regex    : in Boolean);
  procedure Set_Criteria (Criteria : in String;
                          Call     : in Call_Access) is separate;

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
  if Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    -- Any path/file spec must be after options
    Error;
  end if;

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
  One_Row := Arg_Dscr.Is_Set (04);
  List_Only_Dirs := Arg_Dscr.Is_Set (05);
  Sort_Reverse := Arg_Dscr.Is_Set (06);
  Recursive := Arg_Dscr.Is_Set (07) or else Arg_Dscr.Is_Set (22);
  Sort_By_Size := Arg_Dscr.Is_Set (08);
  Sort_By_Time := Arg_Dscr.Is_Set (09) or else Arg_Dscr.Is_Set (22);
  Merge_Lists := Arg_Dscr.Is_Set (10) or else Arg_Dscr.Is_Set (22);
  Classify := Arg_Dscr.Is_Set (23);
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
  if Arg_Dscr.Get_Nb_Occurences (22) /= 0 then
    Date1 := Parse_Date ("ge" & Arg_Dscr.Get_Option(22, 1));
    Date2.Oper := Entities.None;
  end if;
  List_Only_Links := Arg_Dscr.Is_Set (14);
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
    Separator := Ada.Strings.Unbounded.To_Unbounded_String
     (Arg_Dscr.Get_Option (20));
  end if;
  -- Put total size
  Put_Total := Arg_Dscr.Is_Set (21);
  -- Set output criteria
  declare
    Sort_Kind : Output.Sort_Kind_List;
    Format_Kind : Output.Format_Kind_List;
  begin
    if Sort_By_Time then
      -- Time is higher criteria than size
      Sort_Kind := Output.Time;
    elsif Sort_By_Size then
      Sort_Kind := Output.Size;
    else
      Sort_Kind := Output.Alpha;
    end if;
    -- Output format style
    if Long_List then
      Format_Kind := Output.Long;
    elsif One_Row or else Merge_Lists then
      Format_Kind := Output.One_Row;
    else
      Format_Kind := Output.Simple;
    end if;
    Output.Set_Style (Sort_Kind, Sort_Reverse, Format_Kind, Merge_Lists,
                      Classify, Separator);
  end;

  -- Set selection criteria in Lister, activate Total computation
  Lister.Set_Criteria (List_Only_Dirs, List_Only_Links, List_Only_Files,
                       Date1, Date2);
  if Put_Total then
    Lister.Activate_Total;
  end if;

  -- List each target
  if Targets.List (Dots, Recursive, Merge_Lists, Arg_Dscr) then
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

