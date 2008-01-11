with Ada.Calendar, Ada.Text_Io;
with Basic_Proc, Argument, Argument_Parser;
with Entities, Output, Targets;
procedure Als is
  Version : constant String  := "V1.5";

  -- Usage
  procedure Usage is
    use Basic_Proc;
  begin
    Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " [ { <option> } ] [ { <file_spec> } ]");
    Put_Line_Error (" <option> ::= -a (--all) | -A (--All) | -l (--list) | -1 (--1row)");
    Put_Line_Error ("            | -D (--directories) | -L (--links) | -F (--files)");
    Put_Line_Error ("            | -r (--reverse) | -R (--recursive)");
    Put_Line_Error ("            | -s (--size) | -t (--time) | -m (--merge)");
    Put_Line_Error ("            | <date_spec>");
    Put_Line_Error (" <date_spec> ::= -d <date_comp><date> | --date=<date_comp><date>");
    Put_Line_Error (" <date_comp> ::= eq | lt | le | gt | ge");
    Put_Line_Error (" <date> ::= [ yyyy/mm/dd-hh:mm  |  hh:mm  |  <positive> Y|M|D|h|m");
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
   10 => ('m', Asu_Tus ("merge"), False, False),
   11 => ('d', Asu_Tus ("date"), True, True),
   12 => ('h', Asu_Tus ("help"), False, False),
   13 => ('v', Asu_Tus ("version"), False, False),
   14 => ('L', Asu_Tus ("links"), False, False),
   15 => ('F', Asu_Tus ("files"), False, False));
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

  -- Parse a date argument
  function Parse_Date (Str : String) return Entities.Date_Spec_Rec is separate;

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
  Recursive := Arg_Dscr.Is_Set (07);
  Sort_By_Size := Arg_Dscr.Is_Set (08);
  Sort_By_Time := Arg_Dscr.Is_Set (09);
  Merge_Lists := Arg_Dscr.Is_Set (10);
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
  List_Only_Links := Arg_Dscr.Is_Set (14);
  List_Only_Files := Arg_Dscr.Is_Set (15);

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
    Output.Set_Style (Sort_Kind, Sort_Reverse, Format_Kind, Merge_Lists);
  end;

  -- List
  Targets.List (Dots, List_Only_Dirs, List_Only_Links, List_Only_Files,
                Date1, Date2,
                Recursive, Merge_Lists, Arg_Dscr);

exception
  when Error_Exception =>
    Basic_Proc.Set_Error_Exit_Code;
end Als;

