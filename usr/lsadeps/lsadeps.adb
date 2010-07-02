with Ada.Exceptions;
with Argument, Argument_Parser, Basic_Proc, Mixed_Str;
with As.U; use As.U;
with Debug, Sourcer, Tree_Mng, Sort, Output;
procedure Lsadeps is

  use type Asu_Us;

  -- Usage and Error
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
      & " [ <display> ] [ <revert_mode> ] [ <file_mode> ] [ <include_dirs> ] <target>");
    Basic_Proc.Put_Line_Error (
     "  <display> ::= <list> | <tree>     // Default: list");
    Basic_Proc.Put_Line_Error (
     "   <list>   ::= -l | --list         // List dependencies");
    Basic_Proc.Put_Line_Error (
     "   <tree>   ::= -t | --tree         // Tree of dependencies");
    Basic_Proc.Put_Line_Error (
     "   <revert_mode> ::= -r | --revert  // List units depending on target");
    Basic_Proc.Put_Line_Error (
     "                                    //   i.o. units withed by target");
    Basic_Proc.Put_Line_Error (
     "   <file_mode>    ::= -f | --files  // Show files i.o. units");
    Basic_Proc.Put_Line_Error (
     "   <include_dirs> ::= { -I <dir> | --directory=<dir> }");
    Basic_Proc.Put_Line_Error (
     "   <target> := <unit>");
  end Usage;

  Error_Raised : exception renames Sourcer.Error_Raised;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    raise Error_Raised;
  end Error;

  -- The keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => ('l', Asu_Tus ("list"), False, False),
   02 => ('t', Asu_Tus ("tree"), False, False),
   03 => ('r', Asu_Tus ("revert"), False, False),
   04 => ('f', Asu_Tus ("files"), False, False),
   05 => ('I', Asu_Tus ("include"), True, True),
   06 => ('h', Asu_Tus ("help"), False, False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Option management
  Tree_Mode : Boolean := False;
  Revert_Mode : Boolean := False;
  File_Mode : Boolean := False;
  Target : Asu_Us;
  Dir : Asu_Us;

  -- Unit descriptor
  Unit : Sourcer.Src_Dscr;
  Found : Boolean;
  use type Sourcer.Src_Kind_List;

begin
  ---------------------
  -- PARSE ARGUMENTS --
  ---------------------
  -- Parse keys and options
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error (Arg_Dscr.Get_Error);
  end if;

  -- Help
  if Arg_Dscr.Is_Set (6) then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Mode: at most once
  if Arg_Dscr.Is_Set (2) then
    if Arg_Dscr.Is_Set (1) then
      Error ("At most one display mode expected");
    end if;
    Tree_Mode := True;
  end if;
  if Arg_Dscr.Is_Set (3) then
    Revert_Mode := True;
  end if;

  -- File mode
  if Arg_Dscr.Is_Set (4) then
    File_Mode := True;
  end if;

  -- Target: only once and at the end
  if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 1 then
    Error ("One and only one target required");
  elsif Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    Error ("Invalid argument");
  end if;
  Target := Asu_Tus (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index));

  -- Includes: must not be empty
  -- Declare include priorities
  for I in 1 .. Arg_Dscr.Get_Nb_Occurences (5) loop
    Dir := Asu_Tus (Arg_Dscr.Get_Option (5, I));
    if Dir = Asu_Null then
      Error ("Missing include dir");
    end if;
    Sort.Set_Prio (Asu_Tus (Arg_Dscr.Get_Option (5, I)), I);
  end loop;

  ---------------------------
  -- BUILD LIST OF SOURCES --
  ---------------------------
  Sourcer.Build_List (Arg_Dscr);
  -- Check that target is found, as spec or standalone body and is local
  Unit.Unit := Asu_Tus (Mixed_Str (Asu_Ts (Target)));
  Unit.Kind := Sourcer.Unit_Spec;
  Sourcer.List.Search (Unit, Found);
  if not Found then
    Unit.Kind := Sourcer.Unit_Body;
    Sourcer.List.Search (Unit, Found);
  end if;
  if not Found then
    Error ("Target unit " & Asu_Ts (Unit.Unit) & " not found");
  end if;
  Sourcer.List.Read (Unit, Unit);
  if Unit.Kind = Sourcer.Unit_Body and then not Unit.Standalone then
    Error ("Target unit, if a body, must be standalone");
  end if;
  if Debug.Is_Set then
    Basic_Proc.Put_Line_Output ("Target checked: " & Sourcer.Image (Unit));
  end if;

  ----------------------------
  -- BUILD TREE OF SOURCES --
  ----------------------------
  Tree_Mng.Build (Unit, Revert_Mode);

  -------------------
  -- PUT LIST/TREE --
  -------------------
  Output.Put (Tree_Mode, Revert_Mode, File_Mode);

exception
  when Error_Raised | Sourcer.Error_Raised =>
    Basic_Proc.Set_Error_Exit_Code;
  when Err:others =>
    Basic_Proc.Put_Line_Error ("ERROR: " &
      "Exception " & Ada.Exceptions.Exception_Name (Err) & " raised");
    Basic_Proc.Set_Error_Exit_Code;
end Lsadeps;

