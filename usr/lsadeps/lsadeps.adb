with Ada.Exceptions;
with As.U, Argument, Argument_Parser, Basic_Proc, Mixed_Str, Directory;
with Debug, Sourcer, Tree_Mng, Sort, Output, Checker;
procedure Lsadeps is

  Version : constant String := "V2.1";

  -- Usage and Error
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
      & " [ <display> ] [ <revert_mode> ] [ <file_mode> ] [ <include_dirs> ] <target>");
    Basic_Proc.Put_Line_Error (
     "   or: " & Argument.Get_Program_Name & " <check> [ <path> ]");
    Basic_Proc.Put_Line_Error (
     "   or: " & Argument.Get_Program_Name & " -v | --version | -h | --help");
    Basic_Proc.Put_Line_Error (
     "  <display>       ::= <list> | <tree> // Default: list");
    Basic_Proc.Put_Line_Error (
     "    <list>        ::= -l | --list     // List dependencies");
    Basic_Proc.Put_Line_Error (
     "    <tree>        ::= -t | --tree     // Tree of dependencies");
    Basic_Proc.Put_Line_Error (
     "  <revert_mode>   ::= -r | --revert   // List units depending on target");
    Basic_Proc.Put_Line_Error (
     "                                      //   i.o. units withed by target");
    Basic_Proc.Put_Line_Error (
     "  <file_mode>     ::= -f | --files    // Show files i.o. units");
    Basic_Proc.Put_Line_Error (
     "  <include_dirs>  ::= { -I <dir> | --directory=<dir> }");
    Basic_Proc.Put_Line_Error (
     "  <target>        ::= <unit>");
    Basic_Proc.Put_Line_Error (
     "  <check>         ::= -c | --check    // Detects redundant withs");
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
   01 => ('l', As.U.Tus ("list"), False, False),
   02 => ('t', As.U.Tus ("tree"), False, False),
   03 => ('r', As.U.Tus ("revert"), False, False),
   04 => ('f', As.U.Tus ("files"), False, False),
   05 => ('I', As.U.Tus ("include"), True, True),
   06 => ('h', As.U.Tus ("help"), False, False),
   07 => ('c', As.U.Tus ("check"), False, False),
   08 => ('v', As.U.Tus ("version"), False, False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Option management
  Tree_Mode : Boolean := False;
  Revert_Mode : Boolean := False;
  File_Mode : Boolean := False;
  Check_Mode : Boolean := False;
  Target : As.U.Asu_Us;
  Dir : As.U.Asu_Us;

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

  -- Version
  if Arg_Dscr.Is_Set (8) then
    Basic_Proc.Put_Line_Output (Argument.Get_Program_Name & " " & Version);
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

  -- Check mode
  if Arg_Dscr.Is_Set (7) then
    if Tree_Mode or else Revert_Mode or else File_Mode then
      Error ("Check mode is exclusive with other modes");
    end if;
    Check_Mode := True;
  end if;

  if Check_Mode then
    -- An optional target directory
    if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 1 then
      Target := As.U.Tus (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index));
    elsif Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 0 then
      Error ("At most one target accepted");
    end if;
  else
    -- Target: only once and at the end
    if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 1 then
      Error ("One and only one target required");
    elsif Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
      Error ("Invalid argument");
    end if;
    Target := As.U.Tus (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index));
  end if;

  if Check_Mode then
    -- No include
    if Arg_Dscr.Get_Nb_Occurences (5) /= 0 then
      Error ("Check mode is exclusive with includes");
    end if;
  else
    -- Includes: must not be empty
    -- Declare include priorities
    for I in 1 .. Arg_Dscr.Get_Nb_Occurences (5) loop
      Dir := As.U.Tus (Arg_Dscr.Get_Option (5, I));
      if Dir.Is_Null then
        Error ("Missing include dir");
      end if;
      Sort.Set_Prio (As.U.Tus (Arg_Dscr.Get_Option (5, I)), I);
    end loop;
  end if;

  ------------------
  -- CHECK TARGET --
  ------------------
  -- Check that target directory is readable, change to it
  begin
    if Check_Mode then
      -- Optional Target is the directory
      if not Target.Is_Null then
        Directory.Change_Current (Directory.Make_Full_Path (Target.Image));
      end if;
    else
      -- Target is the directory + unit
      Directory.Change_Current (Directory.Make_Full_Path (Directory.Dirname
          (Target.Image)));
    end if;
  exception
    when others =>
      Error ("Cannot change to target directory "
           & Directory.Make_Full_Path (Directory.Dirname (Target.Image)));
  end;
  if Debug.Is_Set then
    Basic_Proc.Put_Line_Output ("In " &
      Directory.Make_Full_Path (Directory.Get_Current));
  end if;

  --------------------------------------------
  -- BUILD LIST OF SOURCES and CHECK TARGET --
  --------------------------------------------
  Sourcer.Build_List (Arg_Dscr);
  if not Check_Mode then
    -- Check that target is found, as spec or standalone body
    Unit.Unit := As.U.Tus (Mixed_Str (Directory.Basename (Target.Image)));
    Unit.Kind := Sourcer.Unit_Spec;
    Sourcer.List.Search (Unit, Found);
    if not Found then
      Unit.Kind := Sourcer.Unit_Body;
      Sourcer.List.Search (Unit, Found);
    end if;
    if not Found then
      Error ("Target unit " & Unit.Unit.Image & " not found");
    end if;
    Sourcer.List.Read (Unit);
    if Unit.Kind = Sourcer.Unit_Body and then not Unit.Standalone then
      Error ("Target unit, if a body, must be standalone");
    end if;
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Target checked: " & Sourcer.Image (Unit));
    end if;
  end if;

  -----------------
  -- CHECK UNITS --
  -----------------
  if Check_Mode then
    if not Checker.Check then
      -- Check failed
      Basic_Proc.Set_Error_Exit_Code;
    end if;
    return;
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

