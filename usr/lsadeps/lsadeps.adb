with Ada.Exceptions;
with As.U, Argument, Argument_Parser, Basic_Proc, Mixed_Str, Directory;
with Debug, Sourcer, Tree_Mng, Sort, Output, Checker;
procedure Lsadeps is

  Version : constant String := "V7.0";

  -- Usage and Error
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
     & " <version> | <help> | <check> | <depend>");
    Basic_Proc.Put_Line_Error (
     " <version>     ::= -v | --version");
    Basic_Proc.Put_Line_Error (
     " <help>        ::= -h | --help");
    Basic_Proc.Put_Line_Error (
     " <check>       ::= -c | --check  [ <target_dir> ]");
    Basic_Proc.Put_Line_Error (
     " <depend>      ::= <options> <target_unit> [ <path_unit> ]");
    Basic_Proc.Put_Line_Error (
     " <target_unit> ::=  [<path>/]<unit>");
    Basic_Proc.Put_Line_Error (
     " <options>     ::=  [ <specs> | <revert> ] [ <tree> ] [ <files> ] [ <include> ]");
    Basic_Proc.Put_Line_Error (
     " <specs>       ::= -s | --specs");
    Basic_Proc.Put_Line_Error (
     " <revert>      ::= -r | --revert");
    Basic_Proc.Put_Line_Error (
     " <tree>        ::= -t | --tree");
    Basic_Proc.Put_Line_Error (
     " <files>       ::= -f | --files");
    Basic_Proc.Put_Line_Error (
     " <include>     ::= { <dir> | <recursive> }");
    Basic_Proc.Put_Line_Error (
     " <dir>         ::= -I <dir> | --include=<dir>");
    Basic_Proc.Put_Line_Error (
     " <recursive>   ::= -R <dir> | --recursive=<dir>");
    Basic_Proc.Put_Line_Error (
     "Check function shows redundant ""with"" clauses in a dir (default: current dir).");
    Basic_Proc.Put_Line_Error (
     "Dependency function by default lists units on which <target_unit> depends,");
    Basic_Proc.Put_Line_Error (
     "    which are withed units, their body and subunits, units withed by these");
    Basic_Proc.Put_Line_Error (
     "    units... recursively. Alternative modes are:");
    Basic_Proc.Put_Line_Error (
     " <specs> to show only units withed by specs and standalone bodies,");
    Basic_Proc.Put_Line_Error (
     " <revert> to show units withing <target_unit>... recursively,");
    Basic_Proc.Put_Line_Error (
     " <path_unit> to show dependency paths between two units.");
    Basic_Proc.Put_Line_Error (
     " Other options are:");
    Basic_Proc.Put_Line_Error (
     " <tree> to show the tree of dependencies (instead of a sorted unique list),");
    Basic_Proc.Put_Line_Error (
     " <file> to show the file names (instead of unit names),");
    Basic_Proc.Put_Line_Error (
     " <include> to add some directories or some directory trees to the search path");
    Basic_Proc.Put_Line_Error (
     "   (default is current and target directories),");
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
   01 => ('h', As.U.Tus ("help"), False, False),
   02 => ('v', As.U.Tus ("version"), False, False),
   03 => ('c', As.U.Tus ("check"), False, False),
   04 => ('s', As.U.Tus ("specs"), False, False),
   05 => ('r', As.U.Tus ("revert"), False, False),
   06 => ('t', As.U.Tus ("tree"), False, False),
   07 => ('f', As.U.Tus ("files"), False, False),
   08 => ('I', As.U.Tus ("include"), True, True),
   09 => ('R', As.U.Tus ("recursive"), True, True));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Option management
  Check_Mode : Boolean := False;
  Specs_Mode : Boolean := False;
  Revert_Mode : Boolean := False;
  Tree_Mode : Boolean := False;
  Files_Mode : Boolean := False;
  Target, Target_Dir : As.U.Asu_Us;
  Path, Path_Dir : As.U.Asu_Us;

  -- Current directory
  Current_Dir : As.U.Asu_Us;

  -- Unit descriptor of the target and of the path
  Target_Dscr, Path_Dscr : Sourcer.Src_Dscr;
  use type Sourcer.Src_Kind_List;

  -- Check that a target/include directory is usable
  procedure Check_Dir (Dir : in String) is
  begin
    if Dir /= "" then
      begin
        Directory.Change_Current (Dir);
      exception
        when Directory.Name_Error | Directory.Access_Error =>
          Error ("Cannot change to directory " & Dir);
      end;
    end if;
    begin
      Directory.Change_Current (Current_Dir.Image);
    exception
      when Directory.Name_Error | Directory.Access_Error =>
        Error ("Cannot change back to current directory " & Current_Dir.Image);
    end;
  end Check_Dir;

  -- Check that a unit exists and is spec or standalone body
  function Check_Unit (Path, Name : As.U.Asu_Us; Kind : String)
           return Sourcer.Src_Dscr is
    Dscr : Sourcer.Src_Dscr;
  begin
    -- Check that unit is found, as spec or standalone body
    Dscr := Sourcer.Get_Unit (Path, Name);
    if Dscr.Unit.Is_Null or else Dscr.Kind = Sourcer.Subunit then
      Error (Kind & " unit " & Dscr.Unit.Image & " not found");
    end if;
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output (Kind & " unit checked: "
                                & Sourcer.Image (Dscr));
    end if;
    return Dscr;
  end Check_Unit;

  -- Add the paths of -I and -R dirtectives in the proper order
  procedure Add_Paths is separate;

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
  if Arg_Dscr.Is_Set (1) then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Version
  if Arg_Dscr.Is_Set (2) then
    Basic_Proc.Put_Line_Output (Argument.Get_Program_Name & " " & Version);
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Modes
  if Arg_Dscr.Is_Set (4) then
    Specs_Mode := True;
  end if;
  if Arg_Dscr.Is_Set (5) then
    Revert_Mode := True;
  end if;
  if Specs_Mode and then Revert_Mode then
    Error ("Specs and revert mode are mutually exclusive");
  end if;
  if Arg_Dscr.Is_Set (6) then
    Tree_Mode := True;
  end if;
  if Arg_Dscr.Is_Set (7) then
    Files_Mode := True;
  end if;

  -- Check mode
  if Arg_Dscr.Is_Set (3) then
    -- No option
    if Specs_Mode or else Revert_Mode or else Tree_Mode or else Files_Mode then
      Error ("Check mode does not support options");
    end if;
    -- No include
    if Arg_Dscr.Get_Nb_Occurences (8) /= 0
    or else Arg_Dscr.Get_Nb_Occurences (9) /= 0 then
      Error ("Check mode is exclusive with simple or recursive includes");
    end if;
    Check_Mode := True;
  end if;

  -- Save current dir and add it to paths (top prio)
  Directory.Get_Current (Current_Dir);
  Sort.Add_Path (Current_Dir);

  if Check_Mode then
    -- An optional target directory
    if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 1 then
      Target := As.U.Tus (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index));
      Target_Dir := As.U.Tus (Directory.Make_Full_Path (Target.Image));
    elsif Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 0 then
      Error ("At most one target accepted");
    end if;
    Check_Dir (Target_Dir.Image);
  else
    -- Target and path: at most 2 and at the end
    if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) > 2
    or else Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 0 then
      Error ("At least one target and most one path required");
    elsif Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 2
    and then Tree_Mode then
      Error ("Path mode and tree are mutually exculsive");
    elsif Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
      Error ("Invalid argument");
    end if;
    -- Target
    Target := As.U.Tus (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index, 1));
    Target_Dir := As.U.Tus (Directory.Make_Full_Path (Directory.Dirname
        (Target.Image)));
    Check_Dir (Target_Dir.Image);
    Target := As.U.Tus (Mixed_Str (Directory.Basename (Target.Image)));
    -- Path
    if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 2 then
      Path := As.U.Tus (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index, 2));
      Path_Dir := As.U.Tus (Directory.Make_Full_Path (Directory.Dirname
          (Path.Image)));
      Check_Dir (Target_Dir.Image);
      Path := As.U.Tus (Mixed_Str (Directory.Basename (Path.Image)));
    end if;

    -- Include target dir in paths (just after current dir)
    if not Target_Dir.Is_Null then
      Sort.Add_Path (Target_Dir);
    end if;
    -- Include path dir in paths (just after target dir)
    if not Path_Dir.Is_Null then
      Sort.Add_Path (Path_Dir);
    end if;
    -- Include dirs of -I and -R options
    Add_Paths;
  end if;


  ----------------------------
  -- BUILD LISTS OF SOURCES --
  ----------------------------
  Sourcer.Build_Lists;

  ------------------------
  -- MOVE TO TARGET DIR --
  ------------------------
  if not Target_Dir.Is_Null then
    begin
      Directory.Change_Current (Directory.Make_Full_Path (Target_Dir.Image));
    exception
      when others =>
        Error ("Cannot change to target directory " & Target_Dir.Image);
    end;
  end if;
  if Debug.Is_Set then
    Basic_Proc.Put_Line_Output ("In " &
      Directory.Make_Full_Path (Directory.Get_Current));
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

  ------------------
  -- CHECK TARGET --
  ------------------
  if not Check_Mode then
    Target_Dscr := Check_Unit (Target_Dir, Target, "Target");
    if not Path.Is_Null then
      Path_Dscr := Check_Unit (Path_Dir, Path, "Path");
    end if;
  end if;

  ----------------------------
  -- BUILD TREE OF SOURCES --
  ----------------------------
  Tree_Mng.Build (Target_Dscr, Specs_Mode, Revert_Mode);

  -------------------
  -- PUT LIST/TREE --
  -------------------
  -- Back to original dir
  Check_Dir ("");
  Output.Put (Revert_Mode, Tree_Mode, Files_Mode, Path_Dscr);

exception
  when Error_Raised | Sourcer.Error_Raised =>
    Basic_Proc.Set_Error_Exit_Code;
  when Err:others =>
    Basic_Proc.Put_Line_Error ("ERROR: " &
      "Exception " & Ada.Exceptions.Exception_Name (Err) & " raised");
    Basic_Proc.Set_Error_Exit_Code;
end Lsadeps;

