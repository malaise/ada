with Ada.Exceptions;
with As.U, Argument, Argument_Parser, Basic_Proc, Mixed_Str, Directory, Trace;
with Debug, Sourcer, Tree_Mng, Sort, Output, Checker;
procedure Lsadeps is

  Version : constant String := "V16.01";

  -- The keys and descriptor of parsed keys
  Nok : Character renames Argument_Parser.No_Key_Char;
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => (False, 'h', As.U.Tus ("help"),    False),
   02 => (False, 'v', As.U.Tus ("version"),  False),
   03 => (False, 'c', As.U.Tus ("check"),    False),
   04 => (False, 's', As.U.Tus ("specs"),    False),
   05 => (False, 'r', As.U.Tus ("revert"),   False),
   06 => (False, 't', As.U.Tus ("tree"),     False),
   07 => (False, 'd', As.U.Tus ("direct"),   False),
   08 => (False, 'f', As.U.Tus ("files"),    False),
   09 => (True,  'I', As.U.Tus ("include"),   True, True, As.U.Tus ("dir_path")),
   10 => (True,  'R', As.U.Tus ("recursive"), True, True, As.U.Tus ("dir_path")),
   11 => (True,  'E', As.U.Tus ("exclude"),   True, True, As.U.Tus ("dir_name_pattern")),
   12 => (False, 'l', As.U.Tus ("list"),     False),
   13 => (False, 'a', As.U.Tus ("all"),      False),
   14 => (False, 'C', As.U.Tus ("children"), False),
   15 => (False, 'b', As.U.Tus ("bodies"),   False),
   16 => (False, 'F', As.U.Tus ("fast"),     False),
   17 => (False, Nok, As.U.Tus ("restrict"), False),
   18 => (True,  'T', As.U.Tus ("tab"), False, True, As.U.Tus ("size")),
   19 => (False, 'o', As.U.Tus ("once"), False),
   20 => (False, 'L', As.U.Tus ("loop"), False)
  );
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  Include_Index : constant Argument_Parser.The_Keys_Range := 9;
  Recursive_Index : constant Argument_Parser.The_Keys_Range := 10;
  Exclude_Index : constant Argument_Parser.The_Keys_Range := 11;

  -- Usage and Error
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
     & " <version> | <help> | <depend> | <list> | <check> | <loop>");
    Basic_Proc.Put_Line_Error (
     " <version>     ::= " & Argument_Parser.Image (Keys(2)));
    Basic_Proc.Put_Line_Error (
     " <help>        ::= " & Argument_Parser.Image (Keys(1)));
    Basic_Proc.Put_Line_Error (
     " <depend>      ::= <options> <target_unit> [ <path_unit> ]");
    Basic_Proc.Put_Line_Error (
     " <target_unit> ::=  [<path>/]<unit>");
    Basic_Proc.Put_Line_Error (
     " <options>     ::=  [ <specs> | <revert> ]");
    Basic_Proc.Put_Line_Error (
     "                    [ <tree> [ <tab> ] | <fast> | <direct> | <once> ]");
    Basic_Proc.Put_Line_Error (
     "                    [ <bodies> ] [ <files> ] [ <inclusions> ]");
    Basic_Proc.Put_Line_Error (
     " <specs>       ::= " & Argument_Parser.Image (Keys(4)));
    Basic_Proc.Put_Line_Error (
     " <revert>      ::= " & Argument_Parser.Image (Keys(5)));
    Basic_Proc.Put_Line_Error (
     " <tree>        ::= " & Argument_Parser.Image (Keys(6)));
    Basic_Proc.Put_Line_Error (
     " <tab>         ::= " & Argument_Parser.Image (Keys(18)));
    Basic_Proc.Put_Line_Error (
     " <fast>        ::= " & Argument_Parser.Image (Keys(16)));
    Basic_Proc.Put_Line_Error (
     " <direct>      ::= " & Argument_Parser.Image (Keys(7)));
    Basic_Proc.Put_Line_Error (
     " <once>        ::= " & Argument_Parser.Image (Keys(19)));
    Basic_Proc.Put_Line_Error (
     " <files>       ::= " & Argument_Parser.Image (Keys(8)));
    Basic_Proc.Put_Line_Error (
     " <bodies>      ::= " & Argument_Parser.Image (Keys(15)));
    Basic_Proc.Put_Line_Error (
     " <restrict>    ::= " & Argument_Parser.Image (Keys(17)));
    Basic_Proc.Put_Line_Error (
     " <inclusions>  ::= { <include> | <recursive> | <exclude> }");
    Basic_Proc.Put_Line_Error (
     " <include>     ::= " & Argument_Parser.Image (Keys(9)));
    Basic_Proc.Put_Line_Error (
     " <recursive>   ::= " & Argument_Parser.Image (Keys(10)));
    Basic_Proc.Put_Line_Error (
     " <exclude>     ::= " & Argument_Parser.Image (Keys(11)));
    Basic_Proc.Put_Line_Error (
     " <list>        ::= [ <files> ] [ <all> ] [ <children> ] "
                           & Argument_Parser.Image (Keys(12)));
    Basic_Proc.Put_Line_Error (
     "                   [ <path_unit> ]");
    Basic_Proc.Put_Line_Error (
     " <all>         ::= " & Argument_Parser.Image (Keys(13)));
    Basic_Proc.Put_Line_Error (
     " <children>    ::= " & Argument_Parser.Image (Keys(14)));
    Basic_Proc.Put_Line_Error (
     " <check>       ::= " & Argument_Parser.Image (Keys(3)) & " [ <target_dir> ]");
    Basic_Proc.Put_Line_Error (
     " <loop>        ::= " & Argument_Parser.Image (Keys(20)) & " [ <specs> ] <target_unit> ]");
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
     " <tab> to print a '|' every <size> level of the tree (default 3, 0 disables),");
    Basic_Proc.Put_Line_Error (
     " <fast> to show the first dependency way from target_unit to path_unit,");
    Basic_Proc.Put_Line_Error (
     " <direct> to show the direct dependencies between units,");
    Basic_Proc.Put_Line_Error (
     " <once> to show only once in the tree the dependencies of each units,");
    Basic_Proc.Put_Line_Error (
     " <bodies> to include the dependencies of bodies in revert mode,");
    Basic_Proc.Put_Line_Error (
     " <restrict> to discard restricted (limited / private) with,");
    Basic_Proc.Put_Line_Error (
     " <file> to show the file names (instead of unit names),");
    Basic_Proc.Put_Line_Error (
     " <inclusions> to add some directories or some directory trees to the search");
    Basic_Proc.Put_Line_Error (
     "    path (default is current and target directories), or to exclude some");
    Basic_Proc.Put_Line_Error (
     "    directory names (matching pattern) from directory trees,");
    Basic_Proc.Put_Line_Error (
     "List function lists units of a dir (default: current dir), or of a unit,");
    Basic_Proc.Put_Line_Error (
     "    optionally the subunits, alternatively corresponding files.");
    Basic_Proc.Put_Line_Error (
     "Check function shows redundant ""with"" clauses in a dir (default: current dir).");
    Basic_Proc.Put_Line_Error (
     "Loop function detects loops among dependencies of a unit.");
  end Usage;

  Error_Raised : exception renames Sourcer.Error_Raised;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Basic_Proc.Put_Line_Error ("See: " & Argument.Get_Program_Name & " "
                             & Argument_Parser.Image (Keys(1)));
    raise Error_Raised;
  end Error;

  -- Option management
  List_Mode : Boolean := False;
  All_Mode : Boolean := False;
  Children_Mode : Boolean := False;
  Check_Mode : Boolean := False;
  Loop_Mode : Boolean := False;
  Specs_Mode : Boolean := False;
  Revert_Mode : Boolean := False;
  Tree_Mode : Boolean := False;
  Tree_Tab : Natural := 3;
  Fast_Mode : Boolean := False;
  Direct_Mode : Boolean := False;
  Once_Mode : Boolean := False;
  Bodies_Mode : Boolean := False;
  Restrict_Mode : Boolean := False;
  Files_Mode : Boolean := False;
  Target, Target_Dir : As.U.Asu_Us;
  Path, Path_Dir : As.U.Asu_Us;
  List_Path : As.U.Asu_Us;
  Shortest : Boolean := False;

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
      Error (Kind & " unit " & Sort.Make_Path (Path, Name) & " not found");
    end if;
    Debug.Logger.Log_Debug (Kind & " unit checked: " & Sourcer.Image (Dscr));
    return Dscr;
  end Check_Unit;

  -- Change current dir
  procedure Change_Dir (Target : in String) is
  begin
    if Target /= "" then
      begin
        Directory.Change_Current (Directory.Make_Full_Path (Target));
      exception
        when others =>
          Error ("Cannot change to target directory " & Target);
      end;
    end if;
    Debug.Logger.Log_Debug ("In " &
        Directory.Make_Full_Path (Directory.Get_Current));
  end Change_Dir;

  -- Add the paths of -I and -R dirtectives in the proper order
  procedure Add_Paths is separate;

  -- Perfo severity
  use type Trace.Severities;
  Perfo : constant Trace.Severities := Trace.Debug * 2;

  use type As.U.Asu_Us;

  function Parse_Options return Boolean is
  begin
    -- Parse keys and options
    Arg_Dscr := Argument_Parser.Parse (Keys);
    if not Arg_Dscr.Is_Ok then
      Error (Arg_Dscr.Get_Error);
    end if;

    -- Help
    if Arg_Dscr.Is_Set (1) then
      Usage;
      Basic_Proc.Set_Error_Exit_Code;
      return False;
    end if;

    -- Version
    if Arg_Dscr.Is_Set (2) then
      Basic_Proc.Put_Line_Output (Argument.Get_Program_Name & " " & Version);
      Basic_Proc.Set_Error_Exit_Code;
      return False;
    end if;

    -- Modes
    if Arg_Dscr.Is_Set (4) then
      Specs_Mode := True;
    end if;
    if Arg_Dscr.Is_Set (5) then
      Revert_Mode := True;
    end if;
    if Specs_Mode and then Revert_Mode then
      Error ("Specs and revert modes are mutually exclusive");
    end if;
    if Arg_Dscr.Is_Set (6) then
      Tree_Mode := True;
    end if;
    if Arg_Dscr.Is_Set (7) then
      Direct_Mode := True;
    end if;
    if Arg_Dscr.Is_Set (8) then
      Files_Mode := True;
    end if;
    if Arg_Dscr.Is_Set (13) then
      All_Mode := True;
    end if;
    if Arg_Dscr.Is_Set (14) then
      Children_Mode := True;
    end if;
    if Arg_Dscr.Is_Set (15) then
      Bodies_Mode := True;
    end if;
    if Bodies_Mode and then not Revert_Mode then
      Error ("Bodies mode is allowed only in revert mode");
    end if;
    if Arg_Dscr.Is_Set (17) then
      Restrict_Mode := True;
    end if;
    if Bodies_Mode and then Restrict_Mode then
      Error ("Restrict mode is not allowed in bodies mode");
    end if;
    if Arg_Dscr.Is_Set (16) then
      Fast_Mode := True;
    end if;
    if Arg_Dscr.Is_Set (19) then
      Once_Mode := True;
    end if;

    -- Check mode
    if Arg_Dscr.Is_Set (3) then
      -- No option
      if Specs_Mode or else Revert_Mode or else Tree_Mode or else Direct_Mode
      or else Files_Mode or else Bodies_Mode or else Restrict_Mode
      or else Once_Mode then
        Error ("Check mode does not support options");
      end if;
      -- No include
      if Arg_Dscr.Get_Nb_Occurences (9) /= 0
      or else Arg_Dscr.Get_Nb_Occurences (10) /= 0 then
        Error ("Check mode is exclusive with simple or recursive includes");
      end if;
      Check_Mode := True;
    end if;

    -- Loop mode
    if Arg_Dscr.Is_Set (20) then
      -- No option excep Specs
      if Revert_Mode or else Tree_Mode or else Direct_Mode
      or else Files_Mode or else Bodies_Mode or else Restrict_Mode
      or else Once_Mode then
        Error ("Loop mode supports only option specs");
      end if;
      -- No include
      if Arg_Dscr.Get_Nb_Occurences (9) /= 0
      or else Arg_Dscr.Get_Nb_Occurences (10) /= 0 then
        Error ("Loop mode is exclusive with simple or recursive includes");
      end if;
      Loop_Mode := True;
    end if;

    -- List mode
    if Arg_Dscr.Is_Set (12) then
      -- No option except File and all
      if Specs_Mode or else Revert_Mode or else Tree_Mode
      or else Direct_Mode or else Bodies_Mode or else Restrict_Mode
      or else Once_Mode then
        Error ("List mode only supports file, all or children options");
      end if;
      -- No include
      if Arg_Dscr.Get_Nb_Occurences (9) /= 0
      or else Arg_Dscr.Get_Nb_Occurences (10) /= 0 then
        Error ("List mode is exclusive with simple or recursive includes");
      end if;
      List_Mode := True;
    end if;

    -- Check, List and Loop are axclusive
    if Check_Mode and then List_Mode then
      Error ("Check and list modes are mutually exclusive");
    end if;
    if Check_Mode and then Loop_Mode then
      Error ("Check and loop modes are mutually exclusive");
    end if;
    if Loop_Mode and then List_Mode then
      Error ("Loop and list modes are mutually exclusive");
    end if;

    -- Not tree, direct or once together
    if (Tree_Mode and then Direct_Mode)
    or else (Direct_Mode and then Once_Mode)
    or else (Once_Mode and then Tree_Mode) then
      Error ("Tree, Direct and Once modes are mutually exclusive");
    end if;

    -- Not fast and tree
    if Fast_Mode and then Tree_Mode then
      Error ("Tree and Fast mode are mutually exclusive");
    end if;

    -- All in list
    if All_Mode and then not List_Mode then
      Error ("All option is valid only in list mode");
    end if;
    -- Children in list
    if Children_Mode and then not List_Mode then
      Error ("Children option is valid only in list mode");
    end if;

    -- Tree tab
    if Arg_Dscr.Is_Set (18) then
      if not Tree_Mode then
        Error ("Tree tab requires tree mode");
      end if;
      begin
        Tree_Tab := Natural'Value (Arg_Dscr.Get_Option (18));
      exception
        when others =>
          Error ("Invalid tree tab value (expected a natural got "
               & Arg_Dscr.Get_Option (18) & ")");
      end;
    end if;
    return True;
  end Parse_Options;

begin
  ---------------------
  -- PARSE ARGUMENTS --
  ---------------------
  Debug.Logger.Init;
  Debug.Logger.Log (Perfo, "Starting");

  -- Parse options
  if not Parse_Options then
    return;
  end if;

  -- Save current dir and add it to paths (top prio)
  Directory.Get_Current (Current_Dir);
  if not List_Mode and then not Check_Mode then
    Sort.Add_Path (Current_Dir);
  end if;

  -- Target and inclusions
  if Check_Mode then
    if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 1 then
      -- An optional target directory
      Target_Dir := As.U.Tus (Arg_Dscr.Get_Option (
                      Argument_Parser.No_Key_Index));
      Target_Dir := As.U.Tus (Directory.Make_Full_Path (Target_Dir.Image));
      Sort.Add_Path (Target_Dir);
    elsif Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 0 then
      -- Current directory
      Sort.Add_Path (Current_Dir);
    else
      Error ("At most one target accepted");
    end if;
    Check_Dir (Target_Dir.Image);
  elsif Loop_Mode then
    if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 1 then
      Error ("Unit expected");
    end if;
    Target := As.U.Tus (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index, 1));
    Target_Dir := As.U.Tus (Directory.Make_Full_Path (Target.Image));
    List_Path := As.U.Tus (Directory.Dirname (Target.Image));
    Debug.Logger.Log_Debug ("Loops of unit " & Target_Dir.Image);
    -- Store path in Target_Dir, and target in Target
    Target := As.U.Tus (Mixed_Str (Directory.Basename (Target_Dir.Image)));
    Target_Dir := As.U.Tus (Directory.Make_Full_Path (Directory.Dirname
            (Target_Dir.Image)));
    -- Add target or current dir
    Sort.Add_Path (Target_Dir);
  elsif List_Mode then
    if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) > 1 then
      Error ("At most one path or unit accepted");
    elsif Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 1 then
      Target := As.U.Tus (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index, 1));
      Target_Dir := As.U.Tus (Directory.Make_Full_Path (Target.Image));
      List_Path := As.U.Tus (Directory.Dirname (Target.Image));
      begin
        -- See if Target_Dir is a file or dir
        if Directory.Is_File (Target_Dir.Image)
        or else Directory.Is_Link (Target_Dir.Image) then
          Debug.Logger.Log_Debug ("List file " & Target_Dir.Image);
          -- Store path in Target_Dir, and target in Target
          Target := As.U.Tus (Mixed_Str (Directory.Basename (Target_Dir.Image)));
          Target_Dir := As.U.Tus (Directory.Make_Full_Path (Directory.Dirname
            (Target_Dir.Image)));
        elsif Directory.Is_Dir (Target_Dir.Image) then
          -- Keep path in Target_Dir
          Target.Set_Null;
          Debug.Logger.Log_Debug ("List dir " & Target_Dir.Image);
        else
          -- Block, pipe...
          raise Constraint_Error;
        end if;
      exception
        when Directory.Name_Error =>
          -- Unit
          Debug.Logger.Log_Debug ("List unit " & Target_Dir.Image);
          -- Store path in Target_Dir, and target in Target
          Target := As.U.Tus (Mixed_Str (Directory.Basename (Target_Dir.Image)));
          Target_Dir := As.U.Tus (Directory.Make_Full_Path (Directory.Dirname
            (Target_Dir.Image)));
        when others =>
          Error ("Invalid Path_Unit " & Target.Image);
      end;
    end if;
    -- Add target or current dir
    if Target_Dir.Is_Null then
      Sort.Add_Path (Current_Dir);
    else
      Sort.Add_Path (Target_Dir);
    end if;
  else
    -- Target and path: at most 2 and at the end
    if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) > 2
    or else Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 0 then
      Error ("At least one target and at most one path required");
    elsif Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
      Error ("Invalid argument");
    end if;
    -- Target
    Target := As.U.Tus (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index, 1));
    if Target.Element (Target.Length) = '/' then
      -- Target us aunit name (cannot be a dir)
      Error ("Invalid Target_Unit");
    end if;
    Target_Dir := As.U.Tus (Directory.Make_Full_Path (Directory.Dirname
        (Target.Image)));
    Check_Dir (Target_Dir.Image);
    Target := As.U.Tus (Mixed_Str (Directory.Basename (Target.Image)));
    -- Path
    if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 2 then
      Path := As.U.Tus (Arg_Dscr.Get_Option (Argument_Parser.No_Key_Index, 2));
      Path_Dir := As.U.Tus (Directory.Make_Full_Path (Directory.Dirname
          (Path.Image)));
      Check_Dir (Path_Dir.Image);
      Path := As.U.Tus (Mixed_Str (Directory.Basename (Path.Image)));
    end if;
    -- Include target dir in paths (just after current dir)
    if not Target_Dir.Is_Null and then Target_Dir /= Current_Dir then
      Sort.Add_Path (Target_Dir);
    end if;
    -- Include path dir in paths (just after target dir)
    if not Path_Dir.Is_Null and then Path_Dir /= Target_Dir
    and then Path_Dir /= Current_Dir then
      Sort.Add_Path (Path_Dir);
    end if;
    -- Include dirs of -I and -R options
    Add_Paths;
  end if;

  -- Fast mode requires Path_Dir
  if Path_Dir.Is_Null and then Fast_Mode then
    Error ("Fast mode requires a Path_Unit");
  end if;

  -- Path_Dir prevents Direct
  if not Path_Dir.Is_Null and then Direct_Mode then
    Error ("Path_Unit and Direct mode are mutually exclusive");
  end if;
  -- Path_Dir prevents Once
  if not Path_Dir.Is_Null and then Once_Mode then
    Error ("Path_Unit and Once mode are mutually exclusive");
  end if;

  -- Children requires a target unit
  if Children_Mode and then Target.Is_Null then
    Error ("Children option requires a target unit");
  end if;

  Debug.Logger.Log (Perfo, "Arguments parsed");

  -- Path_Dir triggers shortest tree by default, otherwise full tree or fast
  if not Path_Dir.Is_Null then
    Shortest := not Fast_Mode and then not Tree_Mode;
    if Shortest then
      Tree_Mode := True;
    end if;
  end if;

  -- Loop triggers Once
  if Loop_Mode then
    Once_Mode := True;
  end if;
  -- Once triggers tree
  if Once_Mode then
    Tree_Mode := True;
  end if;

  ----------------------------
  -- BUILD LISTS OF SOURCES --
  ----------------------------
  Sourcer.Build_Lists;
  Debug.Logger.Log (Perfo, "Lists built");

  if Check_Mode then
    -----------------
    -- CHECK UNITS --
    -----------------
    if not Checker.Check then
      -- Check failed
      Basic_Proc.Set_Error_Exit_Code;
    end if;
    Debug.Logger.Log (Perfo, "Check done.");
    return;
  end if;

  ------------------------
  -- MOVE TO TARGET DIR --
  ------------------------
  Change_Dir (Target_Dir.Image);

  ------------------
  -- CHECK TARGET --
  ------------------
  if not Target.Is_Null then
    Target_Dscr := Check_Unit (Target_Dir, Target, "Target");
  end if;
  if not Path.Is_Null then
    Path_Dscr := Check_Unit (Path_Dir, Path, "Path");
  end if;
  Debug.Logger.Log (Perfo, "Target checked");

  -----------------
  -- LIST TARGET --
  -----------------
  if List_Mode then
    Output.List (Target.Image, Target_Dir.Image, List_Path.Image, Files_Mode,
                 All_Mode, Children_Mode);
    Debug.Logger.Log (Perfo, "List done.");
    return;
  end if;

  ----------------------------
  -- BUILD TREE OF SOURCES --
  ----------------------------
  Tree_Mng.Build (Target_Dscr, Specs_Mode, Revert_Mode,
                  Tree_Mode, Shortest, Direct_Mode,
                  Once_Mode,
                  Files_Mode, Bodies_Mode, Restrict_Mode, Loop_Mode);
  Debug.Logger.Log (Perfo, "Tree built");

  -------------------
  -- PUT LIST/TREE --
  -------------------
  if not Loop_Mode then
    -- Back to original dir
    Check_Dir ("");
    Output.Put (Revert_Mode, Tree_Mode, Shortest, Files_Mode, Tree_Tab,
                Path_Dscr);
    Debug.Logger.Log (Perfo, "Dependency done.");
  end if;

exception
  when Error_Raised | Sourcer.Error_Raised | Output.Error_Raised =>
    Basic_Proc.Set_Error_Exit_Code;
  when Err:others =>
    Basic_Proc.Put_Line_Error ("ERROR: " &
      "Exception " & Ada.Exceptions.Exception_Name (Err) & " raised");
    Basic_Proc.Set_Error_Exit_Code;
end Lsadeps;

