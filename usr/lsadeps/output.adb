with As.U.Utils, Basic_Proc, Directory, Unbounded_Arrays, Parser, Trees;
with Debug, Tree_Mng, Sort;
package body Output is

  -- For tree/path indent
  Tab : constant String := "  ";
  Vab : constant String := "| ";
  Vtab : Natural := 0;

  -- Are we in revert or shortest mode. Tree iterators need it
  Revert : Boolean := False;
  Shortest : Boolean := False;

  -- Current directory
  Curr_Dir : As.U.Asu_Us;

  -- Unit to show Path to/from
  Path_Unit_Full : As.U.Asu_Us;

  -- Path for list
  List_Path : As.U.Asu_Us;

  -- List or not the subunits, the child units. Sourcer iterators need it
  List_Subunits : Boolean := False;
  List_Children : Boolean := False;

  -- Strip path of Str if current dir
  function Strip (Str : String) return String is
    (if Directory.Dirname (Str) = Curr_Dir.Image then Directory.Basename (Str)
     else Str);

  -- Put_Line a stripped string
  procedure Put_Line_Stripped (Str : in String) is
  begin
    Basic_Proc.Put_Line_Output (Strip (Str));
  end Put_Line_Stripped;

  -----------------------
  -- TREE and SHORTEST --
  -----------------------
  -- Is Dscr a parent (spec for a body, body of a subunit)
  --  of current tree element
  function Is_Parent  (Parent, Current : Sourcer.Src_Dscr) return Boolean is
    use type As.U.Asu_Us;
  begin
    if Parent.Unit.Is_Null then
      -- Tree root
      return False;
    end if;
    case Current.Kind is
      when Sourcer.Unit_Spec =>
        -- Current cannot have Parent as parent
        return False;
      when Sourcer.Unit_Body =>
        -- Current is the body of Parent if same name
        return Current.Unit = Parent.Unit;
      when Sourcer.Subunit =>
        -- Current is a subunit of Parent if Current.Parent matches Parent
        return Current.Parent = Parent.Unit;
    end case;
  end Is_Parent;

  -- Shortest needs 2 passes of scan
  Pass : Positive := 1;
  -- First pass to determine the shortest way
  Best_Level : Integer := Integer'Last;
  -- Second pass to store the current path and show it if its final
  --  length is Best_Level
  Way : As.U.Utils.Asu_Ua.Unb_Array;

  -- Store (replace or append) an entry (unit/file) at a level
  procedure Store (Name : in String; Level : in Natural) is
  begin
    if Level + 1 <= Way.Length then
      Way.Replace_Element (Level + 1, As.U.Tus (Name));
    elsif Level = Way.Length then
      Way.Append (As.U.Tus (Name));
    else
      Debug.Logger.Log_Fatal ("Hole in the Way");
      raise Error_Raised;
    end if;
  end Store;

  -- Process one unit of the tree
  function Tree_Unit_Process (Dscr  : Tree_Mng.Src_Dscr;
                              Level : Natural;
                              Name  : As.U.Asu_Us) return Boolean is
    Str, Unit, Indent : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    if Name.Is_Null then
      -- Not a valid unit to process
      return True;
    end if;
    if Shortest then
      if Level > Best_Level then
        -- Reached Best_Level, skip this branch
        return False;
      end if;
      Unit := As.U.Tus (Sort.Make_Path (Dscr.Dscr.Path, Name));
      if Level = Best_Level and then Unit /= Path_Unit_Full then
        -- Reached Best_Level, skip this branch
        return False;
      end if;
      if Pass = 1 then
        -- 1st pass: Determine shortest path
        if Unit =  Path_Unit_Full then
          -- Leaf
          if Level < Best_Level then
            Best_Level := Level;
          end if;
        end if;
      else
        -- 2nd: Detect and put shortests paths
        -- Store current unit
        Store (Strip (Unit.Image), Level);
        -- Level is either < Best (and we have children) or Best
        if Unit =  Path_Unit_Full then
          -- Leaf of best level => put
          for I in 0 .. Level loop
            -- Put name
            Basic_Proc.Put_Line_Output (Indent.Image
                                      & Way.Element (I + 1).Image);
            Indent.Append (Tab);
          end loop;
        end if;
      end if;
    else
      -- Dump tree: Indent
      for I in 1 .. Level loop
        Str.Append (if Vtab /= 0 and then I mod Vtab = 2 then Vab else Tab);
      end loop;
      -- Put name
      Str.Append (Strip (Sort.Make_Path (Dscr.Dscr.Path, Name)));
      Basic_Proc.Put_Line_Output (Str.Image);
    end if;
    return True;
  end Tree_Unit_Process;

  -- Dump Units of tree
  Level : Integer := -1;
  procedure Tree_Unit_Walker (Parent : in Sourcer.Src_Dscr) is
    Dscr : Tree_Mng.Src_Dscr;
    Incr : Boolean := False;
    Name : As.U.Asu_Us;
    Nb_Children : Natural;
    use type Sourcer.Src_Kind_List;
  begin
    -- Get current item
    Tree_Mng.Tree.Read (Dscr);

    -- Update level
    if Revert
    or else Dscr.Dscr.Kind = Sourcer.Unit_Spec
    or else (Dscr.Dscr.Kind = Sourcer.Unit_Body
             and then Dscr.Dscr.Standalone) then
      Level := Level + 1;
      Incr := True;
      -- PathOfFile / UnitName
      if Revert then
        -- Discard if we are body or subunit of parent
        if Is_Parent (Parent, Dscr.Dscr) then
          -- Don't display this entry
          Name.Set_Null;
        else
          -- Put unit name, body ancestor of the subunit
          if Dscr.Dscr.Kind = Sourcer.Subunit then
            Name := Sourcer.Get_Root_Body (Dscr.Dscr).Unit;
          else
            Name := Dscr.Dscr.Unit;
          end if;
        end if;
      else
        Name := Dscr.Dscr.Unit;
      end if;
      -- Got a valid unit
      if not Tree_Unit_Process (Dscr, Level, Name) then
        Level := Level - 1;
        return;
      end if;
    end if;

    -- No recursion if looping
    if Dscr.Looping then
      Level := Level - 1;
      return;
    end if;

    -- Iterate on children
    Nb_Children := Tree_Mng.Tree.Children_Number;
    for I in 1 .. Nb_Children loop
      if I = 1 then
        Tree_Mng.Tree.Move_Child (False);
      else
        Tree_Mng.Tree.Move_Brother;
      end if;
      Tree_Unit_Walker (Dscr.Dscr);
    end loop;
    -- Move back to current in tree
    if Nb_Children /= 0 then
      Tree_Mng.Tree.Move_Father;
    end if;

    -- Restore initial level
    if Incr then
      Level := Level - 1;
    end if;

  end Tree_Unit_Walker;

  -- Dump files of tree
  function Tree_File_Iterator (
      Dscr : in out Tree_Mng.Src_Dscr;
      Level : Natural) return Trees.Iteration_Policy is
    Str : As.U.Asu_Us;
  begin
    -- Indent
    for I in 1 .. Level loop
      Str.Append (if Vtab /= 0 and then I mod Vtab = 2 then Vab else Tab);
    end loop;
    -- Put name
    Str.Append (Strip (Sort.Make_Path (Dscr.Dscr.Path, Dscr.Dscr.File)));
    Basic_Proc.Put_Line_Output (Str.Image);
    return Trees.Go_On;
  end Tree_File_Iterator;

  -- 1st pass: Determine shortest path
  -- 2nd pass: Store path then put it
  function Shortest_File_Iterator (
      Dscr : in out Tree_Mng.Src_Dscr;
      Level : Natural) return Trees.Iteration_Policy is
    Unit, Indent : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    if Level > Best_Level then
      -- Reached Best_Level, skip this branch
      return Trees.Skip;
    end if;
    Unit := As.U.Tus (Sort.Make_Path (Dscr.Dscr.Path, Dscr.Dscr.Unit));
    if Level = Best_Level and then Unit /= Path_Unit_Full then
      -- Reached Best_Level, skip this branch
      return Trees.Skip;
    end if;
    if Pass = 1 then
      if Unit =  Path_Unit_Full then
        -- Leaf
        if Level < Best_Level then
          Best_Level := Level;
        end if;
      end if;
    else
      -- Second pass to put shortests paths
      -- Store current file
      Store (Strip (Sort.Make_Path (Dscr.Dscr.Path, Dscr.Dscr.File)), Level);
      -- Level is either < Best (and we have children) or Best
      if Unit =  Path_Unit_Full then
        -- Leaf of best level => put
        for I in 0 .. Level loop
          -- Put name
          Basic_Proc.Put_Line_Output (Indent.Image & Way.Element (I + 1).Image);
          Indent.Append (Tab);
        end loop;
      end if;
    end if;
    return Trees.Go_On;
  end Shortest_File_Iterator;

  -- Put tree of units or files
  procedure Put_Tree (File_Mode : in Boolean) is
    Dscr : Sourcer.Src_Dscr;
  begin
    if File_Mode then
      if Shortest then
        Tree_Mng.Tree.Iterate (Shortest_File_Iterator'Access, False);
      else
        Tree_Mng.Tree.Iterate (Tree_File_Iterator'Access, False);
      end if;
    else
      Tree_Unit_Walker (Dscr);
    end if;
  end Put_Tree;

  -- Put shortest paths of tree
  procedure Put_Shortest (File_Mode : in Boolean) is
  begin
    -- First pass
    Put_Tree (File_Mode);
    Debug.Logger.Log_Debug ("Best level is" & Best_Level'Img);
    -- Second  pass
    Pass := Pass + 1;
    Level := -1;
    Put_Tree (File_Mode);
  end Put_Shortest;

  ----------
  -- LIST --
  ----------
  -- Unique list of entries (units or files)
  Ulist : As.U.Utils.Asu_Unique_List_Mng.Unique_List_Type;
  -- Dynamic list of sorted entries (units or files)
  Dlist : As.U.Utils.Asu_Dyn_List_Mng.List_Type;
  -- Store unit of tree
  function List_Unit_Iterator (
      Dscr : in out Tree_Mng.Src_Dscr;
      Unused_Level : Natural) return Trees.Iteration_Policy is
    Name : As.U.Asu_Us;
    use type Sourcer.Src_Kind_List;
  begin
    -- Discard Looping info
    if Dscr.Looping then
      return Trees.Go_On;
    end if;
    if not Revert then
      -- Keep only spec or standalone body
      if (Dscr.Dscr.Kind = Sourcer.Subunit
          and then not List_Subunits)
         or else (Dscr.Dscr.Kind = Sourcer.Unit_Body
                  and then not Dscr.Dscr.Standalone) then
        return Trees.Go_On;
      end if;
      Name := Dscr.Dscr.Unit;
    else
      -- Put unit name, body ancestor of the subunit
      if Dscr.Dscr.Kind = Sourcer.Subunit then
        Name := Sourcer.Get_Root_Body (Dscr.Dscr).Unit;
      else
        Name := Dscr.Dscr.Unit;
      end if;
    end if;
    -- PathOfFile / UnitName
    Ulist.Insert (Sort.Make_Path (Dscr.Dscr.Path, Name));
    return Trees.Go_On;
  end List_Unit_Iterator;

  -- Store file of tree
  function List_File_Iterator (
      Dscr : in out Tree_Mng.Src_Dscr;
      Unused_Level : Natural) return Trees.Iteration_Policy is
  begin
    -- Discard Looping info
    if not Dscr.Looping then
      -- File
      Ulist.Insert (Sort.Make_Path (Dscr.Dscr.Path, Dscr.Dscr.File));
    end if;
    return Trees.Go_On;
  end List_File_Iterator;

  -- Store unit of list
  procedure Src_Unit_Iterator (Dscr  : in Sourcer.Src_Dscr;
                               Go_On : in out Boolean) is
    use type Sourcer.Src_Kind_List;
  begin
    -- Skip subunits, store each unit once
    if Dscr.Kind /= Sourcer.Subunit or else List_Subunits then
      Ulist.Insert_If_New (Sort.Make_Path (List_Path, Dscr.Unit));
    end if;
    Go_On := True;
  end Src_Unit_Iterator;

  -- Store unit of list
  procedure Src_File_Iterator (Dscr  : in Sourcer.Src_Dscr;
                               Go_On : in out Boolean) is
    use type Sourcer.Src_Kind_List;
  begin
    -- Skip subunits
    if Dscr.Kind /= Sourcer.Subunit or else List_Subunits then
      Ulist.Insert (Sort.Make_Path (List_Path, Dscr.File));
    end if;
    Go_On := True;
  end Src_File_Iterator;

  -- Put list of units or files
  procedure Put_List (File_Mode : in Boolean; From_Tree : in Boolean) is
    Str : As.U.Asu_Us;
    Moved : Boolean;
  begin
    -- Build unique list of entries
    if From_Tree then
      Debug.Logger.Log_Debug ("Scanning tree");
      if File_Mode then
        Tree_Mng.Tree.Iterate (List_File_Iterator'Access);
      else
        Tree_Mng.Tree.Iterate (List_Unit_Iterator'Access);
      end if;
    else
      Debug.Logger.Log_Debug ("Scanning list");
      if File_Mode then
        Sourcer.List.Iterate (Src_File_Iterator'Access);
      else
        Sourcer.List.Iterate (Src_Unit_Iterator'Access);
      end if;
    end if;
    -- Sort this list
    Debug.Logger.Log_Debug ("Copying list");
    if Ulist.Is_Empty then
      Debug.Logger.Log_Fatal ("No list of unit or file");
      raise Error_Raised;
    end if;
    Ulist.Rewind;
    loop
      Ulist.Read_Next (Str, Moved);
      Dlist.Insert (Str);
      exit when not Moved;
    end loop;
    Debug.Logger.Log_Debug ("Sorting list");
    Sort.Sort (Dlist);
    -- Put entries
    Debug.Logger.Log_Debug ("Listing:");
    Dlist.Rewind;
    loop
      Dlist.Read (Str, Moved => Moved);
      Put_Line_Stripped (Str.Image);
      exit when not Moved;
    end loop;
  end Put_List;

  ----------
  -- PATH --
  ----------
  -- Put path from Root to Path_Unit or revert
  -- Ubnounded array of paths so far
  type Path_Rec is record
    Level : Natural;
    Path : As.U.Asu_Us;
  end record;
  type Path_Array is array (Positive range <>) of Path_Rec;
  package Unb_Paths_Mng is new Unbounded_Arrays (Path_Rec, Path_Array);
  Paths : Unb_Paths_Mng.Unb_Array;
  -- Remove tail of Paths until last Path.Level < Level
  procedure Move_Up (Level : in Natural) is
    Path : Path_Rec;
    Len : Natural;
  begin
    loop
      Len := Paths.Length;
      exit when Len = 0;
      Path := Paths.Element (Len);
      exit when Path.Level < Level;
      Paths.Delete (Len, Len);
    end loop;
  end Move_Up;
  -- Show the units of the path
  procedure Show is
    Indent : As.U.Asu_Us;
  begin
    for I in 1 .. Paths.Length loop
      Basic_Proc.Put_Output (Indent.Image);
      Put_Line_Stripped (Paths.Element(I).Path.Image);
      Indent.Append (Tab);
    end loop;
  end Show;

  -- Reuse the Level
  -- Path of units
  function Path_Unit_Iterator (Dscr : in out Tree_Mng.Src_Dscr;
                               Level : Natural) return Trees.Iteration_Policy is
    Name : As.U.Asu_Us;
    use type As.U.Asu_Us;
    use type Sourcer.Src_Kind_List;
  begin
    -- Clean any previous path if we are movin up
    Move_Up (Level);
    -- Discard Looping info
    if Dscr.Looping then
      return Trees.Go_On;
    end if;
    if not Revert then
      -- Keep only spec or standalone body
      if Dscr.Dscr.Kind = Sourcer.Subunit
         or else (Dscr.Dscr.Kind = Sourcer.Unit_Body
                  and then not Dscr.Dscr.Standalone) then
        return Trees.Go_On;
      end if;
      Name := Dscr.Dscr.Unit;
    else
      -- Put unit name, body ancestor of the subunit
      if Dscr.Dscr.Kind = Sourcer.Subunit then
        Name := Sourcer.Get_Root_Body (Dscr.Dscr).Unit;
      else
        Name := Dscr.Dscr.Unit;
      end if;
    end if;
    -- Append and check PathOfFile / UnitName
    Name := Sort.Make_Path (Dscr.Dscr.Path, Name);
    -- Avoid adding a subunit after its body
    if Paths.Is_Null or else Name /= Paths.Element (Paths.Length).Path then
      Paths.Append ( Path_Rec'(Level, Name));
      if Name = Path_Unit_Full then
        Show;
      end if;
    end if;
    return Trees.Go_On;
  end Path_Unit_Iterator;

  -- Path of files
  function Path_File_Iterator (Dscr : in out Tree_Mng.Src_Dscr;
                               Level : Natural) return Trees.Iteration_Policy is
    Name, File : As.U.Asu_Us;
    use type As.U.Asu_Us;
    use type Sourcer.Src_Kind_List;
  begin
    -- Clean any previous path if we are movin up
    Move_Up (Level);
    -- Discard Looping info
    if Dscr.Looping then
      return Trees.Go_On;
    end if;
    -- Append and check PathOfFile / Unit_Name and same for Unit_File
    Name := As.U.Tus (Sort.Make_Path (Dscr.Dscr.Path, Dscr.Dscr.Unit));
    File := As.U.Tus (Sort.Make_Path (Dscr.Dscr.Path, Dscr.Dscr.File));
    Paths.Append ( Path_Rec'(Level, File));
    -- See if we are at Path_Unit
    -- Avoid showing non standalone body of Path_Unit
    if Name = Path_Unit_Full
    and then (Dscr.Dscr.Kind = Sourcer.Unit_Spec
              or else (Dscr.Dscr.Kind = Sourcer.Unit_Body
                       and then Dscr.Dscr.Standalone)) then
      Show;
    end if;
    return Trees.Go_On;
  end Path_File_Iterator;

  procedure Put_Path (File_Mode : in Boolean) is
  begin
    -- Build unique list of entries
    Debug.Logger.Log_Debug ("Scanning tree");
    if File_Mode then
      Tree_Mng.Tree.Iterate (Path_File_Iterator'Access);
    else
      Tree_Mng.Tree.Iterate (Path_Unit_Iterator'Access);
    end if;
  end Put_Path;

  -- Put list/tree, normal/revert of units/files
  procedure Put (Revert_Mode, Tree_Mode, Shortest_Mode, File_Mode : in Boolean;
                 Tree_Vtab : in Natural;
                 Path_Unit : in Sourcer.Src_Dscr) is
  begin
    Directory.Get_Current (Curr_Dir);
    if Curr_Dir.Image /= "/" then
      Curr_Dir.Append ("/");
    end if;
    Revert := Revert_Mode;
    Shortest := Shortest_Mode;
    Output.Vtab := Tree_Vtab;
    Path_Unit_Full := Sort.Make_Path (Path_Unit.Path, Path_Unit.Unit);
    if not Path_Unit.Unit.Is_Null and then not Shortest_Mode then
      -- Show a path from Root to Path_Unit or revert
      Put_Path (File_Mode);
    elsif Shortest_Mode then
      -- Show shortest paths of the tree from Root to Path_Unit or revert
      Put_Shortest (File_Mode);
    elsif Tree_Mode then
      -- Show tree from Root or reverse
      Put_Tree (File_Mode);
    else
      -- Show list from Root or reverse
     Put_List (File_Mode, True);
    end if;
  end Put;

  ----------------
  -- LIST UNITS --
  ----------------
  -- Add a unit and its subunits
  -- For parsing list of subunits
  function Is_Sep (C : Character) return Boolean is (C = Sourcer.Separator);
  procedure Add_Unit (Str : in As.U.Asu_Us; File_Mode : Boolean) is
    Spec_Dscr, Body_Dscr : Sourcer.Src_Dscr;
    Iter : Parser.Iterator;
    Subunit, Child : As.U.Asu_Us;
    use type As.U.Asu_Us, Sourcer.Src_Kind_List;
  begin
    -- Get unit
    Spec_Dscr := Sourcer.Get_Unit (Str);
    if Spec_Dscr.Unit.Is_Null then
      -- Unit not found
      return;
    end if;
    -- Add this unit or file
    if File_Mode then
      Dlist.Insert (Sort.Make_Path (List_Path, Spec_Dscr.File));
    else
      Dlist.Insert (Sort.Make_Path (List_Path, Spec_Dscr.Unit));
    end if;

    -- Get the body of a spec (if any), add file
    if Spec_Dscr.Kind = Sourcer.Unit_Spec
    and then not Spec_Dscr.Standalone then
      Body_Dscr := Sourcer.Get_Body (Spec_Dscr);
      if File_Mode then
        Dlist.Insert (Sort.Make_Path (List_Path, Body_Dscr.File));
      end if;
    elsif Spec_Dscr.Kind = Sourcer.Unit_Body
    or else Spec_Dscr.Kind = Sourcer.Subunit then
      Body_Dscr := Spec_Dscr;
    end if;

    -- Iterate on its subunits (body or subunit)
    if List_Subunits and then not Body_Dscr.Subunits.Is_Null then
      Iter.Set (Body_Dscr.Subunits.Image, Is_Sep'Access);
      loop
        Subunit := As.U.Tus (Iter.Next_Word);
        exit when Subunit.Is_Null;
        Add_Unit (Body_Dscr.Path & Subunit, File_Mode);
      end loop;
    end if;

    -- Iterate on its children
    if List_Children and then not Spec_Dscr.Children.Is_Null then
      Iter.Set (Spec_Dscr.Children.Image, Is_Sep'Access);
      loop
        Child := As.U.Tus (Iter.Next_Word);
        exit when Child.Is_Null;
        Add_Unit (Spec_Dscr.Path & Child, File_Mode);
      end loop;
    end if;

  end Add_Unit;

  -- List a unit or all
  procedure List (Target, Dir, Path : in String;
                  File_Mode, Subunits, Children : in Boolean) is
    Str : As.U.Asu_Us;
    Moved : Boolean;
  begin
    List_Path := As.U.Tus (Path);
    Revert := False;
    List_Subunits := Subunits;
    List_Children := Children;
    if Target = "" then
      Debug.Logger.Log_Debug ("Listing dir: " & Path);
      Put_List (File_Mode, False);
    else
      Str := Sort.Make_Path (Dir, Target);
      Debug.Logger.Log_Debug ("Listing unit: " & Str.Image);
      -- Put unit and maybe subunits of Target
      -- Insert it and its subunits
      Add_Unit (Str, File_Mode);
      -- Sort
      Sort.Sort (Dlist);
      -- Put entries
      Dlist.Rewind;
      loop
        Dlist.Read (Str, Moved => Moved);
        Put_Line_Stripped (Str.Image);
        exit when not Moved;
      end loop;
    end if;
  end List;

end Output;

