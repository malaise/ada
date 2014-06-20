with As.U.Utils, Basic_Proc, Directory, Unbounded_Arrays, Parser;
with Debug, Tree_Mng, Sort;
package body Output is

  -- For tree/path indent
  Tab : constant String := "  ";

  -- Are we in revert mode. Tree iterators need it
  Revert : Boolean := False;

  -- Current directory
  Curr_Dir : As.U.Asu_Us;

  -- Unit to show Path to/from
  Path_Unit_Full : As.U.Asu_Us;

  -- Path for list
  List_Path : As.U.Asu_Us;

  -- Strip path of Str if current dir
  function Strip (Str : String) return String is
  begin
    if Directory.Dirname (Str) = Curr_Dir.Image then
      return Directory.Basename (Str);
    else
      return Str;
    end if;
  end Strip;

  -- Put_Line a stripped string
  procedure Put_Line_Stripped (Str : in String) is
  begin
    Basic_Proc.Put_Line_Output (Strip (Str));
  end Put_Line_Stripped;

  ----------
  -- TREE --
  ----------
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

  -- Limit to level one (direct mode)
  Level1 : Boolean;

  -- Dump Units of tree
  Level : Integer := -1;
  procedure Tree_Unit_Walker (Parent : in Sourcer.Src_Dscr) is
    Dscr : Tree_Mng.Src_Dscr;
    Str : As.U.Asu_Us;
    Incr : Boolean := False;
    Name : As.U.Asu_Us;
    Nb_Children : Natural;
    use type Sourcer.Src_Kind_List;
  begin
    -- Get current item
    Tree_Mng.Tree.Read (Dscr);

    -- Discard Looping info
    if Dscr.Looping then
      return;
    end if;

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
      if not Name.Is_Null
      and then (not Level1 or else Level <= 1) then
        for I in 1 .. Level loop
          Str.Append (Tab);
        end loop;
        Str.Append (Strip (Sort.Make_Path (Dscr.Dscr.Path, Name)));
        Basic_Proc.Put_Line_Output (Str.Image);
      end if;
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
  function Tree_File_Iterator (Dscr : Tree_Mng.Src_Dscr;
                               Level : Natural) return Boolean is
    Str : As.U.Asu_Us;
    use type Sourcer.Src_Kind_List;
  begin
    -- Discard Looping info
    if Dscr.Looping then
      return True;
    end if;
    if not Level1 or else Level <= 1 then
      for I in 1 .. Level loop
        Str.Append (Tab);
      end loop;
      -- File
      Str.Append (Strip (Sort.Make_Path (Dscr.Dscr.Path, Dscr.Dscr.File)));
      Basic_Proc.Put_Line_Output (Str.Image);
    end if;
    return True;
  end Tree_File_Iterator;

  -- Put tree of units or files
  procedure Put_Tree (File_Mode : in Boolean) is
    Dscr : Sourcer.Src_Dscr;
  begin
    if File_Mode then
      Tree_Mng.Tree.Iterate (Tree_File_Iterator'Access, False);
    else
      Tree_Unit_Walker (Dscr);
    end if;
  end Put_Tree;

  ----------
  -- LIST --
  ----------
  -- Unique list of entries (units or files)
  Ulist : As.U.Utils.Asu_Unique_List_Mng.Unique_List_Type;
  -- Dynamic list of sorted entries (units or files)
  Dlist : As.U.Utils.Asu_Dyn_List_Mng.List_Type;
  -- Store unit of tree
  function List_Unit_Iterator (Dscr : Tree_Mng.Src_Dscr;
                               Level : Natural) return Boolean is
    pragma Unreferenced (Level);
    Name : As.U.Asu_Us;
    use type Sourcer.Src_Kind_List;
  begin
    -- Discard Looping info
    if Dscr.Looping then
      return True;
    end if;
    if not Revert then
      -- Keep only spec or standalone body
      if Dscr.Dscr.Kind = Sourcer.Subunit
         or else (Dscr.Dscr.Kind = Sourcer.Unit_Body
                  and then not Dscr.Dscr.Standalone) then
        return True;
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
    return True;
  end List_Unit_Iterator;

  -- Store file of tree
  function List_File_Iterator (Dscr : Tree_Mng.Src_Dscr;
                               Level : Natural) return Boolean is
    pragma Unreferenced (Level);
    use type Sourcer.Src_Kind_List;
  begin
    -- Discard Looping info
    if Dscr.Looping then
      return True;
    end if;
    -- File
    Ulist.Insert (Sort.Make_Path (Dscr.Dscr.Path, Dscr.Dscr.File));
    return True;
  end List_File_Iterator;

  -- Store unit of list
  procedure Src_Unit_Iterator (Dscr  : in Sourcer.Src_Dscr;
                               Go_On : in out Boolean) is
    use type Sourcer.Src_Kind_List;
  begin
    -- Skip subunits, store each unit once
    if Dscr.Kind /= Sourcer.Subunit then
      Ulist.Insert_If_New (Sort.Make_Path (List_Path, Dscr.Unit));
    end if;
    Go_On := True;
  end Src_Unit_Iterator;

  -- Store unit of list
  procedure Src_File_Iterator (Dscr  : in Sourcer.Src_Dscr;
                               Go_On : in out Boolean) is
  begin
    Ulist.Insert (Sort.Make_Path (List_Path, Dscr.File));
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
  function Path_Unit_Iterator (Dscr : Tree_Mng.Src_Dscr;
                               Level : Natural) return Boolean is
    Name : As.U.Asu_Us;
    use type As.U.Asu_Us;
    use type Sourcer.Src_Kind_List;
  begin
    -- Clean any previous path if we are movin up
    Move_Up (Level);
    -- Discard Looping info
    if Dscr.Looping then
      return True;
    end if;
    if not Revert then
      -- Keep only spec or standalone body
      if Dscr.Dscr.Kind = Sourcer.Subunit
         or else (Dscr.Dscr.Kind = Sourcer.Unit_Body
                  and then not Dscr.Dscr.Standalone) then
        return True;
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
    return True;
  end Path_Unit_Iterator;

  -- Path of files
  function Path_File_Iterator (Dscr : Tree_Mng.Src_Dscr;
                               Level : Natural) return Boolean is
    Name, File : As.U.Asu_Us;
    use type As.U.Asu_Us;
    use type Sourcer.Src_Kind_List;
  begin
    -- Clean any previous path if we are movin up
    Move_Up (Level);
    -- Discard Looping info
    if Dscr.Looping then
      return True;
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
    return True;
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
  procedure Put (Revert_Mode, Tree_Mode, Direct_Mode, File_Mode : in Boolean;
                 Path_Unit : in Sourcer.Src_Dscr) is
  begin
    Directory.Get_Current (Curr_Dir);
    if Curr_Dir.Image /= "/" then
      Curr_Dir.Append ("/");
    end if;
    Revert := Revert_Mode;
    Path_Unit_Full := Sort.Make_Path (Path_Unit.Path, Path_Unit.Unit);
    if not Path_Unit.Unit.Is_Null then
      -- Path from Root to Path_Unit or revert
      Put_Path (File_Mode);
    elsif Tree_Mode then
      -- Tree from Root or reverse
      Level1 := False;
      Put_Tree (File_Mode);
    elsif Direct_Mode then
      -- Level one of tree
      Level1 := True;
      Put_Tree (File_Mode);
    else
      -- List from Root or reverse
     Put_List (File_Mode, True);
    end if;
  end Put;

  -- Add a unit and its subunits
  -- For parsing list of subunits
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Sourcer.Separator;
  end Is_Sep;
  procedure Add_Unit (Str : in As.U.Asu_Us; File_Mode : Boolean) is
    Dscr : Sourcer.Src_Dscr;
    Iter : Parser.Iterator;
    Subunit : As.U.Asu_Us;
    use type As.U.Asu_Us, Sourcer.Src_Kind_List;
  begin
    -- Get unit
    Dscr := Sourcer.Get_Unit (Str);
    if Dscr.Unit.Is_Null then
      -- Unit not found
      return;
    end if;
    -- Add this unit or file
    if File_Mode then
      Dlist.Insert (Sort.Make_Path (List_Path, Dscr.File));
    else
      Dlist.Insert (Sort.Make_Path (List_Path, Dscr.Unit));
    end if;

    -- Get the body of a spec (if any), add file
    if Dscr.Kind = Sourcer.Unit_Spec and then not Dscr.Standalone then
      Dscr := Sourcer.Get_Body (Dscr);
      if File_Mode then
        Dlist.Insert (Sort.Make_Path (List_Path, Dscr.File));
      end if;
    end if;

    -- Iterate on its subunits
    Iter.Set (Dscr.Subunits.Image, Is_Sep'Access);
    loop
      Subunit := As.U.Tus (Iter.Next_Word);
      exit when Subunit.Is_Null;
      Add_Unit (Dscr.Path & Subunit, File_Mode);
    end loop;
  end Add_Unit;

  -- List a unit or all
  procedure List (Target, Dir, Path : in String; File_Mode : in Boolean) is
    Str : As.U.Asu_Us;
    Moved : Boolean;
  begin
    List_Path := As.U.Tus (Path);
    if Target = "" then
      Debug.Logger.Log_Debug ("Listing dir: " & Path);
      Put_List (File_Mode, False);
    else
      Str := Sort.Make_Path (Dir, Target);
      Debug.Logger.Log_Debug ("Listing unit: " & Str.Image);
      -- Put unit and subunits of Target
      -- Insert it and its subunits
      Add_Unit (Str, File_Mode);
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

