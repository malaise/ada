with Basic_Proc, Directory;
with As.U; use As.U;
with Debug, Sourcer, Tree_Mng, Sort;
package body Output is

  ----------
  -- TREE --
  ----------
  -- Dump Units of tree
  Level : Natural := 0;
  procedure Tree_Unit_Iterator is
    Dscr : Tree_Mng.Src_Dscr;
    Str : Asu_Us;
    Incr : Boolean := False;
    use type Sourcer.Src_Kind_List;
  begin
    -- Get current item
    Tree_Mng.Tree.Read (Dscr);

    -- Discard Looping info
    if Dscr.Looping then
      return;
    end if;

    -- Update level
    if Dscr.Dscr.Kind = Sourcer.Unit_Spec
    or else (Dscr.Dscr.Kind = Sourcer.Unit_Body
             and then Dscr.Dscr.Standalone) then
      Level := Level + 1;
      Incr := True;
      for I in 1 .. Level - 1 loop
        Asu.Append (Str, "  ");
      end loop;
      -- PathOfFile / UnitName
      Asu.Append (Str, Directory.Build_File_Name (
              Directory.Dirname (Asu_Ts (Dscr.Dscr.File)),
              Asu_Ts (Dscr.Dscr.Unit),
              ""));
      Basic_Proc.Put_Line_Output (Asu_Ts (Str));
    end if;

    -- Iterate on first child
    if Tree_Mng.Tree.Children_Number /= 0 then
      Tree_Mng.Tree.Move_Child (False);
      Tree_Unit_Iterator;
      Tree_Mng.Tree.Move_Father;
    end if;

    -- Restore initial level for brothers
    if Incr then
      Level := Level - 1;
    end if;

    -- Iterate on brother
    if Tree_Mng.Tree.Has_Brother then
      Tree_Mng.Tree.Move_Brother;
      Tree_Unit_Iterator;
    end if;

  end Tree_Unit_Iterator;

  -- Dump files of tree
  function Tree_File_Iterator (Dscr : Tree_Mng.Src_Dscr;
                               Level : Natural) return Boolean is
    Str : Asu_Us;
    use type Sourcer.Src_Kind_List;
  begin
    -- Discard Looping info
    if Dscr.Looping then
      return True;
    end if;
    for I in 1 .. Level loop
      Asu.Append (Str, "  ");
    end loop;
    -- File
    Asu.Append (Str, Dscr.Dscr.File);
    Basic_Proc.Put_Line_Output (Asu_Ts (Str));
    return True;
  end Tree_File_Iterator;

  -- Put tree of units or files
  procedure Put_Tree (Units : in Boolean) is
  begin
    if Units then
      Tree_Unit_Iterator;
    else
      Tree_Mng.Tree.Iterate (Tree_File_Iterator'Access, False);
    end if;
  end Put_Tree;


  ----------
  -- LIST --
  ----------
  -- Are we in revert mode
  Revert : Boolean := False;
  -- Unique list of entries (units or files)
  Ulist : Asu_Unique_List_Mng.List_Type;
  -- Dynamic list of sorted entries (units or files)
  Dlist : Asu_Dyn_List_Mng.List_Type;
  -- List Units of tree
  function List_Unit_Iterator (Dscr : Tree_Mng.Src_Dscr;
                               Level : Natural) return Boolean is
    pragma Unreferenced (Level);
    Name : Asu_Us;
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
      -- Put unit name, parent of subunit
      if Dscr.Dscr.Kind = Sourcer.Subunit then
        Name := Sourcer.Get_Root (Dscr.Dscr).Unit;
      else
        Name := Dscr.Dscr.Unit;
      end if;
    end if;
    -- PathOfFile / UnitName
    Ulist.Insert (Asu_Tus (
        Directory.Build_File_Name (
            Directory.Dirname (Asu_Ts (Dscr.Dscr.File)),
            Asu_Ts (Name),
            "")));
    return True;
  end List_Unit_Iterator;

  -- Dump files of tree
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
    Ulist.Insert (Dscr.Dscr.File);
    return True;
  end List_File_Iterator;

  -- Put list of units or files
  procedure Put_List (Revert : in Boolean; Units : in Boolean) is
    Str : Asu_Us;
    Moved : Boolean;
  begin
    -- Build unique list of entries
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Scanning tree");
    end if;
    Output.Revert := Revert;
    if Units then
      Tree_Mng.Tree.Iterate (List_Unit_Iterator'Access);
    else
      Tree_Mng.Tree.Iterate (List_File_Iterator'Access);
    end if;
    -- Sort this list
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Copying list");
    end if;
    Ulist.Rewind;
    loop
      Ulist.Read_Next (Str, Moved);
      Dlist.Insert (Str);
      exit when not Moved;
    end loop;
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Sorting list");
    end if;
    Sort.Sort (Dlist);
    -- Put entries
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Listing:");
    end if;
    Dlist.Rewind;
    loop
      Dlist.Read (Str, Moved => Moved);
      Basic_Proc.Put_Line_Output (Asu_Ts (Str));
      exit when not Moved;
    end loop;
  end Put_List;

end Output;

