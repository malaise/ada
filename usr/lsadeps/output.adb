with Basic_Proc, Directory;
with As.U; use As.U;
with Tree_Mng;
package body Output is

  ----------
  -- TREE --
  ----------
  -- Dump Units of tree
  function Tree_Unit_Iterator (Dscr : Tree_Mng.Src_Dscr;
                               Level : Natural) return Boolean is
    Str : Asu_Us;
    use type Sourcer.Src_Kind_List;
  begin
    -- Discard Looping info. Keep spec or standalone body
    if Dscr.Looping
    or else Dscr.Dscr.Kind = Sourcer.Subunit
    or else (Dscr.Dscr.Kind = Sourcer.Unit_Body
             and then not Dscr.Dscr.Standalone) then
      return True;
    end if;
    for I in 1 .. Level loop
      Asu.Append (Str, "  ");
    end loop;
    -- PathOfFile / UnitName
    Asu.Append (Str, Directory.Build_File_Name (
            Directory.Dirname (Asu_Ts (Dscr.Dscr.File)),
            Asu_Ts (Dscr.Dscr.Unit),
            ""));
    Basic_Proc.Put_Line_Output (Asu_Ts (Str));
    return True;
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
      Tree_Mng.Tree.Iterate (Tree_Unit_Iterator'Access);
    else
      Tree_Mng.Tree.Iterate (Tree_File_Iterator'Access);
    end if;
  end Put_Tree;


  ----------
  -- LIST --
  ----------
  -- Unique list of entries (units or files)
  Ulist : Asu_Unique_List_Mng.List_Type;
  -- List Units of tree
  function List_Unit_Iterator (Dscr : Tree_Mng.Src_Dscr;
                               Level : Natural) return Boolean is
    pragma Unreferenced (Level);
    use type Sourcer.Src_Kind_List;
  begin
    -- Discard Looping info. Keep spec or standalone body
    if Dscr.Looping
    or else Dscr.Dscr.Kind = Sourcer.Subunit
    or else (Dscr.Dscr.Kind = Sourcer.Unit_Body
             and then not Dscr.Dscr.Standalone) then
      return True;
    end if;
    -- PathOfFile / UnitName
    Ulist.Insert (Asu_Tus (
        Directory.Build_File_Name (
            Directory.Dirname (Asu_Ts (Dscr.Dscr.File)),
            Asu_Ts (Dscr.Dscr.Unit),
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
  procedure Put_List (Units : in Boolean) is
    Str : Asu_Us;
    Moved : Boolean;
  begin
    -- Build unique list of entries
    if Units then
      Tree_Mng.Tree.Iterate (List_Unit_Iterator'Access);
    else
      Tree_Mng.Tree.Iterate (List_File_Iterator'Access);
    end if;
    -- Sort this list
    -- @@@
    -- Put entries
    Ulist.Rewind;
    loop
      Ulist.Read_Next (Str, Moved);
      Basic_Proc.Put_Line_Output (Asu_Ts (Str));
      exit when not Moved;
    end loop;
  end Put_List;


  -----------------
  -- REVERT LIST --
  -----------------
  -- Put revert list of units or files
  procedure Put_Revert_List (Unit : in Sourcer.Src_Dscr;
                             Units : in Boolean) is
  begin
    -- @@@
    null;
  end Put_Revert_List;

end Output;

