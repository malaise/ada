with As.U.Utils, Basic_Proc, Directory;
with Debug, Sourcer, Tree_Mng, Sort;
package body Output is

  -- Are we in revert mode. Tree iterators need it
  Revert : Boolean := False;

  -- Current directory
  Curr_Dir : As.U.Asu_Us;

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

  -- Dump Units of tree
  Level : Natural := 0;
  procedure Tree_Unit_Iterator (Parent : in Sourcer.Src_Dscr) is
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
            Name := Sourcer.Get_Body (Dscr.Dscr).Unit;
          else
            Name := Dscr.Dscr.Unit;
          end if;
        end if;
      else
        Name := Dscr.Dscr.Unit;
      end if;
      if not Name.Is_Null then
        for I in 1 .. Level - 1 loop
          Str.Append ("  ");
        end loop;
        Str.Append (Strip (Directory.Build_File_Name (
            Dscr.Dscr.Path.Image, Name.Image, "")));
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
      Tree_Unit_Iterator (Dscr.Dscr);
    end loop;
    -- Move back to current in tree
    if Nb_Children /= 0 then
      Tree_Mng.Tree.Move_Father;
    end if;

    -- Restore initial level
    if Incr then
      Level := Level - 1;
    end if;

  end Tree_Unit_Iterator;

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
    for I in 1 .. Level loop
      Str.Append ("  ");
    end loop;
    -- File
    Str.Append (Strip (Directory.Build_File_Name (
            Dscr.Dscr.Path.Image, Dscr.Dscr.File.Image, "")));
    Basic_Proc.Put_Line_Output (Str.Image);
    return True;
  end Tree_File_Iterator;

  -- Put tree of units or files
  procedure Put_Tree (File_Mode : in Boolean) is
    Dscr : Sourcer.Src_Dscr;
  begin
    if File_Mode then
      Tree_Mng.Tree.Iterate (Tree_File_Iterator'Access, False);
    else
      Tree_Unit_Iterator (Dscr);
    end if;
  end Put_Tree;

  ----------
  -- LIST --
  ----------
  -- Unique list of entries (units or files)
  Ulist : As.U.Utils.Asu_Unique_List_Mng.Unique_List_Type;
  -- Dynamic list of sorted entries (units or files)
  Dlist : As.U.Utils.Asu_Dyn_List_Mng.List_Type;
  -- List Units of tree
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
        Name := Sourcer.Get_Body (Dscr.Dscr).Unit;
      else
        Name := Dscr.Dscr.Unit;
      end if;
    end if;
    -- PathOfFile / UnitName
    Ulist.Insert (As.U.Tus (Directory.Build_File_Name (
            Dscr.Dscr.Path.Image, Name.Image, "")));
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
    Ulist.Insert (As.U.Tus (Directory.Build_File_Name (
              Dscr.Dscr.Path.Image, Dscr.Dscr.File.Image, "")));
    return True;
  end List_File_Iterator;

  -- Put list of units or files
  procedure Put_List (File_Mode : in Boolean) is
    Str : As.U.Asu_Us;
    Moved : Boolean;
  begin
    -- Build unique list of entries
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Scanning tree");
    end if;
    if File_Mode then
      Tree_Mng.Tree.Iterate (List_File_Iterator'Access);
    else
      Tree_Mng.Tree.Iterate (List_Unit_Iterator'Access);
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
      Put_Line_Stripped (Str.Image);
      exit when not Moved;
    end loop;
  end Put_List;

  -- Put list/tree, normal/revert of units/files
  procedure Put (Tree_Mode, Revert_Mode, File_Mode : in Boolean) is
  begin
    Directory.Get_Current (Curr_Dir);
    if Curr_Dir.Image /= "/" then
      Curr_Dir.Append ("/");
    end if;
    Revert := Revert_Mode;
    if Tree_Mode then
     Put_Tree (File_Mode);
    else
     Put_List (File_Mode);
    end if;
  end Put;

end Output;

