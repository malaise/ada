separate (Mng)
package body Deletion is

  -- Number of deleted operations
  Nb_Deleted : Natural := 0;

  -- Flag currently selected operation as deleted
  procedure Flag_Deleted is
    Sel : Sel_Rec;
  begin
    -- Flag selected operation as deleted
    Sel_List.Read(Sel, Sel_List_Mng.Current);
    if not Sel.Deleted then
      Sel.Deleted := True;
      Sel_List.Modify(Sel, Sel_List_Mng.Current);
      Nb_Deleted := Nb_Deleted + 1;
    end if;
  end Flag_Deleted;

  -- Flag currently selected operation as not deleted
  procedure Flag_Undeleted is
    Sel : Sel_Rec;
  begin
    -- Flag selected operation as deleted
    Sel_List.Read(Sel, Sel_List_Mng.Current);
    if Sel.Deleted then
      Sel.Deleted := False;
      Sel_List.Modify(Sel, Sel_List_Mng.Current);
      Nb_Deleted := Nb_Deleted - 1;
    end if;
  end Flag_Undeleted;

  -- Get number of flagged operations
  function Get_Nb_Deleted return Oper_Nb_Range is
  begin
    return Nb_Deleted;
  end Get_Nb_Deleted;

  -- Delete all flagged operation
  procedure Commit_Deletions is
    Sel : Sel_Rec;
    Moved : Boolean;
  begin
    if Nb_Deleted = 0 then
      return;
    end if;

    -- Commit deletions from last to first (so REFs remain correct)
    Sel_List.Rewind(Sel_List_Mng.Prev);
    loop
      Sel_List.Read(Sel, Sel_List_Mng.Current);
      if Sel.Deleted then
        -- Remove current operation
        List_Util.Move_To_Current;
        Oper_List_Mng.Delete(Oper_List, Done => Moved);
        -- Remove current selection, moving to Prev
        Sel_List.Delete(Sel_List_Mng.Prev, Moved);
        -- Update counter
        Nb_Deleted := Nb_Deleted - 1;
        -- Either first item of selection is deleted here
        --  or previous selection records are unchanged
        exit when Nb_Deleted = 0;
      else
        -- Update selection with accurate oper no
        Sel.No := Sel.No - Nb_Deleted;
        Sel_List.Modify(Sel, Sel_List_Mng.Current);
        -- Move to previous selection
        Sel_List.Move_To(Sel_List_Mng.Prev);
      end if;
    end loop;
  end Commit_Deletions;

  -- Cancel all flagged operation
  procedure Cancel_Deletions is
    Sel : Sel_Rec;
  begin
    if Nb_Deleted = 0 then
      return;
    end if;

    Sel_List.Rewind;
    loop
      Sel_List.Read(Sel, Sel_List_Mng.Current);
      if Sel.Deleted then
        -- Restore flag
        Sel.Deleted := False;
        Sel_List.Modify (Sel, Sel_List_Mng.Current);
        -- Update counter
        Nb_Deleted := Nb_Deleted - 1;
        exit when Nb_Deleted = 0;
      end if;
      -- Move to previous selection
      Sel_List.Move_To(Sel_List_Mng.Prev);
    end loop;
  end Cancel_Deletions;

end Deletion;

