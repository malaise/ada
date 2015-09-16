separate (Branch)
package body Rebase_Mng is

  -- Memory of previous rebase
  Rebased, Target, Reference, Temporary : As.U.Asu_Us;


  -- Find a unused Tmp branch name
  function Find_Tmp_Name return String is
    Suffix : constant String := "Tmp";
    Pos : Positive;
    Name : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    Git_If.List_Branches (Local => True, Branches => Branches);
    if Branches.Is_Empty then
      return Suffix;
    end if;

    -- Save Pos
    Pos := Branches.Get_Position;
    -- Find longest branch
    Branches.Rewind;
    loop
      if Branches.Access_Current.Length > Name.Length then
        Branches.Read (Name, Git_If.Branches_Mng.Current);
      end if;
      exit when not Branches.Check_Move;
      Branches.Move_To;
    end loop;
    -- Restore Pos
    Branches.Move_At (Pos);

    -- Done
    return Name.Image & "." & Suffix;
  end Find_Tmp_Name;

  -- Rebase current branch to head of Reference
  function Do_Rebase (Root : String;
                      Target_Branch, Reference_Branch: String;
                      Interactive : Boolean) return String is
    Restart : Boolean;
    Result : As.U.Asu_Us;
    Tmp_Branch : As.U.Asu_Us;
    use type As.U.Asu_Us;
    function Do_Confirm return Boolean is
    begin
      return Confirm (
        (if Restart then "Continue the rebase" else "Rebase")
        & " of branch " & Current_Branch.Image
        & (if Reference_Branch /= "" then " from " & Reference_Branch else ""),
        "on " & Target_Branch);
    end Do_Confirm;
  begin
    Current_Branch := As.U.Tus (Git_If.Current_Branch);
    -- Check if same rebase as previous
    Restart := Current_Branch = Rebased
               and then Target_Branch = Target.Image
               and then Reference_Branch = Reference.Image;

    -- Confirm restart
    if Restart then
      Restart := Do_Confirm;
      if not Restart then
        -- User cancelled restart: clean Tmp branch
        Result := As.U.Tus (Git_If.Delete_Branch (Temporary.Image));
      end if;
    end if;

    if not Restart then
      -- Confirm new rebase
      if not Do_Confirm then
        -- User cancelled new rebase
        return "";
      end if;
    end if;

    if Restart then
      -- Reuse previous Tmp branch
      Tmp_Branch := Temporary;
      Result := As.U.Tus (Git_If.Do_Checkout (Tmp_Branch.Image, ""));
      if not Result.Is_Null then
        return "Cannot checkout tmp branch " & Tmp_Branch.Image & ": "
               & Result.Image;
      end if;
    else
      Reset (True);
      -- Find Tmp branch name
      Tmp_Branch := As.U.Tus (Find_Tmp_Name);
      -- Create a Tmp branch from Target
      Result := As.U.Tus (Git_If.Do_Checkout (Target_Branch,
                                              Tmp_Branch.Image));
      if not Result.Is_Null then
        return "Cannot create tmp branch " & Tmp_Branch.Image
               & " from " & Target_Branch & ": "
               & Result.Image;
      end if;
    end if;

    -- Store current rebasing branches
    Rebased := Current_Branch;
    Target := As.U.Tus (Target_Branch);
    Reference := As.U.Tus (Reference_Branch);
    Temporary := Tmp_Branch;
    -- Cherry pick all the cherries between the Rebased (initally current)
    --  branch and the Reference, into current (Tmp)
    -- Interactively if requested or restart
    case Cherry.Pick (Root, Current_Branch.Image, Reference_Branch,
                      Interactive or else Restart) is
      when Cherry.Ok =>
        -- Done, wipe memory and continue
        Reset (True);
      when Cherry.Error =>
        -- Error already handled
        return "";
      when Cherry.Cancelled =>
        -- Switch back to original branch and remove Tmp branch
        Reset (True);
        Result := As.U.Tus (Git_If.Do_Checkout (Current_Branch.Image, ""));
        if not Result.Is_Null then
          return "Cannot checkout back rebased branch "
                 & Current_Branch.Image & ": " & Result.Image;
        end if;
        Result := As.U.Tus (Git_If.Delete_Branch (Tmp_Branch.Image));
        if not Result.Is_Null then
          return "Cannot delete tmp branch " & Tmp_Branch.Image & ": "
                 & Result.Image;
        end if;
        return "";
    end case;

    -- Hard reset the Rebased branch to Tmp branch
    Result := As.U.Tus (Git_If.Do_Checkout (Current_Branch.Image, ""));
    if not Result.Is_Null then
      return "Cannot checkout back rebased branch " & Current_Branch.Image
             & ": " & Result.Image;
    end if;
    Git_If.Do_Reset_Hard (Tmp_Branch.Image);

    -- Delete the Tmp branch
    Result := As.U.Tus (Git_If.Delete_Branch (Tmp_Branch.Image));
    if not Result.Is_Null then
      return "Cannot delete tmp branch " & Tmp_Branch.Image & ": "
             & Result.Image;
    end if;

    -- Done
    return "";
  end Do_Rebase;

  procedure Reset (Cherries : in Boolean) is
  begin
    Rebased.Set_Null;
    Target.Set_Null;
    Reference.Set_Null;
    Temporary.Set_Null;
    if Cherries then
      Cherry.Reset;
    end if;
  end Reset;

end Rebase_Mng;

