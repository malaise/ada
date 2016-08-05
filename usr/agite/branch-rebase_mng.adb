separate (Branch)
package body Rebase_Mng is

  -- Memory of previous rebase
  Rebased, Target, Reference, Temporary : As.U.Asu_Us;

  -- Search a branch
  function Branch_Search is new Git_If.Branches_Mng.Search (As.U."=");

  -- Find a unused Tmp branch name
  function Find_Tmp_Name (From : in String) return String is
    Branches : Git_If.Branches_Mng.List_Type;
    Suffix : constant String := ".Tmp";
    Name : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    -- List branches
    Git_If.List_Branches (Local => True, Remote => False, Branches => Branches);

    -- Try "{Agite}@From.Tmp" and concat ".Tmp" as long as it already exists
    Branches.Rewind;
    Name := As.U.Tus ("{Agite}@" & From  & Suffix);
    while Branch_Search (Branches, Name, From => Git_If.Branches_Mng.Absolute)
    loop
      Name.Append (Suffix);
    end loop;

    -- Done
    return Name.Image;
  end Find_Tmp_Name;

  -- Check if a branch exists
  function Exists (Branch : As.U.Asu_Us) return Boolean is
    Branches : Git_If.Branches_Mng.List_Type;
  begin
    -- List branches
    Git_If.List_Branches (Local => True, Remote => False, Branches => Branches);
    return Branch_Search (Branches, Branch,
                          From => Git_If.Branches_Mng.Absolute);
  end Exists;

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
        "on " & Target_Branch,
        (if Restart then "" else "This operation will alter the history"));
    end Do_Confirm;
  begin
    Current_Branch := As.U.Tus (Git_If.Current_Branch);
    -- Check if same rebase as previous and temporary branch still here
    Restart := Current_Branch = Rebased
               and then Target_Branch = Target.Image
               and then Reference_Branch = Reference.Image
               and then Exists (Temporary);

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
      -- Find Tmp branch name (build it from rebased branch name)
      Tmp_Branch := As.U.Tus (Find_Tmp_Name (Current_Branch.Image));
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
    -- Interactively (hiding Tmp branch name) if requested or restart
    case Cherry.Pick (Root, Current_Branch.Image, Reference_Branch,
        (if Interactive or else Restart then Cherry.Interactive_Tmp
         else Cherry.Automatic)) is
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

