separate (History)
-- Read history of a file, following renames
procedure List_Renames (Branch, Root, Path : in String;
                        Max : in Natural;
                        Log : in out Git_If.Log_List;
                        End_Reached : out Boolean) is
  -- Intermediate list
  Llog : Git_If.Log_List;
  -- First life
  First : Boolean;
  -- Current file name
  File : As.U.Asu_Us;
  -- Number of allowed remaining entries
  Remain : Natural := 0;
  -- Commit details
  Hash : Git_If.Git_Hash;
  Merged : Boolean;
  Date : Git_If.Iso_Date;
  Comment : Git_If.Comment_Array(1 .. 0);
  Commits : aliased Git_If.Commit_List;
  Commit : Git_If.Commit_Entry_Rec;
  Found, Moved : Boolean;

  use type As.U.Asu_Us, Git_If.Log_Mng.Ll_Natural;
begin
  Log.Delete_List;
  File.Set (Path);
  First := True;
  -- Loop for each commit
  loop
    -- Compute allowed remains entres
    if Max /= 0 then
      if Git_If.Log_Mng.Ll_Natural (Max) > Log.List_Length then
        Remain := Natural (Git_If.Log_Mng.Ll_Natural (Max) - Log.List_Length);
      else
        -- Already reached Max entries
        exit;
      end if;
    end if;
    -- Log (No Sparse otherwise we get the whole repository)
    Git_If.List_Log (Branch, Root & File.Image, Remain, False, Llog,
                     End_Reached);
    exit when Llog.Is_Empty;

    -- If this not the first life of the file, then the first entry
    --  is a dup of the last entry of previous life
    if not First then
      Llog.Rewind;
      Llog.Delete (Moved => Moved);
      exit when Llog.Is_Empty;
    end if;

    -- Reset Merged tag from the commits
    -- Set File in Extra field of the commit
    Llog.Rewind;
    loop
      if Llog.Access_Current.Merged then
        Llog.Access_Current.Merged := False;
      end if;
      Llog.Access_Current.Extra := File;
      exit when Llog.Get_Position = Llog.List_Length;
      Llog.Move_To;
    end loop;

    -- Append this (new) history
    Log.Insert_Copy (Llog);

    -- No chance to find a rename if we didn't reach the origin of the file
    exit when not End_Reached;

    -- Look for renaming to current file in this commit
    Git_If.List_Commit (Log.Access_Current.Hash.Image, Hash, Merged, Date,
                        Comment, Commits);
    exit when Commits.Is_Empty;
    Commits.Rewind;
    -- Search '+' File
    Found := False;
    loop
      Commits.Read (Commit, Git_If.Commit_File_Mng.Current);
      if Commit.Status = '+'
      and then Commit.File = File then
        -- A '+' is always preceeded by a 'R', read its file name
        Commits.Move_To (Git_If.Commit_File_Mng.Prev);
        File := Commits.Access_Current.File;
        Found := True;
        exit;
      end if;
      exit when not Commits.Check_Move;
      Commits.Move_To;
    end loop;
    exit when not Found;
    -- Flag this commit as renaming the file
    Log.Access_Current.Merged := True;
    First := False;

  end loop;
  Log.Rewind;
end List_Renames;

