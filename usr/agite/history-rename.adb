-- Read history of a file, following renames
separate (History)
function Rename (Branch, Root, Path : in String;
                 Max : in Ll_Natural;
                 Log : in out Git_If.Log_List;
                 End_Reached : out Boolean) return Ll_Natural is
  -- Intermediate list
  Llog : Git_If.Log_List;
  -- Current file name
  File : As.U.Asu_Us;
  -- Number of allowed remaining entries
  Remain : Ll_Natural := 0;
  -- Commit details
  Hash : Git_If.Git_Hash;
  Merged : Boolean;
  Date : Git_If.Iso_Date;
  Comment : Git_If.Comment_Array(1 .. 0);
  Commits : aliased Git_If.Commit_List;
  Commit : Git_If.Commit_Entry_Rec;
  Found, Moved : Boolean;
  -- File status in the commit
  Status : Character;
  -- First life
  First_Index : Ll_Natural;

  use type As.U.Asu_Us, Ll_Natural;
begin
  Log.Delete_List;
  First_Index := 0;
  File.Set (Path);
  -- Loop for each commit
  loop
    -- Compute allowed remains entres
    if Max /= 0 then
      if Log.List_Length < Max then
        Remain := Max - Log.List_Length;
        if First_Index /= 0 then
          -- We will remove the first (dup) entry
          Remain := Remain + 1;
        end if;
      else
        -- Already reached Max entries
        exit;
      end if;
    end if;
    -- Log (No Sparse otherwise we get the whole repository)
    Git_If.List_Log (Branch, Root & File.Image, Hash.Image, Remain,
                     False, True, Llog, End_Reached);
    exit when Llog.Is_Empty;

    -- If this is not the first life of the file, then the first entry
    --  is a dup of the last entry of previous life
    if First_Index /= 0 then
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
      Status := Llog.Access_Current.Extra.Element (1);
      Llog.Access_Current.Extra := File;
      if Status = 'A' then
        -- Clean the tail
        while Llog.Get_Position /= Llog.List_Length loop
          Llog.Move_To;
          Llog.Delete (Git_If.Log_Mng.Prev);
        end loop;
        exit;
      end if;
      exit when Llog.Get_Position = Llog.List_Length;
      Llog.Move_To;
    end loop;

    -- Append this (new) history
    Log.Insert_Copy (Llog);

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
    if First_Index = 0 then
      First_Index := Log.Get_Position;
    end if;

  end loop;
  Log.Rewind;
  return First_Index;
end Rename;

