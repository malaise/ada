-- If file is / then restore all the files of the list,
--   otherwise restore the file
-- Reject restoring if a/the file is locally modified
-- Otherwise confirm and restore
-- Return True if Ok
with Git_If, Error, Confirm;
function Restore (Root, File, Hash : in String;
                  Commits : access Git_If.Commit_List) return Boolean is
  Commit : Git_If.Commit_Entry_Rec;
  Modified : Boolean;
begin

  -- See if a/the file is locally modified
  if File = "/" then
    Modified := False;
    -- Iterate on all the list except first
    Commits.Rewind;
    while Commits.Check_Move loop
      Commits.Move_To;
      Commits.Read (Commit, Git_If.Commit_File_Mng.Current);
      Modified := Modified
          or else Git_If.Is_Modified (Root & Commit.File.Image);
    end loop;
  else
    Modified := Git_If.Is_Modified (Root & File);
  end if;

  -- Reject if file is modified
  if Modified then
    if File = "/" then
      Error ("Restoring",
             "Full commit",
             "At least one file is locally modified, revert it first");
    else
      Error ("Restoring", File, "File is locally modified, revert it first");
     end if;
    return False;
  end if;

  -- Confirm file restoration
  if Confirm ("Restore",
              (if File = "/"  then "Full commit" else File)) then
    if File = "/" then
      Commits.Rewind;
      while Commits.Check_Move loop
        Commits.Move_To;
        Commits.Read (Commit, Git_If.Commit_File_Mng.Current);
        Modified := Git_If.Cat (Commit.File.Image, Hash,
                                Root & Commit.File.Image);
      end loop;
    else
      Modified := Git_If.Cat (File, Hash, Root & File);
    end if;
    return True;
  else
    return False;
  end if;
end Restore;

