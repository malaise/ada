-- If file is / then restore all the files of the list,
--   otherwise restore the file
-- Reject restoring if a/the file if it is locally modified
-- Otherwise confirm and restore
with Afpx;
with Git_If, Error, Confirm;
procedure Restore (Root, File, Hash : in String;
                   Commits : access Git_If.Commit_List) is
  Commit : Git_If.Commit_Entry_Rec;
  Modified : Boolean;
begin

  -- See if a/the file is locally modified
  Afpx.Suspend;
  if File = "/" then
    Modified := False;
    -- Iterate on all the list except first
    Commits.Rewind;
    loop
      exit when not Commits.Check_Move;
      Commits.Move_To;
      Commits.Read (Commit);
      Modified := Modified
          or else Git_If.Is_Modified (Root & Commit.File.Image);
    end loop;
  else
    Modified := Git_If.Is_Modified (Root & File);
  end if;
  Afpx.Resume;

  -- Reject if file is modified
  if Modified then
    if File = "/" then
      Error ("Restoring",
             "Full commit",
             "At least one file is locally modified, revert it first");
    else
      Error ("Restoring", File, "File is locally modified, revert it first");
     end if;
    return;
  end if;

  -- Confirm file restoration
  if Confirm ("Ready to restore:",
              (if File = "/"  then "Full commit" else File)) then
    Afpx.Suspend;
    if File = "/" then
      loop
        exit when not Commits.Check_Move;
        Commits.Move_To;
        Commits.Read (Commit);
        Modified := Git_If.Cat (Commit.File.Image, Hash,
                                Root & Commit.File.Image);
      end loop;
    else
      Modified := Git_If.Cat (File, Hash, Root & File);
    end if;
    Afpx.Resume;
  end if;

end Restore;

