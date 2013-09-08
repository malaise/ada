-- Reject restoring a file if it is locally modified
-- Otherwise confirm and restore
with Afpx;
with Git_If, Error, Confirm;
procedure Restore (Root, File, Hash : in String) is
  Modified : Boolean;
begin

  -- See if file is locally modified
  Afpx.Suspend;
  Modified := Git_If.Is_Modified (Root & File);
  Afpx.Resume;

  -- Reject if file is modified
  if Modified then
    Error ("Restoring", File, "File is locally modified, revert it first");
    return;
  end if;

  -- Confirm file restoration
  if Confirm ("Ready to restore:", File) then
    Afpx.Suspend;
    Modified := Git_If.Cat (File, Hash, Root & File);
    Afpx.Resume;
  end if;

end Restore;
