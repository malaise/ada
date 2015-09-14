package Cherry is

  -- Handle the selection of Commits to cherry-pick
  -- Return OK => Completed
  --        Error => Error not recovered by user (e.g. user quit)
  --     or Cancelled (cancelled by user before starting, or nothing to do)
  type Result_List is (Ok, Error, Cancelled);
  function Pick (Root, Branch : String;
                 Interactive : Boolean) return Result_List;

  -- Reset memory of previous uncompleted cherry-pick
  procedure Reset;

end Cherry;

