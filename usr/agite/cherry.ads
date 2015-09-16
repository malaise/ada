package Cherry is

  -- Handle the selection of Commits to cherry-pick
  -- List the cherries between Branch and Reference (default Ref is current)
  -- If Interactive, allow modification of this list
  -- Apply the cherries on current branch
  -- Return OK => Completed
  --        Error => Error not recovered by user (e.g. user quit)
  --     or Cancelled (cancelled by user before starting, or nothing to do)
  type Result_List is (Ok, Error, Cancelled);
  function Pick (Root, Branch, Reference : String;
                 Interactive : Boolean) return Result_List;

  -- Reset memory of previous uncompleted cherry-pick
  procedure Reset;

end Cherry;

