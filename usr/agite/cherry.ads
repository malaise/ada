package Cherry is

  -- Handle the selection of Commits to cherry-pick
  -- List the cherries between Branch and Reference (default Ref is current)
  type Cherry_Mode is (Automatic, Interactive, Interactive_Tmp);
  -- If Interactive, allow modification of this list, showing the target
  -- branch name or Tmp
  -- Apply the cherries on current branch
  -- Return OK => Completed
  --        Error => Error not recovered by user (e.g. user abort)
  --     or Cancelled => Cancelled by user before starting, or nothing to do
  type Result_List is (Ok, Error, Cancelled);
  function Pick (Root, Branch, Reference : String;
                 Mode : Cherry_Mode) return Result_List;

  -- Reset memory of previous uncompleted cherry-pick
  procedure Reset;

end Cherry;

