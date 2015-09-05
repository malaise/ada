package Cherry is

  -- Handle the selection of Commits to cherry-pick
  function Pick (Root, Branch : String) return Boolean;

end Cherry;

