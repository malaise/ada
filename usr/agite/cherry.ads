package Cherry is

  -- Handle the selection of Commits to cherry-pick
  function Pick (Root, Branch : String; Interactive : Boolean) return Boolean;

end Cherry;

