package Cherry is

  -- Handle the selection of Commits to cherry-pick
  -- If Interactive: return True if OK or Error => back to Directory
  --    return False if nothing to do or cancel => back to Branches
  -- Otherwise, return True if completed OK
  function Pick (Root, Branch : String; Interactive : Boolean) return Boolean;

  -- Reset memory of previous uncompleted cherry-pick
  procedure Reset;

end Cherry;

