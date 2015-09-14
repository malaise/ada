package Branch is

  -- Handle the branches
  procedure Handle (Root : in String);

  -- Interactively rebase current branch from rev
  function Reorg (Root, Rev : String) return Boolean;

end Branch;

