package Reflog is

  -- Handle the Reflog for a given branch
  -- Return True if some deep changes performed
  function Handle (Root : String; Branch : String) return Boolean;

end Reflog;

