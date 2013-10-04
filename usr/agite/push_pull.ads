package Push_Pull is

  -- Handle the push of commits to a remote
  -- Or pull a remote branch
  -- Return true if OK (false if "Back")
  function Handle (Root : String; Pull : Boolean) return Boolean;

end Push_Pull;

