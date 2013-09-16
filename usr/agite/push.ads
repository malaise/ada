package Push is

  -- Handle the push of commits to a remote
  -- Or pull from remote
  -- Return true if OK (false if "Back")
  function Handle (Root : String; Pull : Boolean) return Boolean;

end Push;

