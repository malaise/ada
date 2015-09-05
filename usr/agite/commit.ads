with Git_If;
package Commit is

  -- Handle the commit of modifications
  procedure Handle (Root : in String);

  -- Handle a commit of modifications within a loop (of cherry-pick)
  -- Show button Quit instead of Push
  -- Init comment from the one of the provided Hash
  function Handle (Root : String;
                   Hash_For_Comment : Git_If.Git_Hash) return Boolean;

end Commit;

