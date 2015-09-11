with Git_If;
package Commit is

  -- Handle the commit of modifications
  procedure Handle (Root : in String);

  -- Handle a commit of modifications within a loop (of cherry-pick)
  -- Show button Quit instead of Push
  -- Allow modifications of content or only of comment
  -- Forbid, allow commit or required it (no more staged)
  type Commit_Allow_List is (Forbid, Allow, Require);
  -- Init comment from the one of the provided Hash
  function Handle (Root : String;
                   Hash_For_Comment : Git_If.Git_Hash;
                   Allow_Modif : Boolean := True;
                   Allow_Commit : Commit_Allow_List := Allow) return Boolean;

  -- Get comment of a commit or comment previously entered
  function Get_Comment (Hash : Git_If.Git_Hash) return String;
end Commit;

