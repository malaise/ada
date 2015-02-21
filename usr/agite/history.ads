with Git_If;
package History is

  -- List the history of a file or dir
  -- Optionnaly set current entry to given Hash
  procedure List (Root, Path, Name : in String;
                    Is_File : in Boolean;
                    Hash : in Git_If.Git_Hash := Git_If.No_Hash);

  -- Handle the selection of Commits to cherry-pick
  function Cherry_Pick (Root, Branch : String) return Boolean;

end History;

