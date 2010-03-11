with Git_If;
package History is

  -- Handle the history of a file or dir
  -- Optionnaly set current entry to given Hash
  procedure Handle (Root, Path, Name : in String;
                    Is_File : in Boolean;
                    Hash : in Git_If.Git_Hash := Git_If.No_Hash);

end History;

