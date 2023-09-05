with Git_If;
package History is

  -- List the history of a file or dir along a given branch
  -- Optionnaly set current entry to given Hash
  -- If the Hash is set, then the current item shall be this hash if possible
  procedure List (Root, Branch, Path, Name : in String;
                  Is_File : in Boolean;
                  Allow_Modif : in Boolean;
                  Allow_Tag : in Boolean;
                  Hash : in Git_If.Git_Hash := Git_If.No_Hash);

end History;

