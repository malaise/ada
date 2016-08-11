with Git_If;
package History is

  -- List the history of a file or dir
  -- Optionnaly set current entry to given Hash
  procedure List (Root, Path, Name : in String;
                  Is_File : in Boolean;
                  Allow_Modif : in Boolean;
                  Allow_Tag : in Boolean;
                  Hash : in Git_If.Git_Hash := Git_If.No_Hash);

end History;

