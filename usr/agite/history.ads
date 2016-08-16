with Git_If;
package History is

  -- Kind of object listed
  type Kind_List is (File, Dir, Br);

  -- List the history of a file or dir
  -- Optionnaly set current entry to given Hash
  procedure List (Root, Path, Name : in String;
                  Kind : in Kind_List;
                  Allow_Modif : in Boolean;
                  Allow_Tag : in Boolean;
                  Hash : in Git_If.Git_Hash := Git_If.No_Hash);

end History;

