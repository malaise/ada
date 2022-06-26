package Tree is

  -- List the history tree of a file or dir along a given branch
  -- Optionnaly set current entry to given Hash
  -- If the Hash is set, then Prio_Hash indicates if the default item
  --  shall be this hash instead of the head of the remote branch
  procedure List (Root, Branch, Path, Name : in String;
                  Is_File : in Boolean);

end Tree;

