package body Tags is

  -- Search and list tags (handles checkout and deletion of selected tag)
  procedure List (Root : in String) is separate;

  -- Add a tag on a commit
  procedure Add (Rev : in Git_If.Git_Hash) is separate;

end Tags;

