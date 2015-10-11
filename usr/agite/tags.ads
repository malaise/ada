with Git_If;
package Tags is

  -- Search and list tags (handles checkout and deletion of selected tag)
  procedure List (Root : in String);

  -- Add a tag on a commit
  procedure Add (Root : in String; Rev : in Git_If.Git_Hash);

end Tags;

