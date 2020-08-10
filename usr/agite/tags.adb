with As.U;
package body Tags is

  -- Search and list tags matching Template
  --  (handles checkout and deletion of selected tag)
  -- The template is kept in mind from one invocatiopn to the other
  Template : As.U.Asu_Us;
  procedure List (Root : in String) is separate;

  -- Add a tag on a commit
  procedure Add (Rev : in Git_If.Git_Hash) is separate;

end Tags;

