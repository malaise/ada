package Details is
  -- Show the details of a commit
  -- Allow_Modif (Hist and Restore) or not
  -- Or of a Tag and associated commit
  procedure Handle (Root : in String;
                    Rev_Tag : in String;
                    Allow_Modif : in Boolean;
                    Tag_Date, Tag_Comment : in String := "");
end Details;

