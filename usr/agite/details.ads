package Details is
  -- Show the details of a commit
  -- Allow_Modif (Restore, also passed to Hist) or not
  -- Allow_Tad (passed to Hist) or not
  -- Or of a Tag and associated commit
  procedure Handle (Root : in String;
                    Rev_Tag : in String;
                    Allow_Modif : in Boolean;
                    Allow_Tag : in Boolean;
                    Tag_Date, Tag_Comment : in String := "");
end Details;

