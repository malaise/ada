package Details is
  -- Show the details of a commit
  -- Or of a Tag and associated commit
  procedure Handle (Root : in String;
                    Rev_Tag : in String;
                    Tag_Date, Tag_Comment : in String := "");
end Details;

