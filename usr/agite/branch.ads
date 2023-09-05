with As.U, Long_Long_Limited_List;
package Branch is

  -- List of branches
  -- Keep for each branch its name and wether it is remote
  -- If yes: local name and does this local branch exist
  type Branch_Rec_Type is record
    Name : As.U.Asu_Us;
    Is_Remote : Boolean := False;
    Local : As.U.Asu_Us;
    Has_Local : Boolean := False;
  end record;
  procedure Set (To : out Branch_Rec_Type; Val : in Branch_Rec_Type);
  package Branches_Mng is new Long_Long_Limited_List (Branch_Rec_Type, Set);
  -- List all the branches
  procedure List_Branches (List : in out Branches_Mng.List_Type);


  -- Handle the branches
  procedure Handle (Root : in String);

  -- Interactively rebase current branch from rev
  -- Return OK => Completed
  --        Error => Error not recovered by user (e.g. user abort)
  --     or Cancelled => Cancelled by user before starting, or nothing to do
  -- Red warning if current commit has been pushed
  type Result_List is (Ok, Error, Cancelled);
  function Reorg (Root, Rev : String; Pushed : Boolean) return Result_List;

end Branch;

