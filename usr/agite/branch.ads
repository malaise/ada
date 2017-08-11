with As.U, Dynamic_List;
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
  package Branches_Mng is new Dynamic_List (Branch_Rec_Type);
  -- List all the branches
  procedure List_Branches (List : in out Branches_Mng.Dyn_List.List_Type);


  -- Handle the branches
  procedure Handle (Root : in String);

  -- Interactively rebase current branch from rev
  -- Return OK => Completed
  --        Error => Error not recovered by user (e.g. user abort)
  --     or Cancelled => Cancelled by user before starting, or nothing to do
  type Result_List is (Ok, Error, Cancelled);
  function Reorg (Root, Rev : String) return Result_List;

end Branch;

