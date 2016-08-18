with Ada.Exceptions;
with Directory, Afpx.Utils, Basic_Proc, Unicode, Str_Util;
with Git_If, Utils.X, Afpx_Xref, Confirm, Error, History, Cherry, Reset, Commit;
package body Branch is

  -- List width
  List_Width : Afpx.Width_Range;

  -- Search a branch
  function Match (Current, Criteria : As.U.Asu_Us) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current = Criteria;
  end Match;
  function Search is new Git_If.Branches_Mng.Search (Match);

  -- Separator in branch names
  Sep : constant String := Git_If.Separator & "";

  -- Root path and current branch
  Root : As.U.Asu_Us;
  Current_Branch : As.U.Asu_Us;

  -- The branch previously selected (when moving back from a sub-menu or
  --  when being re-called
  Previous_Branch : As.U.Asu_Us;

  -- The local branches and the list of all branches
  Locals : Git_If.Branches_Mng.List_Type;
  Branches : Branches_Mng.Dyn_List.List_Type;

  -- Init Afpx list from Branches
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Branch_Rec_Type) is
  begin
    Afpx.Utils.Encode_Line ("", From.Name.Image, "", List_Width, Line);
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised in branches on " & From.Name.Image);
  end Set;

  function Match (Current, Criteria : Afpx.Line_Rec) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return Current.Str(4 .. Current.Len) = Criteria.Str(4 .. Criteria.Len);
  end Match;
  function Search is new Afpx.Line_List_Mng.Search (Match);

  procedure Init_List is new Afpx.Utils.Init_List (
    Branch_Rec_Type, Branches_Mng, Set, False);
  -- Afpx Ptg stuff
  Get_Handle : Afpx.Get_Handle_Rec;

  -- Init screen
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Branches.Dscr_Num);
    -- Encode Root
    Utils.X.Encode_Field (Root.Image, Afpx_Xref.Branches.Root);
    -- Reset Ptg stuff
    Get_Handle := (others => <>);
  end Init;

  -- List the branches: result of listing of remotes and locals, skipping HEADs
  --  and stripping "remotes/"
  -- Keep for each branch its name and wether it is remote
  -- If yes: local name and does this local branch exist
  procedure List_Branches (List : in out Branches_Mng.Dyn_List.List_Type) is
    Alls : Git_If.Branches_Mng.List_Type;
    Branch : As.U.Asu_Us;
    Branch_Rec : Branch_Rec_Type;
    Sep_Index : Natural;
  begin
    -- Get the list of local and remote branches
    Git_If.List_Branches (Local => True, Remote => True, Branches => Alls);
    -- Filter out entry [Xxx/]"HEAD"
    --  and filter out leading "remotes/" from entries
    List.Delete_List;
    if not Alls.Is_Empty then
      Alls.Rewind;
      loop
        Alls.Read (Branch, Git_If.Branches_Mng.Current);
        -- Last slash
        Sep_Index := Str_Util.Locate (Branch.Image, Sep, Forward => False);
        if Branch.Slice (Sep_Index + 1, Branch.Length) /= "HEAD" then
          -- Else skip this branch
          -- Fill record: First slash
          Sep_Index := Str_Util.Locate (Branch.Image, Sep, Forward => True);
          if Branch.Slice (1, Sep_Index) = "remotes/" then
            -- This is a remote branch: remove leading "remotes/"
            Branch_Rec.Is_Remote := True;
            Branch.Delete (1, Sep_Index);
            -- Last slash
            Sep_Index := Str_Util.Locate (Branch.Image, Sep, Forward => False);
            if Sep_Index = 0 then
              -- No name of remote reference???
              Branch_Rec.Local := Branch;
            else
              -- Tail after last slash
              Branch_Rec.Local := Branch.Uslice (Sep_Index + 1, Branch.Length);
            end if;
            Branch_Rec.Has_Local :=
                Search (Locals, Branch_Rec.Local,
                        From => Git_If.Branches_Mng.Absolute);
          else
            Branch_Rec := (others => <>);
          end if;
          Branch_Rec.Name := Branch;
          List.Insert (Branch_Rec);
        end if;
        -- Move to next if possible
        if Alls.Check_Move then
          Alls.Move_To;
        else
          exit;
        end if;
      end loop;
    end if;
  end List_Branches;

  -- Re assess the list of branches
  procedure Reread (Restore : in Boolean) is
    Line : Afpx.Line_Rec;
    Branch_Rec : Branch_Rec_Type;
  begin
    -- Encode current branch
    Utils.X.Encode_Branch (Afpx_Xref.Branches.Branch);
    Current_Branch := As.U.Tus (Git_If.Current_Branch);

    -- Save current selection
    if Restore and then not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
    elsif not Previous_Branch.Is_Null then
      -- Set default to previous branch
      Branch_Rec.Name := Previous_Branch;
      Set (Line, Branch_Rec);
    else
      -- Set default to current branch
      Branch_Rec.Name := Current_Branch;
      Set (Line, Branch_Rec);
    end if;

    -- Get the list of local branches
    Git_If.List_Branches (Local => True, Remote => False, Branches => Locals);
    -- Get the list of all branches
    List_Branches (Branches);

    -- Encode the Afpx list
    Init_List (Branches);

    -- Move back to the same entry as before (if possible)
    if not Afpx.Line_List.Is_Empty then
      if Search (Afpx.Line_List, Line,
                 From => Afpx.Line_List_Mng.Absolute) then
        Afpx.Line_List.Move_At (Afpx.Line_List.Get_Position);
      else
        Afpx.Line_List.Rewind;
      end if;
    end if;
    -- Center
    Afpx.Update_List (Afpx.Center_Selected);

    -- Set field activity: only Create
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Checkout,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Merge,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.True_Merge,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Rebase,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Cherry_Pick,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Reset,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Rename,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Delete,
                              Afpx.Line_List.Is_Empty);
  end Reread;

  -- Handle Rebase memory (incuding restart)
  package Rebase_Mng is
    -- (Re) start a rebase, return the error message to display
    function Do_Rebase (Root : String;
                        Target_Branch, Reference_Branch: String;
                        Interactive : Boolean) return String;
    -- Reset memory of previous rebase
    procedure Reset (Cherries : in Boolean);
  end Rebase_Mng;
  package body Rebase_Mng is separate;

  -- Actions on branches
  type Action_List is (Create, Rename, Delete, Checkout, Hist,
                       Merge, True_Merge, Rebase, Cherry_Pick, Reset);
  function Do_Action (Action : in Action_List;
                      Ref : in Natural := 0) return Boolean is
    Sel_Name, New_Name, Ref_Name, Tmp_Name : As.U.Asu_Us;
    Sel_Rec : Branch_Rec_Type;
    Remote : Boolean;
    Comment : As.U.Asu_Us;
    Pos, Tmp_Index : Positive;
    Refi : Natural;
    Message1, Message2, Result : As.U.Asu_Us;
    Done : Boolean;
    use type Cherry.Result_List, As.U.Asu_Us;
  begin
    -- Retrieve current name
    if Action /= Create then
      Branches.Move_At (Afpx.Line_List.Get_Position);
      Sel_Rec := Branches.Access_Current.all;
      Sel_Name := Sel_Rec.Name;
    end if;
    -- Get new name from Get field
    if Action = Create or else Action = Rename then
      Afpx.Decode_Field (Afpx_Xref.Branches.Name, 0, New_Name);
      Afpx.Clear_Field (Afpx_Xref.Branches.Name);
      Afpx.Reset (Get_Handle);
      New_Name := As.U.Tus (Str_Util.Strip (New_Name.Image, Str_Util.Both));
      if New_Name.Is_Null then
        -- Cancel Create/Rename if empty name
        return False;
      end if;
    end if;

    -- Get name of right selection
    -- If no right selection then leave empty for default
    if (Action = Rebase or else Action = Cherry_Pick or else Action = Delete)
    and then Ref /= 0 then
      Pos := Branches.Get_Position;
      Branches.Move_At (Ref);
      Ref_Name := Branches.Access_Current.Name;
      Branches.Move_At (Pos);
    end if;
    if Action = Delete then
      Refi := Ref;
      if Refi /= 0 and then Refi < Pos then
        -- Sort the list of branches to delete
        Tmp_Index := Pos;
        Pos := Refi;
        Refi := Tmp_Index;
        Tmp_Name := Sel_Name;
        Sel_Name := Ref_Name;
        Ref_Name := Tmp_Name;
      end if;
    end if;

    -- Cancel if not confirm
    if Action /= Create and then Action /= Rename
    and then Action /= Rebase and then Action /= Cherry_Pick
    and then Action /= Reset and then Action /= Hist then
      if not Confirm (
          (case Action is
             when Create | Rename | Hist | Rebase | Cherry_Pick | Reset =>
               "???",
             when Delete     =>
               (if Ref = 0 then "Delete branch " & Sel_Name.Image
                else "Delete branches from " & Sel_Name.Image),
             when Checkout   => "Checkout branch " & Sel_Name.Image,
             when Merge      => "Merge branch " & Sel_Name.Image,
             when True_Merge => "True Merge branch " & Sel_Name.Image),
          (case Action is
             when Create | Rename | Hist | Rebase | Cherry_Pick | Reset
                | Checkout => "",
             when Delete =>
               (if Ref = 0 then "" else "to " & Ref_Name.Image),
             when Merge | True_Merge  => "into current branch "
                                         & Current_Branch.Image) )
      then
        Init;
        Reread (True);
        return False;
      else
        Init;
      end if;
    end if;

    -- Git_If
    Message2 := As.U.Asu_Null;
    case Action is
      when Create =>
        Message1 := As.U.Tus ("Creating branch " & New_Name.Image);
        if not New_Name.Is_Null then
          Result := As.U.Tus (Git_If.Create_Branch (New_Name.Image));
          if Result.Is_Null then
            Previous_Branch := New_Name;
          end if;
        end if;
      when Rename =>
        Message1 := As.U.Tus ("Renaming branch " & Sel_Name.Image);
        Message2 := As.U.Tus ("to " & New_Name.Image);
        if not New_Name.Is_Null then
          Result := As.U.Tus (Git_If.Rename_Branch (Sel_Name.Image,
                                                    New_Name.Image));
          if Result.Is_Null then
            Previous_Branch := New_Name;
          end if;
        end if;
      when Delete =>
        if Refi = 0 then
          -- No right selection: delete current branch
          Message1 := As.U.Tus ("Deleting branch " & Sel_Name.Image);
          Remote := Str_Util.Locate (Sel_Name.Image, Sep) /= 0;
          Result := As.U.Tus (Git_If.Delete_Branch (Sel_Name.Image, Remote));
          if Result.Is_Null then
            -- Go to next branch in list if possible
            if Branches.Check_Move then
              Branches.Move_To;
              Previous_Branch := Branches.Access_Current.Name;
            else
              Previous_Branch := Current_Branch;
            end if;
          end if;
        else
          -- Delete branches from Pos to Ref
          for I in Pos .. Refi loop
            Branches.Move_At (I);
            Tmp_Name := Branches.Access_Current.Name;
            Message1 := As.U.Tus ("Deleting branch " & Tmp_Name.Image);
            Remote := Str_Util.Locate (Tmp_Name.Image, Sep) /= 0;
            Result := As.U.Tus (Git_If.Delete_Branch (Tmp_Name.Image, Remote));
            exit when not Result.Is_Null;
          end loop;
        end if;
      when Checkout =>
        -- Checkout branch. If it is a remote tracking then check it out
        --  as local name
        Message1 := As.U.Tus ("Checking out branch " & Sel_Name.Image);
        Result := As.U.Tus (Git_If.Do_Checkout (Sel_Name.Image,
            (if Sel_Rec.Is_Remote then Sel_Rec.Local.Image else "")));
        Previous_Branch := Current_Branch;
      when Merge =>
        Message1 := As.U.Tus ("Merging branch " & Sel_Name.Image);
        Previous_Branch := Sel_Name;
        -- Commit comment
        Comment := As.U.Tus ("Merge branch '" & Sel_Name.Image & "'");
        -- Merge, allow fast forward and commit
        Result := As.U.Tus (
           Git_If.Merge_Branch (Sel_Name.Image, Comment.Image, False, False));
        if not Result.Is_Null then
          -- Set comment for on going manual commit
          Commit.Set_Comment (Comment.Image);
        end if;
      when Hist =>
        -- History of branch. Allow modif and tag if current branch
        History.List (Root.Image, Sel_Name.Image, "", History.Br,
                      Sel_Name = Current_Branch, Sel_Name = Current_Branch);
        Init;
      when True_Merge =>
        Message1 := As.U.Tus ("True merging branch " & Sel_Name.Image);
        Previous_Branch := Sel_Name;
        -- Commit comment
        Comment := As.U.Tus ("Merge branch '" & Sel_Name.Image & "'");
        -- Merge, disallow fast forward and do not commit
        Result := As.U.Tus (
           Git_If.Merge_Branch (Sel_Name.Image, "", True, True));
        -- Set comment for on going manual commit
        Commit.Set_Comment (Comment.Image);
      when Rebase =>
        Message1 := As.U.Tus ("Rebasing branch " & Current_Branch.Image);
        Message2 := As.U.Tus ("to head of " & Sel_Name.Image);
        Previous_Branch := Sel_Name;
        Result := As.U.Tus (Rebase_Mng.Do_Rebase (Root.Image,
            Sel_Name.Image, Ref_Name.Image, False));
        Init;
      when Cherry_Pick =>
        -- Reset memory of previous rebase
        Rebase_Mng.Reset (False);
        Previous_Branch := Sel_Name;
        -- Done (back to Directory) if Ok or Error
        -- Remain in Branch only if Cancelled
        Done := Cherry.Pick (Root.Image, Sel_Name.Image, Ref_Name.Image,
                             Cherry.Interactive) /= Cherry.Cancelled;
        Init;
        Reread (False);
        return Done;
      when Reset =>
        Previous_Branch := Sel_Name;
        if Sel_Name = Current_Branch then
          -- Rabse to HEAD of current branch
          Done := Reset (Root.Image, "");
        else
          -- Rabse to another branch
          Done := Reset (Root.Image, Sel_Name.Image);
        end if;
        if not Done then
          Init;
          Reread (False);
        end if;
        return Done;
    end case;

    -- Handle error
    if not Result.Is_Null then
      Error (Message1.Image, Message2.Image, Result.Image);
      Init;
      Reread (False);
      -- Rebase always True, else False
      return Action = Rebase;
    end if;

    -- Call commit after successful true merge
    if Action = True_Merge then
      Commit.Handle (Root.Image, Allow_Modif => False);
      Init;
    end if;

    -- Done
    Reread (False);

    -- Successful checkout, merge, rebase
    return Action = Checkout or else Action = Merge or else Action = Rebase;
  end Do_Action;

  -- Update the list status
  procedure List_Change (Unused_Action : in Afpx.List_Change_List;
                         Status : in Afpx.List_Status_Rec) is
    On_Current, Remote, Has_Local : Boolean;
    -- Only Rebase, Cherrypick and Delete if right selection
    Right : constant Boolean
          := Status.Ids_Selected (Afpx.List_Right) /= 0;
    use type As.U.Asu_Us;
  begin
    if Afpx.Line_List.Is_Empty then
      return;
    end if;
    -- No action on current branch (except Create and Reset_Hard)
    Branches.Move_At (Afpx.Line_List.Get_Position);
    On_Current := Branches.Access_Current.Name = Current_Branch;
    -- Only Merge, TrueMerge CherryPickand Delete of remote, and Create
    Remote := Branches.Access_Current.Is_Remote;
    -- Allow Checkout on Remote that does not have corresponding local
    Has_Local := Branches.Access_Current.Has_Local;
    -- Protect
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Checkout,
                              On_Current
                              or else (Remote and then Has_Local)
                              or else Right);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.History, Right);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Merge,
                              On_Current or else Right);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.True_Merge,
                              On_Current or else Right);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Rebase,
                              On_Current or else Remote);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Cherry_Pick,
                              On_Current);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Reset,
                              Remote or else Right);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Rename,
                              On_Current or else Remote or else Right);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Delete,
                              On_Current);
  end List_Change;

  -- Handle the Branches
  procedure Handle (Root : in String) is
    Ptg_Result : Afpx.Result_Rec;
    Dummy_Res  : Boolean;
    use type Afpx.Field_Range;
  begin

    -- Move to root
    Branch.Root := As.U.Tus (Root);
    Directory.Change_Current (Root);

    -- Init Afpx
    Init;

    -- Reset Afpx list
    Afpx.Line_List.Delete_List (False);

    -- List width
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);

    -- Encode Branches
    Reread (False);

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
                         Right_Select => True,
                         List_Change_Cb => List_Change'Access);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              Dummy_Res := Do_Action (Create);
            when Afpx.Escape_Key =>
              -- Back
              exit;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Utils.X.List_Scroll_Fld_Range =>
              -- Scroll list
              Afpx.Utils.Scroll(
                  Ptg_Result.Field_No
                - Utils.X.List_Scroll_Fld_Range'First
                + 1);
            when Afpx.List_Field_No | Afpx_Xref.Branches.Checkout =>
              if Do_Action (Checkout) then
                exit;
              end if;
            when Afpx_Xref.Branches.History =>
              Dummy_Res := Do_Action (Hist);
            when Afpx_Xref.Branches.Merge =>
              if Do_Action (Merge) then
                exit;
              end if;
            when Afpx_Xref.Branches.True_Merge =>
              if Do_Action (True_Merge) then
                exit;
              end if;
            when Afpx_Xref.Branches.Rebase =>
              if Do_Action (Rebase, Ptg_Result.Id_Selected_Right) then
                exit;
              end if;
            when Afpx_Xref.Branches.Cherry_Pick =>
              if Do_Action (Cherry_Pick, Ptg_Result.Id_Selected_Right) then
                exit;
              end if;
            when Afpx_Xref.Branches.Reset =>
              if Do_Action (Reset) then
                exit;
              end if;
            when Afpx_Xref.Branches.Create =>
              Dummy_Res := Do_Action (Create);
            when Afpx_Xref.Branches.Rename =>
              if Do_Action (Rename) then
                exit;
              end if;
            when Afpx_Xref.Branches.Delete =>
              if Do_Action (Delete, Ptg_Result.Id_Selected_Right) then
                exit;
              end if;
            when Afpx_Xref.Branches.Back =>
              -- Back button
              exit;
            when others =>
              null;
          end case;

        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          -- Encode current branch
          Utils.X.Encode_Branch (Afpx_Xref.Branches.Branch);
      end case;
    end loop;

    if not Afpx.Line_List.Is_Empty then
      Branches.Move_At (Afpx.Line_List.Get_Position);
      Previous_Branch := Branches.Access_Current.Name;
    end if;
  end Handle;

  -- Interactively rebase current branch from rev
  function Reorg (Root, Rev : String) return Boolean is
    Msg : As.U.Asu_Us;
  begin
    Msg := As.U.Tus (Rebase_Mng.Do_Rebase (Root, Rev, "", True));
    if Msg.Is_Null then
      return True;
    end if;
    Error ("Rebase from " & Rev, "", Msg.Image);
    return False;
  end Reorg;

end Branch;

