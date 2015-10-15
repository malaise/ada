with Ada.Exceptions;
with As.U.Utils, Directory, Afpx.Utils, Basic_Proc, Unicode, Str_Util;
with Git_If, Utils.X, Afpx_Xref, Confirm, Error, Cherry, Reset, Commit;
package body Branch is

  -- List width
  List_Width : Afpx.Width_Range;

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in As.U.Asu_Us) is
  begin
    Afpx.Utils.Encode_Line ("", From.Image, "", List_Width, Line);
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised in branches on " & From.Image);
  end Set;

  function Match (Current, Criteria : Afpx.Line_Rec) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return Current.Str(4 .. Current.Len) = Criteria.Str(4 .. Criteria.Len);
  end Match;
  function Search is new Afpx.Line_List_Mng.Search (Match);

  procedure Init_List is new Afpx.Utils.Init_List (
    As.U.Asu_Us, As.U.Utils.Asu_List_Mng, Set, False);

  -- Separator in branch names
  Sep : constant String := Git_If.Separator & "";

  -- Root path and current branch
  Root : As.U.Asu_Us;
  Current_Branch : As.U.Asu_Us;

  -- The branch previously selected (when moving back from a sub-menu or
  --  when being re-called
  Previous_Branch : As.U.Asu_Us;

  -- The Branches
  Branches : Git_If.Branches_Mng.List_Type;

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

  -- Re assess the list of branches
  procedure Reread (Restore : in Boolean) is
    Branch : As.U.Asu_Us;
    Sep_Index : Natural;
    Moved : Boolean;
    Line : Afpx.Line_Rec;
  begin
    -- Encode current branch
    Utils.X.Encode_Branch (Afpx_Xref.Branches.Branch);
    Current_Branch := As.U.Tus (Git_If.Current_Branch);

    -- Save current selection
    if Restore and then not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
    elsif not Previous_Branch.Is_Null then
      -- Set default to previous branch
      Set (Line, Previous_Branch);
    else
      -- Set default to current branch
      Set (Line, Current_Branch);
    end if;

    -- Get the list of local and remote branches
    Git_If.List_Branches (Local => True, Remote => True, Branches => Branches);
    -- Filter out entry [Xxx/]"HEAD"
    --  and filter out leading "remotes/" from entries
    if not Branches.Is_Empty then
      Branches.Rewind;
      loop
        Branches.Read (Branch, Git_If.Branches_Mng.Current);
        -- Last slash
        Sep_Index := Str_Util.Locate (Branch.Image, Sep, Forward => False);
        if Branch.Slice (Sep_Index + 1, Branch.Length) = "HEAD" then
          Branches.Delete (Moved => Moved);
          exit when not Moved;
        else
          -- First slash
          Sep_Index := Str_Util.Locate (Branch.Image, Sep, Forward => True);
          if Branch.Slice (1, Sep_Index) = "remotes/" then
            Branch.Delete (1, Sep_Index);
            Branches.Modify (Branch, Git_If.Branches_Mng.Current);
          end if;
          -- Move to next if possible
          if Branches.Check_Move then
            Branches.Move_To;
          else
            exit;
          end if;
        end if;
      end loop;
    end if;

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

    -- Set field activity
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Rename,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Delete,
                              Afpx.Line_List.Is_Empty);
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
  type Action_List is (Create, Rename, Delete, Checkout, Merge, True_Merge,
                       Rebase, Cherry_Pick, Reset);
  function Do_Action (Action : in Action_List;
                      Ref : in Natural := 0) return Boolean is
    Sel_Name, New_Name, Ref_Name : As.U.Asu_Us;
    Remote : Boolean;
    Comment : As.U.Asu_Us;
    Pos : Positive;
    Message, Result : As.U.Asu_Us;
    Done : Boolean;
    use type Cherry.Result_List;
  begin
    -- Retrieve current name
    if Action /= Create then
      Branches.Move_At (Afpx.Line_List.Get_Position);
      Sel_Name := Branches.Access_Current.all;
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
    if (Action = Rebase or else Action = Cherry_Pick)
    and then Ref /= 0 then
      Pos := Branches.Get_Position;
      Branches.Move_At (Ref);
      Ref_Name := Branches.Access_Current.all;
      Branches.Move_At (Pos);
    end if;

    -- Cancel if not confirm
    if Action /= Create and then Action /= Rename
    and then Action /= Rebase and then Action /= Cherry_Pick
    and then Action /= Reset then
      if not Confirm (
          (case Action is
             when Create | Rename | Rebase | Cherry_Pick | Reset => "???",
             when Delete     => "Delete branch " & Sel_Name.Image,
             when Checkout   => "Checkout branch " & Sel_Name.Image,
             when Merge      => "Merge branch " & Sel_Name.Image,
             when True_Merge => "True Merge branch " & Sel_Name.Image),
          (case Action is
             when Create | Rename | Rebase | Cherry_Pick | Reset | Delete
                | Checkout => "",
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
    case Action is
      when Create =>
        Message := As.U.Tus ("Creating branch " & New_Name.Image);
        if not New_Name.Is_Null then
          Result := As.U.Tus (Git_If.Create_Branch (New_Name.Image));
          if Result.Is_Null then
            Previous_Branch := New_Name;
          end if;
        end if;
      when Rename =>
        Message := As.U.Tus ("Renaming branch " & Sel_Name.Image
                           & " to " & New_Name.Image);
        if not New_Name.Is_Null then
          Result := As.U.Tus (Git_If.Rename_Branch (Sel_Name.Image,
                                                    New_Name.Image));
          if Result.Is_Null then
            Previous_Branch := New_Name;
          end if;
        end if;
      when Delete =>
        Message := As.U.Tus ("Deleting branch " & Sel_Name.Image);
        Remote := Str_Util.Locate (Sel_Name.Image, Sep) /= 0;
        Result := As.U.Tus (Git_If.Delete_Branch (Sel_Name.Image, Remote));
        if Result.Is_Null then
          -- Go to next branch in list if possible
          if Branches.Check_Move then
            Branches.Move_To;
            Previous_Branch := Branches.Access_Current.all;
          else
            Previous_Branch := Current_Branch;
          end if;
        end if;
      when Checkout =>
        Message := As.U.Tus ("Checking out branch " & Sel_Name.Image);
        Result := As.U.Tus (Git_If.Do_Checkout (Sel_Name.Image, ""));
        Previous_Branch := Current_Branch;
      when Merge =>
        Message := As.U.Tus ("Merging branch " & Sel_Name.Image);
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
      when True_Merge =>
        Message := As.U.Tus ("True merging branch " & Sel_Name.Image);
        Previous_Branch := Sel_Name;
        -- Commit comment
        Comment := As.U.Tus ("Merge branch '" & Sel_Name.Image & "'");
        -- Merge, disallow fast forward and do not commit
        Result := As.U.Tus (
           Git_If.Merge_Branch (Sel_Name.Image, "", True, True));
        -- Set comment for on going manual commit
        Commit.Set_Comment (Comment.Image);
      when Rebase =>
        Message := As.U.Tus ("Rebasing branch " & Current_Branch.Image
                             & " to head of " & Sel_Name.Image);
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
        Done := Reset (Root.Image, Sel_Name.Image);
        if not Done then
          Init;
          Reread (False);
        end if;
        return Done;
    end case;

    -- Handle error
    if not Result.Is_Null then
      Error (Message.Image, "", Result.Image);
      Init;
      Reread (False);
      -- Rebase always True, else False
      return Action = Rebase;
    end if;

    -- Call commit aftert successful true merge
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
                         Unused_Status : in Afpx.List_Status_Rec) is
    On_Current, Remote : Boolean;
    use type As.U.Asu_Us;
  begin
    if Afpx.Line_List.Is_Empty then
      return;
    end if;
    -- No action on current branch (except Create and Reset_Hard)
    Branches.Move_At (Afpx.Line_List.Get_Position);
    On_Current := Branches.Access_Current.all = Current_Branch;
    -- Only Merge, TrueMerge and Delete of remote, and create
    Remote := Str_Util.Locate (Branches.Access_Current.all.Image, Sep) /= 0;
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Checkout,
                              On_Current or else Remote);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Merge, On_Current);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.True_Merge, On_Current);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Rebase,
                              On_Current or else Remote);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Cherry_Pick,
                              On_Current or else Remote);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Reset, Remote);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Rename,
                              On_Current or else Remote);
    Afpx.Utils.Protect_Field (Afpx_Xref.Branches.Delete, On_Current);
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
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result, True,
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
            when Utils.X.List_Scroll_Fld_Range'First ..
                 Utils.X.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.Utils.Scroll(
                  Ptg_Result.Field_No
                - Utils.X.List_Scroll_Fld_Range'First
                + 1);
            when Afpx.List_Field_No | Afpx_Xref.Branches.Checkout =>
              if Do_Action (Checkout) then
                exit;
              end if;
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
              if Do_Action (Delete) then
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
      Previous_Branch := Branches.Access_Current.all;
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

