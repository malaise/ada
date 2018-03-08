with As.U.Utils, Afpx.Utils, Str_Util, Mixed_Str;
with Utils.X, Git_If, Afpx_Xref, Error, Confirm, Branch;
package body Push_Pull is

  type Menu_List is (Push, Pull_Remote, Pull_Branch);
  -- Handle the Push (of tag), the Pull_Branch and the Pull
  function Do_Handle (Root : String;
                      Menu : Menu_List;
                      Branch_Tag : String) return Boolean;

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in As.U.Asu_Us) is
  begin
    Afpx.Utils.Encode_Line ("", From.Image, "",
                            Afpx.Get_Field_Width (Afpx.List_Field_No), Line);
  end Set;

  procedure Init_List is new Afpx.Utils.Init_List (
    As.U.Asu_Us, As.U.Set, As.U.Utils.Asu_Long_Long_List_Mng, Set, False);

  -- The items of the list
  List : Git_If.Reference_Mng.List_Type;
  function Search_Ref is new Git_If.Reference_Mng.Search (As.U."=");

  -- The local branches
  Branches : Git_If.Branches_Mng.List_Type;

  -- Current branch
  Curr_Branch : As.U.Asu_Us;

  -- Get remote and push
  function Do_Push (Tag : in String;
                    Set_Upstream : in Boolean;
                    Force : in Boolean) return Boolean is
    Log : As.U.Asu_Us;
  begin
    List.Move_At (Afpx.Line_List.Get_Position);
    Log := As.U.Tus (Git_If.Do_Push (List.Access_Current.Image,
                                     Tag, Set_Upstream, Force));
    if Log.Is_Null then
      return True;
    else
      Error ("Pushing to", List.Access_Current.Image, Log.Image);
      return False;
    end if;
  end Do_Push;

  -- Get remote and call Do_Handle (Pull, Remote);
  function Do_Pull_Branch (Root : String) return Boolean is
    Remote : As.U.Asu_Us;
  begin
    -- Get current selected remote
    List.Move_At (Afpx.Line_List.Get_Position);
    List.Read (Remote, Git_If.Reference_Mng.Current);
    return Do_Handle (Root, Pull_Branch, Remote.Image);
  end Do_Pull_Branch;

  -- Name of remote ref, for split
  Remote : As.U.Asu_Us;

  -- Search
  function Search_Branch is new Git_If.Branches_Mng.Search (As.U."=");
  -- Split a remote branch name and deduce action
  type Action_List is (Pull, Fetch, Checkout);
  function Split (Item : in String;
                  Branch : out As.U.Asu_Us) return Action_List is
    Sep : Natural;
    use type As.U.Asu_Us;
  begin
    Branch := As.U.Tus (Item);
    -- Remove remote from branch name if possible
    Sep := Str_Util.Locate (Branch.Image, Git_If.Separator & "");
    if Sep /= 0 then
      Branch.Delete (1, Sep);
    end if;
    -- Pull if same branch name
    if Branch = Curr_Branch then
      return Pull;
    end if;
    -- Fetch if this branch exists locally, otherwise checkout
    return (if Search_Branch (Branches, Branch,
                              From => Git_If.Branches_Mng.Absolute) then
              Fetch
            else Checkout);
  end Split;

  -- Fetch or pull or checkout(remote branch)
  function Do_Get (Force_Fetch : in Boolean) return Boolean is
    Branch, Log : As.U.Asu_Us;
    Action : Action_List;
    function Check (Msg : in String) return Boolean is
    begin
      if Log.Is_Null then
        return True;
      else
        Error (Msg & " branch " & Branch.Image,
               "from remote " & Remote.Image,
               Log.Image);
        return False;
      end if;
    end Check;

  begin
    -- Get selected branch name
    List.Move_At (Afpx.Line_List.Get_Position);
    List.Read (Branch, Git_If.Reference_Mng.Current);
    Action := Split (Branch.Image, Branch);
    -- Fetch
    case Action is
      when Pull =>
        -- Pull or force Fetch
        Log := As.U.Tus (Git_If.Do_Fetch (Remote.Image, Branch.Image,
                                          Pull => not Force_Fetch));
        return Check (if Force_Fetch then "Fetching" else "Pulling");
      when Fetch =>
        -- Fetch
        Log := As.U.Tus (Git_If.Do_Fetch (Remote.Image, Branch.Image, False));
        return Check ("Fetching");
      when Checkout =>
        -- Fetch
        Log := As.U.Tus (Git_If.Do_Fetch (Remote.Image, Branch.Image, False));
        if not Check ("Fetching") then
          return False;
        end if;
        -- Checkout remote tracking into local
        Log := As.U.Tus (Git_If.Do_Checkout (
            Remote.Image & Git_If.Separator & Branch.Image,
            Branch.Image));
        return Check ("Checking out");
    end case;
  end Do_Get;

  function Fetch_All return Boolean is
    Ref : As.U.Asu_Us;
    Res : As.U.Asu_Us;
  begin
    List.Move_At (Afpx.Line_List.Get_Position);
    Ref := List.Access_Current.all;
    if not Confirm ("Fetching all branches from " & Ref.Image,
                    "This may take some time and create many branches") then
       return False;
    end if;
    Res := As.U.Tus (Git_If.Do_Fetch (Ref.Image, "", False));
    if Res.Is_Null then
      return True;
    else
      Error ("Fetching all branches", "from remote " & Ref.Image, Res.Image);
      return False;
    end if;
  end Fetch_All;

  --- Update the list status
  procedure List_Change (Unused_Action : in Afpx.List_Change_List;
                         Unused_Status : in Afpx.List_Status_Rec) is
    Rem_Item : As.U.Asu_Us;
    Unused_Branch : As.U.Asu_Us;
    Action : Action_List;
  begin
    if List.Is_Empty then
      return;
    end if;
    -- Read selected remote branch and split it
    List.Move_At (Afpx.Line_List.Get_Position);
    List.Read (Rem_Item, Git_If.Branches_Mng.Current);
    -- Get possible action and encode in main button
    Action := Split (Rem_Item.Image, Unused_Branch);
    Utils.X.Center_Field (Mixed_Str (Action'Img), Afpx_Xref.Push_Pull.Push);
    -- Update second button to Fetch if Action is Pull or Checkout
    Afpx.Set_Field_Activation (Afpx_Xref.Push_Pull.Push_Upstream,
                               Action /= Fetch);
  end List_Change;

  -- Search in all branches
  function Branch_Match  (Current, Criteria : Branch.Branch_Rec_Type)
           return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end Branch_Match;
  function Search_All_Branches is new Branch.Branches_Mng.Search (Branch_Match);

  -- Handle the Push, the Pull_Branch and the Pull
  function Do_Handle (Root : String;
                      Menu : Menu_List;
                      Branch_Tag : String) return Boolean is
    -- Afpx stuff
    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result   : Afpx.Result_Rec;
    -- Result of Push or Pull
    Result : Boolean;
    use type Afpx.Absolute_Field_Range, As.U.Asu_Us;

    procedure Init is
      Current_Branch : As.U.Asu_Us;
      -- List of all the branches
      All_Branch : Branch.Branch_Rec_Type;
      All_Branches : Branch.Branches_Mng.List_Type;
      -- Origin of the current branch
      Origin : As.U.Asu_Us;
      -- Index of last '/'
      Index : Natural;
    begin
      -- Afpx stuff
      Afpx.Use_Descriptor (Afpx_Xref.Push_Pull.Dscr_Num);
      Get_Handle := (others => <>);

      -- Encode Root
      Utils.X.Encode_Field (Root, Afpx_Xref.Push_Pull.Root);

      -- Encode current branch
      Current_Branch := As.U.Tus (Git_If.Current_Branch);
      Utils.X.Encode_Field (Utils.X.Branch_Image (Current_Branch.Image),
                            Afpx_Xref.Push_Pull.Branch);

      -- Get current branch
      Curr_Branch := As.U.Tus (Git_If.Current_Branch);

      -- Change title and Push button if Pull_Branch or Pull

      -- Push upstream only for push of branch (not tag)
      --  or for Pull_Branch (Fetch)
      Afpx.Set_Field_Activation (Afpx_Xref.Push_Pull.Push_Upstream,
                                 (Menu = Push and then Branch_Tag = "")
                                 or else Menu = Pull_Branch);
      -- Push upstream and force only for push of branch (not tag)
      Afpx.Set_Field_Activation (Afpx_Xref.Push_Pull.Push_Force,
                                 Menu = Push and then Branch_Tag = "");
      -- Fetch and Prune only for Pull_Remote
      Afpx.Set_Field_Activation (Afpx_Xref.Push_Pull.Fetch,
                                 Menu = Pull_Remote);
      Afpx.Set_Field_Activation (Afpx_Xref.Push_Pull.Prune,
                                 Menu = Pull_Remote);
      case Menu is
        when Push =>
          Utils.X.Center_Field ("Push " & Branch_Tag,
                                Afpx_Xref.Push_Pull.Title);
        when Pull_Remote =>
          Utils.X.Center_Field (Branch_Tag, Afpx_Xref.Push_Pull.Sub_Title);
          Utils.X.Center_Field ("Select remote", Afpx_Xref.Push_Pull.Title);
          Utils.X.Center_Field ("OK", Afpx_Xref.Push_Pull.Push);
          Afpx.Reset_Field (Afpx_Xref.Push_Pull.Entries);
        when Pull_Branch =>
          Utils.X.Center_Field ("Get from " & Branch_Tag,
                                Afpx_Xref.Push_Pull.Title);
          Utils.X.Center_Field ("Select branch",
                                Afpx_Xref.Push_Pull.Sub_Title);
          -- Main action will be set by List_Change
          Utils.X.Center_Field ("???", Afpx_Xref.Push_Pull.Push);
          Utils.X.Center_Field ("Fetch", Afpx_Xref.Push_Pull.Push_Upstream);
          Afpx.Clear_Field (Afpx_Xref.Push_Pull.Entries);
          Afpx.Reset_Field (Afpx_Xref.Push_Pull.Entries);
          Afpx.Encode_Field (Afpx_Xref.Push_Pull.Entries, (0, 0), "Branches:");
      end case;
      Remote := As.U.Tus (Branch_Tag);

      -- Get list of references
      if Menu = Pull_Branch then
        -- List local branches to check if remote exists locally
        Git_If.List_Branches (Local => True, Remote => False,
                              Branches => Branches);
        -- List remote branches
        Git_If.List_Branches_Of (Remote.Image, Branches => List);
      else
        -- List remotes
        Git_If.List_References (List);
      end if;
      Init_List (List);

      Afpx.Utils.Protect_Field (Afpx_Xref.Push_Pull.Push, List.Is_Empty);
      Afpx.Utils.Protect_Field (Afpx_Xref.Push_Pull.Push_Upstream,
                                List.Is_Empty);
      Afpx.Utils.Protect_Field (Afpx_Xref.Push_Pull.Push_Force, List.Is_Empty);
      if List.Is_Empty then
        -- No action possible
        Afpx.Clear_Field (Afpx_Xref.Push_Pull.Push);
        Afpx.Clear_Field (Afpx_Xref.Push_Pull.Push_Upstream);
        Afpx.Clear_Field (Afpx_Xref.Push_Pull.Push_Force);
      else
        -- For push and pull_remote, move to the origin of current branch if
        --  possible, otherwise to "origin"
        -- For pull_branch, move to <remote>/<branch> if possible, otherwise to
        --  top
        if Menu /= Pull_Branch then
          -- Get the name of the remote tracking branch
          Origin := As.U.Tus (Git_If.Remote_Branch);
          if not Origin.Is_Null then
            -- Search this origin in the branches
            Branch.List_Branches (All_Branches);
            All_Branch.Name := Origin;
            if Search_All_Branches (All_Branches, All_Branch,
                From => Branch.Branches_Mng.Absolute) then
              -- The remote of current branch is found
              -- Keep the head up to last '/'
              Index := Str_Util.Locate (Origin.Image, Git_If.Separator & "",
                                        Forward => False);
              if Index /= 0 then
                Origin.Delete (Index, Origin.Length);
              else
                Origin.Set_Null;
              end if;
            else
              Origin.Set_Null;
            end if;
          end if;
          if Origin.Is_Null then
            -- Remote of current not found => look for "origin"
            Origin := As.U.Tus (Git_If.Default_Origin);
          end if;
        elsif not Curr_Branch.Is_Null then
          Origin := As.U.Tus (Curr_Branch.Image);
        end if;
        if not Search_Ref (List, Origin,
                           From => Git_If.Reference_Mng.Absolute) then
          List.Rewind;
        end if;
        Afpx.Line_List.Move_At (List.Get_Position);
        Afpx.Update_List (Afpx.Center_Selected);
      end if;

    end Init;

  begin
    -- Init Afpx
    Init;

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
          List_Change_Cb => (if Menu = Pull_Branch then
                             List_Change'Access else null));

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              null;
            when Afpx.Escape_Key =>
              -- Back
              return False;
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
            when Afpx_Xref.Push_Pull.Fetch =>
              -- Fetch whole remote
              if Fetch_All then
                return True;
              end if;
              Init;
            when Afpx_Xref.Push_Pull.Prune =>
              -- Prune remotes of selected remote
              List.Move_At (Afpx.Line_List.Get_Position);
              Git_If.Do_Prune (List.Access_Current.Image);
            when Afpx.List_Field_No | Afpx_Xref.Push_Pull.Push =>
              case Menu is
                when Pull_Remote =>
                  -- Pull branch
                  Result := Do_Pull_Branch (Root);
                when Pull_Branch =>
                  -- Pull/Fetch/Checkout branch
                  Result := Do_Get (False);
                when Push =>
                  -- Push branch or tag
                  Result := Do_Push (Branch_Tag, False, False);
              end case;
              if Result then
                -- Push/Pull OK
                return True;
              else
                -- Push/Pull KO
                Init;
              end if;
            when Afpx_Xref.Push_Pull.Push_Upstream =>
              case Menu is
                when Pull_Remote =>
                  null;
                when Pull_Branch =>
                  -- Only fetch current branch
                  Result := Do_Get (True);
                when Push =>
                  -- Push current branch with --set-upstream
                  Result := Do_Push (Curr_Branch.Image, True, False);
              end case;
              if Result then
                -- Push/Pull OK
                return True;
              else
                -- Push/Pull KO
                Init;
              end if;
            when Afpx_Xref.Push_Pull.Push_Force =>
              -- Push current branch with --force
              Result := Do_Push (Curr_Branch.Image, False, True);
              if Result then
                -- Push/Pull OK
                return True;
              else
                -- Push/Pull KO
                Init;
              end if;
            when Afpx_Xref.Push_Pull.Back =>
              -- Back button
              return False;
            when others =>
              null;
          end case;

       when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
         null;
      when Afpx.Refresh =>
        -- Encode current branch
        Utils.X.Encode_Branch (Afpx_Xref.Push_Pull.Branch);
      end case;
    end loop;

  end Do_Handle;

  -- Public interface
  -- Handle the Push or pull
  function Handle (Root : String;
                   Pull : Boolean;
                   Tag : String := "") return Boolean is
    (if Pull then Do_Handle (Root, Pull_Remote, "")
    else          Do_Handle (Root, Push, Tag));
end Push_Pull;

