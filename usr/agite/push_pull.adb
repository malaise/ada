with As.U.Utils, Afpx.Utils, Unicode;
with Utils.X, Git_If, Afpx_Xref, Error;
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
    As.U.Asu_Us, As.U.Utils.Asu_List_Mng, Set, False);

  -- Afpx line list with "origin"
  Origin : Afpx.Line_Rec;
  function Match (Current, Criteria : Afpx.Line_Rec) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return Current.Str(1 .. Current.Len) = Criteria.Str(1 .. Criteria.Len);
  end Match;
  function Search is new Afpx.Line_List_Mng.Search (Match);

  -- The references
  References : Git_If.Reference_Mng.List_Type;

  -- Current branch
  Curr_Branch : As.U.Asu_Us;

  -- Get remote and push
  function Do_Push (Tag : in String;
                    Set_Upstream : in Boolean;
                    Force : in Boolean) return Boolean is
    Log : As.U.Asu_Us;
  begin
    References.Move_At (Afpx.Line_List.Get_Position);
    Log := As.U.Tus (Git_If.Do_Push (References.Access_Current.Image,
                                     Tag, Set_Upstream, Force));
    if Log.Is_Null then
      return True;
    else
      Error ("Pushing to", References.Access_Current.Image, Log.Image);
      return False;
    end if;
  end Do_Push;

  -- Get remote and call Do_Handle (Pull, Remote);
  function Do_Pull_Branch (Root : String) return Boolean is
    Remote : As.U.Asu_Us;
  begin
    -- Get current selected remote
    References.Move_At (Afpx.Line_List.Get_Position);
    References.Read (Remote, Git_If.Reference_Mng.Current);
    return Do_Handle (Root, Pull_Branch, Remote.Image);
  end Do_Pull_Branch;

  -- Fetch or pull (remote branch)
  function Do_Fetch (Remote : String) return Boolean is
    Branch, Log : As.U.Asu_Us;
    Remote_Sep : constant String := Remote & Git_If.Separator;
    Pull : Boolean;
  begin
    -- Get selected branch name
    References.Move_At (Afpx.Line_List.Get_Position);
    References.Read (Branch, Git_If.Reference_Mng.Current);
    -- Remove remote from branch name if possible
    if Branch.Length > Remote_Sep'Length
    and then Branch.Slice (1, Remote_Sep'Length) = Remote_Sep then
      Branch.Delete ( 1, Remote_Sep'Length);
      -- Pull if same branch name
      Pull := Curr_Branch.Image = Branch.Image;
    else
      -- Fetch
      Pull := False;
    end if;
    -- Fetch
    Log := As.U.Tus (Git_If.Do_Fetch (Remote, Branch.Image, Pull));
    if Log.Is_Null then
      return True;
    else
      Error ("Pulling branch " &  Branch.Image,"from remote " & Remote,
             Log.Image);
      return False;
    end if;
  end Do_Fetch;

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
    begin
      -- Afpx stuff
      Afpx.Use_Descriptor (Afpx_Xref.Push_Pull.Dscr_Num);
      Get_Handle := (others => <>);

      -- Encode Root
      Utils.X.Encode_Field (Root, Afpx_Xref.Push_Pull.Root);

      -- Encode current branch
      Utils.X.Encode_Branch (Afpx_Xref.Push_Pull.Branch);

      -- Get current branch
      Curr_Branch := As.U.Tus (Git_If.Current_Branch);

      -- Change title and Push button if Pull_Branch or Pull

      -- Push upstream and force only for push of branch (not tag)
      Afpx.Set_Field_Activation (Afpx_Xref.Push_Pull.Push_Upstream,
                                 Menu = Push and then Branch_Tag = "");
      Afpx.Set_Field_Activation (Afpx_Xref.Push_Pull.Push_Force,
                                 Menu = Push and then Branch_Tag = "");
      -- Prune only for Pull_Remote
      Afpx.Set_Field_Activation (Afpx_Xref.Push_Pull.Prune,
                                 Menu = Pull_Remote);
      case Menu is
        when Push =>
          Utils.X.Center_Field ("Push " & Branch_Tag,
                                Afpx_Xref.Push_Pull.Title);
        when Pull_Remote =>
          Utils.X.Center_Field (Branch_Tag, Afpx_Xref.Push_Pull.Sub_Title);
          Utils.X.Center_Field ("Select remote", Afpx_Xref.Push_Pull.Title);
          Utils.X.Center_Field ("Pull", Afpx_Xref.Push_Pull.Push);
          Afpx.Reset_Field (Afpx_Xref.Push_Pull.Entries);
        when Pull_Branch =>
          Utils.X.Center_Field ("Pull from " & Branch_Tag,
                                Afpx_Xref.Push_Pull.Title);
          Utils.X.Center_Field ("Select branch",
                                Afpx_Xref.Push_Pull.Sub_Title);
          Utils.X.Center_Field ("OK", Afpx_Xref.Push_Pull.Push);
          Afpx.Clear_Field (Afpx_Xref.Push_Pull.Entries);
          Afpx.Reset_Field (Afpx_Xref.Push_Pull.Entries);
          Afpx.Encode_Field (Afpx_Xref.Push_Pull.Entries, (0, 0), "Branches:");
      end case;

      -- Get list of references
      if Menu = Pull_Branch then
        Git_If.List_Branches (Local => False, Branches => References);
      else
        Git_If.List_References (References);
      end if;
      Init_List (References);

      Afpx.Utils.Protect_Field (Afpx_Xref.Push_Pull.Push, References.Is_Empty);
      if not References.Is_Empty then
        -- Move to "origin" for push and pull_remote,
        --  or <remote>/<branch> for pull_branch, or top
        if Menu /= Pull_Branch  then
          Afpx.Encode_Line (Origin, "origin");
        elsif not Curr_Branch.Is_Null then
          Set (Origin,
               As.U.Tus (Branch_Tag & Git_If.Separator & Curr_Branch.Image));
        end if;
        if not Search (Afpx.Line_List, Origin,
                       From => Afpx.Line_List_Mng.Absolute) then
          Afpx.Line_List.Rewind;
        end if;
      end if;

    end Init;

  begin
    -- Init Afpx
    Init;

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result, False);

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
            when Utils.X.List_Scroll_Fld_Range'First ..
                 Utils.X.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.Utils.Scroll(
                  Ptg_Result.Field_No
                - Utils.X.List_Scroll_Fld_Range'First
                + 1);
            when Afpx_Xref.Push_Pull.Prune =>
              -- Prune remotes of selected remote
              References.Move_At (Afpx.Line_List.Get_Position);
              Git_If.Do_Prune (References.Access_Current.Image);
            when Afpx.List_Field_No | Afpx_Xref.Push_Pull.Push =>
              case Menu is
                when Pull_Remote =>
                  -- Pull branch
                  Result := Do_Pull_Branch (Root);
                when Pull_Branch =>
                  -- Fetch branch
                  Result := Do_Fetch (Branch_Tag);
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
              -- Push current branch with --set-upstream
              Result := Do_Push (Curr_Branch.Image, True, False);
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
  begin
    if Pull then
      return Do_Handle (Root, Pull_Remote, "");
    else
      return Do_Handle (Root, Push, Tag);
    end if;
  end Handle;
end Push_Pull;

