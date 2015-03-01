with As.U.Utils, Afpx.Utils, Unicode;
with Utils.X, Git_If, Afpx_Xref, Error;
package body Push_Pull is

  type Menu_List is (Push, Pull_Branch, Pull);
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

  -- Get remote and push
  function Do_Push (Tag : in String; Set_Upstream : in Boolean)
           return Boolean is
    Log : As.U.Asu_Us;
  begin
    References.Move_At (Afpx.Line_List.Get_Position);
    Log := As.U.Tus (Git_If.Do_Push (References.Access_Current.Image,
                                     Tag, Set_Upstream));
    if Log.Is_Null then
      return True;
    else
      Error ("Pushing to", References.Access_Current.Image, Log.Image);
      return False;
    end if;
  end Do_Push;

  -- Get branch and call Do_Handle (Pull, Branch);
  function Do_Pull_Branch (Root : String) return Boolean is
    Branch : As.U.Asu_Us;
    Index : Natural;
    Log : As.U.Asu_Us;
  begin
    -- Get current selected branch
    References.Move_At (Afpx.Line_List.Get_Position);
    References.Read (Branch, Git_If.Reference_Mng.Current);
    Index := Branch.Locate (Git_If.Separator & "");
    if Index < 2 or else Index > Branch.Length - 1 then
      Log.Set ("Invalid remote branch format");
      return False;
    else
      return Do_Handle (Root, Pull, Branch.Slice (Index + 1, Branch.Length));
    end if;
  end Do_Pull_Branch;

  -- Fetch or pull (remote branch)
  function Do_Fetch (Branch : String; Pull : Boolean) return Boolean is
    Log : As.U.Asu_Us;
  begin
    References.Move_At (Afpx.Line_List.Get_Position);
    Log := As.U.Tus (
          Git_If.Do_Fetch (References.Access_Current.Image, Branch, Pull));
    if Log.Is_Null then
      return True;
    else
      Error ("Pulling branch",
             References.Access_Current.Image & Git_If.Separator & Branch,
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
    -- Current branch
    Curr_Branch : As.U.Asu_Us;
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

      -- Push upstream only for push of branch (not tag)
      Afpx.Set_Field_Activation (Afpx_Xref.Push_Pull.Push_Upstream,
                                 Menu = Push and then Branch_Tag = "");
      case Menu is
        when Push =>
          Utils.X.Center_Field ("Push " & Branch_Tag,
                                Afpx_Xref.Push_Pull.Title);
        when Pull_Branch =>
          Utils.X.Center_Field ("Pull", Afpx_Xref.Push_Pull.Title);
          Utils.X.Center_Field ("Select branch", Afpx_Xref.Push_Pull.Sub_Title);
          Utils.X.Center_Field ("OK", Afpx_Xref.Push_Pull.Push);
          Afpx.Clear_Field (Afpx_Xref.Push_Pull.Entries);
          Afpx.Encode_Field (Afpx_Xref.Push_Pull.Entries, (0, 0), "Branches:");
        when Pull =>
          Utils.X.Center_Field (Branch_Tag, Afpx_Xref.Push_Pull.Sub_Title);
          if Branch_Tag = Curr_Branch then
            Utils.X.Center_Field ("Pull branch", Afpx_Xref.Push_Pull.Title);
            Utils.X.Center_Field ("Pull", Afpx_Xref.Push_Pull.Push);
          else
            Utils.X.Center_Field ("Fetch branch", Afpx_Xref.Push_Pull.Title);
            Utils.X.Center_Field ("Fetch", Afpx_Xref.Push_Pull.Push);
          end if;
          Afpx.Reset_Field (Afpx_Xref.Push_Pull.Entries);
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
        -- Move to "origin" for push and pull,
        --  or "origin":<branch> for pull branch, or top
        if Menu /= Pull_Branch  then
          Afpx.Encode_Line (Origin, "origin");
        elsif not Curr_Branch.Is_Null then
          Set (Origin,
               As.U.Tus ("origin" & Git_If.Separator & Curr_Branch.Image));
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
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result, True);

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
            when Afpx.List_Field_No | Afpx_Xref.Push_Pull.Push =>
              case Menu is
                when Pull_Branch =>
                  -- Pull branch
                  Result := Do_Pull_Branch (Root);
                when Pull =>
                  -- Fetch branch
                  Result := Do_Fetch (Branch_Tag, Branch_Tag = Curr_Branch);
                when Push =>
                  -- Push branch or tag
                  Result := Do_Push (Branch_Tag, False);
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
              Result := Do_Push (Curr_Branch.Image, True);
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
      return Do_Handle (Root, Pull_Branch, "");
    else
      return Do_Handle (Root, Push, Tag);
    end if;
  end Handle;
end Push_Pull;

