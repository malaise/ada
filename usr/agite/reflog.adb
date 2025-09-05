with Ada.Exceptions;
with As.U, Directory, Afpx.Utils, Basic_Proc, Unicode, Str_Util;
with Git_If, Utils.X, Utils.Store, Afpx_Xref, Details, Checkout, Reset, Confirm;
package body Reflog is

  -- List width
  List_Width : Afpx.Width_Range;

  -- Root path and current branch
  Root, Target_Branch : As.U.Asu_Us;

  -- Current list of refs
  Refs : Git_If.Reflog_List;

  -- Init Afpx list from Refs
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Reflog_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line (
        "", Utils.Image (From.Date) & " " & From.Comment.Image, "",
        List_Width, Line, False);
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised in reflog on " & From.Comment.Image);
  end Set;
  procedure Init_List is new Afpx.Utils.Init_List (
    Git_If.Reflog_Entry_Rec, Git_If.Set, Git_If.Reflog_Mng, Set, False);
  function Match (Current, Criteria : Afpx.Line_Rec) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return Current.Str(1 .. Current.Len) = Criteria.Str(1 .. Criteria.Len);
  end Match;
  function Search is new Afpx.Line_List_Mng.Search (Match);

  -- Search by Hash
  Occurence : Git_If.Reflog_Mng.Ll_Positive;
  -- To search matching hash in Log
  function List_Hash_Match (Current, Criteria : Git_If.Reflog_Entry_Rec)
           return Boolean is
    use type As.U.Asu_Us;
  begin
    if Current.Hash.Length >= Criteria.Hash.Length then
      return Current.Hash.Uslice (1, Criteria.Hash.Length) = Criteria.Hash;
    else
      return False;
    end if;
  end List_Hash_Match;
  function List_Hash_Search is
           new Git_If.Reflog_Mng.Search (List_Hash_Match);

  -- Local: encode currently stored hash
  procedure Encode_Hash (Hash : in Git_If.Git_Hash) is
  begin
    Utils.X.Encode_Field (Hash.Image, Afpx_Xref.Reflog.Hash);
  end Encode_Hash;

 -- Move Afpx list at Hash
  -- First if No_Hash or not found
  procedure Move_At (Hash : in Git_If.Git_Hash) is
    Ref : Git_If.Reflog_Entry_Rec;
    use type As.U.Asu_Us, Git_If.Reflog_Mng.Ll_Positive;
  begin
    if Refs.Is_Empty or else Afpx.Line_List.Is_Empty then
      return;
    end if;
    Ref.Hash := Hash;
    if Hash /= Git_If.No_Hash
    and then List_Hash_Search (Refs, Ref,
                               Occurence => Occurence,
                               From => Git_If.Reflog_Mng.Absolute)
    then
      -- Move to found, ready for next occurence
      Afpx.Line_List.Move_At (Refs.Get_Position);
      Afpx.Update_List (Afpx.Center_Selected);
      Occurence := Occurence + 1;
    else
       Afpx.Line_List.Rewind;
       Refs.Rewind;
       Afpx.Update_List (Afpx.Top);
      Occurence := 1;
    end if;
  end Move_At;

  -- Afpx Ptg stuff
  Get_Handle : Afpx.Get_Handle_Rec;

  -- Re assess the list of refs
  procedure Reread is
    Line : Afpx.Line_Rec;
  begin
    -- Save position
    if not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
    else
      Line.Len := 0;
    end if;
    -- Encode the Afpx list
    -- Get the list of local and remote branches
    Git_If.List_Reflog (Target_Branch.Image, Refs);
    Init_List (Refs);
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
    Afpx.Update_List (Afpx.Top_Selected);
  end Reread;

  -- Init screen
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Reflog.Dscr_Num);
    -- Encode Root
    Utils.X.Encode_Field (Root.Image, Afpx_Xref.Reflog.Root);
    -- Encode current branch
    Utils.X.Encode_Branch (Afpx_Xref.Reflog.Branch);
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
    -- Encode target branch
    Utils.X.Encode_Field (
        (if Target_Branch.Is_Null then
           Utils.X.Branch_Image (Git_If.Current_Branch)
         else
           Target_Branch.Image),
        Afpx_Xref.Reflog.Target_Branch);
    -- Reset Ptg stuff
    Get_Handle := (others => <>);
    -- Reread
  end Init;

  -- Actions on refs
  type Action_List is (Detail, Chkout, Mark, Search, Reset, Delete);
  function Do_Action (Action : in Action_List) return Boolean is
    Position : Afpx.Line_List_Mng.Ll_Positive;
    Result : Boolean;
    Hash : Git_If.Git_Hash;
    use type As.U.Asu_Us, Afpx.Line_List_Mng.Ll_Positive;
  begin
    -- Save current position and move to proper ref
    Position := Afpx.Line_List.Get_Position;
    Refs.Move_At (Position);
    case Action is
      when Detail =>
        Details.Handle (Root.Image, Target_Branch.Image,
                        Refs.Access_Current.Hash.Image, False, False);
        Result := False;
      when Chkout =>
        Result := Checkout.Handle (Root.Image,
                                   "From reflog of branch",
                                   Target_Branch.Image,
                                   Refs.Access_Current.Hash);
      when Mark =>
        Utils.Store.Hash := Refs.Access_Current.Hash;
        Occurence := 1;
        return False;
      when Search =>
        -- Use got hash or else stored hash
        Hash := As.U.Tus (Str_Util.Strip (Afpx.Decode_Field (
                          Afpx_Xref.Reflog.Hash, 0, True)));
        if Hash = Git_If.No_Hash then
          Hash := Utils.Store.Hash;
          Encode_Hash (Hash);
        end if;
        if Hash = Git_If.No_Hash then
          return False;
        end if;
        -- Search
        Move_At (Hash);
        return False;
      when Reset =>
        Result := Reset (Root.Image, Refs.Access_Current.Hash.Image,
                         Only_Hard => True, Allow_Clean => False,
                         Comment => "from reflog");
      when Delete =>
        if Confirm ("Deleting reference", Refs.Access_Current.Hash.Image,
              Warning => "This may alter history and cannot be undone") then
          Git_If.Delete_Ref (Refs.Access_Current.Id.Image);
        end if;
        -- Remain in reflog menu
        Result := False;
    end case;
    Init;
    Reread;
    if Position <= Afpx.Line_List.List_Length then
      Afpx.Line_List.Move_At (Position);
    elsif not  Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Rewind;
    end if;
    Afpx.Update_List (Afpx.Top_Selected);
    return Result;
  end Do_Action;

    -- Update the list status
  procedure List_Change (Dummy_Action : in Afpx.List_Change_List;
                         Dummy_Status : in Afpx.List_Status_Rec) is
    Position : Afpx.Line_List_Mng.Ll_Positive;
    Protect : Boolean;
  begin
    -- Disable action buttons if no Id (Git_If aborted the list)
    if Afpx.Line_List.Is_Empty then
      return;
    end if;
    Position := Afpx.Line_List.Get_Position;
    Refs.Move_At (Position);
    Protect := Refs.Access_Current.Id.Is_Null;
    Afpx.Utils.Protect_Field (Afpx_Xref.Reflog.Detail, Protect);
    Afpx.Utils.Protect_Field (Afpx_Xref.Reflog.Checkout, Protect);
    Afpx.Utils.Protect_Field (Afpx_Xref.Reflog.Mark, Protect);
    Afpx.Utils.Protect_Field (Afpx_Xref.Reflog.Search, Protect);
    Afpx.Utils.Protect_Field (Afpx_Xref.Reflog.Reset, Protect);
    Afpx.Utils.Protect_Field (Afpx_Xref.Reflog.Delete, Protect);
  end List_Change;

  -- Handle the reflogs
  function Handle (Root : String; Branch: String) return Boolean  is
    Ptg_Result : Afpx.Result_Rec;
    Result  : Boolean;
    use type Afpx.Field_Range;
  begin

    Reflog.Root := As.U.Tus (Root);
    Target_Branch := As.U.Tus (Branch);

    -- Move to root
    Directory.Change_Current (Root);

    -- Init Afpx
    Init;
    Afpx.Line_List.Delete_List (False);

    -- Encode refs
    Reread;
    Occurence := 1;

    -- Main loop
    loop
      -- Deactivate actions if empty list
      Afpx.Set_Field_Activation (Afpx_Xref.Reflog.Hash,
                                 not Afpx.Line_List.Is_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Reflog.Detail,
                                 not Afpx.Line_List.Is_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Reflog.Checkout,
                                 not Afpx.Line_List.Is_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Reflog.Mark,
                                 not Afpx.Line_List.Is_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Reflog.Search,
                                 not Afpx.Line_List.Is_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Reflog.Reset,
                                 not Afpx.Line_List.Is_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Reflog.Delete,
                                 not Afpx.Line_List.Is_Empty);

      -- Put then get
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
                         Right_Select => True,
                         List_Change_Cb => List_Change'Access);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              Result := Do_Action (Search);
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
            when Afpx_Xref.Reflog.Detail =>
              Result := Do_Action (Detail);
            when Afpx_Xref.Reflog.Checkout =>
              Result := Do_Action (Chkout);
              if Result then
                -- Back to Dir menu
                return True;
              end if;
            when Afpx_Xref.Reflog.Mark =>
              Result := Do_Action (Mark);
            when Afpx_Xref.Reflog.Search =>
              Result := Do_Action (Search);
            when Afpx_Xref.Reflog.Reset =>
              Result := Do_Action (Reset);
              if Result then
                -- Back to Dir menu
                return True;
              end if;
            when Afpx_Xref.Reflog.Delete =>
              Result := Do_Action (Delete);
            when Afpx_Xref.Reflog.Back =>
              -- Back button
              exit;
            when others =>
              null;
          end case;

        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          -- Encode current branch
          Utils.X.Encode_Branch (Afpx_Xref.Reflog.Branch);
      end case;
    end loop;

    -- No big change
    return False;
  end Handle;

end Reflog;

