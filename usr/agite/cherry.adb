with As.U, Afpx.Utils, Dynamic_List;
with Utils.X, Git_If, Details, Afpx_Xref, Confirm, Error, Commit;
package body Cherry is

  -- A log with cherry status
  type Cherry_Status_List is (Merged, Pick, Reword, Edit, Squash, Fixup,
                              Drop);
  type Cherry_Rec is record
    Status : Cherry_Status_List := Merged;
    Commit : Git_If.Log_Entry_Rec;
  end record;
  package Cherries_Mng is new Dynamic_List (Cherry_Rec);
  -- Search by Hash
  function Match (Current, Criteria : Cherry_Rec) return Boolean is
  begin
    return Current.Commit.Hash = Criteria.Commit.Hash;
  end Match;
  function Search_Hash is new Cherries_Mng.Dyn_List.Search (Match);

  -- The cherries
  Cherries : Cherries_Mng.Dyn_List.List_Type;
  -- Memory of previous Cherry operation: the Cherries and branch names
  From_Branch, To_Branch : As.U.Asu_Us;

  -- The number of Cherries selected
  Nb_Cherries : Natural := 0;

  ---------------------
  -- Encoding Afpx line
  ---------------------
  function Image1 (From : in Cherry_Rec) return String is
  begin
    return
      (case From.Status is
         when Merged => "=",
         when Pick   => "P",
         when Reword => "R",
         when Edit   => "E",
         when Squash => "S",
         when Fixup  => "F",
         when Drop   => " ")
      & " "
      -- "YYYY-MM-DD HH:MM:SS" -> "YYMMDD HH:MM "
      & From.Commit.Date(03 .. 04) & From.Commit.Date(06 .. 07)
      & From.Commit.Date(09 .. 10) & '-'
      & From.Commit.Date(12 .. 13) & From.Commit.Date(15 .. 16) & ' ';
  end Image1;
  function Image2 (From : in Cherry_Rec) return String is
  begin
    return
      -- 1 or 2 lines of comment
      From.Commit.Comment(1).Image
      & (if not From.Commit.Comment(2).Is_Null then
           "$" & From.Commit.Comment(2).Image
         else "");
  end Image2;

  -- Confirm list
  Confirm_Width : Afpx.Width_Range;
  procedure Set_Confirm (Line : in out Afpx.Line_Rec;
                         From : in Cherry_Rec) is
  begin
    Afpx.Utils.Encode_Line (Image1 (From), Image2 (From), "",
                            Confirm_Width, Line, False);
  end Set_Confirm;
  procedure Init_Confirm is new Afpx.Utils.Init_List (
    Cherry_Rec, Cherries_Mng, Set_Confirm, False);

  -- Cherry list
  List_Width : Afpx.Width_Range;
  procedure Set_Cherry (Line : in out Afpx.Line_Rec;
                        From : in Cherry_Rec) is
  begin
    Afpx.Utils.Encode_Line (Image1 (From), Image2 (From), "",
                            List_Width, Line, False);
  end Set_Cherry;
  procedure Init_Cherry is new Afpx.Utils.Init_List (
    Cherry_Rec, Cherries_Mng, Set_Cherry, False);

  -- Get list of cherries
  procedure Init_Cherries (Branch : in String;
                           Interactive : in Boolean) is
    Logs : Git_If.Log_List;
    Cherry, Old_Cherry : Cherry_Rec;
    Curr_Branch : constant String := Git_If.Current_Branch;
    Old_Cherries : Cherries_Mng.Dyn_List.List_Type;
    Merge : Boolean;
  begin
    -- Check if same branches as previously uncompleted cherry-pick
    Merge := Branch = From_Branch.Image
             and then Curr_Branch = To_Branch.Image;
    -- Confirm reuse
    Merge := Merge and then Confirm (
      "Restart cherry pick from branch " & Branch,
      "into " & Curr_Branch, Ok_Cancel => False);
    if Merge then
      Old_Cherries.Insert_Copy (Cherries);
    end if;

    -- Init list
    if Interactive then
      Afpx.Line_List.Delete_List;
    end if;
    -- List Cherries
    Nb_Cherries := 0;
    Git_If.Cherry_List (Branch, Curr_Branch, Logs);
    if Logs.Is_Empty then
      return;
    end if;

    -- Fill Status, Date and Comment info
    Cherries.Delete_List;
    Logs.Rewind;
    loop
      Logs.Read (Cherry.Commit, Git_If.Log_Mng.Dyn_List.Current);
      if Cherry.Commit.Merged then
        Cherry.Status := Merged;
      else
        -- Not already merged cherries are init as 'Pick'
        Nb_Cherries := Nb_Cherries + 1;
        Cherry.Status := Pick;
      end if;
      -- Fill commit date and comment
      Git_If.Info_Commit (Cherry.Commit);
      Cherries.Insert (Cherry);
      -- Next Log entry
      exit when not Logs.Check_Move;
      Logs.Move_To;
    end loop;

    if Merge then
      -- Any non-merged of the new list that exists non-merged in the old
      --  list gets the old status
      Cherries.Rewind;
      loop
        Cherry := Cherries.Access_Current.all;
        if Cherry.Status /= Merged
        and then Search_Hash (Old_Cherries, Cherry,
                        From => Cherries_Mng.Dyn_List.Current_Absolute) then
          Old_Cherry := Old_Cherries.Access_Current.all;
          if Old_Cherry.Status /= Merged then
            -- Copy status from previous list
            Cherry.Status := Old_Cherry.Status;
            Cherries.Modify (Cherry, Cherries_Mng.Dyn_List.Current);
            -- Update Nb_Cherries ifi Pick -> Drop
            if Old_Cherry.Status = Drop then
              Nb_Cherries := Nb_Cherries - 1;
            end if;
          end if;
        end if;
        exit when not Cherries.Check_Move;
        Cherries.Move_To;
      end loop;
    end if;

    -- Set Afpx list
    if Interactive then
      Init_Cherry (Cherries);
      Afpx.Line_List.Rewind;
    end if;
    Cherries.Rewind;
  end Init_Cherries;

  -- Write cherry in list and update Afpx line
  procedure Update (Cherry : in Cherry_Rec) is
    Line : Afpx.Line_Rec;
  begin
    Cherries.Modify (Cherry, Cherries_Mng.Dyn_List.Current);
    Set_Cherry (Line, Cherry);
    Afpx.Line_List.Move_At (Cherries.Get_Position);
    Afpx.Line_List.Modify (Line, Afpx.Line_List_Mng.Current);
  end Update;
  -- Read current cherry
  function Read return Cherry_Rec is
    Cherry : Cherry_Rec;
  begin
    Cherries.Read (Cherry, Cherries_Mng.Dyn_List.Current);
    return Cherry;
  end Read;

  -- Actions
  type Cherry_Actions is (Toggle, Drop, Pick, Reword, Edit, Fixup, Squash,
                          Move_Up, Move_Down, Reset);
  procedure Cherry_Action (Action : in Cherry_Actions;
                           Left_Sel : in Natural) is
    Cherry : Cherry_Rec;
    Line : Afpx.Line_Rec;
    Pos0, Pos1, Pos2 : Positive;

    -- Pick some commits
    procedure Apply (Status : in Cherry_Status_List) is
    begin
      -- Set cherries in slice to Status
      for I in Pos1 .. Pos2 loop
        Cherries.Move_At (I);
        Cherry := Read;
        if Cherry.Status /= Merged then
          if Cherry.Status = Drop then
            Nb_Cherries := Nb_Cherries + 1;
          end if;
          Cherry.Status := Status;
          Update (Cherry);
        end if;
      end loop;
      -- Move to next if one selection
      if Pos1 /= Pos2 then
        Cherries.Move_At (Pos0);
        Afpx.Line_List.Move_At (Pos0);
      elsif Pos1 = Pos2 and then Cherries.Check_Move then
        Cherries.Move_To;
        Afpx.Line_List.Move_To;
      end if;
    end Apply;
  begin
    -- Nothing if no cherry
    if Cherries.Is_Empty then
      return;
    end if;
    -- Save position and Set both indexes, Pos1 <= Pos2
    Pos0 := Afpx.Line_List.Get_Position;
    if Left_Sel = 0 then
      Pos1 := Pos0;
      Pos2 := Pos0;
    elsif Left_Sel > Pos0 then
      Pos1 := Pos0;
      Pos2 := Left_Sel;
    else
      Pos1 := Left_Sel;
      Pos2 := Pos0;
    end if;

    -- Perform action
    case Action is
      when Toggle =>
        Cherries.Move_At (Pos0);
        Cherry := Read;
        if Cherry.Status /= Merged then
          if Cherry.Status /= Drop then
            Nb_Cherries := Nb_Cherries - 1;
            Cherry.Status := Drop;
          else
            Nb_Cherries := Nb_Cherries + 1;
            Cherry.Status := Pick;
          end if;
          Update (Cherry);
          -- Move to next cherry
          if Cherries.Check_Move then
            Cherries.Move_To;
            Afpx.Line_List.Move_To;
          end if;
        end if;
      when Reset =>
        Cherries.Rewind;
        loop
          Cherry := Read;
          if Cherry.Status /= Merged and then Cherry.Status /= Drop then
            Nb_Cherries := Nb_Cherries - 1;
            Cherry.Status := Drop;
            Update (Cherry);
          end if;
          exit when not Cherries.Check_Move;
          Cherries.Move_To;
        end loop;
        Cherries.Rewind;
        Afpx.Line_List.Rewind;
      when Drop =>
        for I in Pos1 .. Pos2 loop
          Cherries.Move_At (I);
          Cherry := Read;
          if Cherry.Status /= Merged and then Cherry.Status /= Drop then
            Nb_Cherries := Nb_Cherries - 1;
            Cherry.Status := Drop;
            Update (Cherry);
          end if;
        end loop;
        -- Move to next if one selection
        if Pos1 /= Pos2 then
          Cherries.Move_At (Pos0);
          Afpx.Line_List.Move_At (Pos0);
        elsif Pos1 = Pos2 and then Cherries.Check_Move then
          Cherries.Move_To;
          Afpx.Line_List.Move_To;
        end if;
      when Pick =>
        Apply (Pick);
      when Reword =>
        Apply (Reword);
      when Edit =>
        Apply (Edit);
      when Squash =>
        Apply (Squash);
      when Fixup =>
        Apply (Fixup);
      when Move_Up =>
        if Pos1 = 1 then
          return;
        end if;
        -- Get and move to previous
        Cherries.Move_At (Pos0);
        Cherries.Get (Cherry, Cherries_Mng.Dyn_List.Prev);
        Afpx.Line_List.Get (Line, Afpx.Line_List_Mng.Prev);
        -- Insert before previous
        Cherries.Insert (Cherry, Cherries_Mng.Dyn_List.Prev);
        Afpx.Line_List.Insert (Line, Afpx.Line_List_Mng.Prev);
      when Move_Down =>
        Cherries.Move_At (Pos0);
        if Pos1 = Cherries.List_Length then
          return;
        end if;
        -- Get and move to next
        Cherries.Get (Cherry, Cherries_Mng.Dyn_List.Next);
        Afpx.Line_List.Get (Line, Afpx.Line_List_Mng.Next);
        -- Insert after next
        Cherries.Insert (Cherry, Cherries_Mng.Dyn_List.Next);
        Afpx.Line_List.Insert (Line, Afpx.Line_List_Mng.Next);

    end case;
  end Cherry_Action;

  -- Confirm (if Interactive) and do the Cherry-pick,
  function Do_Cherry (Root, Branch : in String;
                      Interactive : in Boolean) return Result_List is
    Cherry, First_Cherry : Cherry_Rec;
    Next_Meld : Boolean;
    Picked, Tmp_List : Cherries_Mng.Dyn_List.List_Type;
    Comment_Hash : Git_If.Git_Hash;
    Result : As.U.Asu_Us;
  begin
    -- Nothing if no cherry selected
    if Nb_Cherries = 0 then
      Reset;
      return Cancelled;
    end if;

    if Interactive then
      -- Redo Afpx list of confirm (new width, only not merged)
      Cherries.Rewind;
      loop
        Cherry := Read;
        if Cherry.Status /= Merged then
          Picked.Insert (Cherry);
        end if;
        exit when not Cherries.Check_Move;
        Cherries.Move_To;
      end loop;
      Afpx.Use_Descriptor (Afpx_Xref.Confirm.Dscr_Num);
      Confirm_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
      Init_Confirm (Picked);
      Picked.Delete_List;

      -- Confirm, return False if not
      if not Confirm ("Cherry pick from branch " & Branch,
                      "into current branch " & Git_If.Current_Branch,
                      Show_List => True) then
        return Cancelled;
      end if;
    end if;

    -- Save branches
    From_Branch := As.U.Tus (Branch);
    To_Branch := As.U.Tus (Git_If.Current_Branch);

    -- Do the cherry pick
    Cherries.Rewind;
    -- First_Cherry status is: Drop=Unset, Pick=Set, Edit=In_Commit
    First_Cherry.Status := Drop;
    loop
      -- Get a Cherry and apply, possibly keep edit/keep comment,
      --  possibly commit it
      Cherries.Get (Cherry);

      -- Look at following cherry to see if it is a meld
      Next_Meld := False;
      if not Cherries.Is_Empty then
        Tmp_List.Unchecked_Assign (Cherries);
        loop
          case Tmp_List.Access_Current.Status is
            when Merged | Drop =>
              -- Look for next
              exit when not Tmp_List.Check_Move;
              Tmp_List.Move_To;
            when Squash | Fixup =>
              Next_Meld := True;
              exit;
            when Pick | Reword | Edit =>
              Next_Meld := False;
              exit;
          end case;
        end loop;
      end if;

      -- Init comment for Reword, Edit or manual resolution
      Comment_Hash :=
          (if Cherry.Status = Fixup then
             -- Try to reuse first commit
             (if First_Cherry.Status = Drop then Cherry.Commit.Hash
              elsif First_Cherry.Status = Pick then First_Cherry.Commit.Hash
              else Git_If.No_Hash)
           else Cherry.Commit.Hash);

      if Next_Meld then
        -- Next is Squash or Fixup: don't commit current
        -- Cherry-pick without committing
        Result := As.U.Tus (Git_If.Cherry_Pick (Cherry.Commit, False));
        if not Result.Is_Null then
          -- Cherry pick failed, the error message starts with the
          --  conflicts
          Error ("Cherry pick from", Branch, Result.Image, False);
          -- Propose manual resolution
          if not Commit.Handle (Root, Comment_Hash) then
            -- User gave up: error
            return Error;
          end if;
        else
          -- Cherry-pick without committing OK
          case Cherry.Status is
            when Merged | Drop =>
              null;
            when Pick =>
              -- Keep this comment in case next is Fixup
              First_Cherry := Cherry;
              First_Cherry.Status := Pick;
            when Reword | Edit =>
              -- Launch Commit screen, allow modif of content if Edit,
              -- allow committing if Edit (for splitting)
              if not Commit.Handle (Root, Comment_Hash, Cherry.Status = Edit,
                  (if Cherry.Status = Edit then Commit.Allow
                   else Commit.Forbid)) then
                -- User gave up
                return Error;
              end if;
              -- Keep comment in case next is Fixup
              First_Cherry.Status := Edit;
            when Squash =>
              -- Cherry already applied not committed, update comment
              --  in case next is Fixup
              First_Cherry := Cherry;
              First_Cherry.Status := Pick;
            when Fixup =>
              -- Cherry already applied not committed
              --  keep comment if already set
              if First_Cherry.Status = Drop then
                First_Cherry := Cherry;
                First_Cherry.Status := Pick;
              end if;
          end case;
        end if;
      else
        -- Next is neither Squash nor Fixup: commit
        case Cherry.Status is
          when Merged | Drop =>
            null;
          when Pick =>
            Result := As.U.Tus (Git_If.Cherry_Pick (Cherry.Commit, True));
            if not Result.Is_Null then
              -- Cherry pick failed, the error message starts with the
              --  conflicts
              Error ("Cherry pick from", Branch, Result.Image, False);
              -- Propose manual resolution
              if not Commit.Handle (Root, Comment_Hash) then
                -- User gave up
                return Error;
              end if;
            end if;
          when Reword | Edit =>
            Result := As.U.Tus (Git_If.Cherry_Pick (Cherry.Commit, False));
            if not Result.Is_Null then
              -- Cherry pick failed, the error message starts with the
              --  conflicts
              Error ("Cherry pick from", Branch, Result.Image, False);
              -- Propose manual resolution
              if not Commit.Handle (Root, Comment_Hash) then
                -- User gave up
                return Error;
              end if;
            else
              -- Cherry pick OK, commit it
              -- Require commit
              if not Commit.Handle (Root, Cherry.Commit.Hash,
                                    Cherry.Status = Edit, Commit.Require) then
                -- User gave up
                return Error;
              end if;
            end if;
          when Squash | Fixup =>
            Result := As.U.Tus (Git_If.Cherry_Pick (Cherry.Commit, False));
            if not Result.Is_Null then
              -- Cherry pick failed, the error message starts with the
              --  conflicts
              Error ("Cherry pick from", Branch, Result.Image, False);
              -- Propose manual resolution
              if not Commit.Handle (Root, Comment_Hash) then
                -- User gave up
                return Error;
              end if;
            else
              -- Cherry-pick OK, commit with current or previous comment
              Result := As.U.Tus (Git_If.Do_Commit (
                  Commit.Get_Comment (Comment_Hash)));
              if not Result.Is_Null then
                -- Commit failed, propose manual resolution
                Error ("Commit cherry from", Branch, Result.Image, False);
                -- Propose manual resolution
                if not Commit.Handle (Root, Comment_Hash) then
                  -- User gave up: error if non interactive
                  return Error;
                end if;
              end if;
            end if;
        end case;
        -- In any case, current comment is not kept further on
        First_Cherry.Status := Drop;
      end if;

      exit when Cherries.Is_Empty;
    end loop;

    -- Success
    Reset;
    return Ok;
  end Do_Cherry;

  -- Handle the selection of Commits to cherry-pick
  function Pick (Root, Branch : String;
                 Interactive : in Boolean) return Result_List is
    -- Afpx stuff
    Get_Handle  : Afpx.Get_Handle_Rec;
    Ptg_Result  : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range;

    -- Search found, or result of automatic cherry-pick
    Dummy : Boolean;

    -- Init Afpx
    procedure Init is
    begin
      Afpx.Use_Descriptor (Afpx_Xref.Cherry.Dscr_Num);
      Get_Handle := (others => <>);
      -- List characteristics
      List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
      -- Encode current branch
      Utils.X.Encode_Branch (Afpx_Xref.Cherry.Branch);

      -- Encode Title
      Utils.X.Center_Field ("Cherry pick from " & Branch,
                            Afpx_Xref.Cherry.Title,
                            Keep_Head => False);
      -- Encode Root
      Utils.X.Encode_Field (Root, Afpx_Xref.Cherry.Root);
    end Init;

    -- View commit details
    procedure Show_Details is
      Ref : Positive;
      Cherry : Cherry_Rec;
    begin
      -- Read reference hash in Cherries
      Ref := Afpx.Line_List.Get_Position;
      -- This will also save/restore current position
      Cherries.Move_At (Ref);
      Cherries.Read (Cherry, Cherries_Mng.Dyn_List.Current);
      -- Prevent modif in Cherry_Pick
      Details.Handle (Root, Cherry.Commit.Hash, False);
      Init;
      Init_Cherry (Cherries);
      Afpx.Update_List (Afpx.Center_Selected);
    end Show_Details;

  begin
    if not Interactive then
      -- Automatically pick and commit all cherries
      Init_Cherries (Branch, False);
      return Do_Cherry (Root, Branch, False);
    end if;

    -- Init Afpx
    Init;

    -- Init list
    Init_Cherries (Branch, True);
    -- May have called Confirm
    Init;

    -- Disable buttons if empty list
    if Cherries.Is_Empty then
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Detail, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Pick, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Reword, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Edit, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Squash, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Fixup, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Drop, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Reset, True);
    end if;
    if Cherries.List_Length <= 1 then
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Move_Up, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Move_Down, True);
    end if;

    -- Main loop
    loop
      if Nb_Cherries = 0 then
        Afpx.Encode_Field (Afpx_Xref.Cherry.Go, (1, 1), "Cancel");
      else
        Afpx.Reset_Field (Afpx_Xref.Cherry.Go, Reset_Colors => False);
      end if;
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result, True);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              null;
            when Afpx.Escape_Key =>
              -- Back
              return Cancelled;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Afpx.List_Field_No  =>
              -- Double click or View => View if List file, Toggle cherry
              Cherry_Action (Toggle, Ptg_Result.Id_Selected_Right);
            when Utils.X.List_Scroll_Fld_Range'First ..
                 Utils.X.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.Utils.Scroll(
                 Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
            when Afpx_Xref.Cherry.Detail =>
              -- Details
              Show_Details;
            when Afpx_Xref.Cherry.Pick =>
              -- Pick
              Cherry_Action (Pick, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Reword =>
              -- Reword
              Cherry_Action (Reword, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Edit =>
              -- Edit
              Cherry_Action (Edit, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Squash =>
              -- Squash
              Cherry_Action (Squash, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Fixup =>
              -- Fixup
              Cherry_Action (Fixup, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Drop =>
              -- Drop
              Cherry_Action (Drop, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Move_Up =>
              -- Move Up
              Cherry_Action (Move_Up, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Move_Down =>
              -- Move Down
              Cherry_Action (Move_Down, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Reset =>
              -- Reset
              Cherry_Action (Reset, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Go =>
              -- Done
              return Do_Cherry (Root, Branch, True);
            when others =>
              -- Other button?
              null;
          end case;

        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          -- Encode current branch
          Utils.X.Encode_Branch (Afpx_Xref.Cherry.Branch);
      end case;
    end loop;

  end Pick;

  -- Reset memory
  procedure Reset is
  begin
    From_Branch.Set_Null;
    To_Branch.Set_Null;
    Cherries.Delete_List;
  end Reset;
end Cherry;

