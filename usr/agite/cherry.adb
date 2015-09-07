with As.U, Afpx.Utils, Dynamic_List;
with Utils.X, Git_If, Details, Afpx_Xref, Confirm, Error, Commit;
package body Cherry is

  package Handle_Commit renames Commit;

  -- A log with cherry status
  type Cherry_Status_List is (Merged, Skip, Apply, Edit, Fixup, Commit);
  type Cherry_Rec is record
    Status : Cherry_Status_List := Merged;
    Commit : Git_If.Log_Entry_Rec;
  end record;
  package Cherries_Mng is new Dynamic_List (Cherry_Rec);

  -- The cherries
  Cherries : Cherries_Mng.Dyn_List.List_Type;

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
         when Skip   => " ",
         when Apply  => "A",
         when Edit   => "E",
         when Fixup  => "F",
         when Commit => "C")
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
                           Align_Afpx : in Boolean) is
    Logs : Git_If.Log_List;
    Cherry : Cherry_Rec;
  begin
    if Align_Afpx then
      Afpx.Line_List.Delete_List;
    end if;
    Nb_Cherries := 0;
    Cherries.Delete_List;
    -- List Cherries
    Git_If.Cherry_List (Branch, Git_If.Current_Branch, Logs);
    if Logs.Is_Empty then
      return;
    end if;

    -- Fill Date and Comment info
    Logs.Rewind;
    loop
      Logs.Read (Cherry.Commit, Git_If.Log_Mng.Dyn_List.Current);
      if Cherry.Commit.Merged then
        Cherry.Status := Merged;
      else
        -- Not already merged cherries are init as 'C'
        Nb_Cherries := Nb_Cherries + 1;
        Cherry.Status := Commit;
      end if;
      -- Fill commit date and comment
      Git_If.Info_Commit (Cherry.Commit);
      Cherries.Insert (Cherry);
      -- Next Log entry
      exit when not Logs.Check_Move;
      Logs.Move_To;
    end loop;

    -- Set Afpx list
    if Align_Afpx then
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
  type Cherry_Actions is (Toggle, Reset, Skip, Apply, Edit, Fixup, Commit,
                          Move_Up, Move_Down);
  procedure Cherry_Action (Action : in Cherry_Actions;
                           Left_Sel : in Natural) is
    Cherry : Cherry_Rec;
    Line : Afpx.Line_Rec;
    Pos0, Pos1, Pos2 : Positive;

    -- Pick some commits
    procedure Pick (Status : in Cherry_Status_List) is
    begin
      -- Set cherries in slice to Status
      for I in Pos1 .. Pos2 loop
        Cherries.Move_At (I);
        Cherry := Read;
        if Cherry.Status /= Merged then
          if Cherry.Status = Skip then
            Nb_Cherries := Nb_Cherries + 1;
          end if;
          Cherry.Status := Status;
          Update (Cherry);
        end if;
      end loop;
      -- Move to next if one selection
      if Pos1 = Pos2 and then Cherries.Check_Move then
        Cherries.Move_To;
        Afpx.Line_List.Move_To;
      end if;
    end Pick;
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
          if Cherry.Status /= Skip then
            Nb_Cherries := Nb_Cherries - 1;
            Cherry.Status := Skip;
          else
            Nb_Cherries := Nb_Cherries + 1;
            Cherry.Status := Commit;
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
          if Cherry.Status /= Merged and then Cherry.Status /= Skip then
            Nb_Cherries := Nb_Cherries - 1;
            Cherry.Status := Skip;
            Update (Cherry);
          end if;
          exit when not Cherries.Check_Move;
          Cherries.Move_To;
        end loop;
        Cherries.Rewind;
        Afpx.Line_List.Rewind;
      when Skip =>
        for I in Pos1 .. Pos2 loop
          Cherries.Move_At (I);
          Cherry := Read;
          if Cherry.Status /= Merged and then Cherry.Status /= Skip then
            Nb_Cherries := Nb_Cherries - 1;
            Cherry.Status := Skip;
            Update (Cherry);
          end if;
        end loop;
        -- Move to next if one selection
        if Pos1 = Pos2 and then Cherries.Check_Move then
          Cherries.Move_To;
          Afpx.Line_List.Move_To;
        end if;
      when Apply =>
        Pick (Apply);
      when Edit =>
        Pick (Edit);
      when Fixup =>
        Pick (Fixup);
      when Commit =>
        Pick (Commit);
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
  -- If Interactive: return True if OK or Error => back to Directory
  --    return False if nothing to do or cancel => back to Branches
  -- Otherwise, return True if completed OK
  function Do_Cherry (Root, Branch : in String;
                      Interactive : in Boolean) return Boolean is
    Cherry : Cherry_Rec;
    Picked : Cherries_Mng.Dyn_List.List_Type;
    Prev_Hash : Git_If.Git_Hash := Git_If.No_Hash;
    Result : As.U.Asu_Us;
  begin
    -- Nothing if no cherry selected
    if Nb_Cherries = 0 then
      return False;
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
        return False;
      end if;
    end if;

    -- Do the cherry pick
    Cherries.Rewind;
    loop
      -- Get a Cherry, apply and possibly commit it
      -- Store Hash of first Apply of a serie
      Cherries.Get (Cherry);
      case Cherry.Status is
        when Merged | Skip =>
          null;
        when Apply =>
          Result := As.U.Tus (Git_If.Cherry_Pick (Cherry.Commit, False));
          if Prev_Hash = Git_If.No_Hash then
            Prev_Hash := Cherry.Commit.Hash;
          end if;
        when Edit =>
          Result := As.U.Tus (Git_If.Cherry_Pick (Cherry.Commit, False));
          -- Will commit with current comment
          Prev_Hash := Cherry.Commit.Hash;
        when Fixup =>
          Result := As.U.Tus (Git_If.Cherry_Pick (Cherry.Commit, False));
          -- Will commit with comment of first of previou Applies, if any
          if Prev_Hash = Git_If.No_Hash then
            Prev_Hash := Cherry.Commit.Hash;
          end if;
        when Commit =>
          Result := As.U.Tus (Git_If.Cherry_Pick (Cherry.Commit, True));
          Prev_Hash := Git_If.No_Hash;
      end case;
      -- Check result of cherry-pick
      if not Result.Is_Null then
        -- Cherry pick failed, the error message starts with the
        --  conflicts
        Error ("Cherry pick from", Branch, Result.Image, False);
        -- Propose manual resolution
        if not Handle_Commit.Handle (Root, Cherry.Commit.Hash) then
          -- User gave up:
          -- Done, back to dir if interactive, error if not interactive
          return Interactive;
        end if;
      elsif Cherry.Status = Edit or else Cherry.Status = Fixup then
        -- Success: Commit if necessary
        if not Handle_Commit.Handle (Root, Prev_Hash) then
          -- Commit Quit => cancel cherry-pick and return to Directory
          return Interactive;
        end if;
        Prev_Hash := Git_If.No_Hash;
      end if;
      exit when Cherries.Is_Empty;
    end loop;
    -- Success
    return True;
  end Do_Cherry;

  -- Handle the selection of Commits to cherry-pick
  -- If Interactive: return True if OK or Error => back to Directory
  --    return False if nothing to do or cancel => back to Branches
  -- Otherwise, return True if completed OK
  function Pick (Root, Branch : String;
                 Interactive : in Boolean) return Boolean is
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

    -- Disable buttons if empty list
    if Cherries.Is_Empty then
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Detail, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Apply, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Edit, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Fixup, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Commit, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.Cherry.Skip, True);
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
              return False;
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
            when Afpx_Xref.Cherry.Skip =>
              -- Skip
              Cherry_Action (Skip, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Apply =>
              -- Apply
              Cherry_Action (Apply, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Edit =>
              -- Edit
              Cherry_Action (Edit, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Fixup =>
              -- Fixup
              Cherry_Action (Fixup, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Commit =>
              -- Commit
              Cherry_Action (Commit, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Reset =>
              -- Reset
              Cherry_Action (Reset, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Move_Up =>
              -- Move Up
              Cherry_Action (Move_Up, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Move_Down =>
              -- Move Down
              Cherry_Action (Move_Down, Ptg_Result.Id_Selected_Right);
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

end Cherry;

