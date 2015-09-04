with As.U, Afpx.Utils, Language;
with Utils.X, Git_If, Details, Afpx_Xref, Confirm, Error;
package body Cherry is

  -- The cherries
  Cherries : Git_If.Log_List;

  -- The number of Cherries selected
  Nb_Cherries : Natural := 0;

  -------------------
  -- For CONFIRMATION
  -------------------
  -- Confirm list witdh
  Confirm_Width : Afpx.Width_Range;

  -- Encode a commit for confirmation
  procedure Set_Confirm (Line : in out Afpx.Line_Rec;
                         From : in Git_If.Log_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line (
        -- "YYYY-MM-DD HH:MM:SS" -> "YYMMDD HH:MM "
        From.Date(03 .. 04) & From.Date(06 .. 07) & From.Date(09 .. 10) & '-'
          & From.Date(12 .. 13) & From.Date(15 .. 16)
          & (if From.Merged then '>' else ' '),
        -- 1 or 2 lines of comment
        From.Comment(1).Image
          & (if not From.Comment(2).Is_Null then "$" & From.Comment(2).Image
             else ""),
        "", Confirm_Width, Line, False);
  end Set_Confirm;
  procedure Init_Confirm is new Afpx.Utils.Init_List (
    Git_If.Log_Entry_Rec, Git_If.Log_Mng, Set_Confirm, False);

  -----------------------
  -- For CHERRY-PICK mode
  -----------------------
  List_Width : Afpx.Width_Range;
  procedure Set_Cherry (Line : in out Afpx.Line_Rec;
                        From : in Git_If.Log_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line (
        -- "= " or "- "
        (if From.Merged then '=' else 'C') & ' '
        -- "YYYY-MM-DD HH:MM:SS" -> "YYMMDD HH:MM "
        & From.Date(03 .. 04) & From.Date(06 .. 07) & From.Date(09 .. 10) & '-'
        & From.Date(12 .. 13) & From.Date(15 .. 16) & ' ',
        -- 1 or 2 lines of comment
        From.Comment(1).Image
          & (if not From.Comment(2).Is_Null then "$" & From.Comment(2).Image
             else ""),
        "", List_Width, Line, False);
  end Set_Cherry;
  procedure Init_Cherry is new Afpx.Utils.Init_List (
    Git_If.Log_Entry_Rec, Git_If.Log_Mng, Set_Cherry, False);


  procedure Init_Cherries (Branch : in String;
                           Cherries : in out Git_If.Log_List) is
    Cherry : Git_If.Log_Entry_Rec;
    Moved : Boolean;
  begin
    Nb_Cherries := 0;
    Afpx.Line_List.Delete_List;
    -- List Cherries
    Git_If.Cherry_List (Branch, Git_If.Current_Branch, Cherries);
    if Cherries.Is_Empty then
      return;
    end if;

    -- Fill Date and Comment info
    Cherries.Rewind;
    loop
      Cherries.Read (Cherry, Git_If.Log_Mng.Dyn_List.Current);
      if not Cherry.Merged then
        -- Not already merged cherries are init as 'C'
        Nb_Cherries := Nb_Cherries + 1;
      end if;
      Git_If.Info_Commit (Cherry);
      Cherries.Modify (Cherry, Moved => Moved);
      exit when not Moved;
    end loop;

    -- Set Afpx list
    Init_Cherry (Cherries);
  end Init_Cherries;

  -- Read / Write cherry state
  function Read return Character is
    Line : Afpx.Line_Rec;
  begin
    Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
    return Language.Unicode_To_Char (Line.Str(1));
  end Read;
  procedure Write (C : in Character) is
    Line : Afpx.Line_Rec;
  begin
    Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
    if Language.Unicode_To_Char (Line.Str(1)) /= '=' then
      Line.Str(1) := Language.Char_To_Unicode (C);
      Afpx.Line_List.Modify (Line, Afpx.Line_List_Mng.Current);
    end if;
  end Write;

  -- Actions
  type Cherry_Actions is (Add, Remove, Toggle, Reset);
  procedure Cherry_Action (Action : in Cherry_Actions;
                           Left_Sel : in Natural) is
    Char : Character;
    Pos0, Pos1, Pos2 : Positive;
  begin
    -- Nothing if no cherry
    if Afpx.Line_List.Is_Empty then
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
      when Add =>
        for I in Pos1 .. Pos2 loop
          Afpx.Line_List.Move_At (I);
          Char := Read;
          if Char = ' ' then
            Nb_Cherries := Nb_Cherries + 1;
            Write ('C');
          end if;
        end loop;
        Afpx.Line_List.Move_At (Pos0);
      when Remove =>
        for I in Pos1 .. Pos2 loop
          Afpx.Line_List.Move_At (I);
          Char := Read;
          if Char /= ' ' then
            Nb_Cherries := Nb_Cherries - 1;
            Write (' ');
          end if;
        end loop;
        Afpx.Line_List.Move_At (Pos0);
      when Toggle =>
        Char := Read;
        if Char = 'C' then
          Nb_Cherries := Nb_Cherries - 1;
          Write (' ');
        elsif Char = ' ' then
          Nb_Cherries := Nb_Cherries + 1;
          Write ('C');
        end if;
        -- Move to next cherry in time
        if Afpx.Line_List.Check_Move then
          Afpx.Line_List.Move_To;
        end if;
      when Reset =>
        Nb_Cherries := 0;
        for I in 1 .. Afpx.Line_List.List_Length loop
          Afpx.Line_List.Move_At (I);
          Write (' ');
        end loop;
        Afpx.Line_List.Move_At (Pos0);
    end case;
  end Cherry_Action;

  -- Confirm and do the Cherry-pick, return True if completed
  function Cherry_Done (Branch : in String) return Boolean is
    Moved : Boolean;
    Char : Character;
  begin
    -- Nothing if no cherry
    if Afpx.Line_List.Is_Empty then
      return False;
    end if;

    -- Rebuild Cherry list from Afpx '+'
    Afpx.Line_List.Rewind;
    Cherries.Rewind;
    -- Scan all Afpx list and discard ' ' and '='
    loop
      Char := Read;
      if Char /= '+' then
        -- Discard this cherry
        Cherries.Delete (Moved => Moved);
      elsif Cherries.Check_Move (Check_Empty => False) then
        -- Keep this cherry
        Cherries.Move_To;
      end if;
      -- No more entry to process
      exit when not Afpx.Line_List.Check_Move;
      Afpx.Line_List.Move_To;
    end loop;

    -- Return false if empty
    if Cherries.Is_Empty then
      return False;
    end if;

    -- Redo Afpx list of confirm (new width)
    Afpx.Use_Descriptor (Afpx_Xref.Confirm.Dscr_Num);
    Confirm_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
    Init_Confirm (Cherries);

    -- Confirm, return False if not
    if not Confirm ("Cherry pick", "Ok to cherry pick from " & Branch,
                    Show_List => True) then
      return False;
    end if;

    -- Do the cherry pick
    declare
      Result : constant String := Git_If.Cherry_Pick (Cherries);
    begin
      if Result = "" then
        -- Ok
        return True;
      else
        -- Cherry pick failed, the error message starts with the
        --  conflicts
        Error ("Cherry pick from", Branch, Result, False);
        return False;
      end if;
    end;
  end Cherry_Done;

  -- Handle the selection of Commits to cherry-pick
  function Pick (Root, Branch : String) return Boolean is
    -- Afpx stuff
    Get_Handle  : Afpx.Get_Handle_Rec;
    Ptg_Result  : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range;

    -- Search found
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
      Cherry : Git_If.Log_Entry_Rec;
    begin
      -- Read reference hash in Cherries
      Ref := Afpx.Line_List.Get_Position;
      -- This will also save/restore current position
      Cherries.Move_At (Ref);
      Cherries.Read (Cherry, Git_If.Log_Mng.Dyn_List.Current);
      -- Prevent modif in Cherry_Pick
      Details.Handle (Root, Cherry.Hash, False);
      Init;
      Init_Cherry (Cherries);
      Afpx.Update_List (Afpx.Center_Selected);
    end Show_Details;

  begin
    -- Init Afpx
    Init;

    -- Init list
    Init_Cherries (Branch, Cherries);

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
        Utils.X.Center_Field ("Cancel", Afpx_Xref.Cherry.Go);
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
            when Afpx_Xref.Cherry.Apply =>
              -- Add
              Cherry_Action (Add, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Skip =>
              -- Remove
              Cherry_Action (Remove, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Reset =>
              -- Reset
              Cherry_Action (Reset, Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.Cherry.Go =>
              -- Done
              return (if Nb_Cherries = 0 then False
                      else Cherry_Done (Branch));
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

