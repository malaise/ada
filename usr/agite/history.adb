with As.U, Con_Io, Afpx.Utils, Normal, Rounds, Language, Directory;
with Utils.X, Config, Details, View, Afpx_Xref, Restore, Checkout, Tags,
     Confirm, Error;
package body History is

  -- List Width
  List_Width : Afpx.Width_Range;
  -- Confirm list witdh
  Confirm_Width : Afpx.Width_Range;

  -- Encode a commit on a given length
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Log_Entry_Rec;
                 Width : in Afpx.Width_Range) is
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
        "", Width, Line, False);
  end Set;

  ----------------
  -- For LIST mode
  ----------------
  procedure Set_List (Line : in out Afpx.Line_Rec;
                      From : in Git_If.Log_Entry_Rec) is
  begin
    Set (Line, From, List_Width);
  end Set_List;
  procedure Init_List is new Afpx.Utils.Init_List (
    Git_If.Log_Entry_Rec, Git_If.Log_Mng, Set_List, False);

  -- To search matching hash in Log
  function List_Hash_Match (Current, Criteria : Git_If.Log_Entry_Rec)
           return Boolean is
  begin
    return Current.Hash = Criteria.Hash;
  end List_Hash_Match;
  function List_Hash_Search is
           new Git_If.Log_Mng.Dyn_List.Search (List_Hash_Match);

  -----------------------
  -- For CHERRY-PICK mode
  -----------------------
  procedure Set_Cherry (Line : in out Afpx.Line_Rec;
                        From : in Git_If.Log_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line (
        -- "= " or "- "
        (if From.Merged then '=' else ' ') & ' '
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

  -- For Confirmation
  procedure Set_Confirm (Line : in out Afpx.Line_Rec;
                         From : in Git_If.Log_Entry_Rec) is
  begin
    Set (Line, From, Confirm_Width);
  end Set_Confirm;
  procedure Init_Confirm is new Afpx.Utils.Init_List (
    Git_If.Log_Entry_Rec, Git_If.Log_Mng, Set_Confirm, False);

  procedure Cherry_Init (Branch : in String;
                         Cherries : in out Git_If.Log_List) is
    Cherry : Git_If.Log_Entry_Rec;
    Moved : Boolean;
  begin
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
      Git_If.Info_Commit (Cherry);
      Cherries.Modify (Cherry, Moved => Moved);
      exit when not Moved;
    end loop;

    -- Set Afpx list
    Init_Cherry (Cherries);
  end Cherry_Init;

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
          Write ('+');
        end loop;
        Afpx.Line_List.Move_At (Pos0);
      when Remove =>
        for I in Pos1 .. Pos2 loop
          Afpx.Line_List.Move_At (I);
          Write (' ');
        end loop;
        Afpx.Line_List.Move_At (Pos0);
      when Toggle =>
        Char := Read;
        if Char = '+' then
          Write (' ');
        elsif Char = ' ' then
          Write ('+');
        end if;
        -- Move to next cherry in time, so backwards
        if Afpx.Line_List.Check_Move (Afpx.Line_List_Mng.Prev) then
          Afpx.Line_List.Move_To (Afpx.Line_List_Mng.Prev);
        end if;
      when Reset =>
        for I in 1 .. Afpx.Line_List.List_Length loop
          Afpx.Line_List.Move_At (I);
          Write (' ');
        end loop;
        Afpx.Line_List.Move_At (Pos0);
    end case;
  end Cherry_Action;

  -- Confirm and do the Cherry-pick, return True if completed
  function Cherry_Done (Branch : in String;
                        Cherries : in out Git_If.Log_List) return Boolean is
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

  ------------------
  -- Common HANDLING
  ------------------

  -- Handle the history of a file or dir
  -- Or the cherry-pick from a branch
  function Handle (Cherry_Pick : in Boolean;
                   Root, Path, Name : in String;
                   Is_File : in Boolean;
                   Hash : in Git_If.Git_Hash := Git_If.No_Hash)
           return Boolean is
    -- Afpx stuff
    Get_Handle  : Afpx.Get_Handle_Rec;
    Ptg_Result  : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range;

    -- The log
    Logs : Git_If.Log_List;
    Log : Git_If.Log_Entry_Rec;

    -- Search found
    Dummy : Boolean;

    -- Init Afpx
    procedure Init is
    begin
      Afpx.Use_Descriptor (Afpx_Xref.History.Dscr_Num);
      Get_Handle := (others => <>);
      -- List characteristics
      List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
      -- Encode current branch
      Utils.X.Encode_Branch (Afpx_Xref.History.Branch);

      if Cherry_Pick then
        -- Encode Title
        Utils.X.Center_Field ("Cherry pick from " & Path,
                              Afpx_Xref.History.Title,
                              Keep_Head => False);
        -- Encode Root
        Utils.X.Encode_Field (Root, Afpx_Xref.History.File);
        -- Disable View and diff
        Afpx.Set_Field_Activation (Afpx_Xref.History.View, False);
        Afpx.Set_Field_Activation (Afpx_Xref.History.Diff, False);
        -- Disable scroll and list indicators
        Afpx.Set_Field_Activation (Afpx_Xref.History.Scroll, False);
        Afpx.Set_Field_Activation (Afpx_Xref.History.Tpercent, False);
        Afpx.Set_Field_Activation (Afpx_Xref.History.Percent, False);
        Afpx.Set_Field_Activation (Afpx_Xref.History.Tleftsel, False);
        Afpx.Set_Field_Activation (Afpx_Xref.History.Leftsel, False);
        Afpx.Set_Field_Activation (Afpx_Xref.History.Trightsel, False);
        Afpx.Set_Field_Activation (Afpx_Xref.History.Rightsel, False);
        -- Change button names
        Utils.X.Center_Field ("Add", Afpx_Xref.History.Restore);
        Utils.X.Center_Field ("Remove", Afpx_Xref.History.Checkout);
        Utils.X.Center_Field ("Reset", Afpx_Xref.History.Tag);
        Utils.X.Center_Field ("Done", Afpx_Xref.History.Back);
      else
        -- Encode file/dir
        Utils.X.Encode_Field ((if Is_File then Path & Name
                              elsif Name /= "" then Path & Name & "/"
                              elsif Path /= "" then Path
                              else "/"),
                              Afpx_Xref.History.File);

        -- Protect buttons View and restore on dirs
        Afpx.Utils.Protect_Field (Afpx_Xref.History.View, not Is_File);
        Afpx.Utils.Protect_Field (Afpx_Xref.History.Restore, not Is_File);
      end if;
    end Init;

    -- Show delta from current in list to comp
    procedure Show_Delta (Ref : in Natural) is
      Comp : Positive;
      Ref_Hash, Comp_Hash : Git_If.Git_Hash;
    begin
      -- Save position in List
      Comp := Afpx.Line_List.Get_Position;

      -- Read reference hash in Logs
      if Ref = 0 then
        -- Only Left selection
        Ref_Hash := Git_If.No_Hash;
      else
        Logs.Move_At (Ref);
        Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
        Ref_Hash := Log.Hash;
      end if;

      -- Move to Comp and read comp hash in Logs
      Logs.Move_At (Comp);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      Comp_Hash := Log.Hash;

      -- Restore position in List
      Afpx.Line_List.Move_At (Comp);

      -- Call delta
      if Ref_Hash = Git_If.No_Hash then
        -- Only Left selection: Hash^ and Hash
        Git_If.Launch_Delta (Config.Differator, Root & Path & Name,
                             Comp_Hash & "^", Comp_Hash);
      else
        Git_If.Launch_Delta (Config.Differator, Root & Path & Name,
                             Ref_Hash, Comp_Hash);
      end if;
    end Show_Delta;

    -- Do a restore
    procedure Do_Restore is
      Pos : Positive;
    begin
      -- Save position in List and read it
      Pos := Afpx.Line_List.Get_Position;
      Logs.Move_At (Pos);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      -- Restore file
      Restore (Root, Path & Name, Log.Hash, null);
      -- Restore screen
      Init;
      Init_List (Logs);
      Afpx.Line_List.Move_At (Pos);
      Afpx.Update_List (Afpx.Center_Selected);
    end Do_Restore;

    -- Do a checkout
    function Do_Checkout return Boolean is
      Pos : Positive;
      Commit : As.U.Asu_Us;
    begin
      -- Save position in List and read it
      Pos := Afpx.Line_List.Get_Position;
      Logs.Move_At (Pos);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      Commit := As.U.Tus (Language.Unicode_To_String (
          Afpx.Line_List.Access_Current.Str(
              1 .. Afpx.Line_List.Access_Current.Len)));

      -- Checkout (success will lead to return to Directory)
      if Checkout.Handle (Root, "commit", Commit.Image, Log.Hash) then
        return True;
      else
        -- Restore screen
        Init;
        Init_List (Logs);
        Afpx.Line_List.Move_At (Pos);
        Afpx.Update_List (Afpx.Center_Selected);
        return False;
      end if;
    end Do_Checkout;

    -- Do a tag
    procedure Do_Tag is
      Pos : Positive;
    begin
      -- Save position in List and read it
      Pos := Afpx.Line_List.Get_Position;
      Logs.Move_At (Pos);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      -- Restore file
      Tags.Add (Log.Hash);
      -- Restore screen
      Init;
      Init_List (Logs);
      Afpx.Line_List.Move_At (Pos);
      Afpx.Update_List (Afpx.Center_Selected);
    end Do_Tag;

    -- View file or commit details
    type Show_List is (Show_View, Show_Details);
    procedure Show (What : in Show_List) is
      Ref : Positive;
    begin
      -- Read reference hash in Logs
      Ref := Afpx.Line_List.Get_Position;
      -- This will also save/restore current position
      Logs.Move_At (Ref);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      case What is
        when Show_View =>
          View (Path & Name, Log.Hash);
        when Show_Details =>
          -- Prevent modif in Cherry_Pick
          Details.Handle (Root, Log.Hash, not Cherry_Pick);
          Init;
          if Cherry_Pick then
            Init_Cherry (Logs);
          else
            Init_List (Logs);
          end if;
          Afpx.Update_List (Afpx.Center_Selected);
      end case;
    end Show;

    -- Update the list status
    procedure List_Change (Unused_Action : in Afpx.List_Change_List;
                           Status : in Afpx.List_Status_Rec) is
      Percent : Afpx.Percent_Range;
      Row : Con_Io.Row_Range;
    begin
      -- Put percent value and "scroll bar"
      Percent := Afpx.Get_List_Percent;
      Afpx.Clear_Field (Afpx_Xref.History.Scroll);
      if Percent /= 0 then
        Afpx.Encode_Field (Afpx_Xref.History.Percent, (0, 0),
                           Normal (Percent, 3, True));
        -- 0 <-> 1% and Height-1 <-> 100%
        -- (Percent-1)/99 = Row/(Height-1)
        Row := Con_Io.Row_Range(
          Rounds.Roundiv ((Afpx.Get_Field_Height (Afpx_Xref.History.Scroll) - 1)
                          * (Percent - 1), 99));
        Afpx.Encode_Field (Afpx_Xref.History.Scroll,
                          (Row => Row, Col => 0),
                          "-");
      else
        Afpx.Encode_Field (Afpx_Xref.History.Percent, (0, 0), "-");
      end if;
      -- Put Ids selected
      Afpx.Encode_Field (Afpx_Xref.History.Leftsel, (0, 0),
           Normal (Status.Ids_Selected(Afpx.List_Left),
                   Afpx.Get_Field_Width (Afpx_Xref.History.Leftsel), False));
      Afpx.Encode_Field (Afpx_Xref.History.Rightsel, (0, 0),
           Normal (Status.Ids_Selected(Afpx.List_Right),
                   Afpx.Get_Field_Width (Afpx_Xref.History.Rightsel), False));
    end List_Change;

    -- Move according to click row in scroll field
    procedure Move_At_Scroll (Row : in Con_Io.Row_Range) is
      Percent : Afpx.Percent_Range;
      Saved_Position, Position : Natural;
    begin
      if Afpx.Line_List.Is_Empty then
        return;
      end if;
      Saved_Position := Afpx.Line_List.Get_Position;
      -- 0 <-> 1% and Height-1 <-> 100%
      -- (Percent-1)/99 = Row/(Height-1)
      Percent :=
          Rounds.Roundiv (Row * 99,
                          Afpx.Get_Field_Height (Afpx_Xref.History.Scroll) - 1)
          + 1;
      Position := Afpx.Get_List_Index (Percent);
      if Position = 0 then
        return;
      end if;
      Afpx.Line_List.Move_At (Position);
      Afpx.Update_List (Afpx.Top_Selected);
      Afpx.Line_List.Move_At (Saved_Position);
    end Move_At_Scroll;

  begin
    -- Init Afpx
    Init;

    -- Init list
    if Cherry_Pick then
      -- Path is the Ref branch
      Cherry_Init (Path, Logs);
    else
      -- Get history list
      if Path = "" and then Name = ""
      and then Directory.Get_Current = Directory.Normalize_Path (Root) then
        -- Log in (the root dir of) a bare repository
        --  fails if we provide the full (Root) path
        --  but is OK with '.'
        -- Use '.' if we are in root and target dir is root
        Git_If.List_Log (".", False, Logs);
      else
        -- Log, following renames only if file
        Git_If.List_Log (Root & Path & Name, Name /= "", Logs);
      end if;

      if Hash /= Git_If.No_Hash then
        -- Set current to Hash provided
        Log.Hash := Hash;
        Dummy := List_Hash_Search (Logs, Log,
                     From => Git_If.Log_Mng.Dyn_List.Absolute);
      end if;
      -- Encode history
      Init_List (Logs);
    end if;

    -- Disable buttons if empty list
    if Logs.Is_Empty then
      Afpx.Utils.Protect_Field (Afpx_Xref.History.View, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Details, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Restore, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Checkout, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Tag, True);
    end if;
    if Logs.List_Length <= 1 then
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Diff, True);
    end if;

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result, True,
                         List_Change_Cb => List_Change'Access);

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
              if Cherry_Pick then
                Cherry_Action (Toggle, Ptg_Result.Id_Selected_Right);
              elsif Is_File then
                Show (Show_View);
              end if;
            when Afpx_Xref.History.View =>
              -- View => View if file
              if Is_File then
                Show (Show_View);
              end if;
            when Utils.X.List_Scroll_Fld_Range'First ..
                 Utils.X.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.Utils.Scroll(
                 Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
            when Afpx_Xref.History.Scroll =>
              -- Scroll bar
              Move_At_Scroll (Ptg_Result.Release_Pos.Row);
            when Afpx_Xref.History.Diff =>
              -- Diff
              Show_Delta (Ptg_Result.Id_Selected_Right);
            when Afpx_Xref.History.Details =>
              -- Details
              Show (Show_Details);
            when Afpx_Xref.History.Restore =>
              if Cherry_Pick then
                -- Add
                Cherry_Action (Add, Ptg_Result.Id_Selected_Right);
              else
                -- Restore
                Do_Restore;
              end if;
            when Afpx_Xref.History.Checkout =>
              if Cherry_Pick then
                -- Remove
                Cherry_Action (Remove, Ptg_Result.Id_Selected_Right);
              elsif Do_Checkout then
                -- Checkout
                return True;
              end if;
            when Afpx_Xref.History.Tag =>
              if Cherry_Pick then
                -- Reset
                Cherry_Action (Reset, Ptg_Result.Id_Selected_Right);
              else
                -- Tag
                Do_Tag;
              end if;
            when Afpx_Xref.History.Back =>
              if Cherry_Pick then
                -- Done
                return Cherry_Done (Path, Logs);
              else
                -- Back
                return False;
              end if;
            when others =>
              -- Other button?
              null;
          end case;

        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          -- Encode current branch
          Utils.X.Encode_Branch (Afpx_Xref.History.Branch);
      end case;
    end loop;

  end Handle;

  -- Handle the history of a file or dir
  procedure List (Root, Path, Name : in String;
                    Is_File : in Boolean;
                    Hash : in Git_If.Git_Hash := Git_If.No_Hash) is
    Dummy : Boolean;
  begin
    Dummy := Handle (False, Root, Path, Name, Is_File, Hash);
  end List;

  -- Handle the selection of Commits to cherry-pick
  function Cherry_Pick (Root, Branch : String) return Boolean is
  begin
    return Handle (True, Root, Branch, "", False, Git_If.No_Hash);
  end Cherry_Pick;

end History;

