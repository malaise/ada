with As.U, Con_Io, Afpx.Utils, Normal, Normalization, Rounds, Language,
     Directory, Str_Util, Aski;
with Utils.X, Utils.Store, Config, Details, View, Afpx_Xref, Restore, Checkout,
     Tags, Branch, Confirm_Diff_Dir, Reset, Error;
package body History is

  package Branches renames Branch;

  -- List Width
  List_Width : Afpx.Width_Range;

  -- Full image of a commit: Date, then Comment
  function Image1 (Log : Git_If.Log_Entry_Rec) return String is
    -- "YYYY-MM-DD HH:MM:SS" -> "YYMMDD HH:MM "
    (Log.Date(03 .. 04) & Log.Date(06 .. 07) & Log.Date(09 .. 10) & '-'
   & Log.Date(12 .. 13) & Log.Date(15 .. 16));
  function Image2 (Log : Git_If.Log_Entry_Rec) return String is
    -- 1 or 2 lines of comment
    (Log.Comment(1).Image
   & (if not Log.Comment(2).Is_Null then "$" & Log.Comment(2).Image
      else "") );

  -- Encode a commit on a given length
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Log_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line (
        Image1 (From) & (if From.Merged then '>' else ' '),
        Image2 (From),
        "", List_Width, Line, False);
  end Set;

  -- List mode
  procedure Init_List is new Afpx.Utils.Init_List (
    Git_If.Log_Entry_Rec, Git_If.Set, Git_If.Log_Mng, Set, False);

  -- To search matching hash in Log
  function List_Hash_Match (Current, Criteria : Git_If.Log_Entry_Rec)
           return Boolean is
    Last : Positive;
  begin
    if Criteria.Hash(Criteria.Hash'Last) = ' ' then
      Last := Str_Util.Locate (Criteria.Hash, " ") - 1;
    else
      Last := Criteria.Hash'Last;
    end if;
      return Current.Hash(1 .. Last) = Criteria.Hash(1 .. Last);
  end List_Hash_Match;
  function List_Hash_Search is
           new Git_If.Log_Mng.Search (List_Hash_Match);

  -- Get the Hash of remote HEAD
  function Remote_Head (Of_Branch : in String) return Git_If.Git_Hash is
    Remote : As.U.Asu_Us;
    Remotes : Git_If.Log_List;
    Dummy_End : Boolean;
  begin
    -- Try to read remote branch name
    Remote := As.U.Tus (Git_If.Remote_Branch (Of_Branch));
    if Remote.Is_Null then
      return Git_If.No_Hash;
    end if;
    -- Log its HEAD
    Git_If.List_Log (Remote.Image, "", 1, True, Remotes, Dummy_End);
    if Remotes.Is_Empty then
      return Git_If.No_Hash;
    end if;
    -- Got it
    Remotes.Rewind;
    return Remotes.Access_Current.Hash;
  end Remote_Head;

  -- Local: encode currently stored hash
  procedure Encode_Hash (Hash : in Git_If.Git_Hash) is
  begin
    Utils.X.Encode_Field (Hash, Afpx_Xref.History.Hash);
  end Encode_Hash;

  -- Handle the history of a file or dir
  --  possibly on a  given branch
  procedure List (Root, Branch, Path, Name : in String;
                  Is_File : in Boolean;
                  Allow_Modif : in Boolean;
                  Allow_Tag : in Boolean;
                  Hash : in Git_If.Git_Hash := Git_If.No_Hash) is
    -- Are we in root
    On_Root : Boolean;

    -- Afpx stuff
    Get_Handle  : Afpx.Get_Handle_Rec;
    Ptg_Result  : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range;

    -- The log
    Logs : Git_If.Log_List;
    All_Read : Boolean;

    -- Init Afpx
    procedure Init is
    begin
      Afpx.Use_Descriptor (Afpx_Xref.History.Dscr_Num);
      Get_Handle := (others => <>);
      -- List characteristics
      List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
      -- Encode current branch and hash
      Utils.X.Encode_Branch (Afpx_Xref.History.Branch);
      Encode_Hash (Afpx.Decode_Field (Afpx_Xref.History.Hash, 0, True));

      -- Encode target branch
      if Branch /= "" then
        Utils.X.Encode_Field ("Br:", Afpx_Xref.History.Target_Branch);
        Utils.X.Encode_Field (Branch, Afpx_Xref.History.Target_Branch_Name);
      end if;
      -- Encode file/dir
      Utils.X.Encode_Field (
          (if Is_File then Path & Name
           elsif Name /= "" then Path & Name & "/"
           elsif Path /= "" then Path
           else "/"),
          Afpx_Xref.History.File);
      On_Root := Path = "" and then Name = "";
      -- Disable Root if already on root (including hist of branch)
      if On_Root then
        Afpx.Utils.Protect_Field (Afpx_Xref.History.Root, True);
      end if;
    end Init;

    -- Reset (to ' ') init indicator
    Default_Init_Indicator : constant Character := ' ';
    procedure Reset_Init_Indicator is
    begin
      Afpx.Encode_Field (Afpx_Xref.History.Init, (0, 0),
                         Default_Init_Indicator & "");
    end Reset_Init_Indicator;

    -- Show delta from current in list to comp
    procedure Show_Delta (Ref : in Afpx.Line_List_Mng.Ll_Natural) is
      Comp : Afpx.Line_List_Mng.Ll_Positive;
      Ref_Hash, Comp_Hash : Git_If.Git_Hash;
      File_Name : As.U.Asu_Us;
      Log : Git_If.Log_Entry_Rec;
      use type Afpx.Line_List_Mng.Ll_Natural;

    begin
      -- Confim if diff on a dir
      if not Is_File then
        if not Confirm_Diff_Dir (Path, Name) then
          Init;
          Afpx.Update_List (Afpx.Center_Selected);
          return;
        end if;
        Init;
        Afpx.Update_List (Afpx.Center_Selected);
      end if;

      -- Save position in List
      Comp := Afpx.Line_List.Get_Position;

      -- Read reference hash in Logs
      if Ref = 0 then
        -- Only Left selection
        Ref_Hash := Git_If.No_Hash;
      else
        Logs.Move_At (Ref);
        Logs.Read (Log, Git_If.Log_Mng.Current);
        Ref_Hash := Log.Hash;
      end if;

      -- Move to Comp and read comp hash in Logs
      Logs.Move_At (Comp);
      Logs.Read (Log, Git_If.Log_Mng.Current);
      Comp_Hash := Log.Hash;

      -- Restore position in List
      Afpx.Line_List.Move_At (Comp);

      -- Set file name
      File_Name := As.U.Tus (Root & Path & Name);
      -- Call delta
      if Ref_Hash = Git_If.No_Hash then
        -- Only Left selection: Hash^ and Hash
        Git_If.Launch_Delta (Config.Differator, File_Name.Image,
                             Comp_Hash & "^", Comp_Hash);
      else
        Git_If.Launch_Delta (Config.Differator, File_Name.Image,
                             Ref_Hash, Comp_Hash);
      end if;
    end Show_Delta;

    -- Do a restore
    procedure Do_Restore is
      Pos : Afpx.Line_List_Mng.Ll_Positive;
      Log : Git_If.Log_Entry_Rec;
    begin
      -- Save position in List and read it
      Pos := Afpx.Line_List.Get_Position;
      Logs.Move_At (Pos);
      Logs.Read (Log, Git_If.Log_Mng.Current);
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
      Pos : Afpx.Line_List_Mng.Ll_Positive;
      Commit : As.U.Asu_Us;
      Log : Git_If.Log_Entry_Rec;
    begin
      -- Save position in List and read it
      Pos := Afpx.Line_List.Get_Position;
      Logs.Move_At (Pos);
      Logs.Read (Log, Git_If.Log_Mng.Current);
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

    -- Do a reorg if no local modif,
    --  return True if success or failure => back to Directory
    function Do_Reorg return Boolean is
      Pos : Afpx.Line_List_Mng.Ll_Positive;
      Changes : Git_If.File_List;
      Change : Git_If.File_Entry_Rec;
      Moved : Boolean;
      Result : Branches.Result_List;
      Log : Git_If.Log_Entry_Rec;
      use type Branches.Result_List;
    begin
      -- Save position in List and read it
      Pos := Afpx.Line_List.Get_Position;
      Logs.Move_At (Pos);
      Logs.Read (Log, Git_If.Log_Mng.Current);

      -- Check that no local modif
      Git_If.List_Changes (Changes);
      -- Discard local changes (that are not indexed), status "??"
      if not Changes.Is_Empty then
        Changes.Rewind;
        loop
          Changes.Read (Change, Git_If.File_Mng.Current);
          if Change.S2 = '?' and then Change.S3 = '?' then
            Changes.Delete (Moved => Moved);
            exit when not Moved;
          elsif Changes.Check_Move then
            Changes.Move_To;
          else
            exit;
          end if;
        end loop;
      end if;

      if not Changes.Is_Empty then
        -- Error if some modifs remain
        Error ("Reorg", Root, "There are some local changes," & Aski.Lf
                            & "Please stash or revert them first.");
        Result := Branches.Cancelled;
      else
        -- Reorg success or failure
        Result := Branches.Reorg (Root, Log.Hash);
      end if;
      if Result = Branches.Cancelled then
        -- Cancel => stay in Branch
        Init;
        Init_List (Logs);
        Afpx.Line_List.Move_At (Pos);
        Afpx.Update_List (Afpx.Center_Selected);
        return False;
      else
        -- Success or failure => back to Directory
        return True;
      end if;
    end Do_Reorg;

    -- Do a hard reset
    function Do_Reset return Boolean is
      Pos : Afpx.Line_List_Mng.Ll_Positive;
      Str : As.U.Asu_Us;
      Res : Boolean;
      Log : Git_If.Log_Entry_Rec;
      use type Afpx.Line_List_Mng.Ll_Positive;
    begin
      -- Save position in List and read it
      Pos := Afpx.Line_List.Get_Position;
      Logs.Move_At (Pos);
      Logs.Read (Log, Git_If.Log_Mng.Current);
      -- Reset
      if Pos = 1 then
        -- In fact this is a reset to head  (no warning on history change)
        Res := Reset (Root, "");
      else
        Str := As.U.Tus (Str_Util.Strip (Image1 (Log) & " " & Image2 (Log)));
        Res := Reset (Root, Log.Hash, Comment => Str.Image);
      end if;
      if Res then
        return True;
      else
        -- Restore screen
        Init;
        Init_List (Logs);
        Afpx.Line_List.Move_At (Pos);
        Afpx.Update_List (Afpx.Center_Selected);
        return False;
      end if;
    end Do_Reset;

    -- Do a tag
    procedure Do_Tag is
      Pos : Afpx.Line_List_Mng.Ll_Positive;
      Log : Git_If.Log_Entry_Rec;
    begin
      -- Save position in List and read it
      Pos := Afpx.Line_List.Get_Position;
      Logs.Move_At (Pos);
      Logs.Read (Log, Git_If.Log_Mng.Current);
      -- Restore file
      Tags.Add (Root, Log.Hash);
      -- Restore screen
      Init;
      Init_List (Logs);
      Afpx.Line_List.Move_At (Pos);
      Afpx.Update_List (Afpx.Center_Selected);
    end Do_Tag;

    -- Store current hash
    procedure Do_Mark is
    begin
      Logs.Move_At (Afpx.Line_List.Get_Position);
      Utils.Store.Hash := Logs.Access_Current.Hash;
    end Do_Mark;

    -- Search stored hash
    procedure Do_Search is
      Log : Git_If.Log_Entry_Rec;
    begin
      -- Use got hash or else stored hash
      Log.Hash := Afpx.Decode_Field (Afpx_Xref.History.Hash, 0, True);
      if Log.Hash = Git_If.No_Hash then
        Log.Hash := Utils.Store.Hash;
        Encode_Hash (Log.Hash);
      end if;
      if Log.Hash = Git_If.No_Hash then
        return;
      end if;
      -- Search
      if List_Hash_Search (Logs, Log, From => Git_If.Log_Mng.Absolute) then
        -- Move to found
        Afpx.Line_List.Move_At (Logs.Get_Position);
        Afpx.Update_List (Afpx.Center_Selected);
      end if;
    end Do_Search;

    -- View file or commit details
    type Show_List is (Show_View, Show_Details);
    procedure Show (What : in Show_List) is
      Ref : Afpx.Line_List_Mng.Ll_Positive;
      Log : Git_If.Log_Entry_Rec;
    begin
      -- Read reference hash in Logs
      Ref := Afpx.Line_List.Get_Position;
      -- This will also save/restore current position
      Logs.Move_At (Ref);
      Logs.Read (Log, Git_If.Log_Mng.Current);
      case What is
        when Show_View =>
          View (Path & Name, Log.Hash);
        when Show_Details =>
          -- Allow modif
          Details.Handle (Root, Branch, Log.Hash, Allow_Modif, Allow_Tag);
          Init;
          Init_List (Logs);
          Afpx.Update_List (Afpx.Center_Selected);
      end case;
    end Show;

    -- Index of remote head (0 if unknown)
    Remote_Head_Index : Afpx.Line_List_Mng.Ll_Natural;

    -- Normalize Afpx list index
    function Normal is new Normalization.Normal_Mod
      (Afpx.Line_List_Mng.Ll_Natural);
    -- Update the list status
    procedure List_Change (Action : in Afpx.List_Change_List;
                           Status : in Afpx.List_Status_Rec) is
      -- Left and Right selection in list
      Left  : constant Afpx.Line_List_Mng.Ll_Natural
            := Status.Ids_Selected (Afpx.List_Left);
      Right : constant Afpx.Line_List_Mng.Ll_Natural
            := Status.Ids_Selected (Afpx.List_Right);
      use type Afpx.Line_List_Mng.Ll_Natural;
      Right_Set : constant Boolean := Right /= 0;
      Empty : constant Boolean := Logs.Is_Empty;
      Percent : Afpx.Percent_Range;
      Row : Con_Io.Row_Range;
      use type Afpx.List_Change_List;
    begin
      -- Reset init indicator at first time there is a left selection
      if Action = Afpx.Left_Selection then
        Reset_Init_Indicator;
      end if;
      -- No View, Detail, Restore, Checkout, Reorg, Reset nor Tag if RightSelect
      -- Protect buttons View and restore on dirs
      -- Protect Restore, Checkout, Reorg and Reset if no modif allowed
      -- Protect reorg if not in root or on first commit
      -- Protect Tag if not allowed
      Afpx.Utils.Protect_Field (Afpx_Xref.History.View,
                                not Is_File or else Right_Set or else Empty);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Diff,
                                Logs.List_Length <= 1);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Details,
                                Right_Set or else Empty);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Restore,
                                not Is_File or else not Allow_Modif
                                or else Right_Set or else Empty);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Checkout,
                                not Allow_Modif or else Right_Set
                                or else Empty);
      Afpx.Reset_Field (Afpx_Xref.History.Reorg);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Reorg,
                                not Allow_Modif or else Right_Set
                                or else Empty
                                or else not On_Root
                                or else Left = 1);
      Afpx.Reset_Field (Afpx_Xref.History.Reset);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Reset,
                                not Allow_Modif or else Right_Set
                                or else Empty
                                or else not On_Root);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Tag,
                                Right_Set or else Empty or else not Allow_Tag);
      -- Set in Red the Reorg et Reset if current ref is below remote head
      if not Afpx.Get_Field_Protection (Afpx_Xref.History.Reorg)
      and then Remote_Head_Index /= 0 and then Left > Remote_Head_Index then
        Afpx.Set_Field_Colors (Afpx_Xref.History.Reorg,
                               Con_Io.Color_Of ("Red"));
      end if;
      if not Afpx.Get_Field_Protection (Afpx_Xref.History.Reset)
      and then Remote_Head_Index /= 0 and then Left > Remote_Head_Index then
        Afpx.Set_Field_Colors (Afpx_Xref.History.Reset,
                               Con_Io.Color_Of ("Red"));
      end if;
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
      Afpx.Encode_Field (Afpx_Xref.History.Leftsel, (0, 0), Normal (
          Left, Afpx.Get_Field_Width (Afpx_Xref.History.Leftsel), False));
      Afpx.Encode_Field (Afpx_Xref.History.Rightsel, (0, 0), Normal (
          Right, Afpx.Get_Field_Width (Afpx_Xref.History.Rightsel), False));
      -- Activate button "All" if not all read
      Afpx.Utils.Protect_Field (Afpx_Xref.History.List_All, All_Read);
    end List_Change;

    -- Move according to click row in scroll field
    procedure Move_At_Scroll (Row : in Con_Io.Row_Range) is
      Percent : Afpx.Percent_Range;
      Saved_Position, Position : Afpx.Line_List_Mng.Ll_Natural;
      use type Afpx.Line_List_Mng.Ll_Natural;
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

    -- Reread history
    procedure Reread (Force_All : in Boolean) is
      Max : Natural;
    begin
      -- Read all or the default (Config) number of entires
      if Force_All then
        Max := 0;
      else
        Max := Config.History_Len;
      end if;
      -- Get history list
      if Path = "" and then Name = "" then
        if Directory.Get_Current = Directory.Normalize_Path (Root) then
          -- Log in (the root dir of) a bare repository
          --  fails if we provide the full (Root) path
          --  but is OK with ""
          -- So use "" if we are in root and if target dir is root
          Git_If.List_Log (Branch, "", Max, True, Logs, All_Read);
        else
          -- Use root as the target dir name and set sparse for the full
          --  history
          Git_If.List_Log (Branch, Root, Max, True, Logs, All_Read);
        end if;
      else
        -- Log the non-root target
        -- Not sparse (so no merge) otherwise we get the history of the full
        --  repository
        Git_If.List_Log (Branch, Root & Path & Name, Max, False, Logs,
                         All_Read);
      end if;
    end Reread;

    -- Read all entries and update
    procedure Do_Read_All is
      Pos : Afpx.Line_List_Mng.Ll_Positive;
    begin
      Pos := Afpx.Line_List.Get_Position;
      Reread (True);
      Init_List (Logs);
      Afpx.Line_List.Move_At (Pos);
      Afpx.Update_List (Afpx.Center_Selected);
    end Do_Read_All;

    -- List root
    procedure List_Root is
      Pos : Afpx.Line_List_Mng.Ll_Natural;
      use type Afpx.Line_List_Mng.Ll_Natural;
    begin
      -- Save position in List and read it
      if Afpx.Line_List.Is_Empty then
        Pos := 0;
      else
        Pos := Afpx.Line_List.Get_Position;
      end if;
      List (Root, Branch, "", "", False, Allow_Modif, Allow_Tag);
      -- Restore screen
      Init;
      -- Get history list with default length
      Reread (False);
      Init_List (Logs);
      if Pos /= 0 then
        Afpx.Line_List.Move_At (Pos);
        Afpx.Update_List (Afpx.Center_Selected);
      end if;
    end List_Root;

    -- A log
    Log : Git_If.Log_Entry_Rec;
    -- Head of remote found
    Found : Boolean;
    -- Do we set remote head
    Set_Remote_Head : Boolean;

    -- Init indicator character (C, R or space)
    Init_Indicator : Character;

  begin -- List

    -- Init Afpx
    Init;

    -- Get history list with default length
    Reread (False);

    -- Set current entry
    Log.Hash := Git_If.No_Hash;
    Set_Remote_Head := False;
    Remote_Head_Index := 0;
    Found := False;
    Init_Indicator := Default_Init_Indicator;
    if Hash /= Git_If.No_Hash then
      -- Set current to Hash provided
      Log.Hash := Hash;
      Init_Indicator := 'C';
    elsif On_Root then
      -- Set current to HEAD of remote (if possible)
      Log.Hash := Remote_Head (Branch);
      Set_Remote_Head := True;
      Init_Indicator := 'R';
    end if;
    if Log.Hash /= Git_If.No_Hash then
      Found := List_Hash_Search (Logs, Log, From => Git_If.Log_Mng.Absolute);
      if not Found then
        Init_Indicator := '?';
      end if;
    end if;
    if Set_Remote_Head and then Found then
      Remote_Head_Index := Logs.Get_Position;
    end if;

    -- Encode history
    Init_List (Logs);
    Afpx.Update_List (Afpx.Center_Selected);
    Afpx.Encode_Field (Afpx_Xref.History.Init, (0, 0), Init_Indicator & "");

    -- Disable buttons if empty list
    if Logs.Is_Empty then
      Afpx.Utils.Protect_Field (Afpx_Xref.History.List_All, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.View, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Details, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Restore, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Checkout, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Tag, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Mark, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Search, True);
    end if;

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result, Right_Select => True,
                         List_Change_Cb => List_Change'Access);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              -- Search got or stored hash
              Do_Search;
            when Afpx.Escape_Key =>
              -- Back
              return;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Afpx.List_Field_No  =>
              if Is_File then
                -- Double click or View => View if List file
                Show (Show_View);
              end if;
            when Afpx_Xref.History.List_All =>
              -- List all the entries
              Do_Read_All;
            when Afpx_Xref.History.Root =>
              -- List root
              List_Root;
            when Afpx_Xref.History.View =>
              -- View
              Show (Show_View);
            when Utils.X.List_Scroll_Fld_Range =>
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
              -- Restore
              Do_Restore;
            when Afpx_Xref.History.Checkout =>
              -- Checkout
              if Do_Checkout then
                return;
              end if;
            when Afpx_Xref.History.Reorg =>
              -- Reorg: go back to directory on success or failure
              -- Stay only on cancel
              if Do_Reorg then
                return;
              end if;
            when Afpx_Xref.History.Reset =>
              -- Reset
              if Do_Reset then
                return;
              end if;
            when Afpx_Xref.History.Tag =>
              -- Tag
              Do_Tag;
            when Afpx_Xref.History.Mark =>
              -- Store current hash
              Do_Mark;
            when Afpx_Xref.History.Search =>
              -- Search got or stored hash
              Do_Search;
            when Afpx_Xref.History.Back =>
              -- Back
              return;
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

  end List;

end History;

