with As.U, Con_Io, Afpx.Utils, Normal, Normalization, Rounds, Language,
     Directory, Str_Util, Aski;
with Utils.X, Utils.Store, Config, Details, View, Afpx_Xref, Restore, Checkout,
     Tags, Branch, Confirm_Diff_Dir, Reset, Error;
package body History is

  package Branches renames Branch;
  subtype Ll_Natural is Git_If.Log_Mng.Ll_Natural;

  -- List Width
  List_Width : Afpx.Width_Range;

  -- Full image of a commit: Date, then Comment
  function Image1 (Log : Git_If.Iso_Date) return String
           renames Utils.Image;
  function Image2 (Log : Git_If.Log_Entry_Rec) return String is
    -- 1 or 2 lines of comment
    (Log.Comment(1).Image
   & (if not Log.Comment(2).Is_Null then " " & Log.Comment(2).Image
      else "") );

  -- Encode a commit on a given length
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Log_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line (
        Image1 (From.Date) & (if From.Merged then
                           (if From.Extra.Is_Null then '>'
                            else '+')
                         else ' '),
        Image2 (From),
        "", List_Width, Line, False);
  end Set;

  -- List mode, in normal history or renames
  procedure Init_List is new Afpx.Utils.Init_List (
    Git_If.Log_Entry_Rec, Git_If.Set, Git_If.Log_Mng, Set, False);

  -- To search matching hash in Log
  function List_Hash_Match (Current, Criteria : Git_If.Log_Entry_Rec)
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
    Git_If.List_Log (Remote.Image, "", "", 1, True, False, Remotes, Dummy_End);
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
    Utils.X.Encode_Field (Hash.Image, Afpx_Xref.History.Hash);
  end Encode_Hash;

  -- Get the Hash of an entry
  --  No_Hash if list is empty
  --  Current Afpx position if Position is 0
  function Hash_Of (Logs : in out Git_If.Log_List;
                    Position : in Afpx.Line_List_Mng.Ll_Natural := 0)
                   return Git_If.Git_Hash is
    use type Afpx.Line_List_Mng.Ll_Natural;
  begin
    if Logs.Is_Empty or else Afpx.Line_List.Is_Empty then
      return Git_If.No_Hash;
     end if;
     Logs.Move_At (if Position /= 0 then Position
                   else Afpx.Line_List.Get_Position);
    return Logs.Access_Current.Hash;
  end Hash_Of;

  -- Move Afpx list at Hash
  -- First if No_Hash or not found
  procedure Move_At (Logs : in out Git_If.Log_List;
                     Hash : in Git_If.Git_Hash) is
    Log : Git_If.Log_Entry_Rec;
    use type As.U.Asu_Us;
  begin
    if Logs.Is_Empty or else Afpx.Line_List.Is_Empty then
      return;
    end if;
    Log.Hash := Hash;
    if Hash /= Git_If.No_Hash
    and then List_Hash_Search (Logs, Log, From => Git_If.Log_Mng.Absolute)
    then
      -- Move to found
      Afpx.Line_List.Move_At (Logs.Get_Position);
      Afpx.Update_List (Afpx.Center_Selected);
    else
       Afpx.Line_List.Rewind;
       Logs.Rewind;
       Afpx.Update_List (Afpx.Top);
    end if;
  end Move_At;

  -- Do a patch
  function Patch (All_Logs, Selected : in out Git_If.Log_List;
                  On_Root : in Boolean) return Boolean is separate;


  -- List history of a file, following renames
  -- Log is initially cleared
  -- The Merged flag of the commit indicates that the commit made a rename
  -- The Extra field of the commit  contains the name of the file
  -- Returns the position in Log of the first rename
  function Rename (Branch, Root, Path : in String;
                   Max : in Ll_Natural;
                   Log : in out Git_If.Log_List;
                   End_Reached : out Boolean) return Ll_Natural is separate;

  -- Handle the history of a file or dir
  --  possibly on a given branch
  -- Return True if Go Back
  function List (Root, Branch, Path, Name : String;
                 Is_File : Boolean;
                 Allow_Modif : Boolean;
                 Allow_Tag : Boolean;
                 Hash : Git_If.Git_Hash := Git_If.No_Hash) return Boolean is
    -- Are we in root
    On_Root : Boolean;

    -- Afpx stuff
    Get_Handle  : Afpx.Get_Handle_Rec;
    Ptg_Result  : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range;

    -- The log
    Logs : Git_If.Log_List;
    All_Read : Boolean;
    First_Rename : Ll_Natural;

    -- The current item has been pushed
    Pushed : Boolean;

    -- Get the Hash of an entry
    --  No_Hash if list is empty
    --  Current Afpx position if Position is 0
    function Hash_Of (Position : in Afpx.Line_List_Mng.Ll_Natural := 0)
                     return Git_If.Git_Hash is
      (Hash_Of (Logs, Position));

    -- Move Afpx list at Hash
    -- First if No_Hash or not found
    procedure Move_At (Hash : in Git_If.Git_Hash) is
    begin
      Move_At (Logs, Hash);
    end Move_At;

    -- Init Afpx
    procedure Init is
    begin
      Afpx.Use_Descriptor (Afpx_Xref.History.Dscr_Num);
      Get_Handle := (others => <>);
      -- List characteristics
      List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
      -- Encode current branch and hash
      Utils.X.Encode_Branch (Afpx_Xref.History.Branch);
      Encode_Hash (As.U.Tus (Afpx.Decode_Field (Afpx_Xref.History.Hash,
                                                0, True)));

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
      --  or when following history
      if On_Root then
        Afpx.Utils.Protect_Field (Afpx_Xref.History.Root, True);
      end if;
    end Init;

    -- Set the init indicator
    procedure Encode_Init_Indicator (C : Character) is
    begin
      Afpx.Encode_Field (Afpx_Xref.History.Init, (0, 0), C & "");
    end Encode_Init_Indicator;

    -- Reset (to ' ') init indicator
    Default_Init_Indicator : constant Character := ' ';
    procedure Reset_Init_Indicator is
    begin
      Encode_Init_Indicator (Default_Init_Indicator);
    end Reset_Init_Indicator;

    -- Show delta from current in list to comp
    procedure Show_Delta (Ref : in Afpx.Line_List_Mng.Ll_Natural) is
      Ref_Hash, Comp_Hash : Git_If.Git_Hash;
      Ref_Name, Comp_Name : As.U.Asu_Us;
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

      -- Set comparison hash to current (left) selected and file
      Comp_Hash := Hash_Of;
      if Is_File then
        Comp_Name := Logs.Access_Current.Extra;
      else
        Comp_Name := As.U.Tus (Path & Name);
      end if;

      -- Set reference hash and file
      if Ref = 0 then
        -- Get hash of previous commit
        Afpx.Line_List.Move_To;
        Ref_Hash := Hash_Of;
        Afpx.Line_List.Move_To (Afpx.Line_List_Mng.Prev);
      else
        Ref_Hash := Hash_Of (Ref);
      end if;
      if Is_File then
        Ref_Name := Logs.Access_Current.Extra;
      else
        Ref_Name := As.U.Tus (Path & Name);
      end if;

      Git_If.Launch_Delta (Config.Differator,
          Ref_Name.Image, Ref_Hash.Image,
          Comp_Hash.Image, Comp_Name.Image);
    end Show_Delta;

    -- Do a restore
    function Do_Restore return Boolean is
      Hash : Git_If.Git_Hash;
    begin
      -- Save position in List and read it
      Hash := Hash_Of;
      -- Restore file
      if Restore (Root,
                  (if Is_File then Logs.Access_Current.Extra.Image
                   else Path & Name),
                  Hash.Image, null) then
        return True;
      end if;
      -- Restore screen
      Init;
      Init_List (Logs);
      Move_At  (Hash);
      return False;
    end Do_Restore;

    -- Do a checkout
    function Do_Checkout return Boolean is
      Hash : Git_If.Git_Hash;
      Commit : As.U.Asu_Us;
    begin
      -- Save position in List
      Hash := Hash_Of;
      Commit := As.U.Tus (Language.Unicode_To_String (
          Afpx.Line_List.Access_Current.Str(
              1 .. Afpx.Line_List.Access_Current.Len)));

      -- Checkout (success will lead to return to Directory)
      if Checkout.Handle (Root, "commit", Commit.Image, Hash) then
        return True;
      else
        -- Restore screen
        Init;
        Init_List (Logs);
        Move_At (Hash);
        return False;
      end if;
    end Do_Checkout;

    -- Do a reorg if no local modif,
    --  return True if success or failure => back to Directory
    function Do_Reorg return Boolean is
      Hash : Git_If.Git_Hash;
      Changes : Git_If.File_List;
      Change : Git_If.File_Entry_Rec;
      Moved : Boolean;
      Result : Branches.Result_List;
      use type Branches.Result_List;
    begin
      -- Save position in List and read it
      Hash := Hash_Of;

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
        Result := Branches.Reorg (Root, Hash.Image, Pushed);
      end if;
      if Result = Branches.Cancelled then
        -- Cancel => stay in Branch
        Init;
        Init_List (Logs);
        Move_At (Hash);
        return False;
      else
        -- Success or failure => back to Directory
        return True;
      end if;
    end Do_Reorg;

    -- Do a hard reset
    function Do_Reset return Boolean is
      Pos : Afpx.Line_List_Mng.Ll_Positive;
      Hash : Git_If.Git_Hash;
      Str : As.U.Asu_Us;
      Res : Boolean;
      Log : Git_If.Log_Entry_Rec;
      use type Afpx.Line_List_Mng.Ll_Positive;
    begin
      -- Save position in List and read it
      Pos := Afpx.Line_List.Get_Position;
      Hash := Hash_Of;
      Logs.Read (Log, Git_If.Log_Mng.Current);
      -- Reset
      if Pos = 1 then
        -- In fact this is a reset to head (no warning on history change)
        Res := Reset (Root, "");
      else
        Str := As.U.Tus (Str_Util.Strip (Image1 (Log.Date)
                       & " " & Image2 (Log)));
        Res := Reset (Root, Hash.Image, Comment => Str.Image,
                      Pushed => Pushed);
      end if;
      if Res then
        return True;
      else
        -- Restore screen
        Init;
        Init_List (Logs);
        Move_At (Hash);
        return False;
      end if;
    end Do_Reset;

    -- Do a tag
    procedure Do_Tag is
      Hash : Git_If.Git_Hash;
    begin
      -- Save position in List and read it
      Hash := Hash_Of;
      -- Restore file
      Tags.Add (Hash);
      -- Restore screen
      Init;
      Init_List (Logs);
      Move_At (Hash);
    end Do_Tag;

    -- Do a tag
    function Do_Patch (Ref : in Afpx.Line_List_Mng.Ll_Natural)
                      return Boolean is
      Lref, Lcur : Afpx.Line_List_Mng.Ll_Natural;
      From, To : Afpx.Line_List_Mng.Ll_Natural;
      Llogs : Git_If.Log_List;
      Hash : Git_If.Git_Hash;
      use type Afpx.Line_List_Mng.Ll_Natural;
    begin
      -- Current (left) selection
      Lcur := Afpx.Line_List.Get_Position;
      Lref := Lcur;
      -- Other (right) selection if set and in root (the list is accurate)
      if On_Root and then Ref /= 0 then
        Lref := Ref;
      end if;
      -- Ensure From is before To
      if Lref < Lcur then
        From := Lref;
        To := Lcur;
      else
        From := Lcur;
        To := Lref;
      end if;
      -- Extract logs in reverse order
      Hash := Hash_Of;
      Logs.Move_At (From);
      loop
        Llogs.Insert (Logs.Access_Current.all, Git_If.Log_Mng.Prev);
        exit when Logs.Get_Position = To;
        Logs.Move_To;
      end loop;
      -- Launch patch
      -- Afpx list is newest before oldest,
      --  but order of Hash is oldest then newest
      if Patch (Logs, Llogs, On_Root) then
        return True;
      end if;
      -- Reset
      Init;
      Init_List (Logs);
      Move_At (Hash);
      return False;
    end Do_Patch;

    -- Store current hash
    procedure Do_Mark is
    begin
      Utils.Store.Hash := Hash_Of;
    end Do_Mark;

    -- Search stored hash
    procedure Do_Search is
      Hash : Git_If.Git_Hash;
      use type As.U.Asu_Us;
    begin
      -- Use got hash or else stored hash
      Hash := As.U.Tus (Str_Util.Strip (Afpx.Decode_Field (
                        Afpx_Xref.History.Hash, 0, True)));
      if Hash = Git_If.No_Hash then
        Hash := Utils.Store.Hash;
        Encode_Hash (Hash);
      end if;
      if Hash = Git_If.No_Hash then
        return;
      end if;
      -- Search
      Move_At (Hash);
    end Do_Search;

    -- Set current to HEAD of remote (if possible)
    function Do_Remote_Head return Boolean is
      Log : Git_If.Log_Entry_Rec;
      Found : Boolean;
      use type As.U.Asu_Us;
    begin
      Log.Hash := Remote_Head ( (if Branch /= "" then Branch
                                 else Git_If.Current_Branch) );
      if Log.Hash /= Git_If.No_Hash then
        Found := List_Hash_Search (Logs, Log, From => Git_If.Log_Mng.Absolute);
        if Found then
          return True;
        end if;
      end if;
      return False;
    end Do_Remote_Head;

    -- View file or commit details
    type Show_List is (Show_View, Show_Details);
    procedure Show (What : in Show_List) is
      Hash : Git_If.Git_Hash;
    begin
      -- Read reference hash in Logs
      Hash := Hash_Of;
      case What is
        when Show_View =>
          View ( (if Is_File then Logs.Access_Current.Extra.Image
                  else Path & Name),
                 Hash);
        when Show_Details =>
          -- Allow modif
          Details.Handle (Root, Branch, Hash.Image, Allow_Modif, Allow_Tag);
          Init;
          Init_List (Logs);
          Move_At (Hash);
      end case;
    end Show;

    -- Index of remote head (0 if unknown)
    Remote_Head_Index : Afpx.Line_List_Mng.Ll_Natural;

    -- Update Push and ink versus remote head
    procedure Update_Pushed (Index : in Afpx.Line_List_Mng.Ll_Natural) is
      use type Afpx.Line_List_Mng.Ll_Natural;
    begin
      if Remote_Head_Index = 0 then
        Pushed := True;
      elsif Index > Remote_Head_Index then
        Pushed := True;
      else
        Pushed := False;
      end if;
      if not Afpx.Get_Field_Protection (Afpx_Xref.History.Reorg)
      and then Pushed then
        Afpx.Set_Field_Colors (Afpx_Xref.History.Reorg,
                               Con_Io.Color_Of ("Red"));
      end if;
      if not Afpx.Get_Field_Protection (Afpx_Xref.History.Reset)
      and then Pushed then
        Afpx.Set_Field_Colors (Afpx_Xref.History.Reset,
                               Con_Io.Color_Of ("Red"));
      end if;
    end Update_Pushed;

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
      -- Protect most buttons if empty list
      -- No View, Detail, Restore, Checkout, Reorg, Reset nor Tag if RightSelect
      -- Protect buttons View and restore on dirs
      -- Protect Restore, Checkout, Reorg and Reset if no modif allowed
      -- Protect Reorg if not in root or on first commit
      -- Protect Reset if not in root
      -- Protect Tag if not allowed
      Afpx.Utils.Protect_Field (Afpx_Xref.History.View,
                                not Is_File or else Right_Set or else Empty);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Diff,
          Empty or else Afpx.Line_List.Get_Position
                      = Afpx.Line_List.List_Length);
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
                                or else Left <= 1);
      Afpx.Reset_Field (Afpx_Xref.History.Reset);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Reset,
                                not Allow_Modif or else Right_Set
                                or else Empty
                                or else not On_Root);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Tag,
                                Right_Set or else Empty or else not Allow_Tag);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Patch,
                                (not On_Root and then Right_Set)
                                 or else Empty);
      -- Set in Red the Reorg et Reset if current ref is below remote head
      Update_Pushed (Left);
      -- Show renamed file name if different
      if Is_File then
        if First_Rename /= 0
        and then Afpx.Line_List.Get_Position > First_Rename then
          Logs.Move_At (Afpx.Line_List.Get_Position);
          Utils.X.Encode_Field (" - " & Logs.Access_Current.Extra.Image,
                                Afpx_Xref.History.Renamed);
        else
          Afpx.Clear_Field (Afpx_Xref.History.Renamed);
        end if;
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
      Position : Afpx.Line_List_Mng.Ll_Natural;
      Saved_Position : Afpx.Utils.Backup_Context;
      use type Afpx.Line_List_Mng.Ll_Natural;
    begin
      if Afpx.Line_List.Is_Empty then
        return;
      end if;
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
      -- Center to selected index but keep unchanged selected line
      Saved_Position.Backup;
      Afpx.Line_List.Move_At (Position);
      Afpx.Update_List (Afpx.Top_Selected);
      Saved_Position.Restore (Force_Position => True);
    end Move_At_Scroll;

    -- Reread history
    procedure Reread (Force_All : in Boolean) is
      Max : Git_If.Log_Mng.Ll_Natural;
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
          Git_If.List_Log (Branch, "", "", Max, True, False, Logs, All_Read);
        else
          -- Use root as the target dir name and set sparse for the full
          --  history
          Git_If.List_Log (Branch, Root, "", Max, True, False, Logs, All_Read);
        end if;
      elsif Is_File then
        -- Log the file, following renames
        First_Rename := Rename (Branch, Root, Path & Name, Max, Logs, All_Read);
      else
        -- Log the non-root target dir
        -- Not sparse (so no merge) otherwise we get the history of the full
        --  repository
        Git_If.List_Log (Branch, Root & Path & Name, "", Max, False, False,
                         Logs, All_Read);
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
    function  List_Root return Boolean is
      Hash : Git_If.Git_Hash;
    begin
      -- Save position in List and read it
      Hash := Hash_Of;
      if List (Root, Branch, "", "", False, Allow_Modif, Allow_Tag, Hash) then
        return True;
      end if;
      -- Restore screen
      Init;
      -- Get history list with default length
      Reread (False);
      Init_List (Logs);
      Move_At (Hash);
      return False;
    end List_Root;

    -- A log
    Log : Git_If.Log_Entry_Rec;
    -- Head of remote found
    Found : Boolean;

    -- Init indicator character (C, R or space)
    Init_Indicator : Character;

    use type As.U.Asu_Us;
  begin -- List

    -- Init Afpx
    Init;

    -- Get history list with default length
    Reread (False);

    -- Set remote head
    Remote_Head_Index := 0;
    if Do_Remote_Head then
      Remote_Head_Index := Logs.Get_Position;
    end if;

    -- Set current entry to the provided Hash
    Log.Hash := Git_If.No_Hash;
    Init_Indicator := Default_Init_Indicator;
    Found := False;
    if Hash /= Git_If.No_Hash then
      Log.Hash := Hash;
      Found := List_Hash_Search (Logs, Log, From => Git_If.Log_Mng.Absolute);
      if Found then
        Init_Indicator := 'C';
      else
        Init_Indicator := '?';
      end if;
    end if;
    if not Found and then not Logs.Is_Empty then
      Logs.Rewind;
    end if;

    -- Adjust Alert/warning
    if not Logs.Is_Empty then
      Update_Pushed (Logs.Get_Position);
    else
      Update_Pushed (1);
    end if;

    -- Encode history
    Init_List (Logs);
    Afpx.Update_List (Afpx.Center_Selected);
    Encode_Init_Indicator (Init_Indicator);

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
              return False;
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
              if List_Root then
                return True;
              end if;
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
              if Do_Restore then
                return True;
              end if;
            when Afpx_Xref.History.Checkout =>
              -- Checkout
              if Do_Checkout then
                return True;
              end if;
            when Afpx_Xref.History.Reorg =>
              -- Reorg: go back to directory on success or failure
              -- Stay only on cancel
              if Do_Reorg then
                return True;
              end if;
            when Afpx_Xref.History.Reset =>
              -- Reset
              if Do_Reset then
                return True;
              end if;
            when Afpx_Xref.History.Tag =>
              -- Tag
              Do_Tag;
            when Afpx_Xref.History.Patch =>
              -- Tag
              if Do_Patch (Ptg_Result.Id_Selected_Right) then
                return True;
              end if;
            when Afpx_Xref.History.Mark =>
              -- Store current hash
              Do_Mark;
            when Afpx_Xref.History.Search =>
              -- Search got or stored hash
              Do_Search;
            when Afpx_Xref.History.Remote_Head =>
              -- Move to head of remote
              if Do_Remote_Head then
                Init_Indicator := 'R';
                Remote_Head_Index := Logs.Get_Position;
                Move_At (Hash_Of (Remote_Head_Index));
              else
                Init_Indicator := '?';
              end if;
              Encode_Init_Indicator (Init_Indicator);
            when Afpx_Xref.History.Back =>
              -- Back
              return False;
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

  -- Handle the history of a file or dir
  --  possibly on a given branch
  procedure List (Root, Branch, Path, Name : in String;
                  Is_File : in Boolean;
                  Allow_Modif : in Boolean;
                  Allow_Tag : in Boolean;
                  Hash : in Git_If.Git_Hash := Git_If.No_Hash) is
    Dummy_Result : Boolean;
  begin
    Dummy_Result := List (Root, Branch,
                          Path, Name, Is_File,
                          Allow_Modif, Allow_Tag, Hash);
  end List;
end History;

