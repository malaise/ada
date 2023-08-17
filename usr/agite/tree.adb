with As.U, Con_Io, Afpx.Utils, Normal, Normalization, Rounds, Directory,
     Str_Util;
with Utils.X, Utils.Store, Config, Details, View, Afpx_Xref, Git_If,
     Confirm_Diff_Dir;
package body Tree is

  -- List Width
  List_Width : Afpx.Width_Range;

  -- Encode the entries: <deco> <comment>
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Tree_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line ("", From.Head.Image & " " & From.Tail.Image, "",
                            List_Width, Line, False);
  end Set;
  procedure Init_List is new Afpx.Utils.Init_List (
    Git_If.Tree_Entry_Rec, Git_If.Set, Git_If.Tree_Mng, Set, False);

  -- To search matching hash in tree
  function List_Hash_Match (Current, Criteria : Git_If.Tree_Entry_Rec)
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
           new Git_If.Tree_Mng.Search (List_Hash_Match);

  -- Local: encode currently stored hash
  procedure Encode_Hash (Hash : in Git_If.Git_Hash) is
  begin
    Utils.X.Encode_Field (Hash.Image, Afpx_Xref.History.Hash);
  end Encode_Hash;

    -- Get the Hash of an entry
  --  No_Hash if list is empty
  --  Current Afpx position if Position is 0
  function Hash_Of (Tree : in out Git_If.Tree_Mng.List_Type;
                    Position : in Afpx.Line_List_Mng.Ll_Natural := 0)
                   return Git_If.Git_Hash is
    use type Afpx.Line_List_Mng.Ll_Natural;
  begin
    if Tree.Is_Empty or else Afpx.Line_List.Is_Empty then
      return Git_If.No_Hash;
     end if;
     Tree.Move_At (if Position /= 0 then Position
                   else Afpx.Line_List.Get_Position);
    return Tree.Access_Current.Hash;
  end Hash_Of;

  -- Move Afpx list at Hash
  -- First if No_Hash or not found
  procedure Move_At (Tree : in out Git_If.Tree_Mng.List_Type;
                     Hash : in Git_If.Git_Hash) is
    Tree_Entry : Git_If.Tree_Entry_Rec;
    use type As.U.Asu_Us;
  begin
    if Tree.Is_Empty or else Afpx.Line_List.Is_Empty then
      return;
    end if;
    Tree_Entry.Hash := Hash;
    if Hash /= Git_If.No_Hash
    and then List_Hash_Search (Tree, Tree_Entry,
                               From => Git_If.Tree_Mng.Absolute)
    then
      -- Move to found
      Afpx.Line_List.Move_At (Tree.Get_Position);
      Afpx.Update_List (Afpx.Center_Selected);
    else
       Afpx.Line_List.Rewind;
       Tree.Rewind;
       Afpx.Update_List (Afpx.Top);
    end if;
  end Move_At;

  -- List the history tree of a file or dir along a given branch
  -- Optionnaly set current entry to given Hash
  -- If the Hash is set, then Prio_Hash indicates if the default item
  --  shall be this hash instead of the head of the remote branch
  function List (Root, Branch, Path, Name : in String;
                  Is_File : in Boolean) return Boolean is
    -- Are we in root
    On_Root : Boolean;

    -- Afpx stuff
    Get_Handle  : Afpx.Get_Handle_Rec;
    Ptg_Result  : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range;

    -- The Tree
    Tree : Git_If.Tree_Mng.List_Type;
    All_Read : Boolean;

    -- Get the Hash of an entry
    --  No_Hash if list is empty
    --  Current Afpx position if Position is 0
    function Hash_Of (Position : in Afpx.Line_List_Mng.Ll_Natural := 0)
                     return Git_If.Git_Hash is
      (Hash_Of (Tree, Position));

    -- Move Afpx list at Hash
    -- First if No_Hash or not found
    procedure Move_At (Hash : in Git_If.Git_Hash) is
    begin
      Move_At (Tree, Hash);
    end Move_At;

    -- Init Afpx
    procedure Init is
    begin
      Afpx.Use_Descriptor (Afpx_Xref.History.Dscr_Num);
      Afpx.Clear_Field (Afpx_Xref.History.Title);
      Afpx.Encode_Field (Afpx_Xref.History.Title, (0, 2), "Tree");
      Get_Handle := (others => <>);
      -- List characteristics
      List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
      -- Encode current branch and hash
      Utils.X.Encode_Branch (Afpx_Xref.History.Branch);
      Encode_Hash (As.U.Tus (Afpx.Decode_Field (Afpx_Xref.History.Hash,
                                                0, True)));

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

      -- Hide all unused fields
      Afpx.Set_Field_Activation (Afpx_Xref.History.Init, False);
      Afpx.Set_Field_Activation (Afpx_Xref.History.Restore, False);
      Afpx.Set_Field_Activation (Afpx_Xref.History.Checkout, False);
      Afpx.Set_Field_Activation (Afpx_Xref.History.Reorg, False);
      Afpx.Set_Field_Activation (Afpx_Xref.History.Reset, False);
      Afpx.Set_Field_Activation (Afpx_Xref.History.Tag, False);
      Afpx.Set_Field_Activation (Afpx_Xref.History.Patch, False);

    end Init;

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
      Comp_Name := As.U.Tus (Path & Name);

      -- Set reference hash and file
      if Ref = 0 then
        -- Get hash of previous commit
        Afpx.Line_List.Move_To;
        Ref_Hash := Hash_Of;
        Afpx.Line_List.Move_To (Afpx.Line_List_Mng.Prev);
      else
        Ref_Hash := Hash_Of (Ref);
      end if;
      Ref_Name := As.U.Tus (Path & Name);

      Git_If.Launch_Delta (Config.Differator,
          Ref_Name.Image, Ref_Hash.Image,
          Comp_Hash.Image, Comp_Name.Image);
    end Show_Delta;

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

    -- View file or commit details
    type Show_List is (Show_View, Show_Details);
    procedure Show (What : in Show_List) is
      Hash : Git_If.Git_Hash;
    begin
      -- Read reference hash in Logs
      Hash := Hash_Of;
      case What is
        when Show_View =>
          View (Path & Name, Hash);
        when Show_Details =>
          -- Allow modif
          Details.Handle (Root, Branch, Hash.Image, False, False);
          Init;
          Init_List (Tree);
          Move_At (Hash);
      end case;
    end Show;

    -- Normalize Afpx list index
    function Normal is new Normalization.Normal_Mod
      (Afpx.Line_List_Mng.Ll_Natural);
    -- Update the list status
    procedure List_Change (Dummy_Action : in Afpx.List_Change_List;
                           Status : in Afpx.List_Status_Rec) is
      -- Left and Right selection in list
      Left  : constant Afpx.Line_List_Mng.Ll_Natural
            := Status.Ids_Selected (Afpx.List_Left);
      Right : constant Afpx.Line_List_Mng.Ll_Natural
            := Status.Ids_Selected (Afpx.List_Right);
      use type Afpx.Line_List_Mng.Ll_Natural;
      Right_Set : constant Boolean := Right /= 0;
      Empty : constant Boolean := Tree.Is_Empty;
      Percent : Afpx.Percent_Range;
      Row : Con_Io.Row_Range;
    begin
      -- Protect button View on dirs or if RightSelect
      Afpx.Utils.Protect_Field (Afpx_Xref.History.View,
                                not Is_File or else Right_Set or else Empty);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Diff,
          Empty or else Afpx.Line_List.Get_Position
                      = Afpx.Line_List.List_Length);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Details,
                                Right_Set or else Empty);
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
      Max : Git_If.Tree_Mng.Ll_Natural;
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
          -- Tree in (the root dir of) a bare repository
          --  fails if we provide the full (Root) path
          --  but is OK with ""
          -- So use "" if we are in root and if target dir is root
          Git_If.List_Tree ("", Max, Tree, All_Read);
        else
          -- Use root as the target dir name and set sparse for the full
          --  history
          Git_If.List_Tree (Root, Max, Tree, All_Read);
        end if;
      else
        -- Tree of the non-root target dir
        Git_If.List_Tree (Root & Path & Name, Max, Tree, All_Read);
      end if;
    end Reread;

    -- Read all entries and update
    procedure Do_Read_All is
      Pos : Afpx.Line_List_Mng.Ll_Positive;
    begin
      Pos := Afpx.Line_List.Get_Position;
      Reread (True);
      Init_List (Tree);
      Afpx.Line_List.Move_At (Pos);
      Afpx.Update_List (Afpx.Center_Selected);
    end Do_Read_All;

    -- List root
    function  List_Root return Boolean is
      Hash : Git_If.Git_Hash;
    begin
      -- Save position in List and read it
      Hash := Hash_Of;
      if List (Root, Branch, "", "", False) then
        return True;
      end if;
      -- Restore screen
      Init;
      -- Get history list with default length
      Reread (False);
      Init_List (Tree);
      Move_At (Hash);
      return False;
    end List_Root;

    begin
    -- Init Afpx
    Init;

    -- Get Tree
    Reread (False);

    -- Encode Tree
    Init_List (Tree);
    Afpx.Update_List (Afpx.Center_Selected);

    -- Disable buttons if empty list
    if Tree.Is_Empty then
      Afpx.Utils.Protect_Field (Afpx_Xref.History.List_All, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.View, True);
      Afpx.Utils.Protect_Field (Afpx_Xref.History.Details, True);
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
              when Afpx_Xref.History.Mark =>
              -- Store current hash
              Do_Mark;
            when Afpx_Xref.History.Search =>
              -- Search got or stored hash
              Do_Search;
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
                  Is_File : in Boolean) is
    Dummy_Result : Boolean;
  begin
    Dummy_Result := List (Root, Branch, Path, Name, Is_File);
  end List;

end Tree;

