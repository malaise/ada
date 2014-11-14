with As.U, Con_Io, Afpx.List_Manager, Normal, Rounds, Language;
with Utils.X, Config, Details, View, Afpx_Xref, Restore, Checkout, Tags;
package body History is

  -- List Width
  List_Width : Afpx.Width_Range;
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Log_Entry_Rec) is
  begin
    Utils.X.Encode_Line (
        -- "YYYY-MM-DD HH:MM:SS" -> "YYMMDD HH:MM "
        From.Date(03 .. 04) & From.Date(06 .. 07) & From.Date(09 .. 10) & '-'
          & From.Date(12 .. 13) & From.Date(15 .. 16) & ' ',
        -- 1 or 2 lines of comment
        From.Comment(1).Image
          & (if not From.Comment(2).Is_Null then "$" & From.Comment(2).Image
             else ""),
        "", List_Width, Line, False);
  end Set;
  procedure Init_List is new Afpx.List_Manager.Init_List (
    Git_If.Log_Entry_Rec, Git_If.Log_Mng, Set, False);

  -- To search matching hash in Log
  function Hash_Match (Current, Criteria : Git_If.Log_Entry_Rec) return Boolean is
  begin
    return Current.Hash = Criteria.Hash;
  end Hash_Match;
  function Hash_Search is new Git_If.Log_Mng.Dyn_List.Search (Hash_Match);

  -- Handle the history of a file or dir
  procedure Handle (Root, Path, Name : in String;
                    Is_File : in Boolean;
                    Hash : in Git_If.Git_Hash := Git_If.No_Hash) is
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
      -- Encode file/dir
      Utils.X.Encode_Field ((if Is_File then Path & Name
                            elsif Name /= "" then Path & Name & "/"
                            elsif Path /= "" then Path
                            else "/"),
                            Afpx_Xref.History.File);
      -- Encode current branch
      Utils.X.Encode_Branch (Afpx_Xref.History.Branch);

      -- Suppress button View and restore on dirs
      Afpx.Set_Field_Activation (Afpx_Xref.History.View, Is_File);
      Afpx.Set_Field_Activation (Afpx_Xref.History.Restore, Is_File);
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
      Logs.Move_At (Ref);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      case What is
        when Show_View =>
          View (Path & Name, Log.Hash);
        when Show_Details =>
          Details.Handle (Root, Log.Hash);
          Init;
          Init_List (Logs);
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

    -- Get history
    Afpx.Suspend;
    begin
      if Path = "" and then Name = "" then
        -- Log in (the root dir of) a bare repository
        --  fails if we provide the full (Root) path
        --  but is OK with '.'
        Git_If.List_Log (".", Logs);
      else
        Git_If.List_Log (Root & Path & Name, Logs);
      end if;
      Afpx.Resume;
    exception
      when others =>
        Afpx.Resume;
        raise;
    end;

    -- Encode history
    if Logs.Is_Empty then
      return;
    end if;
    if Hash /= Git_If.No_Hash then
      -- Set current to Hash provided
      Log.Hash := Hash;
      Dummy := Hash_Search (Logs, Log,
                            From => Git_If.Log_Mng.Dyn_List.Absolute);
    end if;
    Init_List (Logs);

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
              return;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Afpx.List_Field_No | Afpx_Xref.History.View =>
              -- Double click or View => View if file
              if Is_File then
                Show (Show_View);
              end if;
            when Utils.X.List_Scroll_Fld_Range'First ..
                 Utils.X.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.List_Manager.Scroll(
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
            when Afpx_Xref.History.Tag =>
              -- Tag
              Do_Tag;
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

  end Handle;

end History;

