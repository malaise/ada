with Aski, As.U, Afpx.Utils, Directory;
with Utils.X, Git_If, View, History, Config, Afpx_Xref, Restore,
     Confirm_Diff_Dir;
package body Details is

  -- The current list of Commit entires
  Commits : aliased Git_If.Commit_List;

  List_Width : Afpx.Width_Range;
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Commit_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line (From.Status & " ", From.File.Image, "",
                            List_Width, Line);
  exception
    when others => null;
  end Set;

  procedure Init_List is new Afpx.Utils.Init_List (
    Git_If.Commit_Entry_Rec, Git_If.Commit_File_Mng, Set, False);


  procedure Handle (Root : in String;
                    Branch : in String;
                    Rev_Tag : in String;
                    Allow_Modif : in Boolean;
                    Allow_Tag : in Boolean;
                    Tag_Date, Tag_Comment : in String := "") is

    -- Afpx stuff
    Get_Handle     : Afpx.Get_Handle_Rec;
    Ptg_Result     : Afpx.Result_Rec;
    Comment_Height : Afpx.Height_Range;
    Comment_Width  : Afpx.Width_Range;
    use type Afpx.Absolute_Field_Range;

    -- Commit details
    Hash : Git_If.Git_Hash;
    Merged : Boolean;
    Date : Git_If.Iso_Date;
    Comment : Git_If.Comment_Array(1 .. 10);

    procedure Init (Get_Details : in Boolean) is
    begin
      -- Init Afpx
      Afpx.Use_Descriptor (Afpx_Xref.Details.Dscr_Num);
      Get_Handle := (others => <>);
      Afpx.Get_Field_Size (Afpx_Xref.Details.Comment,
                           Comment_Height, Comment_Width);
      List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);

      -- Encode current branch
      Utils.X.Encode_Branch (Afpx_Xref.Details.Branch);

      -- Encode target branch
      if Branch /= "" then
        Utils.X.Encode_Field ("Br:", Afpx_Xref.Details.Target_Branch);
        Utils.X.Encode_Field (Branch, Afpx_Xref.Details.Target_Branch_Name);
      end if;


      -- Allow modifications (Hist that can recall Detail, restore) or not
      Afpx.Set_Field_Activation (Afpx_Xref.Details.Restore, Allow_Modif);

      -- Get commit details
      if Get_Details then
        Git_If.List_Commit (Rev_Tag, Hash, Merged, Date, Comment, Commits);
      end if;

      -- Encode info
      if Rev_Tag /= Hash then
        -- Rev_Tag is a tag. Center "Tag: <name>", preserving tail,
        --  put tag date and comment
        Utils.X.Center_Field ("Tag: " & Rev_Tag, Afpx_Xref.Details.Title,
                              False);
        Utils.X.Encode_Field (Tag_Date, Afpx_Xref.Details.Tag_Date);
        Utils.X.Encode_Field (Tag_Comment, Afpx_Xref.Details.Tag_Comment,
                              False);
      end if;
      Utils.X.Encode_Field (Hash, Afpx_Xref.Details.Hash);
      if Merged then
        Utils.X.Encode_Field ("Merge", Afpx_Xref.Details.Merged);
      else
        Afpx.Clear_Field (Afpx_Xref.Details.Merged);
      end if;
      Utils.X.Encode_Field (Date, Afpx_Xref.Details.Date);
      Afpx.Clear_Field (Afpx_Xref.Details.Comment);
      for I in 1 .. Comment_Height loop
        -- No clear, and keep Head
        Afpx.Utils.Encode_Field (Comment(I).Image, Afpx_Xref.Details.Comment,
                                 I - 1, False, False);
      end loop;
      -- Encode list
      Init_List (Commits);
    end Init;

    -- Do a restore
    procedure Do_Restore is
      Pos : Positive;
      Commit : Git_If.Commit_Entry_Rec;
      Dummy : Boolean;
    begin
      -- Save position in List and read it
      Pos := Afpx.Line_List.Get_Position;
      Commits.Move_At (Pos);
      Commits.Read (Commit, Git_If.Commit_File_Mng.Dyn_List.Current);
      -- Restore file
      Restore (Root, Commit.File.Image, Hash, Commits'Access);
      -- Restore screen
      Init (False);
      Afpx.Line_List.Move_At (Pos);
      Afpx.Update_List (Afpx.Center_Selected);
    end Do_Restore;

    -- Launch viewer on current file, or history on current dir or file
    type Show_List is (Show_View, Show_Hist, Show_Diff);
    procedure Show (What : in Show_List) is
      Pos : constant Positive := Afpx.Line_List.Get_Position;
      Commit : Git_If.Commit_Entry_Rec;
      Result : Boolean;
    begin
      Commits.Move_At (Pos);
      Commits.Read (Commit, Git_If.Commit_File_Mng.Dyn_List.Current);
      declare
        Path : constant String := Directory.Dirname (Commit.File.Image);
        File : constant String := Directory.Basename (Commit.File.Image);
      begin
        case What is
          when Show_View =>
            -- Only files except leading "/"
            if Commit.File.Image /= "/" then
              View (Commit.File.Image, Hash);
            end if;
          when Show_Hist =>
            -- History on current hash. Allow or not modif and tag
            if Commit.File.Image = "/" then
              History.List (Root, Branch, "", "", False, Allow_Modif,
                            Allow_Tag, Hash);
            else
              History.List (Root, Branch, Path, File, True, Allow_Modif,
                            Allow_Tag, Hash);
            end if;
            -- Re init sreen
            Init (False);
          when Show_Diff =>
            if Commit.File.Image = "/" then
              -- Diff on '/': Confirm several Diff
              Result := Confirm_Diff_Dir (Path, File);
              Init (False);
              if not Result then
                return;
              end if;
            end if;
            -- Call delta of this commit: all files or one
            Git_If.Launch_Delta (Config.Differator, Root & Path & File,
                             Hash & "^", Hash);
        end case;
      end;
    end Show;

    -- Copy Hash or Comment as X selection
    type Copy_Source_List is (Copy_Hash, Copy_Date, Copy_Comment);
    procedure Copy_Selection (Copy_Source : in Copy_Source_List) is
      Result : As.U.Asu_Us;
      use type As.U.Asu_Us;
    begin
      case Copy_Source is
        when Copy_Hash =>
          Afpx.Set_Selection (Hash);
        when Copy_Date =>
          Afpx.Set_Selection (Date);
        when Copy_Comment =>
          -- Skip tailing empty lines. No LineFeed after last line
          for Row of reverse Comment loop
            if not Row.Is_Null or else not Result.Is_Null then
              if Result.Is_Null then
                Result := Row;
              else
                Result := Row & Aski.Lf & Result;
              end if;
            end if;
          end loop;
          Afpx.Set_Selection (Result.Image);
      end case;
    end Copy_Selection;

    -- Update the list status
    procedure List_Change (Unused_Action : in Afpx.List_Change_List;
                           Unused_Status : in Afpx.List_Status_Rec) is

    begin
      if Afpx.Line_List.Is_Empty then
        return;
      end if;
       -- No view on '/', first entry if list is not empty
       Afpx.Utils.Protect_Field (Afpx_Xref.Details.View,
                                 Afpx.Line_List.Get_Position = 1);
    end List_Change;

  begin
    -- Full init
    Init (True);

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
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
            when Afpx.List_Field_No =>
              -- Double click (View)
              Show (Show_View);
            when Utils.X.List_Scroll_Fld_Range =>
              -- Scroll list
              Afpx.Utils.Scroll (
                 Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
            when Afpx_Xref.Details.View =>
              -- View
              Show (Show_View);
            when Afpx_Xref.Details.History =>
              -- History
              Show (Show_Hist);
            when Afpx_Xref.Details.Diff =>
              -- Diff
              Show (Show_Diff);
            when Afpx_Xref.Details.Restore =>
              -- Restore
              Do_Restore;
            when Afpx_Xref.Details.Back =>
              -- Back
              return;
            when Afpx_Xref.Details.Copyhash =>
              -- Copy hash to clipboard
              Copy_Selection (Copy_Hash);
            when Afpx_Xref.Details.Copydate =>
              -- Copy commit date to clipboard
              Copy_Selection (Copy_Date);
            when Afpx_Xref.Details.Copycomment =>
              -- Copy commit comment to clipboard
              Copy_Selection (Copy_Comment);
            when others =>
              -- Other button?
              null;
          end case;

        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          -- Encode current branch
          Utils.X.Encode_Branch (Afpx_Xref.Details.Branch);
      end case;
    end loop;

  end Handle;

end Details;

