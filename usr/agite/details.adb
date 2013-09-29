with Ada.Characters.Latin_1;
with As.U, Con_Io, Afpx.List_Manager, Directory;
with Utils.X, View, History, Config, Afpx_Xref, Restore;
package body Details is

  -- The current list of Commit entires
  Commits : aliased Git_If.Commit_List;

  List_Width : Afpx.Width_Range;
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Commit_Entry_Rec) is
  begin
    Utils.X.Encode_Line (From.Status & " ", From.File.Image, "",
                         List_Width, Line);
  exception
    when others => null;
  end Set;

  procedure Init_List is new Afpx.List_Manager.Init_List (
    Git_If.Commit_Entry_Rec, Git_If.Commit_File_Mng, Set, False);


  procedure Handle (Root : in String; Hash : in Git_If.Git_Hash) is

    -- Afpx stuff
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    Comment_Height : Afpx.Height_Range;
    Comment_Width  : Afpx.Width_Range;
    use type Afpx.Absolute_Field_Range;

    -- Commit details
    Date : Git_If.Iso_Date;
    Comment : Git_If.Comment_Array(1 .. 10);

    procedure Init (Get_Details : in Boolean) is
    begin
      -- Init Afpx
      Afpx.Use_Descriptor (Afpx_Xref.Details.Dscr_Num);
      Cursor_Field := 1;
      Cursor_Col := 0;
      Insert := False;
      Afpx.Get_Field_Size (Afpx_Xref.Details.Comment,
                           Comment_Height, Comment_Width);
      List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);

      -- Encode current branch
      Utils.X.Encode_Field (Utils.X.Branch_Image (Git_If.Current_Branch),
                            Afpx_Xref.Details.Branch);


      -- Get commit details
      if Get_Details then
        Afpx.Suspend;
        begin
          Git_If.List_Commit (Hash, Date, Comment, Commits);
          Afpx.Resume;
        exception
          when others =>
            Afpx.Resume;
            raise;
        end;
      end if;

      -- Encode info
      Utils.X.Encode_Field (Hash, Afpx_Xref.Details.Hash);
      Utils.X.Encode_Field (Date, Afpx_Xref.Details.Date);
      Afpx.Clear_Field (Afpx_Xref.Details.Comment);
      for I in 1 .. Comment_Height loop
        Utils.X.Encode_Field (Comment(I).Image, Afpx_Xref.Details.Comment);
      end loop;
      -- Encode list
      Init_List (Commits);
    end Init;

    -- Do a restore
    procedure Do_Restore is
      Pos : Positive;
      Commit : Git_If.Commit_Entry_Rec;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
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
            History.Handle (Root, Path, File, Commit.File.Image /= "/",
                            Hash);
            -- Re init sreen
            Init (False);
          when Show_Diff =>
            -- Call delta between previous of this file and this commit
            Git_If.Launch_Delta (Config.Differator, Root & Path & File,
                             Hash & "^", Hash);
        end case;
      end;
    end Show;

    -- Copy Hash or Comment as X selection
    procedure Copy_Selection (Copy_Comment : in Boolean) is
      Result : As.U.Asu_Us;
      use type As.U.Asu_Us;
    begin
      if Copy_Comment then
        -- Skip tailing empty lines. No LineFeed after last line
        for I in reverse Comment'Range loop
          if not Comment(I).Is_Null or else not Result.Is_Null then
            if Result.Is_Null then
              Result := Comment(I);
            else
              Result := Comment(I) & Ada.Characters.Latin_1.Lf & Result;
            end if;
          end if;
        end loop;
        -- Prepend date
        if Result.Is_Null then
          Result := As.U.Tus (Date);
        else
          Result := As.U.Tus (Date) & Ada.Characters.Latin_1.Lf & Result;
        end if;
        Afpx.Set_Selection (Result.Image);
      else
        Afpx.Set_Selection (Hash);
      end if;
    end Copy_Selection;

  begin
    -- Full init
    Init (True);

    -- Main loop
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result);
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
            when Utils.X.List_Scroll_Fld_Range'First ..
                 Utils.X.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.List_Manager.Scroll (
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
              Copy_Selection (False);
            when Afpx_Xref.Details.Copy =>
              -- Copy commit comment to clipboard
              Copy_Selection (True);
            when others =>
              -- Other button?
              null;
          end case;

        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          -- Encode current branch
          Utils.X.Encode_Field (Utils.X.Branch_Image (Git_If.Current_Branch),
                                Afpx_Xref.Details.Branch);
      end case;
    end loop;

  end Handle;

end Details;

