with Ada.Characters.Latin_1, Ada.Exceptions;
with As.U, Con_Io, Afpx.List_Manager, Str_Util, Directory, Basic_Proc;
with Utils.X, View, History, Config, Afpx_Xref;
package body Details is

  List_Width : Afpx.Width_Range;
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Commit_Entry_Rec) is
  begin
    Afpx.Encode_Line (Line, Str_Util.Procuste (
          From.Status & " " & From.File.Image,
          List_Width,
          Trunc_Head => False));
  exception
    when others => null;
  end Set;

  procedure Init_List is new Afpx.List_Manager.Init_List (
    Git_If.Commit_Entry_Rec, Git_If.Commit_File_Mng, Set);


  procedure Handle (Root : in String; Hash : in Git_If.Git_Hash) is

    -- Afpx stuff
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Redisplay    : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    Comment_Height : Afpx.Height_Range;
    Comment_Width  : Afpx.Width_Range;
    use type Afpx.Absolute_Field_Range;

    -- Commit details
    Date : Git_If.Iso_Date;
    Comment : Git_If.Comment_Array(1 .. 10);
    Commits : Git_If.Commit_List;

    procedure Init (Cet_Details : in Boolean) is
    begin
      -- Init Afpx
      Afpx.Use_Descriptor (Afpx_Xref.Details.Dscr_Num);
      Cursor_Field := 1;
      Cursor_Col := 0;
      Insert := False;
      Redisplay := False;

      -- Get commit details
      if Cet_Details then
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
      Afpx.Encode_Field (Afpx_Xref.Details.Hash, (0, 0), Hash);
      Afpx.Encode_Field (Afpx_Xref.Details.Date, (0, 0), Date);
      Afpx.Get_Field_Size (Afpx_Xref.Details.Comment,
                           Comment_Height, Comment_Width);
      Afpx.Clear_Field (Afpx_Xref.Details.Comment);
      for I in 1 .. Comment_Height loop
        begin
          Afpx.Encode_Field (Afpx_Xref.Details.Comment, (I - 1, 0),
               Str_Util.Procuste (Comment(I).Image,
                                    Comment_Width,
                                    Trunc_Head => False));
        exception
          when Error:others =>
            -- Just trace
            Basic_Proc.Put_Line_Error ("Exception "
                & Ada.Exceptions.Exception_Name (Error)
                & " raised on details of " & Hash);
        end;
      end loop;
      -- Encode list
      List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
      Init_List (Commits);
    end Init;

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
            Redisplay := True;
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
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                         Redisplay, Ptg_Result);
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

        when Afpx.Fd_Event =>
          null;
        when Afpx.Timer_Event =>
          null;
        when Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          Redisplay := True;
      end case;
    end loop;

  end Handle;

end Details;

