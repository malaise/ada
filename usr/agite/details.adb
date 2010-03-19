with Ada.Characters.Latin_1, Ada.Exceptions;
with Con_Io, Afpx.List_Manager, String_Mng, Directory, Basic_Proc;
with Utils, View, History;
package body Details is

  List_Width : Afpx.Width_Range;
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Commit_Entry_Rec) is
  begin
    Afpx.Encode_Line (Line, String_Mng.Procuste (
          From.Status & " " & Utils.Asu_Ts (From.File),
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
      Afpx.Use_Descriptor (4);
      Cursor_Field := 1;
      Cursor_Col := 0;
      Insert := False;

      -- Get commit details
      if Cet_Details then
        Redisplay := True;
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
      Afpx.Encode_Field (10, (0, 0), Hash);
      Afpx.Encode_Field (11, (0, 0), Date);
      Afpx.Get_Field_Size (12, Comment_Height, Comment_Width);
      Afpx.Clear_Field (12);
      for I in 1 .. Comment_Height loop
        begin
          Afpx.Encode_Field (12, (I - 1, 0),
               String_Mng.Procuste (Utils.Asu_Ts (Comment(I)),
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
    type Show_List is (Show_View, Show_Hist);
    procedure Show (What : in Show_List) is
      Pos : constant Positive := Afpx.Line_List.Get_Position;
      Commit : Git_If.Commit_Entry_Rec;
    begin
      Commits.Move_To (Git_If.Commit_File_Mng.Dyn_List.Next, Pos - 1,
                       From_Current => False);
      Commits.Read (Commit, Git_If.Commit_File_Mng.Dyn_List.Current);
      case What is
        when Show_View =>
          -- Only files except leading "/"
          if Utils.Asu_Ts (Commit.File) /= "/" then
            View (Utils.Asu_Ts (Commit.File), Hash);
          end if;
          Redisplay := True;
        when Show_Hist =>
          declare
            Path : constant String
                 := Directory.Dirname (Utils.Asu_Ts (Commit.File));
            File : constant String
                 := Directory.Basename (Utils.Asu_Ts (Commit.File));
          begin
            History.Handle (Root, Path, File,
                            Utils.Asu_Ts (Commit.File) /= "/",
                            Hash);
          end;
          -- Re init sreen
          Init (False);
      end case;
    end Show;

    -- Copy Comments as X selection
    procedure Copy_Selection is
      Result : Utils.Asu_Us;
      use type Utils.Asu_Us;
    begin
      -- Skip tailing empty lines. No LineFeed after last line
      for I in reverse Comment'Range loop
        if Comment(I) /= Utils.Asu_Null or else Result /= Utils.Asu_Null then
          if Result = Utils.Asu_Null then
            Result := Comment(I);
          else
            Result := Comment(I) & Ada.Characters.Latin_1.Lf & Result;
          end if;
        end if;
      end loop;
      Afpx.Set_Selection (Utils.Asu_Ts (Result));
    end Copy_Selection;

  begin
    -- Full init
    Init (True);

    -- Main loop
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                         Ptg_Result, Redisplay);

      Redisplay := False;
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
            when Utils.List_Scroll_Fld_Range'First ..
                 Utils.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.List_Manager.Scroll (Ptg_Result.Field_No
                                      - Utils.List_Scroll_Fld_Range'First + 1);
            when 13 =>
              -- View
              Show (Show_View);
            when 14 =>
              -- History
              Show (Show_Hist);
            when 15 =>
              -- Back
              return;
            when 16 =>
              -- Copy commit comment to clipboard
              Copy_Selection;
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

