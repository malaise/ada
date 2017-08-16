with Ada.Exceptions;
with As.U, Directory, Afpx.Utils, Str_Util, Basic_Proc,
     Aski, Images, Trilean, Con_Io;
with Utils.X, Config, Push_Pull, Afpx_Xref, Confirm, Error, Stash, Reset;
package body Commit is

  -- The text of the comment
  Comment : As.U.Asu_Us;
  Nb_Row_Comment : constant := 7;

  -- Encode comment from a commit
  function Encode_Commit (Ref : Git_If.Git_Hash) return As.U.Asu_Us is
    Hash : Git_If.Git_Hash;
    Merged : Boolean;
    Date : Git_If.Iso_Date;
    Comment_Array : Git_If.Comment_Array (1 .. Nb_Row_Comment);
    Commit : Git_If.Commit_List;
    Result : As.U.Asu_Us;
    Do_Copy : Boolean;
  begin
    if Ref = Git_If.No_Hash then
      return Result;
    end if;
    -- Get Commit info (comment)
    Git_If.List_Commit (Ref, Hash, Merged, Date, Comment_Array, Commit);
    -- Append rows, starting from last non-empty
    Do_Copy := False;
    for Row of reverse Comment_Array loop
      if not Row.Is_Null then
        Do_Copy := True;
      end if;
      if Do_Copy then
        Result.Prepend (Row.Image & Aski.Lf);
      end if;
    end loop;
    return Result;
  end Encode_Commit;

  -- List width
  List_Width : Afpx.Width_Range;
  -- Comment width
  Comment_Width : Afpx.Width_Range;

  function Is_Staged (C : Character) return Boolean is
    (C /= ' ' and then C /= '?' and then C /= '-');

  -- Is E Staged
  function Staged (E : Git_If.File_Entry_Rec) return Trilean.Trilean is
    (if    not Is_Staged (E.S2) then Trilean.False
     elsif not Is_Staged (E.S3) then Trilean.True
     else Trilean.Other);

  -- Is E unknown
  function Is_Unknown (E : Git_If.File_Entry_Rec) return Boolean is
    (E.S2 = '?' and then E.S3 = '?');

  -- Separator File entry and Afpx line
  Sep_File : constant Git_If.File_Entry_Rec
           := (Kind => '-', S2 | S3 => '-', others => <>);
  Sep : constant Afpx.Line_Rec := (Len => Afpx.Line_Len_Range'Last,
                                   Str => (others => Character'Pos('-')) );
  function Is_Sep return Boolean is
    use type Afpx.Line_Rec;
  begin
    return Afpx.Line_List.Access_Current.all = Sep;
  end Is_Sep;

  -- Encode Afpx line
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.File_Entry_Rec) is
    use type Git_If.File_Entry_Rec;
  begin
    if From = Sep_File then
      Line := Sep;
    else
      Afpx.Utils.Encode_Line ( (case Staged (From) is
                                  when Trilean.False => ' ',
                                  when Trilean.True  => '+',
                                  when Trilean.Other => '*')
                               & From.S2 & From.S3 & ' ',
                              From.Name.Image, "", List_Width, Line);
    end if;
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised in commit on " & From.Name.Image);
  end Set;

  -- Init Afpx list
  procedure Init_List is new Afpx.Utils.Init_List (
    Git_If.File_Entry_Rec, Git_If.File_Mng, Set, False);


  -- Search entry by kind and name
  function Match (Current, Criteria : Git_If.File_Entry_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Kind = Criteria.Kind and then Current.Name = Criteria.Name;
  end Match;
  function Change_Search is new Git_If.File_Mng.Dyn_List.Search (Match);

  -- Sort entry: Unstaged, Partly staged, Staged
  function Less_Than (E1, E2 : Git_If.File_Entry_Rec) return Boolean is
    S1, S2 : Trilean.Trilean;
    use type Trilean.Trilean, As.U.Asu_Us;
  begin
    S1 := Staged (E1);
    S2 := Staged (E2);
    if S1 = S2 then
      -- Same stage status
      return E1.Name < E2.Name;
    end if;
    return (case S1 is
        -- S1 is not staged at all
        when Trilean.False => True,
        -- S1 is fully staged
        when Trilean.True => False,
        -- S1 is partially staged, S2 is fully or not
        when Trilean.Other => S2 = Trilean.True);
  end Less_Than;
  procedure Sort is new Git_If.File_Mng.Dyn_List.Sort (Less_Than);

  -- Insert separators in List
  procedure Separate_List (List : in out Git_If.File_List) is
    Prev, Curr : Trilean.Trilean;
    use type Trilean.Trilean;
  begin
    if List.Is_Empty then
      return;
    end if;
    -- Scan list and insert Sep when first char changes
    List.Rewind;
    Prev := Trilean.False;
    loop
      -- Get status
      Curr := Staged (List.Access_Current.all);
      if Curr /= Prev then
        -- Insert separator file
        List.Insert (Sep_File, Git_If.File_Mng.Dyn_List.Prev);
        List.Move_To;
        if Prev = Trilean.False and then Curr = Trilean.True then
          -- No partially staged => Insert a second separator file
          List.Insert (Sep_File, Git_If.File_Mng.Dyn_List.Prev);
          List.Move_To;
        end if;
      end if;
      Prev := Curr;
      exit when not List.Check_Move;
      List.Move_To;
    end loop;
    -- Add trailing seperators
    if Curr /= Trilean.True then
      -- Last item was not or partially staged => insert a Sep
      List.Insert (Sep_File);
    end if;
    if Curr = Trilean.False then
      -- Last item was not staged => insert a second Sep
      List.Insert (Sep_File);
    end if;
  end Separate_List;

  -- Decode Comment fields
  procedure Decode_Comment is
  begin
    Comment.Set_Null;
    for Field in Afpx_Xref.Commit.Comment1 .. Afpx_Xref.Commit.Comment7 loop
      -- Decode comment, remove trailing spaces,
      Comment.Append (Str_Util.Strip (Afpx.Decode_Field (Field, 0)) & Aski.Lf);
    end loop;
    -- Skip trailing empty lines
    for I in reverse 1 .. Comment.Length loop
      exit when Comment.Element (I) /= Aski.Lf;
      Comment.Trail (1);
    end loop;
    -- Append the last Lf
    if not Comment.Is_Null
    and then Comment.Element (Comment.Length) /= Aski.Lf then
      Comment.Append (Aski.Lf);
    end if;
    -- Save persistent comment
    Config.Save_Comment (Comment.Image);
  end Decode_Comment;

  -- Encode Comment fields
  procedure Encode_Comment is
    Prev : Positive;
    Field : Afpx.Field_Range;
    use type Afpx.Absolute_Field_Range;
  begin
    -- Encode text of (current) comment
    for F in Afpx_Xref.Commit.Comment1 .. Afpx_Xref.Commit.Comment7 loop
      -- Decode comment, remove trailing spaces,
      Afpx.Clear_Field (F);
    end loop;
    if not Comment.Is_Null then
      Prev := 1;
      Field := Afpx_Xref.Commit.Comment1;
      for I in 1 .. Comment.Length loop
        if Comment.Element (I) = Aski.Lf then
          -- Silently trunk tail
          Utils.X.Encode_Field (Comment.Slice (Prev, I - 1), Field,
                                Keep_Tail => False,
                                Show_Cut => False);
          exit when Field = Afpx_Xref.Commit.Comment7;
          Field := Field + 1;
          Prev := I + 1;
        end if;
      end loop;
    end if;
  end Encode_Comment;

  -- Locate line Line_No (Start and Stop(Lf)) in the comment
  -- Set Start to 1 and and Stop to 0 if not found
  procedure Locate_Line (Line_No : in Positive;
                         Start : out Positive; Stop : out Natural) is
    Line_Nb : Natural;
  begin
    -- Locate start (first char) and stop (Lf) of Line_No
    Line_Nb := 0;
    Start := 1;
    for I in 1 .. Comment.Length loop
      if Comment.Element (I) = Aski.Lf then
        -- Got a line
        Line_Nb := Line_Nb + 1;
        Stop := I;
        if Line_Nb = Line_No then
          return;
        end if;
        Start := Stop + 1;
      end if;
    end loop;
    -- Not found
    Start := 1;
    Stop := 0;
  end Locate_Line;

  -- Delete a line
  procedure Delete_Line (Line_No : in Positive) is
    Start : Positive;
    Stop : Natural;
  begin
    -- Locate line
    Locate_Line (Line_No, Start, Stop);
    -- Delete line if it exists
    if Stop /= 0 then
      Comment.Delete (Start, Stop);
    end if;
  end Delete_Line;

  -- Insert an empty line before current in comment
  procedure Insert_Line (Line_No : in Positive) is
    Start : Positive;
    Stop : Natural;
  begin
    -- Locate line
    Locate_Line (Line_No, Start, Stop);
    -- Insert line before current if it exists
    if Stop /= 0 then
      Comment.Insert (Start, Aski.Lf);
      -- Delete extra last line if it exists
      Delete_Line (Nb_Row_Comment + 1);
    end if;
  end Insert_Line;

  -- Merge current line with the next
  procedure Merge_Line (Line_No : in Positive) is
    Start : Positive;
    Stop : Natural;
  begin
    -- Locate line
    Locate_Line (Line_No, Start, Stop);
    -- Delete the Lf if it exists
    if Stop /= 0 then
      Comment.Delete (Stop, Stop);
    end if;
    -- Procuste the length of the new line (Lf will be removed when encoding)
    Stop := Str_Util.Locate (Comment.Image, Aski.Lf & "", Stop);
    if Stop /= 0 and then Stop - Start > Comment_Width then
      Comment.Delete (Start + Comment_Width, Stop - 1);
    end if;
  end Merge_Line;

  -- Split current line before current cursor position
  procedure Split_Line (Line_No : in Positive;
                        Cursor_Pos : in Con_Io.Col_Range) is
    Start : Positive;
    Stop : Natural;
  begin
    -- Locate line
    Locate_Line (Line_No, Start, Stop);
    -- Insert a Lf before Cursor pos
    if Stop /= 0 then
      Comment.Insert (Start + Cursor_Pos, Aski.Lf);
    end if;
  end Split_Line;


  -- Handle the commit of modifications
  -- Show button Done instead of Back, Quit instead of Push
  -- Init comment from the one of the provided Hash
  function Common_Handle (
           Root : String;
           In_Loop : Boolean;
           Title : String;
           Hash_For_Comment : Git_If.Git_Hash;
           Allow_Modif : Boolean;
           Allow_Commit : Commit_Allow_List) return Boolean is

    -- Editor and Differator
    Editor, Differator : As.U.Asu_Us;

    -- The changes
    Changes : Git_If.File_List;

    -- Afpx Ptg stuff
    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result   : Afpx.Result_Rec;

    -- Reset result
    Dummy_Result : Boolean;

    -- True if a commit has already been performed (disables empty commit)
    Commit_Done : Boolean;

    -- Reset Ptg stuff
    procedure Reset_Ptg is
    begin
      Get_Handle := (others => <>);
    end Reset_Ptg;

    -- Get current cursor line no
    function Get_Line_No return Positive is
      use type Afpx.Absolute_Field_Range;
    begin
      return Natural(Get_Handle.Cursor_Field - Afpx_Xref.Commit.Comment1) + 1;
    end Get_Line_No;

    -- Delete current line
    procedure Delete_Line is
    begin
      Decode_Comment;
      Delete_Line (Get_Line_No);
      Get_Handle.Cursor_Col := 0;
      Get_Handle.Offset := 0;
      Encode_Comment;
    end Delete_Line;

    -- Insert an empty line before current in comment
    procedure Insert_Line is
    begin
      Decode_Comment;
      Insert_Line (Get_Line_No);
      Get_Handle.Cursor_Col := 0;
      Get_Handle.Offset := 0;
      Delete_Line (Nb_Row_Comment + 1);
      Encode_Comment;
    end Insert_Line;

    -- Merge current line with the next
    procedure Merge_Line is
    begin
      Decode_Comment;
      if Get_Line_No /= Nb_Row_Comment then
        Merge_Line (Get_Line_No);
        Get_Handle.Cursor_Col := 0;
        Get_Handle.Offset := 0;
      end if;
      Encode_Comment;
    end Merge_Line;

    -- Split current line before current cursor position
    procedure Split_Line is
      use type Afpx.Absolute_Field_Range;
    begin
      Decode_Comment;
      Split_Line (Get_Line_No, Get_Handle.Cursor_Col + Get_Handle.Offset);
      if Get_Line_No /= Nb_Row_Comment then
        Afpx.Set_Offset (Get_Handle.Cursor_Field);
        Get_Handle.Cursor_Field := Get_Handle.Cursor_Field + 1;
        Get_Handle.Cursor_Col := 0;
        Get_Handle.Offset := 0;
      end if;
      Delete_Line (Nb_Row_Comment + 1);
      Encode_Comment;
    end Split_Line;

    -- Init screen
    procedure Init (In_Loop : in Boolean;
                    Title : in String := "") is
    begin
      Afpx.Use_Descriptor (Afpx_Xref.Commit.Dscr_Num);
      -- Encode Root
      Utils.X.Encode_Field (Root, Afpx_Xref.Commit.Root);
      -- Encode comment
      Encode_Comment;
      -- Reset Ptg stuff
      Reset_Ptg;
      -- Change buttons when in loop
      if In_Loop then
        Afpx.Encode_Field (Afpx_Xref.Commit.Back, (1, 2), "Done");
        Afpx.Encode_Field (Afpx_Xref.Commit.Push, (1, 1), "Abort");
      else
        Afpx.Set_Field_Activation (Afpx_Xref.Commit.Reset, False);
      end if;
      -- Change title
      if Title /= "" then
        Utils.X.Center_Field (Title, Afpx_Xref.Commit.Title);
      end if;
    end Init;

    -- Re assess the status of changes
    -- Duration and end time of last read
    Some_Staged : Boolean;
    Some_Unstaged : Boolean;
    All_Unknown : Boolean;
    procedure Reread (Force : in Boolean) is
      Current_Change : Git_If.File_Entry_Rec;
      Moved : Boolean;
      Pos : Natural := 0;
      Prev_Changes : Git_If.File_List;
      Changed : Boolean;
      Protect : Boolean;
      use type Git_If.File_Entry_Rec, Trilean.Trilean;
    begin
      Changed := Force;
      -- Save current position and entry
      if not Changes.Is_Empty
      and then not Afpx.Line_List.Is_Empty then
        Pos := Afpx.Line_List.Get_Position;
        Changes.Move_At (Pos);
        Changes.Read (Current_Change, Git_If.File_Mng.Dyn_List.Current);
        -- Make a copy of files list
        Prev_Changes.Insert_Copy (Changes);
      end if;

      -- Refresh list only if it has changed
      -- Update list of files and branch
      Utils.Chrono.Start;
      Git_If.List_Changes (Changes);
      Sort (Changes);
      Separate_List (Changes);
      Utils.Chrono.Ended;
      Utils.X.Encode_Branch (Afpx_Xref.Commit.Branch);

      -- Check lengths then content
      if not Changed
      and then Changes.List_Length /= Prev_Changes.List_Length then
        Changed := True;
      end if;
      if not Changed and then not Changes.Is_Empty then
        Changes.Rewind;
        Prev_Changes.Rewind;
        loop
          if Changes.Access_Current.all /= Prev_Changes.Access_Current.all then
            -- Stop as soon as one entry differs
            Changed := True;
            exit;
          end if;
          exit when not Changes.Check_Move;
          Changes.Move_To;
          Prev_Changes.Move_To;
        end loop;
      end if;

      -- Copy in Afpx list
      if not Changed then
        -- No change: nothing
        return;
      elsif Pos = 0 then
        -- Initial list was empty
        Init_List (Changes);
        Afpx.Line_List.Rewind (Check_Empty => False);
        Afpx.Update_List (Afpx.Top);
      else
        Init_List (Changes);
        -- Search position back and move Afpx to it
        if Change_Search (Changes, Current_Change,
                          From => Git_If.File_Mng.Dyn_List.Absolute) then
          Afpx.Line_List.Move_At (Changes.Get_Position);
          Afpx.Update_List (Afpx.Center_Selected);
        else
          Afpx.Line_List.Rewind (Check_Empty => False);
          Afpx.Update_List (Afpx.Top);
        end if;
      end if;

      -- Check if some changes are staged
      -- Check if there are only unknown files (All_Unknown)
      Some_Staged := False;
      Some_Unstaged := False;
      All_Unknown := True;
      if not Changes.Is_Empty then
        Pos := Changes.Get_Position;
        -- See if at least one entry to commit
        Changes.Rewind;
        loop
          Changes.Read (Current_Change, Moved => Moved);
          if Is_Staged (Current_Change.S2) then
            Some_Staged := True;
          end if;
          if Current_Change /= Sep_File then
            if not Is_Unknown (Current_Change) then
              -- This is a known file
              All_Unknown := False;
              if Staged (Current_Change) /= Trilean.True then
                -- Not fully staged
                Some_Unstaged := True;
              end if;
            end if;
          end if;
          exit when not Moved;
        end loop;
        Changes.Move_At (Pos);
      end if;

      -- Allow modification of selection
      Protect := Afpx.Line_List.Is_Empty or else not Allow_Modif;
      Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Stage, Protect);
      Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Unstage, Protect);
      Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Edit, Protect);
      Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Stage_All, Protect);
      -- Allow Diff if some file
      Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Diff,
                                Afpx.Line_List.Is_Empty);
      -- Allow commit only if not forbidden
      -- Allow commit if some stages or first commit, not in loop
      Afpx.Reset_Field (Afpx_Xref.Commit.Commit);
      Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Commit,
          Allow_Commit = Forbid or else
          (not Some_Staged and then (In_Loop or else Commit_Done)) );
      -- If not in a cherry pick and some changes are unstaged => Warn
      -- If not in a cherry pick and no change is staged => Warn
      if not Afpx.Get_Field_Protection (Afpx_Xref.Commit.Commit)
      and then not In_Loop
      and then (Some_Unstaged or else not Some_Staged) then
        Afpx.Set_Field_Colors (Afpx_Xref.Commit.Commit,
                               Con_Io.Color_Of ("Red"));
      end if;

      -- Forbid Done if Commit required and still some known files
      --  (to stage or commit)
      Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Back,
          Allow_Commit = Require and then not All_Unknown);
    end Reread;

    -- Reread (False) if not too long/too often
    procedure Reread_If is
    begin
      -- Reread if (Current - Last_End) >= Last_Duration
      if not Utils.Chrono.Overload then
        Reread (False);
      end if;
    end Reread_If;

    -- Edit
    procedure Do_Edit is
    begin
      if Is_Sep then
        return;
      end if;
      Changes.Move_At (Afpx.Line_List.Get_Position);
      Utils.Launch (Editor.Image & " "
                  & Utils.Protect_Text (Changes.Access_Current.Name.Image),
                  True);
    end Do_Edit;

    -- Diff
    procedure Do_Diff is
    begin
      if Is_Sep then
        return;
      end if;
      Changes.Move_At (Afpx.Line_List.Get_Position);
      Git_If.Launch_Diff (Differator.Image,
                          Changes.Access_Current.Name.Image);
    end Do_Diff;

    -- Staged / Unstage current file
    procedure Do_Stage (Stage : in Boolean; Move : in Boolean) is
      Status : Character;
    begin
      if Is_Sep then
        return;
      end if;
      Changes.Move_At (Afpx.Line_List.Get_Position);
      Status := Changes.Access_Current.S3;
      if Stage then
        if Status = '?' or else Status = 'M' or else Status = 'T'
        or else Status = 'U' or else Status = 'A' then
          -- Stage new file or modif or unresolved conflict
          Git_If.Do_Add (Changes.Access_Current.Name.Image);
        elsif Status = 'D' then
          -- Stage deletion of file
          Git_If.Do_Rm (Changes.Access_Current.Name.Image);
        end if;
      else
        -- File is staged, reset
        Git_If.Do_Reset (Changes.Access_Current.Name.Image);
      end if;
      -- Move to next entry
      if Move
      and then Afpx.Line_List.Get_Position (Afpx.Line_List_Mng.From_Last) /= 1
      then
        Afpx.Line_List.Move_To;
      end if;
      Reread (True);
    end Do_Stage;

    -- Switch stage
    procedure Switch_Stage is
    begin
      if Is_Sep then
        return;
      end if;
      Changes.Move_At (Afpx.Line_List.Get_Position);
      if Is_Staged (Changes.Access_Current.S2)
      and then not Is_Staged (Changes.Access_Current.S3) then
        -- File is fully staged
        Do_Stage (False, False);
      else
        -- File is not staged (at least some changes are not)
        Do_Stage (True, False);
      end if;
    end Switch_Stage;

    -- Stage all unstaged changes
    procedure Do_Stage_All (In_Loop : in Boolean) is
      Change : Git_If.File_Entry_Rec;
      Untracked : Git_If.File_List;
      Moved : Boolean;
      use type Git_If.File_Entry_Rec;
    begin
      -- Reread and update changes
      Git_If.List_Changes (Changes);
      Sort (Changes);
      Separate_List (Changes);
      if not Changes.Is_Empty then
        Changes.Rewind;
        loop
          Changes.Read (Change, Moved => Moved);
          if Change /= Sep_File then
            if Change.S3 = 'M' or else Change.S3 = 'T' or else Change.S3 = 'U'
            or else Change.S3 = 'A' then
              Git_If.Do_Add (Change.Name.Image);
            elsif Change.S3 = 'D' then
              Git_If.Do_Rm (Change.Name.Image);
            elsif Change.S3 = '?' then
              Untracked.Insert (Change);
            end if;
          end if;
          exit when not Moved;
        end loop;
      end if;

      if not Untracked.Is_Empty then
        Untracked.Rewind;
        Init_List (Untracked);
        Decode_Comment;
        if Confirm ("Staging all",
                    "Stage " & Images.Integer_Image (Untracked.List_Length)
                             & " untracked file"
                             & (if Untracked.List_Length = 1 then "" else "s")
                             & "?",
                    Ok_Cancel => False,
                    Show_List => True) then
          -- Add untracked files
          loop
            Untracked.Read (Change, Moved => Moved);
            Git_If.Do_Add (Change.Name.Image);
            exit when not Moved;
          end loop;
        end if;
        Init (In_Loop);
      end if;

      Reread (True);
    end Do_Stage_All;

    -- Sign the comment
    procedure Do_Sign is
      Line : As.U.Asu_Us;
      use type As.U.Asu_Us;
    begin
      -- Get user name and email
      Line := As.U.Tus (Git_If.Get_User);

      -- Decode current comment
      Decode_Comment;
      -- Append signature
      Comment.Append (Line & Aski.Lf);
      -- Re-encode
      Encode_Comment;
      Reset_Ptg;
    end Do_Sign;

    -- Decode comments and commit
    procedure Do_Commit is
      Result : As.U.Asu_Us;
    begin
      Decode_Comment;
      if Comment.Is_Null then
        Error ("Commit", "Empty comment", "");
        return;
      end if;
      if not Some_Staged
      and then not Confirm (
          Title   => "Committing an empty commit",
          Action  => "Commit with no staged change?",
          Warning => "",
          Ok_Cancel => True,
          Show_List => False) then
        -- Cancelled by user
        return;
      end if;
      -- Git_If.Commit
      Result := As.U.Tus (Git_If.Do_Commit (Comment.Image));
      if Result.Is_Null then
        Commit_Done := True;
        return;
      end if;
      -- Show error
      Error ("Commit", "", Result.Image);
    end Do_Commit;

    use type Afpx.Field_Range;
  begin
    -- Init editor and differator
    Editor := As.U.Tus (Config.Editor);
    Differator := As.U.Tus (Config.Differator);

    -- No commit done in this session
    Commit_Done := False;

    -- Move to root
    Directory.Change_Current (Root);

    -- Encode the comment of Hash_For_Comment into Comment
    if Hash_For_Comment /= Git_If.No_Hash then
      Comment := Encode_Commit (Hash_For_Comment);
    end if;

    -- Init Afpx
    -- Modify Title if In_Loop: Edit then
    --   if Allow_Modif then Commit else Comment
    Init (In_Loop, Title);

    -- Reset Afpx list
    Afpx.Line_List.Delete_List (False);

    -- List width
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
    Comment_Width := Afpx.Get_Data_Len (Afpx_Xref.Commit.Comment1);

    -- Encode Changes
    Reread (True);

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              -- Move to next line of comment
              if Get_Handle.Cursor_Field = Afpx_Xref.Commit.Comment7 then
                Get_Handle.Cursor_Field := Afpx_Xref.Commit.Comment1;
              else
                Get_Handle.Cursor_Field := Afpx.Next_Cursor_Field (
                    Get_Handle.Cursor_Field);
              end if;
              Get_Handle.Cursor_Col := 0;
              Get_Handle.Offset := 0;
            when Afpx.Escape_Key =>
              null;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Afpx.List_Field_No =>
              -- Double click: stage or unstage
              if Allow_Modif then
                Switch_Stage;
              end if;
            when Utils.X.List_Scroll_Fld_Range =>
              -- Scroll list
              Afpx.Utils.Scroll(
                  Ptg_Result.Field_No
                - Utils.X.List_Scroll_Fld_Range'First
                + 1);
            when Afpx_Xref.Commit.Reread =>
              -- Reread button
              Reread (True);
            when Afpx_Xref.Commit.Edit =>
              Do_Edit;
            when Afpx_Xref.Commit.Diff =>
              Do_Diff;
            when Afpx_Xref.Commit.Stash =>
              Stash.Handle (Root);
              Init (In_Loop, Title);
              Reread (True);
            when Afpx_Xref.Commit.Reset =>
              -- Allow only hard reset to head
              Dummy_Result := Reset (Root, "", Only_Hard => True);
              Init (In_Loop, Title);
              Reread (True);

            when Afpx_Xref.Commit.Stage =>
              -- Stage button
              Do_Stage (True, True);
            when Afpx_Xref.Commit.Unstage =>
              -- Unstage button
              Do_Stage (False, True);
            when Afpx_Xref.Commit.Stage_All =>
              -- StageAll button
              Do_Stage_All (In_Loop);
            when Afpx_Xref.Commit.Copy =>
              -- Copy button
              Decode_Comment;
              Afpx.Set_Selection (Comment.Image);
            when Afpx_Xref.Commit.Sign =>
              -- Sign button
              Do_Sign;
            when Afpx_Xref.Commit.Clear =>
              -- Clear button
              Comment.Set_Null;
              Encode_Comment;
              Reset_Ptg;
            when Afpx_Xref.Commit.Ins_Line =>
              -- InsLine button
              Insert_Line;
            when Afpx_Xref.Commit.Del_Line =>
              -- DelLine button
              Delete_Line;
            when Afpx_Xref.Commit.Merge_Line =>
              -- MergeLine button
              Merge_Line;
            when Afpx_Xref.Commit.Split_Line =>
              -- SplitLine button
              Split_Line;
            when Afpx_Xref.Commit.Commit =>
              -- Commit button
              Do_Commit;
              Init (In_Loop, Title);
              Reread (True);
            when Afpx_Xref.Commit.Push =>
              if In_Loop then
                -- Quit in a loop
                return False;
              end if;
              -- Push button
              Decode_Comment;
              if Push_Pull.Handle (Root, Pull => False) then
                return True;
              else
                Init (In_Loop, Title);
                Reread (True);
              end if;
            when Afpx_Xref.Commit.Back =>
              -- Back / Done button
              Decode_Comment;
              return True;
            when others =>
              null;
          end case;

       when Afpx.Timer_Event =>
         -- Reread branch and changes if they have changed
         Reread_If;

       when Afpx.Fd_Event | Afpx.Signal_Event =>
          null;
       when Afpx.Refresh =>
         -- Reread branch and changes if they have changed
         Reread_If;
      end case;
    end loop;

  end Common_Handle;

  -- Handle the commit of modifications
  procedure Handle (Root : in String;
                    Allow_Modif : in Boolean := True) is
    Dummy : Boolean;
  begin
    Dummy := Common_Handle (Root, False, "", Git_If.No_Hash,
                            Allow_Modif, Allow);
  end Handle;

  -- Handle the commit of modifications within a loop (of cherry-pick)
  -- Show button Abort instead of Push nd Done i.o. Back
  -- Allow modifications of content or only of comment
  -- Forbid, allow commit or require it (no more staged)
  -- Init comment from the one of the provided Hash
  function Handle (Root : String;
                   Title : String;
                   Hash_For_Comment : Git_If.Git_Hash;
                   Allow_Modif : Boolean := True;
                   Allow_Commit : Commit_Allow_List := Allow) return Boolean is
    (Common_Handle (Root, True, Title, Hash_For_Comment,
                    Allow_Modif, Allow_Commit));

  -- Get comment of a commit or comment previously entered
  function Get_Comment (Hash : Git_If.Git_Hash) return String is
    (if Hash /= Git_If.No_Hash then Encode_Commit (Hash).Image
     else Comment.Image);

  -- Set default comment for next commit
  procedure Set_Comment (Str : in String) is
  begin
    Comment := As.U.Tus (Str);
    if not Comment.Is_Null
    and then Comment.Element (Comment.Length) /= Aski.Lf then
      Comment.Append (Aski.Lf);
    end if;
  end Set_Comment;

  -- Concat a new comment to the default comment for next commit
  procedure Cat_Comment (Str : in String) is
    Line_Nb : Natural;
  begin
    Comment.Append (Str);
    -- Locate last Lf
    Line_Nb := 0;
    for I in 1 .. Comment.Length loop
      if Comment.Element (I) = Aski.Lf then
        -- Got a line
        Line_Nb := Line_Nb + 1;
        if Line_Nb = Nb_Row_Comment then
          -- Delete tail of comment (if any)
          Comment.Delete (I + 1, Comment.Length);
          exit;
        end if;
      end if;
    end loop;
    if not Comment.Is_Null
    and then Comment.Element (Comment.Length) /= Aski.Lf then
      Comment.Append (Aski.Lf);
    end if;
  end Cat_Comment;

end Commit;

