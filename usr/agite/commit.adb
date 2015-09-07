with Ada.Exceptions;
with As.U, Directory, Afpx.Utils, Str_Util, Basic_Proc,
     Aski, Images;
with Utils.X, Config, Push_Pull, Afpx_Xref, Confirm, Error;
package body Commit is

  -- List width
  List_Width : Afpx.Width_Range;

  function Is_Staged (C : Character) return Boolean is
  begin
    return C /= ' ' and then C /= '?';
  end Is_Staged;

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.File_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line (
        (if    not Is_Staged (From.S2) then ' '
         elsif not Is_Staged (From.S3) then '+'
         else '*')
      & From.S2 & From.S3 & ' ',
        From.Name.Image, "", List_Width, Line);
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised in commit on " & From.Name.Image);
  end Set;

  function Match (Current, Criteria : Git_If.File_Entry_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Kind = Criteria.Kind and then Current.Name = Criteria.Name;
  end Match;
  function Change_Search is new Git_If.File_Mng.Dyn_List.Search (Match);


  procedure Init_List is new Afpx.Utils.Init_List (
    Git_If.File_Entry_Rec, Git_If.File_Mng, Set, False);

  -- Editor and Differator
  Editor, Differator : As.U.Asu_Us;

  -- Root path
  Root : As.U.Asu_Us;

  -- The changes
  Changes : Git_If.File_List;

  -- The text of the comment
  Comment : As.U.Asu_Us;

  -- Afpx Ptg stuff
  Get_Handle : Afpx.Get_Handle_Rec;

  -- Reset Ptg stuff
  procedure Reset_Ptg is
  begin
    Get_Handle := (others => <>);
  end Reset_Ptg;

  -- Decode Comment fields
  Nb_Row_Comment : constant := 7;
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
      for I in 2 .. Comment.Length loop
        if Comment.Element (I) = Aski.Lf then
          Utils.X.Encode_Field (Comment.Slice (Prev, I - 1), Field);
          exit when Field = Afpx_Xref.Commit.Comment7;
          Field := Field + 1;
          Prev := I + 1;
        end if;
      end loop;
    end if;
  end Encode_Comment;

  -- Encode comment from a commit
  procedure Encode_Commit (Ref : in Git_If.Git_Hash) is
    Hash : Git_If.Git_Hash;
    Merged : Boolean;
    Date : Git_If.Iso_Date;
    Comment_Array : Git_If.Comment_Array (1 .. Nb_Row_Comment);
    Commit : Git_If.Commit_List;
    Do_Copy : Boolean;
  begin
    if Ref = Git_If.No_Hash then
      return;
    end if;
    -- Get Commit info (comment)
    Git_If.List_Commit (Ref, Hash, Merged, Date, Comment_Array, Commit);
    Comment.Set_Null;
    -- Append rows, starting from last non-empty
    Do_Copy := False;
    for I in reverse Comment_Array'Range loop
      if not Comment_Array(I).Is_Null then
        Do_Copy := True;
      end if;
      if Do_Copy then
        Comment.Prepend (Comment_Array(I).Image & Aski.Lf);
      end if;
    end loop;
  end Encode_Commit;


  -- Init screen
  procedure Init (In_Loop : in Boolean) is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Commit.Dscr_Num);
    -- Encode Root
    Utils.X.Encode_Field (Root.Image, Afpx_Xref.Commit.Root);
    -- Encode comment
    Encode_Comment;
    -- Reset Ptg stuff
    Reset_Ptg;
    -- Change buttons when in loop
    if In_Loop then
      Afpx.Encode_Field (Afpx_Xref.Commit.Back, (1, 2), "Done");
      Afpx.Encode_Field (Afpx_Xref.Commit.Push, (1, 1), "Quit");
    end if;
  end Init;

  -- Re assess the status of changes
  procedure Reread (Force : in Boolean) is
    Current_Change : Git_If.File_Entry_Rec;
    Moved : Boolean;
    To_Commit : Boolean;
    Pos : Natural := 0;
    Prev_Changes : Git_If.File_List;
    Changed : Boolean;
    use type Git_If.File_Entry_Rec;
  begin
    Changed := Force;
    -- Save current position and entry
    if not Force and then not Changes.Is_Empty
    and then not Afpx.Line_List.Is_Empty then
      Pos := Afpx.Line_List.Get_Position;
      Changes.Move_At (Pos);
      Changes.Read (Current_Change, Git_If.File_Mng.Dyn_List.Current);
      -- Make a copy of files list
      Prev_Changes.Insert_Copy (Changes);
    end if;

    -- Refresh list only if it has changed
    -- Update list of files and branch
    Git_If.List_Changes (Changes);
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
    To_Commit := False;
    if not Changes.Is_Empty then
      Pos := Changes.Get_Position;
      -- See if at least one entry to commit
      Changes.Rewind;
      loop
        Changes.Read (Current_Change, Moved => Moved);
        if Is_Staged (Current_Change.S2) then
          To_Commit := True;
        end if;
        exit when not Moved;
      end loop;
      Changes.Move_At (Pos);
    end if;

    -- Set field activity
    Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Stage,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Unstage,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Diff,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Stage_All,
                              Afpx.Line_List.Is_Empty);
    -- Allow commit if some stages
    Afpx.Utils.Protect_Field (Afpx_Xref.Commit.Commit, not To_Commit);
  end Reread;

  -- Edit
  procedure Do_Edit is
  begin
    Changes.Move_At (Afpx.Line_List.Get_Position);
    Utils.Launch (Editor.Image & " "
                & Utils.Protect_Text (Changes.Access_Current.Name.Image),
                True);
  end Do_Edit;

  -- Diff
  procedure Do_Diff is
  begin
    Changes.Move_At (Afpx.Line_List.Get_Position);
    Git_If.Launch_Diff (Differator.Image,
                        Changes.Access_Current.Name.Image);
  end Do_Diff;

  -- Staged / Unstage current file
  procedure Do_Stage (Stage : in Boolean; Move : in Boolean) is
    Status : Character;
  begin
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
  begin
    -- Reread and update changes
    Git_If.List_Changes (Changes);
    if not Changes.Is_Empty then
      Changes.Rewind;
      loop
        Changes.Read (Change, Moved => Moved);
        if Change.S3 = 'M' or else Change.S3 = 'T' or else Change.S3 = 'U'
        or else Change.S3 = 'A' then
          Git_If.Do_Add (Change.Name.Image);
        elsif Change.S3 = 'D' then
          Git_If.Do_Rm (Change.Name.Image);
        elsif Change.S3 = '?' then
          Untracked.Insert (Change);
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
    -- Git_If.Commit
    Result := As.U.Tus (Git_If.Do_Commit (Comment.Image));
    if Result.Is_Null then
      return;
    end if;
    -- Show error
    Error ("Commit", "", Result.Image);
  end Do_Commit;

  -- Handle the commit of modifications
  -- Show button Done instead of Back, Quit instead of Push
  -- Init comment from the one of the provided Hash
  function Common_Handle (
           Root : String;
           In_Loop : Boolean;
           Hash_For_Comment : Git_If.Git_Hash) return Boolean is
    Ptg_Result   : Afpx.Result_Rec;
    use type Afpx.Field_Range;
  begin
    -- Init editor and differator
    Editor := As.U.Tus (Config.Editor);
    Differator := As.U.Tus (Config.Differator);

    -- Move to root
    Commit.Root := As.U.Tus (Root);
    Directory.Change_Current (Root);

    -- Encode the comment of Hash_For_Comment into Comment
    Encode_Commit (Hash_For_Comment);

    -- Init Afpx
    Init (In_Loop);

    -- Reset Afpx list
    Afpx.Line_List.Delete_List (False);

    -- List width
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);

    -- Encode Changes
    Reread (True);

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result, True);

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
            when Afpx.Escape_Key =>
              -- Back
              return True;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Afpx.List_Field_No =>
              -- Double click: stage or unstage
              Switch_Stage;
            when Utils.X.List_Scroll_Fld_Range'First ..
                 Utils.X.List_Scroll_Fld_Range'Last =>
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
            when Afpx_Xref.Commit.Commit =>
              -- Commit button
              Do_Commit;
              Init (In_Loop);
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
                Init (In_Loop);
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
         Reread (False);

       when Afpx.Fd_Event | Afpx.Signal_Event =>
          null;
       when Afpx.Refresh =>
         -- Reread branch and changes if they have changed
         Reread (False);
      end case;
    end loop;

  end Common_Handle;

  -- Handle the commit of modifications
  procedure Handle (Root : in String) is
    Dummy : Boolean;
  begin
    Dummy := Common_Handle (Root, False, Git_If.No_Hash);
  end Handle;

  -- Handle the commit of modifications
  -- Show button Quit instead of Push
  -- Init comment from the one of the provided Hash
  function Handle (Root : String;
                   Hash_For_Comment : Git_If.Git_Hash) return Boolean is
  begin
    return Common_Handle (Root, True, Hash_For_Comment);
  end Handle;

end Commit;

