with Ada.Exceptions;
with As.U, Directory, Con_Io, Afpx.List_Manager, Str_Util, Basic_Proc,
     Aski, Unicode, Images;
with Git_If, Utils.X, Config, Push, Afpx_Xref, Confirm, Error;
package body Commit is

  -- List width minus 4 ("CSU ")
  Width : Afpx.Width_Range;

  function Is_Staged (C : Character) return Boolean is
  begin
    return C /= ' ' and then C /= '?';
  end Is_Staged;

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in  Git_If.File_Entry_Rec) is
  begin
    Afpx.Encode_Line (Line,
      (if    not Is_Staged (From.S2) then ' '
       elsif not Is_Staged (From.S3) then '+'
       else '*')
      & From.S2 & From.S3
      & ' ' & Utils.Normalize (From.Name.Image, Width) );
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised in commit on " & From.Name.Image);
  end Set;

  function Match (Current, Criteria : Afpx.Line_Rec) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return Current.Str(4 .. Current.Len) = Criteria.Str(4 .. Criteria.Len);
  end Match;
  function Search is new Afpx.Line_List_Mng.Search (Match);

  procedure Init_List is new Afpx.List_Manager.Init_List (
    Git_If.File_Entry_Rec, Git_If.File_Mng, Set);

  -- Differator
  Differator : As.U.Asu_Us;

  -- Root path
  Root : As.U.Asu_Us;

  -- The changes
  Changes : Git_If.File_List;

  -- The text of the comment
  Comment : As.U.Asu_Us;

  -- Afpx Ptg stuff
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;

  -- Reset Ptg stuff
  procedure Reset_Ptg is
  begin
    Cursor_Field := Afpx.Next_Cursor_Field (0);
    Cursor_Col := 0;
    Insert := False;
  end Reset_Ptg;

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
          Afpx.Encode_Field (Field, (0, 0), Comment.Slice (Prev, I-1));
          Field := Field + 1;
          Prev := I + 1;
        end if;
      end loop;
    end if;
  end Encode_Comment;

  -- Init screen
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Commit.Dscr_Num);
    -- Encode Root
    Afpx.Encode_Field (Afpx_Xref.Commit.Root, (0, 0),
        Utils.Normalize (Root.Image,
                         Afpx.Get_Field_Width (Afpx_Xref.Commit.Root)));
    -- Encode comment
    Encode_Comment;
    -- Reset Ptg stuff
    Reset_Ptg;
  end Init;

  -- Re assess the status of changes
  procedure Reread is
    Line : Afpx.Line_Rec;
    Change : Git_If.File_Entry_Rec;
    Moved : Boolean;
    To_Commit : Boolean;
  begin
    -- Save current selection
    if not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
    else
      Line.Len := 0;
    end if;

    -- Get list of changes
    Afpx.Suspend;
    Git_If.List_Changes (Changes);
    To_Commit := False;
    if not Changes.Is_Empty then
      -- See if at least one entry to commit
      Changes.Rewind;
      loop
        Changes.Read (Change, Moved => Moved);
        if Is_Staged (Change.S2) then
          To_Commit := True;
        end if;
        exit when not Moved;
      end loop;
    end if;
    Afpx.Resume;

    -- Encode the list
    Init_List (Changes);
    -- Move back to the same entry as before (if possible)
    if not Afpx.Line_List.Is_Empty then
      if Search (Afpx.Line_List, Line,
                 From => Afpx.Line_List_Mng.Absolute) then
        Afpx.Line_List.Move_At (Afpx.Line_List.Get_Position);
      else
        Afpx.Line_List.Rewind;
      end if;
    end if;
    -- Center
    Afpx.Update_List (Afpx.Center_Selected);

    -- Encode current branch
    Afpx.Clear_Field (Afpx_Xref.Commit.Branch);
    Afpx.Encode_Field (Afpx_Xref.Commit.Branch, (0, 0),
        Utils.X.Branch_Image (Git_If.Current_Branch,
            Afpx.Get_Field_Width (Afpx_Xref.Commit.Branch)));
    -- Set field activity
    Afpx.Set_Field_Activation (Afpx_Xref.Commit.Stage,
                               not Afpx.Line_List.Is_Empty);
    Afpx.Set_Field_Activation (Afpx_Xref.Commit.Unstage,
                               not Afpx.Line_List.Is_Empty);
    Afpx.Set_Field_Activation (Afpx_Xref.Commit.Diff,
                               not Afpx.Line_List.Is_Empty);
    Afpx.Set_Field_Activation (Afpx_Xref.Commit.Stage_All,
                               not Afpx.Line_List.Is_Empty);
    Afpx.Set_Field_Activation (Afpx_Xref.Commit.Commit, To_Commit);
   exception
     when others =>
       Afpx.Resume;
       raise;
  end Reread;

  -- Diff
  procedure Do_Diff is
  begin
    Changes.Move_At (Afpx.Line_List.Get_Position);
    Git_If.Launch_Diff (Differator.Image,
                        Changes.Access_Current.Name.Image);
  end Do_Diff;

  -- Staged / Unstage current file
  procedure Do_Stage (Stage : Boolean)  is
    Status : Character;
  begin
    Changes.Move_At (Afpx.Line_List.Get_Position);
    Status := Changes.Access_Current.S2;
    if Stage then
      if Status = ' ' or else Status = '?' then
        -- File is not staged, add
        Git_If.Do_Add (Changes.Access_Current.Name.Image);
        Reread;
      end if;
    else
      -- File is staged, reset
      Git_If.Do_Reset (Changes.Access_Current.Name.Image);
      Reread;
    end if;
  end Do_Stage;

  -- Stage all unstaged changes
  procedure Do_Stage_All is
    Change : Git_If.File_Entry_Rec;
    Untracked : Natural;
    Moved : Boolean;
  begin
    -- Get list of changes,
    Afpx.Suspend;
    Git_If.List_Changes (Changes);
    Untracked := 0;
    if not Changes.Is_Empty then
      Changes.Rewind;
      loop
        Changes.Read (Change, Moved => Moved);
        if Change.S3 = 'M' then
          Git_If.Do_Add (Change.Name.Image);
        elsif Change.S3 = 'D' then
          Git_If.Do_Rm (Change.Name.Image);
        elsif Change.S3 = '?' then
          Untracked := Untracked + 1;
        end if;
        exit when not Moved;
      end loop;
    end if;
    Afpx.Resume;

    if Untracked /= 0 then
      Decode_Comment;
      if Confirm ("Staging all",
                  "Stage " & Images.Integer_Image (Untracked)
                           & " untracked file"
                           & (if Untracked = 1 then "" else "s") & "?",
                  Ok_Cancel => False) then
        -- Add untracked files
        Afpx.Suspend;
        Git_If.List_Changes (Changes);
        if not Changes.Is_Empty then
          Changes.Rewind;
          loop
            Changes.Read (Change, Moved => Moved);
            if Change.S3 = '?' then
              Git_If.Do_Add (Change.Name.Image);
            end if;
            exit when not Moved;
          end loop;
        end if;
        Afpx.Resume;
      end if;
      Init;
    end if;

    Reread;
  end Do_Stage_All;

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

  -- Handle the commits
  procedure Handle (Root : in String) is
    Ptg_Result   : Afpx.Result_Rec;
    use type Afpx.Field_Range;
  begin
    -- Init differator
    Differator := As.U.Tus (Config.Differator);

    -- Move to root
    Commit.Root := As.U.Tus (Root);
    Directory.Change_Current (Root);

    -- Init Afpx
    Init;

    -- Reset Afpx list
    Afpx.Line_List.Delete_List (False);

    -- Width after 4 chars of Commit, Staged, Unstaged and a space
    Width := Afpx.Get_Field_Width (Afpx.List_Field_No) - 4;

    -- Encode Changes
    Reread;

    -- Main loop
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result, True);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              -- Move to next line of comment
              if Cursor_Field = Afpx_Xref.Commit.Comment7 then
                Cursor_Field := Afpx_Xref.Commit.Comment1;
              else
                Cursor_Field := Afpx.Next_Cursor_Field (Cursor_Field);
              end if;
              Cursor_Col := 0;
            when Afpx.Escape_Key =>
              -- Back
              return;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Afpx_Xref.Commit.Reread =>
              -- Reread button
              Reread;
            when Afpx_Xref.Commit.Diff =>
              Do_Diff;
            when Afpx.List_Field_No | Afpx_Xref.Commit.Stage =>
              -- Double click or Stage button
              Do_Stage (True);
            when Afpx_Xref.Commit.Unstage =>
              -- Unstage button
              Do_Stage (False);
            when Afpx_Xref.Commit.Stage_All =>
              -- StageAll button
              Do_Stage_All;
            when Afpx_Xref.Commit.Clear =>
              -- StageAll button
              Comment.Set_Null;
              Encode_Comment;
              Reset_Ptg;
            when Afpx_Xref.Commit.Commit =>
              -- Commit button
              Do_Commit;
              Init;
              Reread;
            when Afpx_Xref.Commit.Push =>
              -- Push button
              Push.Handle (Root);
              Init;
              Reread;
            when Afpx_Xref.Commit.Back =>
              -- Back button
              Decode_Comment;
              return;
            when others =>
              null;
          end case;

       when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
       when Afpx.Refresh =>
         -- Reread branch and changes
         Reread;
         Init_List (Changes);
      end case;
    end loop;

  end Handle;

end Commit;

