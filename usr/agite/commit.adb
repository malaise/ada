with Ada.Exceptions;
with As.U, Directory, Con_Io, Afpx.List_Manager, Str_Util, Basic_Proc,
     Aski, Unicode;
with Git_If, Utils.X, Config, Afpx_Xref, Error;
package body Commit is

  -- Cut string if too long for list
  Width : Afpx.Width_Range;
  function Procuste (Str : String) return String is
  begin
  if Str'Length <= Width then
    -- String fits. OK
    return Str;
  else
    -- Trunk head and show "> " at the beginning
    return Str_Util.Procuste (Str, Width, Trunc_Head => True);
  end if;
  end Procuste;

  function Is_Staged (C : Character) return Boolean is
  begin
    return C /= ' ' and then C /= '?';
  end Is_Staged;

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in  Git_If.File_Entry_Rec) is
  begin
    Afpx.Encode_Line (Line,
      (if Is_Staged (From.S2) then '*' else ' ')
      & From.S2 & From.S3
      & ' ' & Procuste (From.Name.Image) );
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

  -- The changes
  Changes : Git_If.File_List;
  -- The text of the comment
  Comment : As.U.Asu_Us;

  procedure Reread is
    Line : Afpx.Line_Rec;
    Change : Git_If.File_Entry_Rec;
    Reread : Boolean;
    Moved : Boolean;
    To_Commit : Boolean;
  begin
    -- Get list of changes, ensure that each entry is fully staged or not
    --  staged at all, add those that are both,
    --  if still in both then try to reset
    Afpx.Suspend;
    if not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
    else
      Line.Len := 0;
    end if;
    Git_If.List_Changes (Changes);
    Reread := False;
    To_Commit := False;
    if not Changes.Is_Empty then
      Changes.Rewind;
      loop
        Changes.Read (Change, Moved => Moved);
        if Change.S2 /= ' ' and then Change.S3 /= ' '
        and then Is_Staged (Change.S2) then
          -- This entry has somme changes staged and some others not, add all
          Git_If.Do_Add (Change.Name.Image);
          -- Reread and check
          Change := Git_If.Get_Status (Change.Name.Image);
          if Change.S2 /= ' ' and then Change.S3 /= ' '
          and then (Change.S2 /= '?' or else Change.S3 /= '?') then
            -- This entry still has somme changes staged and some others not,
            --  try to reset
            Git_If.Do_Reset (Change.Name.Image);
          end if;
          Reread := True;
        end if;
        exit when not Moved;
      end loop;
      if Reread then
        Git_If.List_Changes (Changes);
      end if;
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
    if not Changes.Is_Empty then
      if Search (Afpx.Line_List, Line,
                 From => Afpx.Line_List_Mng.Absolute) then
        Changes.Move_At (Afpx.Line_List.Get_Position);
      else
        Changes.Rewind;
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
    Afpx.Set_Field_Activation (Afpx_Xref.Commit.Switch,
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

  -- Switch staged status of current file
  procedure Do_Switch is
    Status : Character;
  begin
    Changes.Move_At (Afpx.Line_List.Get_Position);
    Status := Changes.Access_Current.S2;
    if Status = ' ' or else Status = '?' then
      -- File is not staged, add
      Git_If.Do_Add (Changes.Access_Current.Name.Image);
      Reread;
    else
      -- File is staged, reset
      Git_If.Do_Reset (Changes.Access_Current.Name.Image);
      Reread;
    end if;
  end Do_Switch;

  -- Stage all known by unstaged changes
  procedure Do_Stage_All is
    Change : Git_If.File_Entry_Rec;
    Moved : Boolean;
  begin
    -- Get list of changes,
    Afpx.Suspend;
    Git_If.List_Changes (Changes);
    if not Changes.Is_Empty then
      Changes.Rewind;
      loop
        Changes.Read (Change, Moved => Moved);
        if Change.S2 = ' ' and then Change.S3 = 'M' then
          Git_If.Do_Add (Change.Name.Image);
        elsif Change.S2 = ' ' and then Change.S3 = 'D' then
          Git_If.Do_Rm (Change.Name.Image);
        end if;
        exit when not Moved;
      end loop;
    end if;
    Afpx.Resume;
    Reread;
  end Do_Stage_All;

  -- Decode comments and commit
  procedure Do_Commit is
    Result : As.U.Asu_Us;
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
    -- Afpx stuff
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range;

    procedure Init is
      Prev : Positive;
      Field : Afpx.Field_Range;
    begin
      Afpx.Use_Descriptor (Afpx_Xref.Commit.Dscr_Num);
      Cursor_Field := Afpx.Next_Cursor_Field (0);
      Cursor_Col := 0;
      Insert := False;
      -- Encode Root
      Afpx.Encode_Field (Afpx_Xref.Commit.Root, (0, 0),
          Utils.Normalize (Root, Afpx.Get_Field_Width (Afpx_Xref.Commit.Root)));
      -- Encode text of (current) comment
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
    end Init;

  begin
    Differator := As.U.Tus (Config.Differator);

    -- Move to root
    Directory.Change_Current (Root);

    -- Reset comment
    Comment.Set_Null;

    -- Reset Afpx list
    Afpx.Line_List.Delete_List (False);

    -- Init Afpx
    Init;

    -- Width after 3 chars: Staged, Change and a space
    Width := Afpx.Get_Field_Width (Afpx.List_Field_No) - 3;

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
            when Afpx.List_Field_No | Afpx_Xref.Commit.Switch =>
              -- Double click or Switch button
              Do_Switch;
            when Afpx_Xref.Commit.Stage_All =>
              -- StageAll button
              Do_Stage_All;
            when Afpx_Xref.Commit.Commit =>
              -- Commit button
              Do_Commit;
              Init;
              Reread;
            when Afpx_Xref.Commit.Push =>
              -- Push button
              -- @@@
              null;
            when Afpx_Xref.Commit.Back =>
              -- Back button
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

