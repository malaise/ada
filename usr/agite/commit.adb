with Ada.Exceptions;
with Directory, Con_Io, Afpx.List_Manager, Str_Util, Basic_Proc, Aski, Unicode;
with Git_If, Utils.X, Afpx_Xref;
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

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in  Git_If.File_Entry_Rec) is
  begin
    Afpx.Encode_Line (Line,
        (if    From.S2 = ' ' then " " & From.S3                -- unstaged
         elsif From.S3 = ' ' then "*" & From.S2                -- staged
         elsif From.S2 = '?' and then From.S3 = '?' then " ?"  -- unknown
         else  "xx")                                           -- Both!!!
      & " " & Procuste (From.Name.Image) );
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

  subtype Str2 is String(1 .. 2);
  function Get_Status (Line : Afpx.Line_Rec) return Str2 is
  begin
    return Aski.Encode (Line.Str(1 .. 2));
  end Get_Status;

  procedure Init_List is new Afpx.List_Manager.Init_List (
    Git_If.File_Entry_Rec, Git_If.File_Mng, Set);

  -- The changes
  Changes : Git_If.File_List;

  procedure Reread is
    Line : Afpx.Line_Rec;
    Change : Git_If.File_Entry_Rec;
    Reread : Boolean;
    Moved : Boolean;
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
    if not Changes.Is_Empty then
      Changes.Rewind;
      loop
        Changes.Read (Change, Moved => Moved);
        if Change.S2 /= ' ' and then Change.S3 /= ' '
        and then (Change.S2 /= '?' or else Change.S3 /= '?') then
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
    end if;
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
    Afpx.Resume;

    -- Encode current branch
    Afpx.Clear_Field (Afpx_Xref.Commit.Branch);
    Afpx.Encode_Field (Afpx_Xref.Commit.Branch, (0, 0),
        Utils.X.Branch_Image (Git_If.Current_Branch,
            Afpx.Get_Field_Width (Afpx_Xref.Commit.Branch)));
    -- Set field activity
    if Afpx.Line_List.Is_Empty then
      Afpx.Set_Field_Activation (Afpx_Xref.Commit.Switch, False);
      Afpx.Set_Field_Activation (Afpx_Xref.Commit.Stage_All, False);
      Afpx.Set_Field_Activation (Afpx_Xref.Commit.Commit, False);
    else
      Afpx.Set_Field_Activation (Afpx_Xref.Commit.Switch, True);
      Afpx.Set_Field_Activation (Afpx_Xref.Commit.Stage_All, True);
      Afpx.Set_Field_Activation (Afpx_Xref.Commit.Commit, True);
    end if;
   exception
     when others =>
       Afpx.Resume;
       raise;
  end Reread;

  -- Switch staged status of current file
  procedure Do_Switch is
    Status : Str2;
  begin
    Status := Get_Status (Afpx.Line_List.Access_Current.all);
    Changes.Move_At (Afpx.Line_List.Get_Position);
    if Status(1) = ' ' then
      -- File is not staged, add
      Git_If.Do_Add (Changes.Access_Current.Name.Image);
      Reread;
    elsif Status(1) = '*' then
      -- File is staged, reset
      Git_If.Do_Reset (Changes.Access_Current.Name.Image);
      Reread;
    end if;
  end Do_Switch;

  -- Decode comments and commit
  procedure Do_Commit is
  begin
    null;
    -- @@@
    -- Decode comment
    -- Git_If.Commit
    -- if Error then
    --   Error screen
    --   Restore
  end Do_Commit;

  -- Handle the commits
  procedure Handle (Root : in String) is
    -- Afpx stuff
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    List_Height  : Afpx.Height_Range;
    List_Width   : Afpx.Width_Range;
    use type Afpx.Absolute_Field_Range;

  begin
    -- Move to root
    Directory.Change_Current (Root);

    -- Init Afpx
    Afpx.Use_Descriptor (Afpx_Xref.Commit.Dscr_Num);
    Cursor_Field := Afpx.Next_Cursor_Field (0);
    Cursor_Col := 0;
    Insert := False;
    -- List characteristics
    Afpx.Get_Field_Size (Afpx.List_Field_No, List_Height, List_Width);
    -- Encode Root
    Afpx.Encode_Field (Afpx_Xref.Commit.Root, (0, 0),
        Utils.Normalize (Root, Afpx.Get_Field_Width (Afpx_Xref.Commit.Root)));

    -- Width after 3 chars: Staged, Change and a space
    Width := Afpx.Get_Field_Width (Afpx.List_Field_No) - 3;

    -- Encode Changes
    Afpx.Line_List.Delete_List (False);
    Reread;

    -- Main loop
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result, True);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              -- Move to next line of comment
              -- @@@
              null;
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
            when Afpx.List_Field_No | Afpx_Xref.Commit.Switch =>
              -- Double click or Switch button
              Do_Switch;
            when Afpx_Xref.Commit.Stage_All =>
              -- StageAll button
              -- @@@
              null;
            when Afpx_Xref.Commit.Commit =>
              -- Commit button
              Do_Commit;
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

