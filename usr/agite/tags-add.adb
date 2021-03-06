with Afpx.Utils, Str_Util, Directory;
with Utils.X, Utils.Store, Afpx_Xref, Error, Config, View;
separate (Tags)
procedure Add (Rev : in Git_If.Git_Hash) is

  -- The current list of Commit entries
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
    Git_If.Commit_Entry_Rec, Git_If.Set, Git_If.Commit_File_Mng, Set, False);


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
    Dummy_Moved : Boolean;
  begin
    -- Init Afpx
    Afpx.Use_Descriptor (Afpx_Xref.Add_Tag.Dscr_Num);
    Get_Handle := (others => <>);
    Afpx.Get_Field_Size (Afpx_Xref.Add_Tag.Comment,
                         Comment_Height, Comment_Width);
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);

    -- Encode current branch
    Utils.X.Encode_Branch (Afpx_Xref.Add_Tag.Branch);

    -- Get commit details
    if Get_Details then
      Git_If.List_Commit (Rev.Image, Hash, Merged, Date, Comment, Commits);
    end if;
    -- Remove first " /" line
    if not Commits.Is_Empty then
      Commits.Rewind;
      Commits.Delete (Moved => Dummy_Moved);
    end if;

    -- Encode info
    Utils.X.Encode_Field (Hash.Image, Afpx_Xref.Add_Tag.Hash);
    Utils.X.Encode_Field (Date, Afpx_Xref.Add_Tag.Date);
    Afpx.Clear_Field (Afpx_Xref.Add_Tag.Comment);
    for I in 1 .. Comment_Height loop
      -- No clear, and keep Head
      Afpx.Utils.Encode_Field (Comment(I).Image, Afpx_Xref.Add_Tag.Comment,
                               I - 1, False, False);
    end loop;
    -- Encode list
    Init_List (Commits);
  end Init;

  -- Launch viewer on current file, or history on current dir or file
  type Show_List is (Show_View, Show_Diff);
  procedure Show (What : in Show_List) is
    Pos : constant Afpx.Line_List_Mng.Ll_Positive
        := Afpx.Line_List.Get_Position;
    Commit : Git_If.Commit_Entry_Rec;
  begin
    Commits.Move_At (Pos);
    Commits.Read (Commit, Git_If.Commit_File_Mng.Current);
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
        when Show_Diff =>
          -- Call delta between previous of this file and this commit
          Git_If.Launch_Delta (Config.Differator, Path & File,
                           Hash.Image & "^", Hash.Image);
      end case;
    end;
  end Show;

  -- Get tag name and comment, and add the tag on hash. Return true on success
  function Add_Tag (Annotated : in Boolean) return Boolean is
    Tag : As.U.Asu_Us;
    Comment : As.U.Asu_Us;
    Result : As.U.Asu_Us;
  begin
    -- Get Tag name and comment
    Tag := As.U.Tus (Str_Util.Strip (
        Afpx.Decode_Field (Afpx_Xref.Add_Tag.Tag_Name, 0)));
    if Tag.Is_Null then
      return False;
    end if;
    Comment := As.U.Tus (Str_Util.Strip (
      Afpx.Decode_Field (Afpx_Xref.Add_Tag.Tag_Comment, 0)));

    -- Create tag
    Result := As.U.Tus (Git_If.Add_Tag (Tag.Image, Rev, Annotated,
                        Comment.Image));
    if Result.Is_Null then
      return True;
    end if;

    -- Handle error
    Error ("Tag add",
           Tag.Image & (if Annotated then " -a -m " & Comment.Image else ""),
           Result.Image);
    -- restore
    Init (False);
    Afpx.Encode_Field (Afpx_Xref.Add_Tag.Tag_Name, (0, 0), Tag.Image);
    Afpx.Encode_Field (Afpx_Xref.Add_Tag.Tag_Comment, (0, 0), Comment.Image);
    return False;
  end Add_Tag;

begin
  -- Full init
  Init (True);

  -- Main loop
  loop
    Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            null;
          when Afpx.Escape_Key =>
            null;
          when Afpx.Break_Key =>
            raise Utils.Exit_Requested;
        end case;

      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          when Utils.X.List_Scroll_Fld_Range =>
            -- Scroll list
            Afpx.Utils.Scroll (
               Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
          when Afpx_Xref.Add_Tag.View =>
            Show (Show_View);
          when Afpx_Xref.Add_Tag.Diff =>
            Show (Show_Diff);
          when Afpx_Xref.Add_Tag.Mark =>
            Utils.Store.Hash := Hash;
          when Afpx_Xref.Add_Tag.Tag_Annotated =>
            -- Tag annotated
            if Add_Tag (True) then
              return;
            end if;
          when Afpx_Xref.Add_Tag.Tag_Simple =>
            -- Tad not annotated
            if Add_Tag (False) then
              return;
            end if;
          when Afpx_Xref.Add_Tag.Back =>
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
        Utils.X.Encode_Branch (Afpx_Xref.Details.Branch);
    end case;
  end loop;

end Add;

