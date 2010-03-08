with Con_Io, Afpx.List_Manager, Basic_Proc, Int_Image, Directory, Language;
with Utils, Git_If, Config, Bookmarks, History;
procedure Agite is
  -- Version Stuff
  Version : Git_If.Version_Rec;
  Ref_Version : constant Git_If.Version_Rec := (1, 7, 0);
  function Image is new Int_Image (Integer);
  Incorrect_Version : exception;

  -- Afpx stuff
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Redisplay    : Boolean;
  Ptg_Result   : Afpx.Result_Rec;
  Dir_Field    : constant Afpx.Field_Range := 10;
  use type Afpx.Absolute_Field_Range;

  -- Current Git root and path referred to Git root
  Root : Utils.Asu_Us;
  Path : Utils.Asu_Us;

  -- Editor and Differator
  Editor : Utils.Asu_Us;
  Differator : Utils.Asu_Us;

  -- List width and encoding
  List_Width : Afpx.Width_Range;
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.File_Entry_Rec) is
  begin
    Afpx.Encode_Line (Line,
        From.S2 & From.S3 & ' '
      & Utils.Normalize (Utils.Asu_Ts (From.Name), List_Width) & From.Kind);
  end Set;
  procedure Init_List is new Afpx.List_Manager.Init_List (
    Git_If.File_Entry_Rec, Git_If.File_Mng, Set);


  -- Encode current directory
  procedure Encode_Dir is
    Curr : constant String := Directory.Get_Current;
    Width : constant Afpx.Width_Range := Afpx.Get_Field_Width (Dir_Field);
  begin
    Afpx.Clear_Field (Dir_Field);
    Afpx.Encode_Field (Dir_Field, (0, 0), Utils.Normalize (Curr, Width));
  end Encode_Dir;

  -- Encode files
  procedure Encode_Files is
    Files : Git_If.File_List;
    use type Git_If.Asu_Us;
  begin
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No) - 4;
    -- Get info: Path if needed and list
    Afpx.Suspend;
    Redisplay := True;
    begin
      if Root = Git_If.Asu.Null_Unbounded_String then
        Git_If.Get_Root_And_Path (Root, Path);
      end if;
      Git_If.List_Files (Git_If.Asu.To_String (Path), Files);
      Afpx.Resume;
    exception
      when others =>
        Afpx.Resume;
        raise;
    end;

    -- Copy in Afpx list
    Init_List (Files);
  end Encode_Files;

  -- Change dir (or at least try) according to argument or Dir_Field
  procedure Change_Dir (New_Dir : in String := "") is
    Str : constant String
        := Utils.Parse_Spaces (Afpx.Decode_Field (Dir_Field, 0, False));
    Cur_Dir : constant String := Directory.Get_Current;
  begin
    begin
      if New_Dir = "" then
        Directory.Change_Current (Str);
      else
        Directory.Change_Current (New_Dir);
      end if;
      -- Success, reset root path for re-evaluation
      Root := Utils.Asu_Null;
      Encode_Files;
    exception
      when others =>
        -- Cannot change to new dir or cannot process files (No_Git?)
        Directory.Change_Current (Cur_Dir);
    end;
    -- Put new dir or restore current
    Encode_Dir;
  end Change_Dir;

  -- Init Afpx
  procedure Init is
  begin
    Afpx.Use_Descriptor (1);
    Cursor_Field := Afpx.Next_Cursor_Field (0);
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;
    Encode_Dir;
    Encode_Files;
  end;

  procedure Edit (File_Name : in String) is
  begin
    Utils.Launch (Utils.Asu_Ts (Editor) & " " & File_Name);
  end Edit;

  procedure Hist (Name : in String; Is_File : in Boolean) is
    Pos : Positive;
  begin
    -- Call history and restore current entry
    Pos := Afpx.Line_List.Get_Position;
    History.Handle (Utils.Asu_Ts (Path), Name, Is_File);
    Init;
    Afpx.Line_List.Move_To (Number => Pos - 1);
  end Hist;

  -- List action on File or Dir
  type Action_List is (Default, Edit, Diff, History);
  procedure List_Action (Action : in Action_List) is
    Line : Afpx.Line_Rec;
    Last : Natural;
  begin
    Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
    declare
      Str : constant String
          := Language.Wide_To_String (Line.Str(1 ..Line.Len));
    begin
      Last := Utils.Last_Index (Str);
      if Str(Last) = '/' then
        case Action is
          when Default =>
            Change_Dir (Str(4 .. Last - 1));
          when Edit | Diff =>
            null;
          when History =>
            Hist (Str(4 .. Last - 1), False);
        end case;
      elsif Str(Last) /= '@' and then Str(Last) /= '?' then
        case Action is
          when Edit | Default =>
            Edit (Str(4 .. Last));
          when Diff =>
            Git_If.Launch_Diff (Utils.Asu_Ts (Differator), Str(4 .. Last));
          when History =>
           Hist (Str(4 .. Last), True);
        end case;
      end if;
    end;
  end List_Action;

begin
  -- Get and check version
  begin
    Version := Git_If.Get_Version;
  exception
    when Git_If.No_Git =>
      Basic_Proc.Put_Line_Error ("Can't find Git");
      Basic_Proc.Set_Error_Exit_Code;
      return;
  end;
  if Version.Major < Ref_Version.Major then
    raise Incorrect_Version;
  elsif Version.Major = Ref_Version.Major then
    if Version.Medium < Ref_Version.Medium then
      raise Incorrect_Version;
    elsif Version.Medium = Ref_Version.Medium then
      if Version.Minor < Ref_Version.Minor then
        raise Incorrect_Version;
      end if;
    end if;
  end if;

  -- Get or init config
  Editor := Utils.Asu_Tus (Config.Editor);
  Differator := Utils.Asu_Tus (Config.Differator);

  -- Init Afpx
  Init;

  -- Main loop
  loop


    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                       Ptg_Result, Redisplay);
    Redisplay := False;
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            -- Change dir to content of Dir_Field
            Change_Dir;
          when Afpx.Escape_Key =>
            null;
          when Afpx.Break_Key =>
            raise Utils.Exit_Requested;
        end case;

      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          when Afpx.List_Field_No =>
            -- Double click (edit file or change to dir)
            List_Action (Default);
          when Utils.List_Scroll_Fld_Range'First ..
               Utils.List_Scroll_Fld_Range'Last =>
            -- Scroll list
            Afpx.List_Manager.Scroll(Ptg_Result.Field_No
                                   - Utils.List_Scroll_Fld_Range'First + 1);
          when 9 =>
            -- Go (to dir)
            Change_Dir;
          when 11 =>
            -- Reread (change dir .)
            Change_Dir (".");
          when 12 =>
            -- Up (change dir ..)
            Change_Dir ("..");
          when 13 =>
            -- Root (change dir to)
            Change_Dir (Utils.Asu_Ts (Root));
          when 14 =>
            -- Bookmarks (menu)
            declare
              New_Dir : constant String := Bookmarks.Handle;
            begin
              Init;
              if New_Dir /= "" then
                Change_Dir (New_Dir);
              end if;
            end;
          when 15 =>
            -- Edit (file)
            List_Action (Edit);
          when 16 =>
            -- Diff
            List_Action (Diff);
          when 17 =>
            -- History
            List_Action (History);
          when 18 =>
            -- Exit
            raise Utils.Exit_Requested;
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

exception
  when Incorrect_Version =>
    Basic_Proc.Put_Line_Error ("Incorrect Git version. Minimum required: "
      & Image (Ref_Version.Major) & "."
      & Image (Ref_Version.Medium) & "."
      & Image (Ref_Version.Minor) );
    Basic_Proc.Set_Error_Exit_Code;
  when Utils.Exit_Requested =>
    Afpx.Release_Descriptor;
end Agite;

