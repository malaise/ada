with Con_Io, Afpx.List_Manager, Basic_Proc, Int_Image, Directory, Dir_Mng,
     Sys_Calls;
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
  Dir_Field    : constant Afpx.Field_Range := 12;
  use type Afpx.Absolute_Field_Range;

  -- Current Git root and path referred to Git root
  Root : Utils.Asu_Us;
  Path : Utils.Asu_Us;

  -- Editor and Differator
  Editor : Utils.Asu_Us;
  Differator : Utils.Asu_Us;

  -- Files list
  Files : Git_If.File_List;

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


  -- List files of dir
  procedure List_Files (Path : in String;
                        Files : in out Git_If.File_List) is
    Dir : Dir_Mng.File_List_Mng.List_Type;
    Dir_File : Dir_Mng.File_Entry_Rec;
    File : Git_If.File_Entry_Rec;
    Done : Boolean;
    use type Directory.File_Kind_List;
  begin
    Files.Delete_List;
    -- Copy list from Dir_Mng into a Git_If.File_List
    Dir_Mng.List_Dir (Dir, Path);
    Dir_Mng.File_Sort (Dir);
    loop
      Dir.Read (Dir_File, Done => Done);
      if Dir_File.Kind = Directory.Dir then
        File.S2 := ' ';
        File.S3 := ' ';
      else
        File.S2 := '?';
        File.S3 := '?';
      end if;
      File.Name := Utils.Asu_Tus (Dir_File.Name (1 .. Dir_File.Len));
      File.Kind := Git_If.Char_Of (
             Sys_Calls.File_Desc_Kind_List(Dir_File.Kind));
      Files.Insert (File);
      exit when not Done;
    end loop;
    Files.Rewind;
  end List_Files;

  -- Encode current directory and root
  procedure Encode_Dir is
    Curr : constant String := Directory.Get_Current;
    Width : constant Afpx.Width_Range := Afpx.Get_Field_Width (Dir_Field);
  begin
    Afpx.Clear_Field (Dir_Field);
    Afpx.Encode_Field (Dir_Field, (0, 0), Utils.Normalize (Curr, Width));
    -- Move cursor col on last significant char
    Cursor_Col := 0;
    declare
      Wstr : constant Wide_String := Afpx.Decode_Wide_Field (Dir_Field, 0);
    begin
      for I in reverse Wstr'Range loop
        if Wstr(I) /= ' ' then
          Cursor_Col := I;
          exit;
        end if;
      end loop;
    end;
    -- Full field => last col
    if Cursor_Col >= Width then
      Cursor_Col := Width - 1;
    end if;
  end Encode_Dir;

  -- Encode files
  procedure Encode_Files is
    Background : Con_Io.Effective_Basic_Colors;
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
      Git_If.List_Files (Utils.Asu.To_String (Path), Files);
      Afpx.Resume;
    exception
      when Git_If.No_Git =>
        -- This dir is not GIT
        Root := Utils.Asu_Null;
        Path := Utils.Asu_Null;
        -- List dir content the normal way
        List_Files (Utils.Asu.To_String (Path), Files);
        Afpx.Resume;
      when others =>
        Afpx.Resume;
        raise;
    end;

    -- Copy in Afpx list
    Init_List (Files);

    -- Encode root dir
    Afpx.Clear_Field (10);
    Afpx.Encode_Field (10, (0, 0),
       Utils.Normalize (Utils.Asu_Ts (Root), Afpx.Get_Field_Width (12)));

    -- De-activate Diff and history if no in Git
    if Root = Utils.Asu_Null then
      Afpx.Get_Descriptor_Background (Background);
      Afpx.Set_Field_Protection (17, True);
      Afpx.Set_Field_Colors (17, Foreground => Con_Io.Black,
                                 Background => Background);
      Afpx.Set_Field_Protection (18, True);
      Afpx.Set_Field_Colors (18, Foreground => Con_Io.Black,
                                 Background => Background);
    else
      Afpx.Reset_Field (17);
      Afpx.Reset_Field (18);
    end if;

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
    Afpx.Update_List (Afpx.Center);
  end Hist;

  -- List action on File or Dir
  type Action_List is (Default, Edit, Diff, History);
  procedure List_Action (Action : in Action_List) is

    File : Git_If.File_Entry_Rec;
  begin
    Files.Move_To (Number => Afpx.Line_List.Get_Position - 1,
                  From_Current => False);
    Files.Read (File, Git_If.File_Mng.Dyn_List.Current);
    declare
      Str : constant String := Utils.Asu_Ts (File.Name);
    begin
      if File.Kind = '/' then
        case Action is
          when Default =>
            Change_Dir (Str);
          when Edit | Diff =>
            null;
          when History =>
            Hist (Str, False);
        end case;
      elsif File.Kind /= '@' and then File.Kind /= '?' then
        case Action is
          when Edit | Default =>
            Edit (Str);
          when Diff =>
            Git_If.Launch_Diff (Utils.Asu_Ts (Differator), Str);
          when History =>
           Hist (Str, True);
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
          when 10 =>
            -- Root (change dir to)
            Change_Dir (Utils.Asu_Ts (Root));
          when 11 =>
            -- Go (to dir)
            Change_Dir;
          when 13 =>
            -- Reread (change dir .)
            Change_Dir (".");
          when 14 =>
            -- Up (change dir ..)
            Change_Dir ("..");
          when 15 =>
            -- Bookmarks (menu)
            declare
              New_Dir : constant String := Bookmarks.Handle;
            begin
              Init;
              if New_Dir /= "" then
                Change_Dir (New_Dir);
              end if;
            end;
          when 16 =>
            -- Edit (file)
            List_Action (Edit);
          when 17 =>
            -- Diff
            List_Action (Diff);
          when 18 =>
            -- History
            List_Action (History);
          when 19 =>
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

