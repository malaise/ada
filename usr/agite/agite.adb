with As.U; use As.U;
with Con_Io, Afpx.List_Manager, Basic_Proc, Int_Image, Directory, Dir_Mng,
     Sys_Calls, Argument, Argument_Parser, Socket, String_Mng;
with Utils, Git_If, Config, Bookmarks, History;
procedure Agite is

  -- Usage and Error message
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
       & " [ -n | --no_history ] [ <path> | -p | --previous ]");
  end Usage;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Utils.Exit_Requested;
  end Error;

  -- Options
  Keys : constant Argument_Parser.The_Keys_Type := (
    1 => ('h', Asu_Tus ("help"), False, False),
    2 => ('n', Asu_Tus ("no_history"), False, False),
    3 => ('p', Asu_Tus ("previous"), False, False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Got to previous dir (if any) / Save history
  Goto_Previous : Boolean;
  Update_History : Boolean;

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
  Root : Asu_Us;
  Path : Asu_Us;

  -- Editor and Differator
  Editor : Asu_Us;
  Differator : Asu_Us;

  -- Files list
  Files : Git_If.File_List;

  -- List width and encoding
  List_Width : Afpx.Width_Range;
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.File_Entry_Rec) is
  begin
    Afpx.Encode_Line (Line,
        From.S2 & From.S3 & ' '
      & Utils.Normalize (Asu_Ts (From.Name), List_Width) & From.Kind);
  end Set;
  procedure Init_List is new Afpx.List_Manager.Init_List (
    Git_If.File_Entry_Rec, Git_If.File_Mng, Set);


  -- List files of dir
  procedure List_Files (Path : in String;
                        Files : in out Git_If.File_List) is
    Dir : Dir_Mng.File_List_Mng.List_Type;
    Dir_File : Dir_Mng.File_Entry_Rec;
    File : Git_If.File_Entry_Rec;
    Moved : Boolean;
    use type Directory.File_Kind_List;
  begin
    Files.Delete_List;
    -- Copy list from Dir_Mng into a Git_If.File_List
    Dir_Mng.List_Dir (Dir, Path);
    Dir_Mng.File_Sort (Dir);
    loop
      Dir.Read (Dir_File, Moved => Moved);
      if Dir_File.Kind = Directory.Dir then
        File.S2 := ' ';
        File.S3 := ' ';
      else
        File.S2 := '?';
        File.S3 := '?';
      end if;
      File.Name := Asu_Tus (Dir_File.Name (1 .. Dir_File.Len));
      File.Kind := Git_If.Char_Of (
             Sys_Calls.File_Desc_Kind_List(Dir_File.Kind));
      Files.Insert (File);
      exit when not Moved;
    end loop;
    Files.Rewind;
  end List_Files;

  -- Encode files
  procedure Encode_Files is
    Background : constant Con_Io.Effective_Colors
               := Afpx.Get_Descriptor_Background;
    Black : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Black");
  begin
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No) - 4;
    -- Get info: Path if needed and list
    Afpx.Suspend;
    Redisplay := True;
    begin
      if Asu_Is_Null (Root) then
        Git_If.Get_Root_And_Path (Root, Path);
      end if;
      Git_If.List_Files (Asu_Ts (Path), Files);
      Afpx.Resume;
    exception
      when Git_If.No_Git =>
        -- This dir is not GIT
        Root := Asu_Null;
        Path := Asu_Null;
        -- List dir content the normal way
        List_Files (Asu_Ts (Path), Files);
        Afpx.Resume;
      when others =>
        Afpx.Resume;
        raise;
    end;

    -- Copy in Afpx list
    Init_List (Files);
    Afpx.Update_List (Afpx.Top);

    -- Encode root dir
    Afpx.Clear_Field (10);
    Afpx.Encode_Field (10, (0, 0),
       Utils.Normalize (Asu_Ts (Root), Afpx.Get_Field_Width (12)));

    -- De-activate Diff and history if no in Git
    if Asu_Is_Null (Root) then
      Afpx.Set_Field_Protection (21, True);
      Afpx.Set_Field_Colors (21, Foreground => Black,
                                 Background => Background);
      Afpx.Set_Field_Protection (22, True);
      Afpx.Set_Field_Colors (22, Foreground => Black,
                                 Background => Background);
    else
      Afpx.Reset_Field (21);
      Afpx.Reset_Field (22);
    end if;

  end Encode_Files;

  -- Change dir (or at least try) according to argument or Dir_Field
  procedure Change_Dir (New_Dir : in String := "") is
    Str : constant String
        := Utils.Parse_Spaces (Afpx.Decode_Field (Dir_Field, 0, False));
    Cur_Dir : constant String := Directory.Get_Current;
    Width : constant Afpx.Width_Range := Afpx.Get_Field_Width (Dir_Field);
  begin
    begin
      if New_Dir = "" then
        if Str /= "" then
          Directory.Change_Current (Str);
        end if;
      else
        Directory.Change_Current (New_Dir);
      end if;
      -- Success, reset root path for re-evaluation, save current dir
      Root := Asu_Null;
      if Update_History then
        Config.Save_Curr_Dir (Directory.Get_Current);
      end if;
      Encode_Files;
    exception
      when others =>
        -- Cannot change to new dir or cannot process files (No_Git?)
        Directory.Change_Current (Cur_Dir);
    end;
    Afpx.Clear_Field (Dir_Field);
    Afpx.Encode_Field (Dir_Field, (0, 0), Utils.Normalize (Cur_Dir, Width));
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
  end Change_Dir;

  -- To find current position back
  function Match (Current, Criteria : Git_If.File_Entry_Rec) return Boolean is
    use type Asu_Us;
  begin
    return Current.Kind = Criteria.Kind and then Current.Name = Criteria.Name;
  end Match;
  procedure File_Search is new Git_If.File_Mng.Dyn_List.Search (Match);

  -- Reread curent dir: change dir to "." and try to restore current pos
  procedure Reread is
    Current_File : Git_If.File_Entry_Rec;
    Found : Boolean;
  begin
    -- Save current entry
    Files.Move_At (Afpx.Line_List.Get_Position);
    Files.Read (Current_File, Git_If.File_Mng.Dyn_List.Current);
    -- Re-build list
    Change_Dir (".");
    -- Search position back and move Afpx to it
    File_Search (Files, Found, Current_File,
                 From => Git_If.File_Mng.Dyn_List.Absolute);
    if Found then
      Afpx.Line_List.Move_At (Files.Get_Position);
      Afpx.Update_List (Afpx.Center);
    end if;
  end Reread;

  -- Local host: "on (<host>)" if possible
  -- else "(<host>)" if possible
  -- else "<host>" if possible
  -- else ">tail"
  Local_Host : Asu_Us;
  function Host_Str return String is
    use type Asu_Us;
    Len : constant Positive := Afpx.Get_Field_Width (17);
  begin
    if Local_Host /= Asu_Null then
      return Asu_Ts (Local_Host);
    end if;
    Local_Host := Asu_Tus (Socket.Local_Host_Name);
    if Asu.Length (Local_Host) + 5 <= Len then
      Local_Host := "(on " & Local_Host & ")";
    elsif Asu.Length (Local_Host) + 2 <= Len then
      Local_Host := "(" & Local_Host & ")";
    end if;
    Local_Host := Asu_Tus (String_Mng.Procuste (
        Asu_Ts (Local_Host),
        Len,
        Align_Left => False,
        Gap => ' ',
        Trunc_Head => True,
        Show_Trunc => True));
    return Asu_Ts (Local_Host);
  end Host_Str;

  -- Init Afpx
  procedure Init is
  begin
    Afpx.Use_Descriptor (1);
    Cursor_Field := Afpx.Next_Cursor_Field (0);
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;
    Afpx.Encode_Field (17, (0, 0), Host_Str);
    Change_Dir;
    Encode_Files;
  end;

  procedure Edit (File_Name : in String) is
  begin
    Utils.Launch (Asu_Ts (Editor) & " " & File_Name);
  end Edit;

  procedure Hist (Name : in String; Is_File : in Boolean) is
    Pos : Positive;
  begin
    -- Call history and restore current entry
    Pos := Afpx.Line_List.Get_Position;
    History.Handle (Asu_Ts (Root), Asu_Ts (Path), Name, Is_File);
    Init;
    Afpx.Line_List.Move_At (Pos);
    Afpx.Update_List (Afpx.Center);
  end Hist;

  -- List action on File or Dir
  type Action_List is (Default, Edit, Diff, History);
  procedure List_Action (Action : in Action_List) is

    File : Git_If.File_Entry_Rec;
    use type Asu_Us;
  begin
    Files.Move_At (Afpx.Line_List.Get_Position);
    Files.Read (File, Git_If.File_Mng.Dyn_List.Current);
    declare
      Str : constant String := Asu_Ts (File.Name);
    begin
      if File.Kind = '/' then
        case Action is
          when Default =>
            Change_Dir (Str);
          when Edit  =>
            null;
          when Diff =>
            if Str = "." then
              Git_If.Launch_Diff (Asu_Ts (Differator), Str);
            end if;
          when History =>
            if Str = "." then
              Hist ("", False);
            elsif Str /= ".." then
              Hist (Str, False);
            end if;
        end case;
      elsif File.Kind /= '@' and then File.Kind /= '?' then
        case Action is
          when Edit | Default =>
            Edit (Str);
          when Diff =>
            Git_If.Launch_Diff (Asu_Ts (Differator), Str);
          when History =>
           Hist (Str, True);
        end case;
      end if;
    end;
  end List_Action;

begin
  -- Check/Parse arguments
   Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error (Arg_Dscr.Get_Error);
  end if;

  if Arg_Dscr.Is_Set (1) then
    -- Help
    Usage;
    return;
  end if;

  -- No history
  Update_History := not Arg_Dscr.Is_Set (2);

  -- Goto previous or provided path
  Goto_Previous := Arg_Dscr.Is_Set (3);
  begin
    if Arg_Dscr.Is_Set (Argument_Parser.No_Key_Index) then
      if Goto_Previous then
        Error ("""Previous"" option and path are mutually exclusive");
      end if;
      -- Go to path
      Directory.Change_Current (Arg_Dscr.Get_Option (
                           Argument_Parser.No_Key_Index));
    elsif Goto_Previous then
      -- Goto previous if possible
      if Config.Prev_Dir /= "" then
        Directory.Change_Current (Config.Prev_Dir);
      end if;
    end if;
  exception
    when Directory.Name_Error =>
        null;
  end;

  -- Get and check version
  begin
    Version := Git_If.Get_Version;
  exception
    when Git_If.No_Git =>
      Error ("Can't find Git");
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
  Editor := Asu_Tus (Config.Editor);
  Differator := Asu_Tus (Config.Differator);

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
            Change_Dir (Asu_Ts (Root));
          when 11 =>
            -- Go (to dir)
            Change_Dir;
          when 13 =>
            -- Reread (change dir . and restore pos)
            Reread;
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
          when 18 =>
            -- GUI
            Utils.Launch ("git gui");
          when 19 =>
            -- XTerm
            Utils.Launch (Config.Xterminal);
          when 20 =>
            -- Edit (file)
            List_Action (Edit);
          when 21 =>
            -- Diff
            List_Action (Diff);
          when 22 =>
            -- History
            List_Action (History);
          when 23 =>
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
    begin
      Afpx.Release_Descriptor;
    exception
      when Afpx.No_Descriptor =>
        null;
   end;
end Agite;

