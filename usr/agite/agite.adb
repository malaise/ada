with As.U, Con_Io, Afpx.List_Manager, Basic_Proc, Images, Directory,
     Dir_Mng, Sys_Calls, Argument, Argument_Parser, Socket, Str_Util;
with Utils.X, Git_If, Config, Bookmarks, History, Confirm, Error, Afpx_Xref;
procedure Agite is

  -- Options
  Keys : constant Argument_Parser.The_Keys_Type := (
    1 => (False, 'h', As.U.Tus ("help"),       False),
    2 => (False, 'n', As.U.Tus ("no_history"), False),
    3 => (False, 'p', As.U.Tus ("previous"),   False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Usage and Error message
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
       & " <no_history> [ <path> | <previous> ]");
    Basic_Proc.Put_Line_Error (
       " <no_history> ::= " & Argument_Parser.Image (Keys(2)));
    Basic_Proc.Put_Line_Error (
       " <previous>   ::= " & Argument_Parser.Image (Keys(3)));
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Utils.Exit_Requested;
  end Error;

  -- Got to previous dir (if any) / Save history
  Goto_Previous : Boolean;
  Update_History : Boolean;

  -- Version Stuff
  Version : Git_If.Version_Rec;
  Ref_Version : constant Git_If.Version_Rec := (1, 7, 0);
  Incorrect_Version : exception;

  -- Afpx stuff
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Redisplay    : Boolean;
  Ptg_Result   : Afpx.Result_Rec;
  Dir_Field    : constant Afpx.Field_Range := 15;
  use type Afpx.Absolute_Field_Range;

  -- Current Git root and path referred to Git root
  Root : As.U.Asu_Us;
  Path : As.U.Asu_Us;

  -- Editor and Differator
  Editor : As.U.Asu_Us;
  Differator : As.U.Asu_Us;

  -- Files list and current branch
  Files : Git_If.File_List;
  Branch : As.U.Asu_Us;

  -- List width and encoding
  List_Width : Afpx.Width_Range := Afpx.Width_Range'First;
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.File_Entry_Rec) is
  begin
    Afpx.Encode_Line (Line,
        From.S2 & From.S3 & ' '
      & Utils.Normalize (From.Name.Image, List_Width) & From.Kind);
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
      File.Name := Dir_File.Name;
      File.Kind := Git_If.Char_Of (
             Sys_Calls.File_Desc_Kind_List(Dir_File.Kind));
      Files.Insert (File);
      exit when not Moved;
    end loop;
    Files.Rewind;
  end List_Files;

  -- Update list of files
  procedure List_Files is
  begin
    -- Get info: Path if needed and list
    Afpx.Suspend;
    Redisplay := True;
    begin
      if Root.Is_Null then
        Git_If.Get_Root_And_Path (Root, Path);
      end if;
      Branch := As.U.Tus (Git_If.Current_Branch);
      Git_If.List_Files (Path.Image, Files);
      Afpx.Resume;
    exception
      when Git_If.No_Git =>
        -- This dir is not GIT
        Root.Set_Null;
        Path.Set_Null;
        -- List dir content the normal way
        List_Files (Path.Image, Files);
        Afpx.Resume;
      when others =>
        Afpx.Resume;
        raise;
    end;
  end List_Files;

  -- To find current position back
  function Match (Current, Criteria : Git_If.File_Entry_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Kind = Criteria.Kind and then Current.Name = Criteria.Name;
  end Match;
  procedure File_Search is new Git_If.File_Mng.Dyn_List.Search (Match);

  -- Encode Afpx list with files, if list has changed or if Force
  procedure Encode_Files (Force : in Boolean) is
    Position : Natural := 0;
    Current_File : Git_If.File_Entry_Rec;
    Prev_Files : Git_If.File_List;
    Changed : Boolean;
    Found : Boolean;
    use type Git_If.File_Entry_Rec;
  begin
    if List_Width = Afpx.Width_Range'First then
      List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No) - 4;
    end if;
    Changed := Force;
    -- Save current position and entry
    if not Files.Is_Empty then
      Position := Afpx.Line_List.Get_Position;
      Files.Move_At (Position);
      Files.Read (Current_File, Git_If.File_Mng.Dyn_List.Current);
      -- Make a copy of files list
      Prev_Files.Insert_Copy (Files);
    end if;
    -- Refresh list only if it has changed

    -- Update list of files
    List_Files;

    -- Check lengths then content
    if not Changed
    and then Files.List_Length /= Prev_Files.List_Length then
      Changed := True;
    end if;
    if not Changed then
      Files.Rewind;
      Prev_Files.Rewind;
      loop
        if Files.Access_Current.all /= Prev_Files.Access_Current.all then
          -- Stop as soon as one entry differs
          Changed := True;
          exit;
        end if;
        exit when not Files.Check_Move;
        Files.Move_To;
        Prev_Files.Move_To;
      end loop;
    end if;

    if not Changed then
      -- No change: nothing
      return;
    elsif Position = 0 then
      -- Initial list was empty
      Init_List (Files);
      Afpx.Update_List (Afpx.Top);
    else
      Init_List (Files);
      -- Search position back and move Afpx to it
      File_Search (Files, Found, Current_File,
                   From => Git_If.File_Mng.Dyn_List.Absolute);
      if Found then
        Afpx.Line_List.Move_At (Files.Get_Position);
        Afpx.Update_List (Afpx.Center_Selected);
      else
        Afpx.Line_List.Rewind;
        Afpx.Update_List (Afpx.Top);
      end if;
    end if;
    -- Copy in Afpx list
  end Encode_Files;

  -- Change dir (or at least try) according to argument or Dir_Field
  procedure Change_Dir (New_Dir : in String := "") is
    Str : constant String
        := Utils.Parse_Spaces (Afpx.Decode_Field (Dir_Field, 0, False));
    Width : constant Afpx.Width_Range := Afpx.Get_Field_Width (Dir_Field);
    Target : As.U.Asu_Us;
  begin
    begin
      if New_Dir = "" then
        if Str /= "" then
          Target := As.U.Tus (Str);
        else
          Target := As.U.Tus (".");
        end if;
      else
        Target := As.U.Tus (New_Dir);
      end if;
      Directory.Change_Current (Target.Image);
    exception
      when others =>
        -- Cannot change to new dir or cannot process files (No_Git?)
        Error ("Changing directory to:",
               Target.Image,
               "Staying in current.");
        Afpx.Use_Descriptor (Afpx_Xref.Main.Dscr_Num);
        Directory.Change_Current (Directory.Get_Current);
    end;
    -- Success, reset root path for re-evaluation, save current dir
    Root.Set_Null;
    if Update_History then
      Config.Save_Curr_Dir (Directory.Get_Current);
    end if;
    Encode_Files (Force => True);

    -- Encode current dir (get field)
    Afpx.Clear_Field (Dir_Field);
    Afpx.Encode_Field (Dir_Field, (0, 0),
           Utils.Normalize (Directory.Get_Current, Width));
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

    -- Encode root dir
    Afpx.Clear_Field (Afpx_Xref.Main.Root);
    Afpx.Encode_Field (Afpx_Xref.Main.Root, (0, 0),
       Utils.Normalize (Root.Image, Afpx.Get_Field_Width (Afpx_Xref.Main.Root)));

    -- Encode current branch
    Afpx.Clear_Field (Afpx_Xref.Main.Branch);
    if Branch.Image = ("(no branch)") then
      Branch := As.U.Tus ("None.");
    end if;
    Afpx.Encode_Field (Afpx_Xref.Main.Branch, (0, 0),
         Utils.Normalize ("Br: " & Branch.Image, Afpx.Get_Field_Width (9),
                          False));

    -- De-activate Diff and history if no in Git
    if Root.Is_Null then
      Utils.X.Protect_Field (Afpx_Xref.Main.Diff);
      Utils.X.Protect_Field (Afpx_Xref.Main.History);
    else
      Afpx.Reset_Field (Afpx_Xref.Main.Diff);
      Afpx.Reset_Field (Afpx_Xref.Main.History);
    end if;
  end Change_Dir;

  -- Refresh list of files
  -- If Set_Dir then change dir to "."
  -- Try to restore current pos
  procedure Reread (Set_Dir : in Boolean) is
  begin
    -- Re-build list
    if Set_Dir then
      Change_Dir (".");
    else
      Encode_Files (Force => False);
    end if;
  end Reread;

  -- Periodical timer to refresh list/status of file
  package Timer is
    procedure Start (Periodic : in Boolean := False);
    procedure Stop;
  end Timer;
  package body Timer is separate;

  -- Local host: "on (<host>)" if possible
  -- else "(<host>)" if possible
  -- else "<host>" if possible
  -- else ">tail"
  Local_Host : As.U.Asu_Us;
  function Host_Str return String is
    Len : constant Positive := Afpx.Get_Field_Width (Afpx_Xref.Main.Host);
    use type As.U.Asu_Us;
  begin
    if not Local_Host.Is_Null then
      return Local_Host.Image;
    end if;
    Local_Host := As.U.Tus (Socket.Local_Host_Name);
    if Local_Host.Length + 5 <= Len then
      Local_Host := "(on " & Local_Host & ")";
    elsif Local_Host.Length + 2 <= Len then
      Local_Host := "(" & Local_Host & ")";
    end if;
    Local_Host := As.U.Tus (Str_Util.Procuste (
        Local_Host.Image,
        Len,
        Align_Left => False,
        Gap => ' ',
        Trunc_Head => True,
        Show_Trunc => True));
    return Local_Host.Image;
  end Host_Str;

  -- Init Afpx
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Main.Dscr_Num);
    Afpx.Get_Console.Set_Name ("Agite (on " & Socket.Local_Host_Name & ")");
    Cursor_Field := Afpx.Next_Cursor_Field (0);
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;
    Afpx.Encode_Field (Afpx_Xref.Main.Host, (0, 0), Host_Str);
    Change_Dir;
  end;

  procedure Do_Edit (File_Name : in String) is
  begin
    Timer.Start;
    Utils.Launch (Editor.Image & " " & File_Name, True);
  end Do_Edit;

  procedure Do_History (Name : in String; Is_File : in Boolean) is
    Pos : Positive;
  begin
    -- Call history and restore current entry
    Pos := Afpx.Line_List.Get_Position;
    History.Handle (Root.Image, Path.Image, Name, Is_File);
    Init;
    Afpx.Line_List.Move_At (Pos);
    Afpx.Update_List (Afpx.Center_Selected);
  end Do_History;

  procedure Do_Revert (Name : in String) is
    Pos : Positive;
    File : Git_If.File_Entry_Rec;
  begin
    -- Call Confirm and restore current entry
    Pos := Afpx.Line_List.Get_Position;
    Files.Move_At (Pos);
    Files.Read (File, Git_If.File_Mng.Dyn_List.Current);

    -- Handle Dir and reject non regular nor link
    if File.Kind = '/' then
      if File.Name.Image = "."
      or else File.Name.Image = ".." then
        return;
      end if;
      if Confirm ("Ready to remove directory:",
                  Directory.Build_File_Name (Path.Image, Name, "")) then
        begin
          Directory.Remove (Name);
          Pos := Pos - 1;
        exception
          when Directory.Access_Error =>
            -- Directory not empty
            Error ("Cannot remove directory",
                    Directory.Build_File_Name (Path.Image, Name, ""),
                   "Access error (not empty?)");
          when Directory.Name_Error =>
            -- Very unlikely mut maybe the dir has disappeared meanwhile
            null;
        end;
      end if;
    elsif File.Kind /= ' ' and then File.Kind /= '@' then
      -- Only for regular files or symbolic links
      return;
    elsif File.S2 = '?' then
      -- File is untracked
      if Confirm ("Ready to remove:",
                  Directory.Build_File_Name (Path.Image, Name, "")) then
        Sys_Calls.Unlink (Name);
        Pos := Pos - 1;
      end if;
    elsif File.S2 /= ' ' then
      -- File is modified in index, reset it
      if Confirm ("Ready to reset:",
                  Directory.Build_File_Name (Path.Image, Name, "")) then

        Git_If.Do_Reset (Name);
      end if;
    elsif File.S3 /= ' ' then
      -- File is modified locally, checkout from repository
      if Confirm ("Ready to revert:",
                  Directory.Build_File_Name (Path.Image, Name, "")) then
        Git_If.Do_Revert (Name);
      end if;
    end if;
    Init;
    Afpx.Line_List.Move_At (Pos);
    Afpx.Update_List (Afpx.Center_Selected);
  end Do_Revert;

  -- Add a dir or file
  procedure Do_Add_File (File : in Git_If.File_Entry_Rec) is
  begin
    if File.S2 = '?' or else File.S2 = ' ' then
      -- Untracked or not in index
      Git_If.Do_Add (File.Name.Image);
      Encode_Files (Force => False);
    end if;
  end Do_Add_File;

  -- List action on File or Dir
  type Action_List is (Default, Edit, Diff, History, Revert, Add);
  procedure List_Action (Action : in Action_List) is
    File : Git_If.File_Entry_Rec;
  begin
    Files.Move_At (Afpx.Line_List.Get_Position);
    Files.Read (File, Git_If.File_Mng.Dyn_List.Current);
    declare
      File_Name : constant String := File.Name.Image;
    begin
      if File.Kind = '/' then
        -- Dir
        case Action is
          when Default =>
            Change_Dir (File_Name);
          when Edit  =>
            null;
          when Diff =>
            if File_Name /= ".." then
              Git_If.Launch_Diff (Differator.Image, File_Name);
            end if;
          when History =>
            if File_Name = "." then
              Do_History ("", False);
            elsif File_Name /= ".." then
              Do_History (File_Name, False);
            end if;
          when Revert =>
            if File_Name /= ".." then
              Do_Revert (File_Name);
            end if;
          when Add =>
            if File_Name /= ".." then
              Git_If.Do_Add (File_Name);
            end if;
        end case;
      elsif File.Kind /= '@' and then File.Kind /= '?' then
        -- File
        case Action is
          when Edit | Default =>
            Do_Edit (File_Name);
          when Diff =>
            if File.S2 = ' ' and then File.S3 = ' ' then
              -- File is unmodified: diff on last commit of it
              declare
                Hash : constant Git_If.Git_Hash := Git_If.Last_Hash (File_Name);
              begin
                if Hash /= Git_If.No_Hash then
                  Git_If.Launch_Delta (Differator.Image, File_Name,
                                       Hash & "^", Hash);
                end if;
              end;
            else
              -- File is "modified"
              Git_If.Launch_Diff (Differator.Image, File_Name);
            end if;
          when History =>
            Do_History (File_Name, True);
          when Revert =>
            Do_Revert (File_Name);
          when Add =>
            Do_Add_File (File);
        end case;
      elsif File.Kind = '?' and then Action = Revert then
        -- Deleted file
        Do_Revert (File_Name);
      end if;
    end;
  end List_Action;

  -- Locate a file that has its first letter = Key
  -- Move to it in Afpx list if found
  procedure Locate_File (Key : in Character) is
    File : Git_If.File_Entry_Rec;
    Moved : Boolean;
  begin
    if Files.Is_Empty then
      return;
    end if;
    Files.Rewind;
    loop
      Files.Read (File, Moved => Moved);
      -- Regular file or other kind, and matching Key
      if (File.Kind = ' ' or else File.Kind = '?')
      and then File.Name.Element(1) = Key then
        -- Got it
        if Moved then
          -- Move back to matching entry
          Files.Move_To (Git_If.File_Mng.Dyn_List.Prev);
        end if;
        -- Move to it
        Afpx.Line_List.Move_At (Files.Get_Position);
        Afpx.Update_List (Afpx.Top_Selected);
        -- Done
        return;
      end if;
      exit when not Moved;
    end loop;
    -- Not found
  end Locate_File;

  -- Push/pop dir
  Dir1, Dir2 : As.U.Asu_Us;
  procedure Push_Dir is
  begin
    Dir2 := Dir1;
    Dir1 := As.U.Tus (Directory.Get_Current);
  end Push_Dir;
  procedure Pop_Dir is
    New_Dir : As.U.Asu_Us;
  begin
    if Dir2.Is_Null then
      Push_Dir;
    end if;
    -- Change to Dir2 and swap Dir1 and Dir2
    New_Dir := Dir2;
    Dir2 := Dir1;
    Dir1 := New_Dir;
    Change_Dir (New_Dir.Image);
  end Pop_Dir;
  procedure Init_Dir is
  begin
    Push_Dir;
  end Init_Dir;
  function Can_Push return Boolean is
  begin
    return True;
  end Can_Push;
  function Can_Pop return Boolean is
    Curr_Dir : constant String := Directory.Get_Current;
  begin
    return (Dir2.Is_Null and then Curr_Dir /= Dir1.Image)
    or else (not Dir2.Is_Null and then Curr_Dir /= Dir2.Image);
  end Can_Pop;

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

  -- Check configuration file
  Config.Check;

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

  -- No history
  Update_History := not Arg_Dscr.Is_Set (2);

  -- Goto previous or provided path
  Goto_Previous := Arg_Dscr.Is_Set (3);
  declare
    Target_Dir : As.U.Asu_Us;
  begin
    if Arg_Dscr.Is_Set (Argument_Parser.No_Key_Index) then
      if Goto_Previous then
        Error ("""Previous"" option and path are mutually exclusive");
      end if;
      -- Go to path
      Target_Dir := As.U.Tus (Arg_Dscr.Get_Option (
                                 Argument_Parser.No_Key_Index));
    elsif Goto_Previous then
      -- Goto previous if possible
      if Config.Prev_Dir /= "" then
        Target_Dir := As.U.Tus (Config.Prev_Dir);
      end if;
    end if;
    if not Target_Dir.Is_Null then
      Directory.Change_Current (Target_Dir.Image);
    end if;
  exception
    when Directory.Name_Error =>
      Error ("Changing directory to:",
             Target_Dir.Image,
             "No such directory, starting in current.");
  end;

  -- Get or init config
  Editor := As.U.Tus (Config.Editor);
  Differator := As.U.Tus (Config.Differator);

  -- Init Afpx and Timer
  Init;
  Timer.Start (Periodic => True);

  -- Now we can reset this env variables for our children
  Sys_Calls.Unsetenv ("AFPX_DATA_DIR");

  -- Init Push/Pop dirs
  Init_Dir;

  -- Main loop
  loop
    -- Activate PushD and PopD
    Afpx.Set_Field_Activation (Afpx_Xref.Main.Pushd, Can_Push);
    Afpx.Set_Field_Activation (Afpx_Xref.Main.Popd,  Can_Pop);

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
          when Utils.X.List_Scroll_Fld_Range'First ..
               Utils.X.List_Scroll_Fld_Range'Last =>
            -- Scroll list
            Afpx.List_Manager.Scroll(
                Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
          when Afpx_Xref.Main.Root =>
            -- Root (change dir to)
            Change_Dir (Root.Image);
          when Afpx_Xref.Main.Chdir =>
            -- Go (to dir)
            Change_Dir;
          when Afpx_Xref.Main.Reread =>
            -- Reread (change dir . and restore pos)
            Reread (True);
          when Afpx_Xref.Main.Dirup =>
            -- Up (change dir ..)
            Change_Dir ("..");
          when Afpx_Xref.Main.Bookmarks =>
            -- Bookmarks (menu)
            declare
              New_Dir : constant String := Bookmarks.Handle;
            begin
              Init;
              if New_Dir /= "" then
                Change_Dir (New_Dir);
              end if;
            end;
          when Afpx_Xref.Main.Pushd =>
            -- PushD
            Push_Dir;
          when Afpx_Xref.Main.Popd =>
            -- PopD
            Pop_Dir;
          when Afpx_Xref.Main.Quick_Search =>
            -- Quick search
            Locate_File (
              Afpx.Decode_Field (Ptg_Result.Field_No, Ptg_Result.Click_Pos.Row)
                                  (Ptg_Result.Click_Pos.Col + 1));
          when Afpx_Xref.Main.Gui =>
            -- GUI
            Timer.Start;
            Utils.Launch ("git gui", True);
          when Afpx_Xref.Main.Xterm =>
            -- XTerm
            Utils.Launch (Config.Xterminal);
          when Afpx_Xref.Main.Make =>
            -- Make
            Utils.Launch (Config.Make);
          when Afpx_Xref.Main.Edit =>
            -- Edit (file)
            List_Action (Edit);
          when Afpx_Xref.Main.Diff =>
            -- Diff
            List_Action (Diff);
          when Afpx_Xref.Main.History =>
            -- History
            List_Action (History);
          when Afpx_Xref.Main.Revert =>
            -- Revert
            List_Action (Revert);
          when Afpx_Xref.Main.Add =>
            -- Add
            List_Action (Add);
          when Afpx_Xref.Main.Quit =>
            -- Exit
            raise Utils.Exit_Requested;
          when others =>
            -- Other button?
            null;
        end case;

      when Afpx.Fd_Event =>
        null;
      when Afpx.Timer_Event =>
        -- A child likely to change status (Edit, GitGUI) is running
        Reread (False);
      when Afpx.Signal_Event =>
        -- Exit of child likely to change status
        Reread (False);
      when Afpx.Refresh =>
        Reread (False);
        Redisplay := True;
    end case;
  end loop;

exception
  when Incorrect_Version =>
    Basic_Proc.Put_Line_Error ("Incorrect Git version. Minimum required: "
      & Images.Integer_Image (Ref_Version.Major) & "."
      & Images.Integer_Image (Ref_Version.Medium) & "."
      & Images.Integer_Image (Ref_Version.Minor) );
    Basic_Proc.Set_Error_Exit_Code;
  when Config.Invalid_Config =>
    Basic_Proc.Put_Line_Error ("Invalid configuration");
    Basic_Proc.Set_Error_Exit_Code;
  when Utils.Exit_Requested =>
    begin
      Afpx.Release_Descriptor;
    exception
      when Afpx.No_Descriptor =>
        null;
    end;
    Timer.Stop;
end Agite;

