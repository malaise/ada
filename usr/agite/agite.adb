with As.U, Afpx.List_Manager, Basic_Proc, Images, Directory,
     Dir_Mng, Sys_Calls, Argument, Argument_Parser, Socket, Environ;
with Utils.X, Git_If, Config, Bookmarks, History, Tags, Commit, Push_Pull,
     Confirm, Error, Stash, Branch, Afpx_Xref;
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

  -- Name of AFPX_DATA_DIR env variable
  Afpx_Data_Dir : constant String := "AFPX_DATA_DIR";

  -- Got to previous dir (if any) / Save history
  Goto_Previous : Boolean;
  Update_History : Boolean;

  -- Version Stuff
  Version : Git_If.Version_Rec;
  Ref_Version : constant Git_If.Version_Rec := (1, 7, 0);
  Incorrect_Version : exception;

  -- Afpx stuff
  Get_Handle : Afpx.Get_Handle_Rec;
  Ptg_Result   : Afpx.Result_Rec;
  Dir_Field    : Afpx.Field_Range;
  use type Afpx.Absolute_Field_Range;

  -- Current Git root and path referred to Git root
  Root : As.U.Asu_Us;
  Path : As.U.Asu_Us;

  -- Editor and Differator
  Editor : As.U.Asu_Us;
  Differator : As.U.Asu_Us;

  -- Files list and current branch
  Files : Git_If.File_List;

  -- Quick search dir or file
  Search_Dir : Boolean;

  -- Position in list
  Position : Positive;

  -- List width and encoding
  List_Width : Afpx.Width_Range := Afpx.Width_Range'First;
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.File_Entry_Rec) is
  begin
    Utils.X.Encode_Line (From.S2 & From.S3 & ' ',
                         From.Name.Image & From.Kind, "",
                         List_Width, Line);
  end Set;
  procedure Init_List is new Afpx.List_Manager.Init_List (
    Git_If.File_Entry_Rec, Git_If.File_Mng, Set, False);


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
    begin
      if Root.Is_Null then
        Git_If.Get_Root_And_Path (Root, Path);
      end if;
      Git_If.List_Files (Path.Image, Files);
      Afpx.Resume;
    exception
      when Git_If.No_Git =>
        -- This dir is not Git
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
  function File_Search is new Git_If.File_Mng.Dyn_List.Search (Match);

  -- Encode Afpx list with files, if list has changed or if Force
  procedure Encode_Files (Force : in Boolean) is
    Pos : Natural := 0;
    Current_File : Git_If.File_Entry_Rec;
    Prev_Files : Git_If.File_List;
    Changed : Boolean;
    use type Git_If.File_Entry_Rec;
  begin
    Changed := Force;
    -- Save current position and entry
    if not Force and then not Files.Is_Empty
    and then not Afpx.Line_List.Is_Empty then
      Pos := Afpx.Line_List.Get_Position;
      Files.Move_At (Pos);
      Files.Read (Current_File, Git_If.File_Mng.Dyn_List.Current);
      -- Make a copy of files list
      Prev_Files.Insert_Copy (Files);
    end if;

    -- Refresh list only if it has changed
    -- Update list of files and branch
    List_Files;
    Utils.X.Encode_Branch (Afpx_Xref.Main.Branch);

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
    elsif Pos = 0 then
      -- Initial list was empty
      Init_List (Files);
      Afpx.Line_List.Rewind (Check_Empty => False);
      Afpx.Update_List (Afpx.Top);
    else
      Init_List (Files);
      -- Search position back and move Afpx to it
      if File_Search (Files, Current_File,
                      From => Git_If.File_Mng.Dyn_List.Absolute) then
        Afpx.Line_List.Move_At (Files.Get_Position);
        Afpx.Update_List (Afpx.Center_Selected);
      else
        Afpx.Line_List.Rewind;
        Afpx.Update_List (Afpx.Top);
      end if;
    end if;
    -- Copy in Afpx list
  end Encode_Files;

  -- Read the target of link and its kind ('?' if error)
  procedure Link_Target (File_Name : in String;
                         Target : out As.U.Asu_Us; Kind : out Character) is
    Desc_Kind : Directory.File_Kind_List;
  begin
    Target := As.U.Tus (Directory.Read_Link (File_Name));
    Desc_Kind := Directory.File_Kind (Target.Image);
    case Desc_Kind is
      when Directory.File =>
        Kind := ' ';
      when Directory.Dir =>
        Kind := '/';
      when others =>
        Kind := '?';
    end case;
  exception
    when others =>
      Target.Set_Null;
      Kind := '?';
  end Link_Target;

  -- Get the file corresponding to current position in Afpx list
  function Get_Current_File return Git_If.File_Entry_Rec is
    File : Git_If.File_Entry_Rec;
  begin
    Files.Move_At (Afpx.Line_List.Get_Position);
    Files.Read (File, Git_If.File_Mng.Dyn_List.Current);
    return File;
  end Get_Current_File;

  -- Update cursor col to the last char of Dir_Field
  procedure Update_Cursor_Col is
    Width : constant Afpx.Width_Range := Afpx.Get_Field_Width (Dir_Field);
    Wstr : constant Wide_String := Afpx.Decode_Wide_Field (Dir_Field, 0);
  begin
    -- Move cursor col on last significant char
    Get_Handle.Cursor_Col := 0;
    Get_Handle.Offset := 0;
    Get_Handle.Insert := False;
    for I in reverse Wstr'Range loop
      if Wstr(I) /= ' ' then
        Get_Handle.Cursor_Col := I;
        exit;
      end if;
    end loop;
    -- String is longer that field width
    if Get_Handle.Cursor_Col >= Width then
      -- Width + Offset = Data_Len
      Get_Handle.Offset := Get_Handle.Cursor_Col - Width;
      Get_Handle.Cursor_Col := Width - 1;
    end if;
  end Update_Cursor_Col;

  -- Change dir (or at least try) according to argument or Dir_Field
  Last_Valid_Dir : As.U.Asu_Us;
  procedure Change_Dir (New_Dir : in String := "") is
    Str : constant String
        := Utils.Parse_Spaces (Afpx.Decode_Field (Dir_Field, 0, False));
    Target : As.U.Asu_Us;
  begin
    begin
      if New_Dir = "" then
        -- Use the Str got from Dir_Field (the Get field)
        if Str /= "" then
          Target := As.U.Tus (Str);
        else
          Target := As.U.Tus (".");
        end if;
      else
        -- Use the provided path
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
    Last_Valid_Dir := As.U.Tus (Directory.Get_Current);
    if Update_History then
      Config.Save_Curr_Dir (Directory.Get_Current);
    end if;
    if Target.Image /= "." then
      -- Dir has changed
      Afpx.Line_List.Rewind (Check_Empty => False);
    end if;
    Encode_Files (Force => True);

    -- Set bookmark variables AGITE_CUR_DIR and GIT_ROOT_DIR
    Bookmarks.Set_Var ("AGITE_CUR_DIR", Directory.Get_Current);
    Bookmarks.Set_Var ("GIT_ROOT_DIR", Root.Image);

    -- Reset Quick search dir option
    Search_Dir := False;
    Afpx.Clear_Field (Afpx_Xref.Main.Search_Dir);

    -- Encode current dir (get field)
    Utils.X.Encode_Field (Directory.Get_Current, Dir_Field);
    Update_Cursor_Col;

    -- Encode root dir
    Utils.X.Encode_Field (Root.Image, Afpx_Xref.Main.Root);

    -- Protect Git functions if not in Git
    Utils.X.Protect_Field (Afpx_Xref.Main.Branch, Root.Is_Null);
    Utils.X.Protect_Field (Afpx_Xref.Main.Diff, Root.Is_Null);
    Utils.X.Protect_Field (Afpx_Xref.Main.History, Root.Is_Null);
    Utils.X.Protect_Field (Afpx_Xref.Main.Tags, Root.Is_Null);
    Utils.X.Protect_Field (Afpx_Xref.Main.Add, Root.Is_Null);
    Utils.X.Protect_Field (Afpx_Xref.Main.Revert, Root.Is_Null);
    Utils.X.Protect_Field (Afpx_Xref.Main.Stash, Root.Is_Null);
    Utils.X.Protect_Field (Afpx_Xref.Main.Commit, Root.Is_Null);
    Utils.X.Protect_Field (Afpx_Xref.Main.Pull, Root.Is_Null);
  end Change_Dir;

  -- Check validity of current directory
  Lost_Dir : exception;
  function Check_Dir return Boolean is
    Dir : As.U.Asu_Us;
  begin
    begin
      -- Check validity of current directory
      Dir := As.U.Tus (Directory.Get_Current);
      Last_Valid_Dir := Dir;
      return True;
    exception
      when Directory.Name_Error | Directory.Access_Error =>
        -- Fallback hereafter
        null;
    end;
    -- Fallback to las valid dir or one of its parents
    Dir := Last_Valid_Dir;
    loop
      begin
        Directory.Change_Current (Dir.Image);
        -- Ok
        exit;
      exception
        when Directory.Name_Error | Directory.Access_Error =>
          -- Try parent
          if Dir.Image = "/" then
            -- No more parent: game over
            raise Lost_Dir;
          end if;
          Dir := As.U.Tus (Directory.Dirname (Dir.Image));
          -- Remove trailing '/'
          if Dir.Element (Dir.Length) = '/' then
            Dir.Trail (1);
          end if;
      end;
    end loop;
    -- Save this dir for a later fallback
    Last_Valid_Dir := Dir;
    Error ("Current directory has vanished",
           "falling back into:",
           Dir.Image);
    Afpx.Use_Descriptor (Afpx_Xref.Main.Dscr_Num);
    return False;
  end Check_Dir;

  -- Refresh list of files
  -- If Set_Dir then change dir to "."
  -- Try to restore current pos
  procedure Reread (Set_Dir : in Boolean) is
  begin
    -- Re-build list if needed or requested
    if not Check_Dir or else Set_Dir then
      Change_Dir (".");
    else
      Encode_Files (Force => False);
    end if;
  end Reread;

  -- Start periodical timer to refresh list/status of file
  --  or force a (earlier) single shot
  -- Or stop timer
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
    Local_Host := As.U.Tus (Utils.Normalize (Local_Host.Image, Len,
                                             Align_Left => False));
    return Local_Host.Image;
  end Host_Str;

  -- Init Afpx
  procedure Init (Pos : in Natural; Dir : in String := "") is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Main.Dscr_Num);
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
    Dir_Field := Afpx_Xref.Main.Dir;
    Afpx.Get_Console.Set_Name ("Agite (on " & Socket.Local_Host_Name & ")");
    Get_Handle := (others => <>);
    Utils.X.Encode_Field (Host_Str, Afpx_Xref.Main.Host);
    Change_Dir (Dir);
    if Pos /= 0 and then Pos <= Afpx.Line_List.List_Length then
      Afpx.Line_List.Move_At (Pos);
    else
      Afpx.Line_List.Rewind;
    end if;
    Afpx.Update_List (Afpx.Center_Selected);
  end Init;

  procedure Do_Edit (File_Name : in String) is
  begin
    Timer.Start;
    Utils.Launch (Editor.Image & " " & File_Name, True);
  end Do_Edit;

  procedure Do_History (Name : in String; Is_File : in Boolean) is
  begin
    -- Call history and restore current entry
    Position := Afpx.Line_List.Get_Position;
    History.Handle (Root.Image, Path.Image, Name, Is_File);
    Init (Position);
  end Do_History;

  procedure List_Tags is
  begin
    Position := Afpx.Line_List.Get_Position;
    Tags.List (Root.Image);
    Init (Position);
  end List_Tags;

  -- Add a dir or file
  procedure Do_Add_File (File : in Git_If.File_Entry_Rec) is
  begin
    if File.S2 = '?' or else File.S2 = ' ' or else File.S2 = 'U' then
      -- Untracked or not in index or unmerged
      Git_If.Do_Add (File.Name.Image);
      Encode_Files (Force => False);
    end if;
  end Do_Add_File;

  procedure Do_Revert (Name, Prev : in String) is
    File : Git_If.File_Entry_Rec;
  begin
    -- Call Confirm and restore current entry
    Position := Afpx.Line_List.Get_Position;
    Files.Move_At (Position);
    Files.Read (File, Git_If.File_Mng.Dyn_List.Current);

    if File.Kind = '/' then
      -- Handle Dir
      if File.Name.Image = "." then
        if Confirm ("Ready to reset --hard the whole worktree", "") then
          Git_If.Do_Reset_Hard;
          Position := 1;
        end if;
      elsif File.Name.Image = ".." then
        return;
      elsif Confirm ("Ready to remove directory:",
                     Directory.Build_File_Name (Path.Image, Name, "")) then
        begin
          Directory.Remove (Name);
          Position := Position - 1;
        exception
          when Directory.Access_Error =>
            -- Directory not empty
            Error ("Cannot remove directory",
                    Directory.Build_File_Name (Path.Image, Name, ""),
                   "Access error (not empty?)");
          when Directory.Name_Error =>
            -- Very unlikely but maybe the dir has disappeared meanwhile
            null;
        end;
        Position := 1;
      end if;
    elsif File.Kind = '?' then
      if File.S2 = ' ' and then File.S3 = 'D' then
        -- File is deleted locally, checkout from repository
        if Confirm ("Ready to revert:",
                    Directory.Build_File_Name (Path.Image, Name, "")) then
          Git_If.Do_Revert (Name);
          Position := 1;
        end if;
      elsif File.S2 = 'D' and then File.S3 = ' ' then
        -- File is deleted in Git, reset and checkout from repository
        if Confirm ("Ready to reset and revert:",
                    Directory.Build_File_Name (Path.Image, Name, "")) then
          Git_If.Do_Reset (Name);
          Git_If.Do_Revert (Name);
          Position := 1;
        end if;
      elsif File.S3 = 'D' then
        -- File is staged and has unstaged deletion, reset
        if Confirm ("Ready to reset:",
                    Directory.Build_File_Name (Path.Image, Name, "")) then
          Git_If.Do_Reset (Name);
        end if;
      end if;
    elsif File.Kind /= ' ' and then File.Kind /= '@' then
      -- Only for regular files or symbolic links
      return;
    elsif File.S2 = '?' then
      -- File is untracked
      if Confirm ("Ready to remove:",
                  Directory.Build_File_Name (Path.Image, Name, "")) then
        Sys_Calls.Unlink (Name);
        Position := Position - 1;
      end if;
    elsif File.S2 /= ' ' then
      if File.S2 = 'R' then
        -- File is renamed (moved) in index, reset it and restore previous
        if Confirm ("Ready to reset: "
                       & Directory.Build_File_Name (Path.Image, Name, ""),
                    "and restore: " & Prev) then
          Git_If.Do_Reset (Name);
          -- Prev has the relative path to root
          Git_If.Do_Reset (Root.Image & Prev);
          Git_If.Do_Revert (Root.Image & Prev);
          Position := Position - 1;
        end if;
      elsif File.S2 = 'M' then
        -- File is updated in index, reset the index and restore
        if Confirm ("Ready to reset and revert:",
                    Directory.Build_File_Name (Path.Image, Name, "")) then
          Git_If.Do_Reset (Name);
          Git_If.Do_Revert (Name);
        end if;
      else
        -- File is updated (A, C or U) in index, just reset the index
        if Confirm ("Ready to reset:",
                    Directory.Build_File_Name (Path.Image, Name, "")) then
          Git_If.Do_Reset (Name);
        end if;
      end if;
    elsif File.S3 /= ' ' then
      -- File is modified locally, restore
      if Confirm ("Ready to revert:",
                  Directory.Build_File_Name (Path.Image, Name, "")) then
        Git_If.Do_Revert (Name);
      end if;
    elsif File.S2 = ' ' and then File.S3 = ' ' then
      -- File is up to date, remove it from Git
      if Confirm ("Ready to remove for Git:",
                  Directory.Build_File_Name (Path.Image, Name, "")) then
        Git_If.Do_Rm (Name);
        Position := 1;
      end if;
    else
      -- File is staged and has unstaged changes, reset
      if Confirm ("Ready to reset:",
                    Directory.Build_File_Name (Path.Image, Name, "")) then
          Git_If.Do_Reset (Name);
      end if;
    end if;
    Init (Position);
  end Do_Revert;

  procedure Do_Branch is
    Curr_Dir : constant String := Directory.Get_Current;
  begin
    Position := Afpx.Line_List.Get_Position;
    Branch.Handle (Root.Image);
    Init (Position, Curr_Dir);
  end Do_Branch;

  procedure Do_Commit is
    Curr_Dir : constant String := Directory.Get_Current;
  begin
    Position := Afpx.Line_List.Get_Position;
    Commit.Handle (Root.Image);
    Init (Position, Curr_Dir);
  end Do_Commit;

  procedure Do_Stash is
    Curr_Dir : constant String := Directory.Get_Current;
  begin
    Position := Afpx.Line_List.Get_Position;
    Stash.Handle (Root.Image);
    Init (Position, Curr_Dir);
  end Do_Stash;

  procedure Do_Pull is
    Curr_Dir : constant String := Directory.Get_Current;
    Dummy : Boolean;
  begin
    Position := Afpx.Line_List.Get_Position;
    Dummy := Push_Pull.Handle (Root.Image, Pull => True);
    Init (Position, Curr_Dir);
  end Do_Pull;

  -- List action on File or Dir:
  --  Launch the diff depending on file kind and status
  --  or dispatch other actions
  type Action_List is (Default, Edit, Diff, History, Revert, Add);
  procedure List_Action (Action : in Action_List) is
    File : constant Git_If.File_Entry_Rec := Get_Current_File;
    File_Name : constant String := File.Name.Image;
    Target : As.U.Asu_Us;
    Kind : Character;
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
            Do_Revert (File_Name, File.Prev.Image);
          end if;
        when Add =>
          if File_Name /= ".." then
            Git_If.Do_Add (File_Name);
          end if;
      end case;
    elsif File.Kind = '?' then
      case Action is
        when Revert =>
          -- File or link deleted in Git
          Do_Revert (File_Name, File.Prev.Image);
        when Diff =>
          -- File is deleted: diff from last commit to null
          declare
            Hash : constant Git_If.Git_Hash := Git_If.Last_Hash (File_Name);
          begin
            if Hash /= Git_If.No_Hash then
              Git_If.Launch_Delta (Differator.Image, File_Name,
                                   Hash, "");
            end if;
          end;
        when others =>
          null;
      end case;
    elsif File.Kind = '@' then
      case Action is
        -- Link
        when Default =>
          Link_Target (File_Name, Target, Kind);
          if Kind = '/' then
            Change_Dir (Target.Image);
          elsif Kind = ' ' then
             Do_Edit (Target.Image);
          end if;
        when Revert =>
          Do_Revert (File_Name, File.Prev.Image);
        when Add =>
          Do_Add_File (File);
        when others =>
          null;
      end case;
    else
      -- Regular tracked file
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
          Do_Revert (File_Name, File.Prev.Image);
        when Add =>
          Do_Add_File (File);
      end case;
    end if;
  end List_Action;

  -- Locate a file that has its first letter = Key
  -- Move to it in Afpx list if found
  procedure Locate_Entry (Key : in Character) is
    File : Git_If.File_Entry_Rec;
    Kind_Ok : Boolean;
    Moved : Boolean;
  begin
    if Files.Is_Empty then
      return;
    end if;
    Files.Rewind;
    loop
      Files.Read (File, Moved => Moved);
      if Search_Dir then
        -- Directory
        Kind_Ok := File.Kind = '/';
      else
        -- Regular file or other kind
        Kind_Ok := File.Kind = ' ' or else File.Kind = '?';
      end if;
      if Kind_Ok and then File.Name.Element(1) = Key then
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
  end Locate_Entry;

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
    -- Swap Dir1 and Dir2 if possible and propose Goto to new Dir1
    if Dir2.Is_Null then
      New_Dir := Dir1;
    else
      New_Dir := Dir2;
      Dir2 := Dir1;
      Dir1 := New_Dir;
    end if;
    -- Propose Goto to new Dir1
    Utils.X.Encode_Field (New_Dir.Image, Dir_Field);
    Update_Cursor_Col;
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
  begin
    return True;
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
      raise;
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

  -- Init Bookmarks memory
  Bookmarks.Init;
  -- AFPX_DATA_DIR and AGITE_START_DIR
  Bookmarks.Set_Var (Afpx_Data_Dir,
      (if Environ.Is_Set (Afpx_Data_Dir) then Environ.Getenv (Afpx_Data_Dir)
       else Directory.Get_Current));
  Bookmarks.Set_Var ("AGITE_START_DIR", Directory.Get_Current);

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
  -- Set variable AGITE_INIT_DIR
  Bookmarks.Set_Var ("AGITE_INIT_DIR", Directory.Get_Current);

  -- Get or init config
  Editor := As.U.Tus (Config.Editor);
  Differator := As.U.Tus (Config.Differator);

  -- Init Afpx (dir, files..) and Timer
  Init (0);
  Timer.Start (Periodic => True);

  -- Now we can reset this env variables for our children
  Sys_Calls.Unsetenv (Afpx_Data_Dir);

  -- Init Push/Pop dirs
  Init_Dir;

  -- Main loop
  loop
    -- Protect PushD and PopD
    Utils.X.Protect_Field (Afpx_Xref.Main.Pushd, not Can_Push);
    Utils.X.Protect_Field (Afpx_Xref.Main.Popd,  not Can_Pop);

    Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
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
          when Afpx_Xref.Main.Branch =>
            -- Branches menu
            Do_Branch;
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
              Init (0, New_Dir);
            end;
          when Afpx_Xref.Main.Pushd =>
            -- PushD
            Push_Dir;
          when Afpx_Xref.Main.Popd =>
            -- PopD
            Pop_Dir;
          when Afpx_Xref.Main.Quick_Search =>
            -- Quick search
            Locate_Entry (
              Afpx.Decode_Field (Ptg_Result.Field_No, Ptg_Result.Click_Pos.Row)
                                  (Ptg_Result.Click_Pos.Col + 1));
          when Afpx_Xref.Main.Search_Dir =>
            -- Flip flop quick search dir option
            Search_Dir := not Search_Dir;
            if Search_Dir then
              Afpx.Encode_Field (Afpx_Xref.Main.Search_Dir, (0, 0), "X");
            else
              Afpx.Clear_Field (Afpx_Xref.Main.Search_Dir);
            end if;
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
          when Afpx_Xref.Main.Tags =>
            List_Tags;
          when Afpx_Xref.Main.Stash =>
            -- Stash
            Do_Stash;
          when Afpx_Xref.Main.Add =>
            -- Add
            List_Action (Add);
          when Afpx_Xref.Main.Revert =>
            -- Revert
            List_Action (Revert);
          when Afpx_Xref.Main.Commit =>
            -- Commit screen
            Do_Commit;
          when Afpx_Xref.Main.Pull =>
            -- Pull screen
            Do_Pull;
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
    Basic_Proc.Put_Line_Error ("Invalid configuration.");
    Basic_Proc.Set_Error_Exit_Code;
  when Utils.Exit_Requested =>
    begin
      Afpx.Release_Descriptor;
    exception
      when Afpx.No_Descriptor =>
        null;
    end;
    Timer.Stop;
  when Lost_Dir =>
    Basic_Proc.Put_Line_Error ("Cannot read current directory.");
    Basic_Proc.Set_Error_Exit_Code;
  when Git_If.No_Git =>
     Basic_Proc.Set_Error_Exit_Code;
end Agite;

