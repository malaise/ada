with As.U, Con_Io, Afpx.List_Manager, Basic_Proc, Integer_Image, Directory,
     Dir_Mng, Sys_Calls, Argument, Argument_Parser, Socket, String_Mng,
     Upper_Char;
with Utils.X, Git_If, Config, Bookmarks, History, Confirm;
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
    1 => ('h', As.U.Tus ("help"), False, False),
    2 => ('n', As.U.Tus ("no_history"), False, False),
    3 => ('p', As.U.Tus ("previous"), False, False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Got to previous dir (if any) / Save history
  Goto_Previous : Boolean;
  Update_History : Boolean;
  Prev_Dir, Curr_Dir : As.U.Asu_Us;

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
  Dir_Field    : constant Afpx.Field_Range := 12;
  use type Afpx.Absolute_Field_Range;

  -- Current Git root and path referred to Git root
  Root : As.U.Asu_Us;
  Path : As.U.Asu_Us;

  -- Editor and Differator
  Editor : As.U.Asu_Us;
  Differator : As.U.Asu_Us;

  -- Files list
  Files : Git_If.File_List;

  -- List width and encoding
  List_Width : Afpx.Width_Range;
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

  -- Encode files
  procedure Encode_Files is
    Branch : As.U.Asu_Us;
  begin
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No) - 4;
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

    -- Copy in Afpx list
    Init_List (Files);
    Afpx.Update_List (Afpx.Top);

    -- Encode root dir
    Afpx.Clear_Field (10);
    Afpx.Encode_Field (10, (0, 0),
       Utils.Normalize (Root.Image, Afpx.Get_Field_Width (12)));

   -- Encode current branch
   Afpx.Clear_Field (17);
   if Branch.Image = ("(no branch)") then
     Branch := As.U.Tus ("None.");
   end if;
   Afpx.Encode_Field (17, (0, 0),
         Utils.Normalize ("Br: " & Branch.Image, Afpx.Get_Field_Width (17),
                          False));

    -- De-activate Diff and history if no in Git
    if Root.Is_Null then
      Utils.X.Protect_Field (23);
      Utils.X.Protect_Field (24);
    else
      Afpx.Reset_Field (23);
      Afpx.Reset_Field (24);
    end if;

  end Encode_Files;

  -- Change dir (or at least try) according to argument or Dir_Field
  procedure Change_Dir (New_Dir : in String := "") is
    Str : constant String
        := Utils.Parse_Spaces (Afpx.Decode_Field (Dir_Field, 0, False));
    Width : constant Afpx.Width_Range := Afpx.Get_Field_Width (Dir_Field);
    Saved_Prev_Dir : As.U.Asu_Us;
  begin
    begin
      Saved_Prev_Dir := As.U.Tus (Directory.Get_Current);
      if New_Dir = "" then
        if Str /= "" then
          Directory.Change_Current (Str);
        end if;
      else
        Directory.Change_Current (New_Dir);
      end if;
    exception
      when others =>
        -- Cannot change to new dir or cannot process files (No_Git?)
        Directory.Change_Current (Directory.Get_Current);
    end;
    -- Success, reset root path for re-evaluation, save current dir
    Root.Set_Null;
    if Update_History then
      Config.Save_Curr_Dir (Directory.Get_Current);
    end if;
    Encode_Files;
    -- Set this new dir for Prev if no Prev yet
    -- Otherwise Prev is set only on significant action
    if Prev_Dir.Is_Null then
      Prev_Dir := Saved_Prev_Dir;
    end if;

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
  end Change_Dir;

  -- Save current dir as Prev_Dir
  procedure Save_Dir is
  begin
    Curr_Dir := As.U.Tus (Directory.Get_Current);
  end Save_Dir;

  -- To find current position back
  function Match (Current, Criteria : Git_If.File_Entry_Rec) return Boolean is
    use type As.U.Asu_Us;
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
  Local_Host : As.U.Asu_Us;
  function Host_Str return String is
    Len : constant Positive := Afpx.Get_Field_Width (19);
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
    Local_Host := As.U.Tus (String_Mng.Procuste (
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
    Afpx.Use_Descriptor (1);
    Cursor_Field := Afpx.Next_Cursor_Field (0);
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;
    Afpx.Encode_Field (19, (0, 0), Host_Str);
    Change_Dir;
  end;

  procedure Edit (File_Name : in String) is
  begin
    Utils.Launch (Editor.Image & " " & File_Name);
  end Edit;

  procedure Do_History (Name : in String; Is_File : in Boolean) is
    Pos : Positive;
  begin
    -- Call history and restore current entry
    Pos := Afpx.Line_List.Get_Position;
    History.Handle (Root.Image, Path.Image, Name, Is_File);
    Init;
    Afpx.Line_List.Move_At (Pos);
    Afpx.Update_List (Afpx.Center);
  end Do_History;

  procedure Do_Revert (Name : in String) is
    Pos : Positive;
  begin
    -- Call Confirm and restore current entry
    Pos := Afpx.Line_List.Get_Position;
    if Confirm ("Ready to revert:",
                Directory.Build_File_Name (Path.Image, Name, "")) then
      Git_If.Do_Revert (Name);
    end if;
    Init;
    Afpx.Line_List.Move_At (Pos);
    Afpx.Update_List (Afpx.Center);
  end Do_Revert;

  -- List action on File or Dir
  type Action_List is (Default, Edit, Diff, History, Revert);
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
            if File_Name = "." then
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
        end case;
      elsif File.Kind /= '@' and then File.Kind /= '?' then
        -- File
        Save_Dir;
        case Action is
          when Edit | Default =>
            Edit (File_Name);
          when Diff =>
            Git_If.Launch_Diff (Differator.Image, File_Name);
          when History =>
            Do_History (File_Name, True);
          when Revert =>
            Do_Revert (File_Name);
        end case;
      elsif File.Kind = '?' and then Action = Revert then
        -- Deleted file
        Save_Dir;
        Do_Revert (File_Name);
      end if;
    end;
  end List_Action;

  -- Locate a file that has Upper_Char (First_Letter) >= Key
  -- Move pos in list if found
  procedure Locate_File (Key : in Character) is
    File : Git_If.File_Entry_Rec;
    Char : Character;
    Moved : Boolean;
  begin
    if Afpx.Line_List.Is_Empty then
      return;
    end if;
    Files.Move_At (Afpx.Line_List.Get_Position);
    loop
      Files.Read (File, Moved => Moved);
      -- regular file or unknown
      if File.Kind = ' ' or else File.Kind = '?' then
        Char := Upper_Char (File.Name.Element(1));
        if Char >= Key then
          -- Got it
          if Moved then
            Files.Move_To (Git_If.File_Mng.Dyn_List.Prev);
          end if;
          Afpx.Line_List.Move_At (Files.Get_Position);
          Afpx.Update_List (Afpx.Center);
          return;
        end if;
      end if;
      exit when not Moved;
    end loop;
    -- Not found
  end Locate_File;

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
  Editor := As.U.Tus (Config.Editor);
  Differator := As.U.Tus (Config.Differator);

  -- Init Afpx
  Init;
  Sys_Calls.Unsetenv ("AFPX_DATA_DIR");

  -- Main loop
  loop
    Afpx.Set_Field_Activation (16, Directory.Get_Current /= Prev_Dir.Image);

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
          when 10 =>
            -- Root (change dir to)
            Change_Dir (Root.Image);
          when 11 =>
            -- Go (to dir)
            Change_Dir;
          when 13 =>
            -- Reread (change dir . and restore pos)
            Save_Dir;
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
          when 16 =>
            -- Prev dir
            Change_Dir (Prev_Dir.Image);
            if not Curr_Dir.Is_Null then
              Prev_Dir := Curr_Dir;
            end if;
            Save_Dir;
          when 20 =>
            -- GUI
            Save_Dir;
            Utils.Launch ("git gui");
          when 21 =>
            -- XTerm
            Save_Dir;
            Utils.Launch (Config.Xterminal);
          when 22 =>
            -- Edit (file)
            Save_Dir;
            List_Action (Edit);
          when 23 =>
            -- Diff
            Save_Dir;
            List_Action (Diff);
          when 24 =>
            -- History
            Save_Dir;
            List_Action (History);
          when 25 =>
            -- Revert
            Save_Dir;
            List_Action (Revert);
          when 26 =>
            -- Exit
            raise Utils.Exit_Requested;
          when 28 =>
            -- Quick search
            Locate_File (Afpx.Decode_Field (Ptg_Result.Field_No, 0)
                              (Ptg_Result.Click_Pos.Col + 1));
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
      & Integer_Image (Ref_Version.Major) & "."
      & Integer_Image (Ref_Version.Medium) & "."
      & Integer_Image (Ref_Version.Minor) );
    Basic_Proc.Set_Error_Exit_Code;
  when Utils.Exit_Requested =>
    begin
      Afpx.Release_Descriptor;
    exception
      when Afpx.No_Descriptor =>
        null;
   end;
end Agite;

