with Ada.Exceptions;
with Environ, Basic_Proc, Many_Strings, Command, Directory, Dir_Mng, Str_Util,
     Aski, Images, Regular_Expressions, Afpx, Trace.Loggers;
with Utils;
package body Git_If is

  -- Logger of external calls
  Logger : Trace.Loggers.Logger;

  -- For Command
  -- Two ouput flows as lists
  Out_Flow_1, Out_Flow_2 : aliased Command.Flow_Rec(Command.List);
  -- One ouput flows as string
  Out_Flow_3 : aliased Command.Flow_Rec(Command.Str);
  -- One Error flow as string
  Err_Flow_1 : aliased Command.Flow_Rec(Command.Str);
  -- Exit code
  Exit_Code : Command.Exit_Code_Range;

  -- Protection of text for shell
  function Pt (Str : String) return String renames Utils.Protect_Text;

  -- For encapsulation of Command.Execute
  In_Afpx : Boolean := False;
  procedure Entering_Afpx is
  begin
    In_Afpx := True;
  end Entering_Afpx;
  procedure Leaving_Afpx is
  begin
    In_Afpx := False;
  end Leaving_Afpx;
  procedure Execute (Cmd : in Many_Strings.Many_String;
                     Use_Shell : in Boolean;
                     Mix_Policy : in Command.Flow_Mixing_Policies;
                     Out_Flow : in Command.Flow_Access;
                     Err_Flow : in Command.Flow_Access;
                     Exit_Code : out Command.Exit_Code_Range) is
  begin
    -- Log call
    if not Logger.Is_Init then
      Logger.Init ("Git");
    end if;
    if Logger.Info_On  then
      Logger.Log_Info (Cmd.Image ('|'));
    end if;
    -- Call GIT, with or without protecting Afpx
    if In_Afpx and then not Afpx.Is_Suspended then
      Afpx.Suspend;
      Command.Execute (Cmd, Use_Shell, Mix_Policy,
                       Out_Flow, Err_Flow, Exit_Code);
      Afpx.Resume;
    else
      Command.Execute (Cmd, Use_Shell, Mix_Policy,
                       Out_Flow, Err_Flow, Exit_Code);
    end if;
  exception
    when others =>
      if Afpx.Is_Suspended then
        Afpx.Resume;
      end if;
      raise;
  end Execute;

  -- Kind file/dir/...
  function Kind_Of (Path : String) return Sys_Calls.File_Kind_List is
    Stat : Sys_Calls.File_Stat_Rec;
  begin
    Stat := Sys_Calls.File_Stat (Path);
    return Stat.Kind;
  exception
    when others =>
      return Sys_Calls.Unknown;
  end Kind_Of;


  function Char_Of (Kind : Sys_Calls.File_Kind_List) return Character is
  begin
    case Kind is
      when Sys_Calls.File => return ' ';
      when Sys_Calls.Link => return '@';
      when Sys_Calls.Dir  => return '/';
      when others         => return '?';
    end case;
  end Char_Of;

  function Char_Of (Path : String) return Character is
  begin
    return Char_Of (Kind_Of (Path));
  end Char_Of;

  -- Current Git version
  function Get_Version return Version_Rec is
    Cmd : Many_Strings.Many_String;
    D1, D2, D3 : Natural;
    Result : Version_Rec;
  begin
    -- Git --version
    Cmd.Set ("git");
    Cmd.Cat ("--version");
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git --version: " & Err_Flow_1.Str.Image);
      raise No_Git;
    end if;
    -- Remove tailing line feed - Check and remove heading string
    Out_Flow_3.Str.Delete (Out_Flow_3.Str.Length,
                           Out_Flow_3.Str.Length);
    if Out_Flow_3.Str.Slice (1, 12) /= "git version " then
      Basic_Proc.Put_Line_Error ("git --version: " & Out_Flow_3.Str.Image);
      raise No_Git;
    end if;
    Out_Flow_3.Str.Delete (1, 12);
    -- Parse number
    D1 := Str_Util.Locate (Out_Flow_3.Str.Image, ".", Occurence => 1);
    D2 := Str_Util.Locate (Out_Flow_3.Str.Image, ".", Occurence => 2);
    D3 := Str_Util.Locate (Out_Flow_3.Str.Image, ".", Occurence => 3);
    if D1 <= 1 or else D2 <= D1 + 1
    or else D2 = Out_Flow_3.Str.Length then
      -- Incorrect format
      Basic_Proc.Put_Line_Error ("git --version: " & Out_Flow_3.Str.Image);
      raise No_Git;
    end if;
    if D3 = 0 then
      -- Only major, minor and sub
      D3 := Out_Flow_3.Str.Length + 1;
    end if;
    Result.Major  := Natural'Value (Out_Flow_3.Str.Slice (1, D1 - 1));
    Result.Medium := Natural'Value (Out_Flow_3.Str.Slice (D1 + 1, D2 - 1));
    Result.Minor  := Natural'Value (Out_Flow_3.Str.Slice (D2 + 1, D3 - 1));
    return Result;
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("git --version => "
          & Ada.Exceptions.Exception_Name (Error));
      raise No_Git;
  end Get_Version;

  -- Current Root and relative path to git, empty or "/" appended
  procedure Get_Root_And_Path (Root, Path : out As.U.Asu_Us) is
    Git_Dir : As.U.Asu_Us;
    Kind : Sys_Calls.File_Kind_List;
    Dir : As.U.Asu_Us;
    use type As.U.Asu_Us, Sys_Calls.File_Kind_List;
  begin
    Git_Dir := As.U.Tus (".git");
    Environ.Get_Us ("GIT_DIR", Git_Dir);
    -- Get basename
    Git_Dir := As.U.Tus (Directory.Basename (Git_Dir.Image));

    -- Look for ".git" in current then upper directories
    Root := As.U.Tus (Directory.Get_Current);
    Path.Set_Null;
    loop
      begin
        Kind := Kind_Of (Root.Image & "/" & Git_Dir.Image);
        -- Found?
        exit when Kind = Sys_Calls.Dir;
      exception
        when Sys_Calls.Name_Error => null;
        when others => raise No_Git;
      end;
      -- Not found here
      -- Can we get above?
      if Root.Image = "" then
        raise No_Git;
      end if;
      -- Append current Dir to Result, remove it from Path (cd ..)
      Dir := As.U.Tus (Directory.Basename (Root.Image));
      Path := Dir & "/" & Path;
      Root.Delete (Root.Length - Dir.Length, Root.Length);
    end loop;
    Root.Append ("/");
  end Get_Root_And_Path;

  -- Is current repository a bare one
  function Is_Bare return Boolean is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("rev-parse");
    Cmd.Cat ("--is-bare-repository");
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return False;
    end if;
    -- Result is "true" or "false"
    return Out_Flow_3.Str.Image = "true";
  end Is_Bare;

  -- LIST OF FILES AND STATUS
  -- A file entry
  -- type File_Entry_Rec is record
  --   S2 : Character;
  --   S3 : Character;
  --   Name : As.U.Asu_Us;
  -- end record;

  -- package File_Mng is newDynamic_List (File_Entry_Rec);

  -- subtype File_List is File_Mng.Dyn_List.List_Type;


  -- For searching a file in File_List and sorting File_List
  function Match (Current, Criteria : File_Entry_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end Match;
  function File_Search is new File_Mng.Dyn_List.Search (Match);
  function Less_Than (El1, El2 : File_Entry_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    if El1.Kind = El2.Kind then
      return El1.Name < El2.Name;
    elsif El1.Kind = '/' then
      return True;
    elsif El2.Kind = '/' then
      return False;
    elsif El1.Kind = '?' then
      return False;
    elsif El2.Kind = '?' then
      return True;
    else
      -- File or link
      return El1.Name < El2.Name;
    end if;
  end Less_Than;
  procedure File_Sort is new File_Mng.Dyn_List.Sort (Less_Than);

  -- Parse file output by '--porcelain' : Nothing if first char is not '"'
  -- Else: remove leading and trailing '"'
  -- Replace any "\x" by 'x'
  procedure Parse_Filename (Txt : in out As.U.Asu_Us) is
    I : Positive;
  begin
    if Txt.Length < 2 or else Txt.Element (1) /= '"'
    or else Txt.Element (Txt.Length) /= '"' then
      return;
    end if;
    -- Remove leading and trailing '"'
    Txt.Delete (1, 1);
    Txt.Trail (1);
    -- Remove any '\' except if it is last char
    if Txt.Is_Null then
      return;
    end if;
    I := 1;
    loop
      if Txt.Element (I) = '\' and then I /= Txt.Length then
        Txt.Delete (I, I);
      end if;
      exit when I = Txt.Length;
      I := I + 1;
    end loop;
  end Parse_Filename;

  -- Internal: parse a line of "git status --porcelain"
  function Parse (Line : As.U.Asu_Us) return File_Entry_Rec is
    Str : As.U.Asu_Us;
    File_Entry : File_Entry_Rec;
    Redirect : Natural;
  begin
    Str := Line;
    File_Entry.S2 := Str.Element(1);
    File_Entry.S3 := Str.Element(2);
    -- Remove "XY "
    Str.Delete (1, 3);
    Redirect := Str_Util.Locate (Str.Image, " -> ");
    if Redirect /= 0 then
      -- File is a move (or copy?) ("<old_name> -> <new_name>")
      -- Split and store Remove "<old_name> -> "
      File_Entry.Prev := Str.Uslice (1, Redirect - 1);
      Parse_Filename (File_Entry.Prev);
      Str.Delete (1, Redirect + 3);
    end if;
    File_Entry.Name := Str;
    Parse_Filename (File_Entry.Name);
    File_Entry.Kind := Char_Of (File_Entry.Name.Image);
    return File_Entry;
  end Parse;

  -- List the files and status
  procedure List_Files (Current_Path : in String;
                        Files : in out File_List) is
    Cmd : Many_Strings.Many_String;
    Str : As.U.Asu_Us;
    File_Entry : File_Entry_Rec;
    Moved : Boolean;
    Dir_List : Dir_Mng.File_List_Mng.List_Type;
    Dir_Entry : Dir_Mng.File_Entry_Rec;

    procedure Init_List is
    begin
      Files.Delete_List;
      File_Entry := (S2 => ' ', S3 => ' ',
                     Name => As.U.Tus ("."),
                     Kind => Char_Of (Sys_Calls.Dir),
                     Prev => As.U.Asu_Null);
      Files.Insert (File_Entry);
      File_Entry.Name := As.U.Tus ("..");
      Files.Insert (File_Entry);
    end Init_List;

    use type Directory.File_Kind_List;
  begin
    -- Init result
    Files.Delete_List;

    -- Git ls-files
    Cmd.Set ("git");
    Cmd.Cat ("ls-files");
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git ls-files: " & Err_Flow_1.Str.Image);
      Init_List;
      return;
    end if;

    -- Git status --porcelain"
    Cmd.Set ("git");
    Cmd.Cat ("status");
    Cmd.Cat ("--porcelain");
    Cmd.Cat (".");
    Execute (Cmd, True, Command.Both,
        Out_Flow_2'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Init_List;
      return;
    end if;

    -- Copy local files in result
    if not Out_Flow_1.List.Is_Empty then
      Out_Flow_1.List.Rewind;
      loop
        Out_Flow_1.List.Read (Str, Moved => Moved);
        Parse_Filename (Str);
        if Directory.Dirname (Str.Image) = "" then
          File_Entry.Name := Str;
          -- Only one entry per name
          --  (ls-files lists unresolved conflicts 3 times)
          if not File_Search (Files, File_Entry,
                              From => File_Mng.Dyn_List.Absolute) then
            File_Entry.S2 := ' ';
            File_Entry.S3 := ' ';
            File_Entry.Kind := Char_Of (Str.Image);
            Files.Insert (File_Entry);
          else
            -- Skip and be ready to append next entry
            Files.Rewind (File_Mng.Dyn_List.Prev, False);
          end if;
        end if;
        exit when not Moved;
      end loop;
    end if;

    -- Update status of files in result
    if not Out_Flow_2.List.Is_Empty then
      Out_Flow_2.List.Rewind;
      loop
        Out_Flow_2.List.Read (Str, Moved => Moved);
        File_Entry := Parse (Str);
        if File_Entry.Name.Element (File_Entry.Name.Length) /= '/'
        and then (File_Entry.S2 /= ' ' or else File_Entry.S3 /= ' ')
        and then Directory.Dirname (File_Entry.Name.Image) = Current_Path then
          -- This file is in current dir, look for it
          File_Entry.Name := As.U.Tus (Directory.Basename (
              File_Entry.Name.Image));
          File_Entry.Kind := Char_Of (File_Entry.Name.Image);
          if File_Search (Files, File_Entry,
            From => File_Mng.Dyn_List.Absolute) then
            -- This file is found: overwrite
            Files.Modify (File_Entry, File_Mng.Dyn_List.Current);
          else
            -- This file is not found (deleted?), insert
            Files.Insert (File_Entry);
          end if;
        end if;
        exit when not Moved;
      end loop;
    end if;

    -- Add directories except "." and ".."
    Dir_Mng.List_Dir (Dir_List, "", "");
    if not Dir_List.Is_Empty then
      Files.Rewind (Check_Empty => False);
      Dir_List.Rewind;
      loop
        Dir_List.Read (Dir_Entry, Moved => Moved);
        if Dir_Entry.Kind = Directory.Dir
        and then Dir_Entry.Name.Image /= "."
        and then Dir_Entry.Name.Image /= ".." then
          File_Entry.S2 := ' ';
          File_Entry.S3 := ' ';
          File_Entry.Kind := '/';
          File_Entry.Name := Dir_Entry.Name;
          Files.Insert (File_Entry);
        end if;
        exit when not Moved;
      end loop;
    end if;

    -- Sort the list because of insertion of dirs and deleted
    File_Sort (Files);

    -- Finally insert "." then ".." at head
    Files.Rewind (Check_Empty => False);
    File_Entry.S2 := ' ';
    File_Entry.S3 := ' ';
    File_Entry.Kind := '/';
    File_Entry.Name := As.U.Tus ("..");
    Files.Insert (File_Entry, File_Mng.Dyn_List.Prev);
    File_Entry.Name := As.U.Tus (".");
    Files.Insert (File_Entry, File_Mng.Dyn_List.Prev);
    Files.Rewind;

  end List_Files;

  -- List all the files modified in the current repository
  procedure List_Changes (Files : in out File_List) is
    Cmd : Many_Strings.Many_String;
    Str : As.U.Asu_Us;
    File_Entry : File_Entry_Rec;
    Moved : Boolean;

  begin
    -- Init result
    Files.Delete_List;

    -- Git status --porcelain"
    Cmd.Set ("git");
    Cmd.Cat ("status");
    Cmd.Cat ("--porcelain");
    Cmd.Cat (".");
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return;
    end if;

    if Out_Flow_1.List.Is_Empty then
      return;
    end if;

    -- Copy status and name
    Out_Flow_1.List.Rewind;
    loop
      Out_Flow_1.List.Read (Str, Moved => Moved);
      File_Entry := Parse (Str);
      Files.Insert (File_Entry);
      exit when not Moved;
    end loop;

  end List_Changes;

  -- Status of a file
  function Get_Status (File : String) return File_Entry_Rec is
    Cmd : Many_Strings.Many_String;
    Line : As.U.Asu_Us;
    Result : File_Entry_Rec;
  begin
    Result.S2 := ' ';
    Result.S3 := ' ';
    Result.Kind := ' ';
    -- Git status --porcelain"
    Cmd.Set ("git");
    Cmd.Cat ("status");
    Cmd.Cat ("--porcelain");
    Cmd.Cat (Pt (File));
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Result;
    end if;
    if Out_Flow_1.List.Is_Empty then
      -- File unknown
      return Result;
    end if;
    -- Read first line
    Out_Flow_1.List.Rewind;
    Out_Flow_1.List.Read (Line, Command.Res_Mng.Dyn_List.Current);
    Result := Parse (Line);
    return Result;
  end Get_Status;

  -- Is a file (full path) locally modified
  function Is_Modified (File : String) return Boolean is
    File_Entry : File_Entry_Rec;
  begin
    File_Entry := Get_Status (File);
    return File_Entry.S2 /= ' ' or else File_Entry.S3 /= ' ';
  end Is_Modified;

  -- Assert
  Log_Error : exception;
  procedure Assert (Cond : in Boolean) is
  begin
    if not Cond then
      raise Log_Error;
    end if;
  end Assert;

  -- Flow format of log is:
  -- commit <hash>
  -- Author: ...
  -- Date:   YYYY-MM-DD HH:MM:SS ...
  --
  --     <Comment>
  --     ....
  -- C       <file>
  --     ....
  -- possibly
  -- Notes:
  --     <Some note>
  --     ....
  -- ....
  -- except for last block
  procedure Read_Block (Flow : in out Command.Res_List;
                        Details : in Boolean;
                        Hash : out Git_Hash;
                        Merge : out Boolean;
                        Date : out Iso_Date;
                        Comments : out Comment_Array;
                        Files : access Commit_List;
                        Done : out Boolean) is
    Line : As.U.Asu_Us;
    Ind : Natural;
    File : Commit_Entry_Rec;
  begin
    Logger.Log_Debug ("  Block length: " & Integer'Image (Flow.List_Length));
    -- commit <hash>
    Flow.Read (Line);
    Assert (Line.Length = 47);
    Assert (Line.Slice (1, 7) = "commit ");
    Hash := Line.Slice (8, 47);

    -- Possible "Merge:... ..." then Author: ...
    Flow.Read (Line);
    if Line.Slice (1, 7) = "Merge: " then
      Logger.Log_Debug ("  Block skip " & Line.Image);
      Merge := True;
      Flow.Read (Line);
    else
      Merge := False;
    end if;
    Assert (Line.Slice (1, 8) = "Author: ");
    Logger.Log_Debug ("  Block skip " & Line.Image);

    -- Date:   YYYY-MM-DD HH:MM:SS ...
    Flow.Read (Line, Moved => Done);
    Assert (Line.Length >= 27);
    Assert (Line.Slice (1, 8) = "Date:   ");
    Date := Line.Slice (9, 27);
    Logger.Log_Debug ("  Block got Date: " & Date);
    if not Done then
      -- No comment and last block
      Logger.Log_Debug ("  Block done cause no comment nor change");
      Done := not Done;
      return;
    end if;

    -- Empty line then a comment
    Flow.Read (Line);
    Assert (Line.Length = 0);

    -- Several comments until empty line
    Ind := 0;
    Comments := (others => As.U.Asu_Null);
    loop
      Flow.Read (Line, Moved => Done);
      exit when Line.Length = 0;
      Ind := Ind + 1;
      if Ind = 1 and then Line.Length >= 2
      and then Line.Slice (1, 2) /= "  " then
        -- No Comment at all in short mode (=> notes or next commit)
        -- No Comment at all in detailed mode (=> notes or modified files)
        if Done then
          -- When reding details (one bloc) with no comment and one change
          -- Done is False and we shall remain on this line
          Logger.Log_Debug ("  Block no comment");
          Flow.Move_To (Command.Res_Mng.Dyn_List.Prev);
        end if;
        exit;
      end if;
      Assert (Line.Length >= 4);
      Assert (Line.Slice (1, 4) = "    ");
      -- Copy first comments
      if Ind <= Comments'Last then
        Comments(Ind) := Line.Uslice (5,  Line.Length);
        Logger.Log_Debug ("  Block got Comment: " & Comments(Ind).Image);
      else
        Logger.Log_Debug ("  Block skip " & Line.Image);
      end if;
      exit when not Done;
    end loop;

    -- Comment has been read. Discard potential "Notes:"
    if Done then
      Flow.Read (Line, Moved => Done);
      if Line.Length = 6 and then Line.Slice (1, 6) = "Notes:" then
        Logger.Log_Debug ("  Block skip " & Line.Image);
        -- Discard notes until empty line
        while Done loop
          Flow.Read (Line, Moved => Done);
          Logger.Log_Debug ("  Block skip " & Line.Image);
          exit when Line.Length = 0;
        end loop;
      else
        -- No Notes: rollback
        Logger.Log_Debug ("  Block no notes");
        if Done then
          Flow.Move_To (Command.Res_Mng.Dyn_List.Prev);
        else
          Done := True;
        end if;
        Flow.Read (Line, Command.Res_Mng.Dyn_List.Current);
      end if;
    end if;

    -- No files if no detail
    if (Line.Length = 0 or else not Done) and then not Details then
      -- The Dyn_List.Read Done is set to False when reaching the end
      -- Our Done shall be True as long as not the end
      Done := not Done;
      Logger.Log_Debug ("  Block done cause no details requested");
      return;
    elsif not Done and then Details then
      -- No change in detail (merge....)
      Files.Delete_List;
      Done := not Done;
      Logger.Log_Debug ("  Block done cause no change");
      return;
    end if;

    -- Several updates until empty_line or end
    Ind := 0;
    if Details then
      Files.Delete_List;
    end if;
    loop
      Flow.Read (Line, Moved => Done);
      exit when Line.Length = 0;
      Ind := Ind + 1;
      if Ind = 1 and then Line.Length = 47
      and then Line.Slice (1, 7) = "commit " then
        -- No change at all
        Logger.Log_Debug ("  Block done cause found new commit (no change)");
        Flow.Move_To (Command.Res_Mng.Dyn_List.Prev);
        exit;
      end if;
      if Details then
        Assert (Line.Length > 2);
        Assert (Line.Element (2) = Aski.Ht);
        File.Status := Line.Element (1);
        File.File := Line.Uslice (3, Line.Length);
        Files.Insert (File);
        Logger.Log_Debug ("  Block got: " & File.Status
                        & " " & File.File.Image);
      end if;
      exit when not Done;
    end loop;

    -- The Dyn_List.Read Done is set to False when reaching the end
    -- Our Done shall be True as long as not the end
    Logger.Log_Debug ("  Block done.");
    Done := not Done;
  exception
    when others =>
      Basic_Proc.Put_Line_Error ("At line "
                               & Positive'Image (Flow.Get_Position));
      raise Log_Error;
  end Read_Block;

  -- List the log of a dir or file
  procedure List_Log (Path : in String;
                      Log : in out Log_List) is
    Cmd : Many_Strings.Many_String;
    Done : Boolean;
    Log_Entry : Log_Entry_Rec;
  begin
    -- Init result
    Log.Delete_List;

    -- Git log
    Cmd.Set ("git");
    Cmd.Cat ("log");
    Cmd.Cat ("--date=iso");
    Cmd.Cat ("--");
    Cmd.Cat (Pt (Path));
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git log: " & Err_Flow_1.Str.Image);
      return;
    end if;

    -- Done if no log
    if Out_Flow_1.List.Is_Empty then
      return;
    end if;

    -- Encode entries
    Out_Flow_1.List.Rewind;
    loop
      Read_Block (Out_Flow_1.List, False, Log_Entry.Hash, Log_Entry.Merged,
                  Log_Entry.Date, Log_Entry.Comment, null, Done);
      Log.Insert (Log_Entry);
      exit when Done;
    end loop;
    Log.Rewind;
  end List_Log;

  -- Get last hash (hash of last commit) of file or dir
  function Last_Hash (Path : in String) return Git_Hash is
    Cmd : Many_Strings.Many_String;
    Line : As.U.Asu_Us;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("log");
    Cmd.Cat ("--pretty=format:'%H'");
    Cmd.Cat ("-n");
    Cmd.Cat ("-1");
    Cmd.Cat ("--");
    Cmd.Cat (Pt (Path));
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git log1: " & Err_Flow_1.Str.Image);
      return No_Hash;
    end if;

    -- Encode info
    if Out_Flow_1.List.Is_Empty then
      return No_Hash;
    else
      Out_Flow_1.List.Rewind;
      Out_Flow_1.List.Read (Line, Command.Res_Mng.Dyn_List.Current);
      return Line.Image;
    end if;
  end Last_Hash;

  -- Get info on a commit: fill Date and Comment
  procedure Info_Commit (Commit : in out Log_Entry_Rec) is
    Cmd : Many_Strings.Many_String;
    Dummy_Done : Boolean;
  begin
    -- Git ls-files
    Cmd.Set ("git");
    Cmd.Cat ("log");
    Cmd.Cat ("--date=iso");
    Cmd.Cat ("-n");
    Cmd.Cat ("1");
    Cmd.Cat (Commit.Hash);
    Cmd.Cat ("--");
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git log2: " & Err_Flow_1.Str.Image);
      return;
    end if;

    -- Encode info
    if Out_Flow_1.List.Is_Empty then
      Commit.Date := (others => ' ');
      Commit.Comment := (others => As.U.Asu_Null);
      return;
    else
      Out_Flow_1.List.Rewind;
      Read_Block (Out_Flow_1.List, False, Commit.Hash, Commit.Merged,
                  Commit.Date, Commit.Comment, null, Dummy_Done);
    end if;
  end Info_Commit;

  -- List detailed info on a commit
  procedure List_Commit (Rev_Tag : in String;
                         Hash : out Git_Hash;
                         Merged : out Boolean;
                         Date : out Iso_Date;
                         Comment : out Comment_Array;
                         Commit : in out Commit_List) is
    Cmd : Many_Strings.Many_String;
    Dummy_Done : Boolean;
  begin
    -- Default values
    Hash := No_Hash;
    Merged := False;
    Date := (others => ' ');
    Comment := (others => As.U.Asu_Null);
    Commit.Delete_List;
    -- Command
    Cmd.Set ("git");
    Cmd.Cat ("log");
    Cmd.Cat ("--name-status");
    Cmd.Cat ("--date=iso");
    Cmd.Cat ("-n");
    Cmd.Cat ("1");
    Cmd.Cat (Rev_Tag);
    Cmd.Cat ("--");
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git log3: " & Err_Flow_1.Str.Image);
      return;
    end if;

    -- Encode info
    if not Out_Flow_1.List.Is_Empty then
Logger.Log_Debug ("  log commit not empty");
      Out_Flow_1.List.Rewind;
      Read_Block (Out_Flow_1.List, True, Hash, Merged, Date,
                  Comment, Commit'Access, Dummy_Done);
    end if;
    if not Commit.Is_Empty then
      Commit.Rewind;
      Commit.Insert ((' ', As.U.Tus ("/")), Commit_File_Mng.Dyn_List.Prev);
    end if;
  end List_Commit;

  -- List references
  procedure List_References (References : in out Reference_Mng.List_Type) is
    Cmd : Many_Strings.Many_String;
    Line : As.U.Asu_Us;
    Moved : Boolean;
  begin
    References.Delete_List;
    Cmd.Set ("git");
    Cmd.Cat ("remote");
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git remote: " & Err_Flow_1.Str.Image);
      return;
    end if;

    -- Encode info
    if not Out_Flow_1.List.Is_Empty then
      Out_Flow_1.List.Rewind;
      loop
        Out_Flow_1.List.Read (Line, Moved => Moved);
        References.Insert (Line);
        exit when not Moved;
      end loop;
    end if;
  end List_References;

  -- Cat a file at a Hash in a file
  function Cat (Name : String; Hash : String; File : String;
                Log_Error : Boolean := True) return Boolean is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("show");
    Cmd.Cat (Pt (Hash & ":" & Name));
    Cmd.Cat (">" & Pt (File));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      if Log_Error then
        Basic_Proc.Put_Line_Error ("git show: " & Err_Flow_1.Str.Image);
      end if;
      return False;
    end if;
    return True;
  end Cat;

  -- Launch a diff (asynchronous)
  procedure Launch_Diff (Differator, File_Name : in String) is
  begin
    Utils.Launch ("git difftool -y " & " -x " & Differator
                & " HEAD -- " & Pt (File_Name));
  end Launch_Diff;

  -- Launch a diff (asynchronous) from Comp to Ref
  procedure Launch_Delta (Differator, File_Name : in String;
                          Ref_Rev, Comp_Rev : in String) is
  begin
    Utils.Launch ("git difftool -y " & " -x " & Differator
          & " " & Ref_Rev & " " & Comp_Rev & " -- " & Pt (File_Name));
  end Launch_Delta;

   -- Launch a revert (checkout) synchronous
  procedure Do_Revert (File : in String) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("checkout");
    Cmd.Cat ("HEAD");
    Cmd.Cat ("--");
    Cmd.Cat (Pt (File));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git checkout: " & Err_Flow_1.Str.Image);
    end if;
  end Do_Revert;

  -- Launch a reset of index synchronous
  procedure Do_Reset (File : in String) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("reset");
    Cmd.Cat ("--");
    Cmd.Cat (Pt (File));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Don't handle error because git exits with 1 if some unstaged changes
    --  remain
  end Do_Reset;

  -- Launch a reset --hard [ <rev> ]
  procedure Do_Reset_Hard (Rev : in String := "") is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("reset");
    Cmd.Cat ("--hard");
    if Rev /= "" then
      Cmd.Cat (Pt (Rev));
    end if;
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
  end Do_Reset_Hard;

  -- Launch a soft or mixed reset
  procedure Do_Reset (Rev : in String; Soft : in Boolean) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("reset");
    if Soft then
      Cmd.Cat ("--soft");
    end if;
    if Rev /= "" then
      Cmd.Cat (Pt (Rev));
    end if;
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
  end Do_Reset;

  -- Launch a clean
  procedure Do_Clean is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("clean");
    Cmd.Cat ("-d");
    Cmd.Cat ("-f");
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
  end Do_Clean;

  -- Launch a global checkout, return "" if OK, else the error
  function Do_Checkout (Rev_Tag, Branch : String) return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("checkout");
    if Branch /= "" then
      Cmd.Cat ("-b");
      Cmd.Cat (Pt (Branch));
    end if;
    Cmd.Cat (Pt (Rev_Tag));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    else
      return "";
    end if;
  end Do_Checkout;

   -- Launch a add to index synchronous
  procedure Do_Add (File : in String) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("add");
    Cmd.Cat ("--");
    Cmd.Cat (Pt (File));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git add: " & Err_Flow_1.Str.Image);
    end if;
  end Do_Add;

  -- Launch a rm to index synchronous
  procedure Do_Rm (File : in String) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("rm");
    Cmd.Cat ("--");
    Cmd.Cat (Pt (File));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git rm: " & Err_Flow_1.Str.Image);
    end if;
  end Do_Rm;

  -- Launch a commit synchronous
  function Do_Commit (Comment : String) return String is
    Cmd : Many_Strings.Many_String;
    use type As.U.Asu_Us;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("commit");
    Cmd.Cat ("--allow-empty");
    Cmd.Cat ("-m");
    Cmd.Cat (Pt (Comment));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    end if;
    return "";
  end Do_Commit;

  -- Launch a push synchronous
  function Do_Push (Remote : String; Tag : String;
                    Set_Upstream : Boolean;
                    Force : Boolean) return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("push");
    if Set_Upstream then
      Cmd.Cat ("--set-upstream");
    end if;
    if Force then
      Cmd.Cat ("--force");
    end if;
    Cmd.Cat (Pt (Remote));
    if Tag /= "" then
      Cmd.Cat (Pt (Tag));
    end if;
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    else
      return "";
    end if;
  end Do_Push;

  -- Launch a pull synchronous
  function Do_Fetch (Remote : String; Branch : String; Pull : Boolean)
           return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    if Pull then
      Cmd.Cat ("pull");
      Cmd.Cat ("--no-commit");
    else
      Cmd.Cat ("fetch");
    end if;
    Cmd.Cat ("--tags");
    Cmd.Cat (Pt (Remote));
    Cmd.Cat (Pt (Branch));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      if not Err_Flow_1.Str.Is_Null then
        -- Something in Err flow
        return Err_Flow_1.Str.Image;
      else
        -- Output flow
        return Out_Flow_3.Str.Image;
      end if;
    else
      return "";
    end if;
  end Do_Fetch;

  -- Prune useless tracked branches on Remote
  procedure Do_Prune (Remote : String) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("remote");
    Cmd.Cat ("prune");
    Cmd.Cat (Pt (Remote));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
  end Do_Prune;

  -- Get current branch name
  No_Branch : constant String := "(no branch)";
  function Current_Branch return String is
    Cmd : Many_Strings.Many_String;
    Branch : As.U.Asu_Us;
    Moved : Boolean;
    Error : constant String := "ERROR:!";
  begin
    Cmd.Set ("git");
    Cmd.Cat ("branch");
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Error;
    end if;
    -- Look for "* "
    if not Out_Flow_1.List.Is_Empty then
      Out_Flow_1.List.Rewind;
      loop
        Out_Flow_1.List.Read (Branch, Moved => Moved);
        if Branch.Length > 2
        and then Branch.Slice (1, 2) = "* " then
          -- Current branch
          Branch.Delete (1, 2);
          return (if Branch.Image = No_Branch then ""
                  else Branch.Image);
        end if;
        exit when not Moved;
      end loop;
    end if;
    -- No active branch???
    -- Even in the middle of a rebase there is a "* (no branch)"
    return Error;
  exception
    when others =>
      return Error;
  end Current_Branch;

  -- List local or remote branches or both
  -- package Branches_Mng renames As.U.Utils.Asu_Dyn_List_Mng;
  procedure List_Branches (Local, Remote : in Boolean;
                           Branches : in out Branches_Mng.List_Type) is
    Cmd : Many_Strings.Many_String;
    Line, Opt : As.U.Asu_Us;
    Index : Natural;
    Moved : Boolean;
  begin
    Branches.Delete_List;
    if not Local and then not Remote then
      return;
    end if;
    -- List branches
    Cmd.Set ("git");
    Cmd.Cat ("branch");
    if not Local then
      -- Only remote
      Cmd.Cat ("-r");
      Opt := As.U.Tus (" -r");
    elsif Remote then
      -- Both remote and local
      Cmd.Cat ("-a");
      Opt := As.U.Tus (" -a");
    end if;
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git branch " & Opt.Image
                               & ": " & Err_Flow_1.Str.Image);
      return;
    end if;

    -- Encode info
    if not Out_Flow_1.List.Is_Empty then
      Out_Flow_1.List.Rewind;
      loop
        Out_Flow_1.List.Read (Line, Moved => Moved);
        -- Replace "* <branch>" or "  <branch>" by "<branch>"
        if Line.Length > 2 then
          Line.Delete (1, 2);
        end if;
        -- Remove potential " -> remote/branch"
        Index := Str_Util.Locate (Line.Image, " -> ");
        if Index /= 0 then
          Line.Delete (Index, Line.Length);
        end if;
        Branches.Insert (Line);
        exit when not Moved;
      end loop;
    end if;
  end List_Branches;

  -- List branches of a reference
  procedure List_Branches_Of (Reference : in String;
                              Branches : in out Branches_Mng.List_Type) is
    Cmd : Many_Strings.Many_String;
    Line: As.U.Asu_Us;
    Index : Natural;
    Moved : Boolean;
  begin
    Branches.Delete_List;
    Cmd.Set ("git");
    Cmd.Cat ("ls-remote");
    Cmd.Cat ("--heads");
    Cmd.Cat (Pt (Reference));
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git ls-remote --heads " & Reference
                               & ": " & Err_Flow_1.Str.Image);
      return;
    end if;

    -- Encode info: <hash> Tab refs/heads/<branch>
    if not Out_Flow_1.List.Is_Empty then
      Out_Flow_1.List.Rewind;
      loop
        Out_Flow_1.List.Read (Line, Moved => Moved);
        -- Keep tail after last separator
        Index := Str_Util.Locate (Line.Image, Separator & "", Forward => False);
        if Index /= 0 then
          Line.Delete (1 , Index);
          Branches.Insert (Line);
        end if;
        exit when not Moved;
      end loop;
    end if;
  end List_Branches_Of;

  -- Create a branch, return "" if Ok else the error
  function Create_Branch (Name : String) return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("branch");
    Cmd.Cat (Pt (Name));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    else
      return "";
    end if;
  end Create_Branch;

  -- Rename a branch, return "" if Ok else the error
  function Rename_Branch (Name, New_Name : String) return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("branch");
    Cmd.Cat ("-m");
    Cmd.Cat (Pt (Name));
    Cmd.Cat (Pt (New_Name));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    else
      return "";
    end if;
  end Rename_Branch;

  -- Delete a branch, return "" if Ok else the error
  function Delete_Branch (Name : String; Remote : in Boolean := False)
           return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("branch");
    Cmd.Cat ("-D");
    if Remote then
      Cmd.Cat ("-r");
    end if;
    Cmd.Cat (Pt (Name));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    else
      return "";
    end if;
  end Delete_Branch;

  -- Merge a branch, return "" if Ok else the error
  function Merge_Branch (Name : String;
                         Comment : String;
                         No_Fast_Forward : Boolean;
                         No_Commit : Boolean) return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("merge");
    if No_Commit then
      Cmd.Cat ("--no-commit");
    elsif Comment /= "" then
      Cmd.Cat ("-m");
      Cmd.Cat (Pt (Comment));
    end if;
    if No_Fast_Forward then
      Cmd.Cat ("--no-ff");
    end if;
    Cmd.Cat (Pt (Name));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    else
      return "";
    end if;
  end Merge_Branch;

  -- Get current user email
  function Get_User return String is
    Cmd : Many_Strings.Many_String;
    Line : As.U.Asu_Us;
    Result : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    -- Get name
    Cmd.Set ("git");
    Cmd.Cat ("config");
    Cmd.Cat ("user.name");
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git config user.name: " & Err_Flow_1.Str.Image);
      return "";
    end if;
    -- Read first line
    if not Out_Flow_1.List.Is_Empty then
      Out_Flow_1.List.Rewind;
      Out_Flow_1.List.Read (Line, Command.Res_Mng.Dyn_List.Current);
      Result := Line;
    end if;

    -- Get email
    Cmd.Set ("git");
    Cmd.Cat ("config");
    Cmd.Cat ("user.email");
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git config user.email: "
                               & Err_Flow_1.Str.Image);
      return "";
    end if;
    -- Read first line
    if not Out_Flow_1.List.Is_Empty then
      Out_Flow_1.List.Rewind;
      Out_Flow_1.List.Read (Line, Command.Res_Mng.Dyn_List.Current);
      if not Line.Is_Null then
        Result.Append (" <" & Line & ">");
      end if;
    end if;
    return Result.Image;
  end Get_User;

  -- List the stashes
  procedure List_Stashes (Stashes : in out Stash_List) is
    Cmd : Many_Strings.Many_String;
    Moved : Boolean;
    Line : As.U.Asu_Us;
    Stash : Stash_Entry_Rec;
    I1, I2 : Positive;
  begin
    Stashes.Delete_List;
    -- Git stash list
    Cmd.Set ("git");
    Cmd.Cat ("stash");
    Cmd.Cat ("list");
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git stash list: " & Err_Flow_1.Str.Image);
      return;
    end if;
    -- Read lines
    if Out_Flow_1.List.Is_Empty then
      return;
    end if;
    Out_Flow_1.List.Rewind;
    loop
      Out_Flow_1.List.Read (Line, Moved => Moved);
      -- stash@{<Num>}: WIP on <Branch>: <Name>
      -- stash@{<Num>}: On <Branch>: <Name>
      begin
       -- "{" <num> "}"
       I1 := Str_Util.Locate (Line.Image, "{");
       I2 := Str_Util.Locate (Line.Image, "}");
       Stash.Num := Stash_Number'Value (Line.Slice (I1 + 1, I2 - 1));
       -- "}: WIP on " <branch> ":"
       -- "}: On " <branch> ":"
       if Line.Slice (I2 + 3, I2 + 5) = "WIP" then
         I1 := I2 + 9;
       else
         I1 := I2 + 5;
       end if;
       I2 := Str_Util.Locate (Line.Image, ":", I1);
       Stash.Branch := Line.Uslice (I1 + 1, I2 - 1);
       -- ":" [ " <Name>" ]
       if I2 /= Line.Length then
         Stash.Name := Line.Uslice (I2 + 2, Line.Length);
       else
         Stash.Name.Set_Null;
       end if;
       Stashes.Insert (Stash);
      exception
        when Error:others =>
          Basic_Proc.Put_Line_Error ("Error "
              & Ada.Exceptions.Exception_Name (Error)
              & " when parsing stash " & Line.Image);
          Stashes.Delete_List;
          return;
      end;
      exit when not Moved;
    end loop;
    Stashes.Rewind;
  end List_Stashes;

  -- Stash current context, return "" if OK
  function Add_Stash (Name : String) return String is
    Cmd : Many_Strings.Many_String;
    use type As.U.Asu_Us;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("stash");
    Cmd.Cat ("save");
    Cmd.Cat (Pt (Name));
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      if not Err_Flow_1.Str.Is_Null then
        -- Something in Err flow
        return Err_Flow_1.Str.Image;
      else
        -- Output flow
        return Out_Flow_3.Str.Image;
      end if;
    else
      if Str_Util.Start_With (Out_Flow_3.Str.Image,
                              "Saved working directory and index state") then
        -- Success
        return "";
      else
        return Out_Flow_3.Str.Image;
      end if;
    end if;
  end Add_Stash;

  -- Apply a stash, return "" if OK
  function Apply_Stash (Num : Stash_Number) return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("stash");
    Cmd.Cat ("apply");
    Cmd.Cat ("-q");
    Cmd.Cat ("stash@{" & Images.Integer_Image (Num) & "}");
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    else
      return "";
    end if;
  end Apply_Stash;

  -- Pop (apply & delete) a stash, return "" if OK
  function Pop_Stash (Num : Stash_Number) return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("stash");
    Cmd.Cat ("pop");
    Cmd.Cat ("-q");
    Cmd.Cat ("stash@{" & Images.Integer_Image (Num) & "}");
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    else
      return "";
    end if;
  end Pop_Stash;

  -- Delete a stash, return "" if OK
  function Drop_Stash (Num : Stash_Number) return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("stash");
    Cmd.Cat ("drop");
    Cmd.Cat ("-q");
    Cmd.Cat ("stash@{" & Images.Integer_Image (Num) & "}");
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    else
      return "";
    end if;
  end Drop_Stash;

  -- Internal: read tag Tag.Name and fill Tag
  procedure Read_Tag (Tag : in out Tag_Entry_Rec) is
    Cmd : Many_Strings.Many_String;
    Line : As.U.Asu_Us;
    Commit_Str :constant String := "commit ";
    procedure Get_Hash is
    begin
      Assert (Line.Length = Commit_Str'Length + Git_Hash'Length);
      Assert (Line.Slice (1, Commit_Str'Length) = Commit_Str);
      Tag.Hash := Line.Slice (Commit_Str'Length + 1, Line.Length);
    end Get_Hash;
  begin
    -- Default result
    Tag.Hash := No_Hash;
    Tag.Annotated := False;
    Tag.Date := No_Date;
    Tag.Comment := As.U.Asu_Null;

    -- Command
    Cmd.Set ("git");
    Cmd.Cat ("show");
    Cmd.Cat ("--date=iso");
    Cmd.Cat ("--quiet");
    Cmd.Cat ("-s");
    Cmd.Cat (Tag.Name.Image);
    Cmd.Cat ("--");
    Execute (Cmd, True, Command.Both,
        Out_Flow_2'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git show: " & Err_Flow_1.Str.Image);
      return;
    end if;
    -- See if tag is annoted : first line of "show " is "tag <name>"
    --  otherwise it is a commit (starts with "commit <hash>")
    if Out_Flow_2.List.Is_Empty then
      raise Log_Error;
    end if;
    Out_Flow_2.List.Rewind;
    Out_Flow_2.List.Read (Line);
    if Line.Slice (1, 4) /= "tag " then
      -- Not annoted tag
      -- "commit <hash>"
      Get_Hash;
      Tag.Annotated := False;
      return;
    end if;
    Tag.Annotated := True;
    -- Line are "tag <tag_name>", "Tagger: <tagger_email>", "Date: <date_iso>",
    -- "", "<tag_comment>", "", then the commit (starts with "commit <hash>")
    Out_Flow_2.List.Read (Line);
    if Line.Length >= 8 and then Line.Slice (1, 8) = "Tagger: " then
      -- Skip tagger
      Out_Flow_2.List.Read (Line);
    end if;
    -- Read date
    if Line.Length >= 8 and then Line.Slice (1, 8) = "Date:   " then
      Tag.Date := Line.Slice (9, 27);
      Out_Flow_2.List.Read (Line);
    end if;
    -- Skip "" and read first line of comment
    if Line.Is_Null then
      -- Read 1st line of Comment
      Out_Flow_2.List.Read (Tag.Comment);
    else
      Basic_Proc.Put_Line_Error ("Unrecognized tag " & Tag.Name.Image);
      return;
    end if;

    -- Skip other lines of comment, until "commit <hash>
    loop
      Out_Flow_2.List.Read (Line);
      exit when Line.Length = Commit_Str'Length + Git_Hash'Length
      and then Line.Slice (1, Commit_Str'Length) = Commit_Str
      and then Regular_Expressions.Match (
                   "[0-9a-z]{40}",
                   Line.Slice (Commit_Str'Length + 1, Line.Length),
                   Strict => True);
    end loop;
    Get_Hash;
  exception
    when Log_Error =>
      if Out_Flow_2.List.Is_Empty then
        Basic_Proc.Put_Line_Error ("git show " & Tag.Name.Image
                                 & ": empty flow");
      else
        Basic_Proc.Put_Line_Error ("git show " & Tag.Name.Image & ": At line "
                              & Positive'Image (Out_Flow_2.List.Get_Position));
      end if;
      raise;
    when Command.Res_Mng.Dyn_List.Not_In_List =>
      -- Commit line not found
      Basic_Proc.Put_Line_Error ("Unrecognized tag " & Tag.Name.Image);
    when others =>
      Basic_Proc.Put_Line_Error ("git show " & Tag.Name.Image);
      raise;
  end Read_Tag;

  -- List tags matching Template
  procedure List_Tags (Template : in String;
                       Tags : in out Tag_List) is
    Cmd : Many_Strings.Many_String;
    Tag : Tag_Entry_Rec;
    Moved : Boolean;
    use type As.U.Asu_Us;
  begin
    Tags.Delete_List;
    Cmd.Set ("git");
    Cmd.Cat ("tag");
    if Template /= "" then
      Cmd.Cat ("-l");
      Cmd.Cat (Pt (Template));
    end if;

    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git tag"
        & (if Template /= "" then " -l " & Pt (Template)
           else "")
        & ": " & Err_Flow_1.Str.Image);
      return;
    end if;
    -- Encode info
    if not Out_Flow_1.List.Is_Empty then
      Out_Flow_1.List.Rewind;
      loop
        Out_Flow_1.List.Read (Tag.Name, Moved => Moved);
        Read_Tag (Tag);
        Tags.Insert (Tag);
        exit when not Moved;
      end loop;
    end if;
  end List_Tags;

  -- Delete tag
  procedure Delete_Tag (Tag : in String) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("tag");
    Cmd.Cat ("-d");
    Cmd.Cat (Pt (Tag));
    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git tag -d " & Tag
                                 & ": " & Err_Flow_1.Str.Image);
      return;
    end if;
  end Delete_Tag;

  -- Add a tag, return "" if Ok else the error
  function Add_Tag (Tag : String;
                    Hash : Git_Hash;
                    Annotated : Boolean;
                    Comment : in String) return String is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("tag");
    if Annotated then
      Cmd.Cat ("-a");
      Cmd.Cat ("-m");
      Cmd.Cat (Pt (Comment));
    end if;
    Cmd.Cat (Pt (Tag));
    Cmd.Cat (Hash);
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      return Err_Flow_1.Str.Image;
    else
      return "";
    end if;
  end Add_Tag;

   -- List cherry commits: the commits in Ref, and indicates if they are
  --  or not merged in target
  -- Inserts the Log_Entry_Rec with Hash and Merged set
  procedure Cherry_List (Ref, Target : in String;
                         Commits : in out Log_List) is
    Cmd : Many_Strings.Many_String;
    Line : As.U.Asu_Us;
    Commit : Log_Entry_Rec;
    Moved : Boolean;
  begin
    Commits.Delete_List;
    Cmd.Set ("git");
    Cmd.Cat ("cherry");
    Cmd.Cat (Pt (Target));
    Cmd.Cat (Pt (Ref));

    Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git cherry: "
                               & Err_Flow_1.Str.Image);
      return;
    end if;
    -- Encode info (oldest first)
    if not Out_Flow_1.List.Is_Empty then
      Out_Flow_1.List.Rewind;
      loop
        Out_Flow_1.List.Read (Line, Moved => Moved);
        if Line.Element (1) = '-' then
          Commit.Merged := True;
        elsif Line.Element (1) = '+' then
          Commit.Merged := False;
        else
          Basic_Proc.Put_Line_Error ("git cherry unexpected output : "
                                   & Line.Image);
          Commits.Delete_List;
          return;
        end if;
        Commit.Hash := Line.Slice (3, 2 + Git_Hash'Length);
        Commits.Insert (Commit);
        exit when not Moved;
      end loop;
    end if;
  end Cherry_List;

  -- Cherry pick a commit into current branch
  function Cherry_Pick (Commit : in Log_Entry_Rec;
                        Do_Commit : in Boolean) return String is
    Cmd : Many_Strings.Many_String;
    -- Unavoidable error message, to skip
    Empty_Cherry_Error : constant String :=
      "The previous cherry-pick is now empty, "
    & "possibly due to conflict resolution.";
  begin
    Cmd.Set ("git");
    Cmd.Cat ("cherry-pick");
    Cmd.Cat ("--allow-empty");
    if Do_Commit then
      Cmd.Cat ("--ff");
    else
      Cmd.Cat ("--no-commit");
    end if;
    Cmd.Cat (Commit.Hash);
    Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow_1'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      -- In case of Empty_Cherry_Error, consider Ok
      if Err_Flow_1.Str.Length >= Empty_Cherry_Error'Length
      and then Err_Flow_1.Str.Slice (1, Empty_Cherry_Error'Length)
               = Empty_Cherry_Error then
        return "";
      else
        return Err_Flow_1.Str.Image;
      end if;
    else
      return "";
    end if;
  end Cherry_Pick;

end Git_If;

