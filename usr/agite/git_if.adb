with Ada.Exceptions, Ada.Characters.Latin_1;
with Environ, Basic_Proc, Many_Strings, Command, Directory, Dir_Mng, String_Mng;
with Utils;
package body Git_If is

  -- For Command
  -- Two ouput flows as lists
  Out_Flow_1, Out_Flow_2 : aliased Command.Flow_Rec(Command.List);
  -- One ouput flows as string
  Out_Flow_3 : aliased Command.Flow_Rec(Command.Str);
  -- One Error flow as string
  Err_Flow : aliased Command.Flow_Rec(Command.Str);
  -- Exit code
  Exit_Code : Command.Exit_Code_Range;

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
    Cmd : Asu_Us;
    D1, D2, D3 : Natural;
    Result : Version_Rec;
  begin
    -- Git --version
    Cmd := Asu_Tus ("git");
    Many_Strings.Cat (Cmd, "--version");
    Command.Execute (
        Asu_Ts (Cmd),
        True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git --version: " & Asu_Ts (Err_Flow.Str));
      raise No_Git;
    end if;
    -- Remove tailing line feed - Check and remove heading string
    Asu.Delete (Out_Flow_3.Str, Asu.Length (Out_Flow_3.Str),
                                Asu.Length (Out_Flow_3.Str));
    if Asu.Slice (Out_Flow_3.Str, 1, 12) /= "git version " then
      Basic_Proc.Put_Line_Error ("git --version: " & Asu_Ts (Out_Flow_3.Str));
      raise No_Git;
    end if;
    Asu.Delete (Out_Flow_3.Str, 1, 12);
    -- Parse number
    D1 := String_Mng.Locate (Asu_Ts (Out_Flow_3.Str), ".", Occurence => 1);
    D2 := String_Mng.Locate (Asu_Ts (Out_Flow_3.Str), ".", Occurence => 2);
    D3 := String_Mng.Locate (Asu_Ts (Out_Flow_3.Str), ".", Occurence => 3);
    if D1 <= 1 or else D2 <= D1 + 1
    or else D2 = Asu.Length (Out_Flow_3.Str) then
      -- Incorrect format
      Basic_Proc.Put_Line_Error ("git --version: " & Asu_Ts (Out_Flow_3.Str));
      raise No_Git;
    end if;
    if D3 = 0 then
      -- Only major, minor and sub
      D3 := Asu.Length (Out_Flow_3.Str) + 1;
    end if;
    Result.Major  := Natural'Value (Asu.Slice (Out_Flow_3.Str, 1, D1 - 1));
    Result.Medium := Natural'Value (Asu.Slice (Out_Flow_3.Str, D1 + 1, D2 - 1));
    Result.Minor  := Natural'Value (Asu.Slice (Out_Flow_3.Str, D2 + 1, D3 - 1));
    return Result;
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("git --version => "
          & Ada.Exceptions.Exception_Name (Error));
      raise No_Git;
  end Get_Version;

  -- Current Root and relative path to git, empty or "/" appended
  procedure Get_Root_And_Path (Root, Path : out Asu_Us) is
    Git_Dir : Asu_Us;
    Kind : Sys_Calls.File_Kind_List;
    Dir : Asu_Us;
    use type Asu_Us, Sys_Calls.File_Kind_List;
  begin
    Git_Dir := Asu_Tus (Environ.Getenv ("GIT_DIR"));
    if Git_Dir /= Asu_Null then
      -- Get basename
      Git_Dir := Asu_Tus (Directory.Basename (Asu_Ts (Git_Dir)));
    else
      Git_Dir := Asu_Tus (".git");
    end if;

    -- Look for ".git" in current then upper directories
    Root := Asu_Tus (Directory.Get_Current);
    Path := Asu_Null;
    loop
      begin
        Kind := Kind_Of (Asu_Ts (Root & "/" & Git_Dir));
        -- Found?
        exit when Kind = Sys_Calls.Dir;
      exception
        when Sys_Calls.Name_Error => null;
        when others => raise No_Git;
      end;
      -- Not found here
      -- Can we get above?
      if Asu_Ts (Root) = "" then
        raise No_Git;
      end if;
      -- Append current Dir to Result, remove it from Path (cd ..)
      Dir := Asu_Tus (Directory.Basename (Asu_Ts (Root)));
      Path := Dir & "/" & Path;
      Asu.Delete (Root, Asu.Length (Root) - Asu.Length (Dir),
                        Asu.Length (Root));
    end loop;
    Asu.Append (Root, "/");
  end Get_Root_And_Path;

  -- LIST OF FILES AND STATUS
  -- A file entry
  -- type File_Entry_Rec is record
  --   S2 : Character;
  --   S3 : Character;
  --   Name : Asu_Us;
  -- end record;

  -- package File_Mng is newDynamic_List (File_Entry_Rec);

  -- subtype File_List is File_Mng.Dyn_List.List_Type;


  -- For searching a file in File_List and sorting File_List
  function Match (Current, Criteria : File_Entry_Rec) return Boolean is
    use type Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end Match;
  procedure File_Search is new File_Mng.Dyn_List.Search (Match);
  function Less_Than (El1, El2 : File_Entry_Rec) return Boolean is
    use type Asu_Us;
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


  -- List the files and status
  procedure List_Files (Current_Path : in String;
                        Files : in out File_List) is
    Cmd : Asu_Us;
    Str : Asu_Us;
    File_Entry : File_Entry_Rec;
    Moved : Boolean;
    Found : Boolean;
    Dir_List : Dir_Mng.File_List_Mng.List_Type;
    Dir_Entry : Dir_Mng.File_Entry_Rec;
    use type Directory.File_Kind_List;
  begin
    -- Init result
    Files.Delete_List;

    -- Git ls-files
    Cmd := Asu_Tus ("git");
    Many_Strings.Cat (Cmd, "ls-files");
    Command.Execute (
        Asu_Ts (Cmd),
        True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git ls-files: " & Asu_Ts (Err_Flow.Str));
      return;
    end if;

    -- Git status --uno --porcelain"
    Cmd := Asu_Tus ("git");
    Many_Strings.Cat (Cmd, "status");
    Many_Strings.Cat (Cmd, "--porcelain");
    Many_Strings.Cat (Cmd, ".");
    Command.Execute (
        Asu_Ts (Cmd),
        True, Command.Both,
        Out_Flow_2'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git status: " & Asu_Ts (Err_Flow.Str));
      return;
    end if;

    -- Copy local files in result
    if not Out_Flow_1.List.Is_Empty then
      Out_Flow_1.List.Rewind;
      loop
        Out_Flow_1.List.Read (Str, Moved => Moved);
        if Directory.Dirname (Asu_Ts (Str)) = "" then
          File_Entry.Name := Str;
          File_Entry.S2 := ' ';
          File_Entry.S3 := ' ';
          File_Entry.Kind := Char_Of (Asu_Ts (Str));
          Files.Insert (File_Entry);
        end if;
        exit when not Moved;
      end loop;
    end if;

    -- Update status of files in result
    if not Out_Flow_2.List.Is_Empty then
      Out_Flow_2.List.Rewind;
      loop
        Out_Flow_2.List.Read (Str, Moved => Moved);
        File_Entry.S2 := Asu.Element (Str, 1);
        File_Entry.S3 := Asu.Element (Str, 2);
        if Asu.Element (Str, Asu.Length (Str)) /= '/'
        and then (File_Entry.S2 /= ' ' or else File_Entry.S3 /= ' ') then
          -- This is a file, and in 2nd or 3rd stage or untracked
          -- Remove "XY "
          Asu.Delete (Str, 1, 3);
          if Directory.Dirname (Asu_Ts (Str)) = Current_Path then
            -- This file is in current dir, look for it
            File_Entry.Name := Asu_Tus (Directory.Basename (Asu_Ts (Str)));
            File_Entry.Kind := Char_Of (Asu_Ts (File_Entry.Name));
            File_Search (Files, Found, File_Entry,
                         From => File_Mng.Dyn_List.Absolute);
            if Found then
              -- This file is found: overwrite
              Files.Modify (File_Entry, File_Mng.Dyn_List.Current);
            else
              -- This file is not found (deleted?), insert
              Files.Insert (File_Entry);
            end if;
          end if;
        end if;
        exit when not Moved;
      end loop;
    end if;

    -- Add directories except "." and ".."
    Dir_Mng.List_Dir (Dir_List, "", "");
    if not Dir_List.Is_Empty then
      Files.Rewind (False);
      Dir_List.Rewind;
      loop
        Dir_List.Read (Dir_Entry, Moved => Moved);
        if Dir_Entry.Kind = Directory.Dir
        and then Dir_Entry.Name (1 .. Dir_Entry.Len) /= "."
        and then Dir_Entry.Name (1 .. Dir_Entry.Len) /= ".." then
          File_Entry.S2 := ' ';
          File_Entry.S3 := ' ';
          File_Entry.Kind := '/';
          File_Entry.Name := Asu_Tus (Dir_Entry.Name (1 .. Dir_Entry.Len));
          Files.Insert (File_Entry);
        end if;
        exit when not Moved;
      end loop;
    end if;

    -- Sort the list because of insertion of dirs and deleted
    File_Sort (Files);

    -- Finally insert "." then ".." at head
    Files.Rewind (False);
    File_Entry.S2 := ' ';
    File_Entry.S3 := ' ';
    File_Entry.Kind := '/';
    File_Entry.Name := Asu_Tus ("..");
    Files.Insert (File_Entry, File_Mng.Dyn_List.Prev);
    File_Entry.Name := Asu_Tus (".");
    Files.Insert (File_Entry, File_Mng.Dyn_List.Prev);
    Files.Rewind;

  end List_Files;

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
  -- ....
  -- except for last block
  procedure Read_Block (Flow : in out Command.Res_List;
                        Details : in Boolean;
                        Hash : out Git_Hash;
                        Date : out Iso_Date;
                        Comments : out Comment_Array;
                        Files : access Commit_List;
                        Done : out Boolean) is
    Line : Asu_Us;
    Ind : Natural;
    File : Commit_Entry_Rec;
  begin
    -- commit <hash>
    Flow.Read (Line);
    Assert (Asu.Length (Line) = 47);
    Assert (Asu.Slice (Line, 1, 7) = "commit ");
    Hash := Asu.Slice (Line, 8, 47);

    -- possible "Merge:... ..." then Author: ...
    Flow.Read (Line);
    if Asu.Slice (Line, 1, 7) = "Merge: " then
      Flow.Read (Line);
    end if;
    Assert (Asu.Slice (Line, 1, 8) = "Author: ");

    -- Date:   YYYY-MM-DD HH:MM:SS ...
    Flow.Read (Line, Moved => Done);
    Assert (Asu.Length (Line) >= 27);
    Assert (Asu.Slice (Line, 1, 8) = "Date:   ");
    Date := Asu.Slice (Line, 9, 27);
    if not Done then
      -- No comment and last block
      Done := not Done;
      return;
    end if;

    -- Empty line then a comment
    Flow.Read (Line);
    Assert (Asu.Length (Line) = 0);

    -- Several comments until empty line
    Ind := 0;
    Comments := (others => Asu_Null);
    loop
      Flow.Read (Line, Moved => Done);
      exit when Asu.Length (Line) = 0;
      Ind := Ind + 1;
      if Ind = 1 and then Asu.Length (Line) >= 2
      and then Asu.Slice (Line, 1, 2) /= "  " then
        -- No Comment at all in short mode (=> next commit)
        -- No Comment at all in detailed mode (=> modified files)
        if Done then
          -- When reding details (one bloc) with no comment and one change
          -- Done is False and we shall remain on this line
          Flow.Move_To (Command.Res_Mng.Dyn_List.Prev);
        end if;
        exit;
      end if;
      Assert (Asu.Length (Line) >= 4);
      Assert (Asu.Slice (Line, 1, 4) = "    ");
      -- Copy first comments
      if Ind <= Comments'Last then
        Comments(Ind) := Asu_Uslice (Line, 5,  Asu.Length (Line));
      end if;
      exit when not Done;
    end loop;

    -- No files if no detail
    if (Asu.Length (Line) = 0 or else not Done) and then not Details then
      -- The Dyn_List.Read Done is set to False when reaching the end
      -- Our Done shall be True as long as not the end
      Done := not Done;
      return;
    elsif not Done and then Details then
      -- No change in detail (merge....)
      Done := not Done;
      return;
    end if;

    -- Several updates until empty_line or end
    Ind := 0;
    if Details then
      Files.Delete_List;
    end if;
    loop
      Flow.Read (Line, Moved => Done);
      exit when Asu.Length (Line) = 0;
      Ind := Ind + 1;
      if Ind = 1 and then Asu.Length (Line) = 47
      and then Asu.Slice (Line, 1, 7) = "commit " then
        -- No change at all
        Flow.Move_To (Command.Res_Mng.Dyn_List.Prev);
        exit;
      end if;
      if Details then
        Assert (Asu.Length (Line) > 2);
        Assert (Asu.Element (Line, 2) = Ada.Characters.Latin_1.Ht);
        File.Status := Asu.Element (Line, 1);
        File.File := Asu_Uslice (Line, 3, Asu.Length (Line));
        Files.Insert (File);
      end if;
      exit when not Done;
    end loop;

    -- The Dyn_List.Read Done is set to False when reaching the end
    -- Our Done shall be True as long as not the end
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
    Cmd : Asu_Us;
    Done : Boolean;
    Log_Entry : Log_Entry_Rec;
  begin
    -- Init result
    Log.Delete_List;

    -- Git ls-files
    Cmd := Asu_Tus ("git");
    Many_Strings.Cat (Cmd, "log");
    Many_Strings.Cat (Cmd, "--date=iso");
    Many_Strings.Cat (Cmd, "--topo-order");
    Many_Strings.Cat (Cmd, "--");
    Many_Strings.Cat (Cmd, Path);
    Command.Execute (
        Asu_Ts (Cmd),
        True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git log: " & Asu_Ts (Err_Flow.Str));
      return;
    end if;

    -- Done if no log
    if Out_Flow_1.List.Is_Empty then
      return;
    end if;

    -- Encode entries
    Out_Flow_1.List.Rewind;
    loop
      Read_Block (Out_Flow_1.List, False, Log_Entry.Hash, Log_Entry.Date,
                  Log_Entry.Comment, null, Done);
      Log.Insert (Log_Entry);
      exit when Done;
    end loop;
    Log.Rewind;
  end List_Log;

  -- List detailed info on a commit
  procedure List_Commit (Hash : in Git_Hash;
                         Date : out Iso_Date;
                         Comment : out Comment_Array;
                         Commit : in out Commit_List) is
    Cmd : Asu_Us;
    Dummy_Hash : Git_Hash;
    pragma Unreferenced (Dummy_Hash);
    Dummy_Done : Boolean;
    pragma Unreferenced (Dummy_Done);
  begin
    Cmd := Asu_Tus ("git");
    Many_Strings.Cat (Cmd, "log");
    Many_Strings.Cat (Cmd, "--name-status");
    Many_Strings.Cat (Cmd, "--date=iso");
    Many_Strings.Cat (Cmd, "-n");
    Many_Strings.Cat (Cmd, "1");
    Many_Strings.Cat (Cmd, Hash);
    Command.Execute (
        Asu_Ts (Cmd),
        True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git log1: " & Asu_Ts (Err_Flow.Str));
      return;
    end if;

    -- Encode info
    if Out_Flow_1.List.Is_Empty then
      Date := (others => ' ');
      Comment := (others => Asu_Null);
      Commit.Delete_List;
    else
      Out_Flow_1.List.Rewind;
      Read_Block (Out_Flow_1.List, True, Dummy_Hash, Date,
                    Comment, Commit'Access, Dummy_Done);
    end if;
    if not Commit.Is_Empty then
      Commit.Rewind;
      Commit.Insert ((' ', Asu_Tus ("/")), Commit_File_Mng.Dyn_List.Prev);
    end if;

  end List_Commit;

  -- Cat a file at a Hash in a file
  procedure Cat (Name : in String; Hash : in Git_Hash; File : in String) is
    Cmd : Asu_Us;
  begin
    Cmd := Asu_Tus ("git");
    Many_Strings.Cat (Cmd, "show");
    Many_Strings.Cat (Cmd, Hash & ":" & Name);
    Many_Strings.Cat (Cmd, ">" & File);
    Command.Execute (
        Asu_Ts (Cmd),
        True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git show: " & Asu_Ts (Err_Flow.Str));
      return;
    end if;
  end Cat;

  -- Launch a diff (asynchronous)
  procedure Launch_Diff (Differator, File_Name : in String) is
  begin
    Utils.Launch ("git difftool -y " & " -t " & Differator
                & " HEAD -- " & File_Name);
  end Launch_Diff;

  -- Launch a diff (asynchronous) from Comp to Ref
  procedure Launch_Delta (Differator, File_Name : in String;
                          Ref_Rev, Comp_Rev : in String) is
  begin
    Utils.Launch ("git difftool -y " & " -t " & Differator
          & " " & Ref_Rev & " " & Comp_Rev & " -- " & File_Name);
  end Launch_Delta;

   -- Launch a revert (checkout) synchronous
  procedure Do_Revert (File : in String) is
    Cmd : Asu_Us;
  begin
    Cmd := Asu_Tus ("git");
    Many_Strings.Cat (Cmd, "checkout");
    Many_Strings.Cat (Cmd, File);
    Command.Execute (
        Asu_Ts (Cmd),
        True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git checkout: " & Asu_Ts (Err_Flow.Str));
      return;
    end if;
  end Do_Revert;

end Git_If;

