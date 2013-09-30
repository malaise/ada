with Ada.Exceptions, Ada.Characters.Latin_1;
with Environ, Basic_Proc, Many_Strings, Command, Directory, Dir_Mng, Str_Util;
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
    Cmd : Many_Strings.Many_String;
    D1, D2, D3 : Natural;
    Result : Version_Rec;
  begin
    -- Git --version
    Cmd.Set ("git");
    Cmd.Cat ("--version");
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git --version: " & Err_Flow.Str.Image);
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
    Redirect := Str_Util.Locate (Str.Image, "-> ");
    if Redirect /= 0 then
      -- File is a move (or copy?) ("<old_name> -> <new_name>")
      -- Split and store Remove "<old_name> -> "
      File_Entry.Prev := Str.Uslice (1, Redirect - 2);
      Str.Delete (1, Redirect + 2);
    end if;
    File_Entry.Name := Str;
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
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git ls-files: " & Err_Flow.Str.Image);
      Init_List;
      return;
    end if;

    -- Git status --porcelain"
    Cmd.Set ("git");
    Cmd.Cat ("status");
    Cmd.Cat ("--porcelain");
    Cmd.Cat (".");
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_2'Access, Err_Flow'Access, Exit_Code);
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
        if Directory.Dirname (Str.Image) = "" then
          File_Entry.Name := Str;
          File_Entry.S2 := ' ';
          File_Entry.S3 := ' ';
          File_Entry.Kind := Char_Of (Str.Image);
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
      Files.Rewind (False);
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
    Files.Rewind (False);
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
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
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
    Cmd.Cat (File);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
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
  -- ....
  -- except for last block
  procedure Read_Block (Flow : in out Command.Res_List;
                        Details : in Boolean;
                        Hash : out Git_Hash;
                        Date : out Iso_Date;
                        Comments : out Comment_Array;
                        Files : access Commit_List;
                        Done : out Boolean) is
    Line : As.U.Asu_Us;
    Ind : Natural;
    File : Commit_Entry_Rec;
  begin
    -- commit <hash>
    Flow.Read (Line);
    Assert (Line.Length = 47);
    Assert (Line.Slice (1, 7) = "commit ");
    Hash := Line.Slice (8, 47);

    -- possible "Merge:... ..." then Author: ...
    Flow.Read (Line);
    if Line.Slice (1, 7) = "Merge: " then
      Flow.Read (Line);
    end if;
    Assert (Line.Slice (1, 8) = "Author: ");

    -- Date:   YYYY-MM-DD HH:MM:SS ...
    Flow.Read (Line, Moved => Done);
    Assert (Line.Length >= 27);
    Assert (Line.Slice (1, 8) = "Date:   ");
    Date := Line.Slice (9, 27);
    if not Done then
      -- No comment and last block
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
        -- No Comment at all in short mode (=> next commit)
        -- No Comment at all in detailed mode (=> modified files)
        if Done then
          -- When reding details (one bloc) with no comment and one change
          -- Done is False and we shall remain on this line
          Flow.Move_To (Command.Res_Mng.Dyn_List.Prev);
        end if;
        exit;
      end if;
      Assert (Line.Length >= 4);
      Assert (Line.Slice (1, 4) = "    ");
      -- Copy first comments
      if Ind <= Comments'Last then
        Comments(Ind) := Line.Uslice (5,  Line.Length);
      end if;
      exit when not Done;
    end loop;

    -- No files if no detail
    if (Line.Length = 0 or else not Done) and then not Details then
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
      exit when Line.Length = 0;
      Ind := Ind + 1;
      if Ind = 1 and then Line.Length = 47
      and then Line.Slice (1, 7) = "commit " then
        -- No change at all
        Flow.Move_To (Command.Res_Mng.Dyn_List.Prev);
        exit;
      end if;
      if Details then
        Assert (Line.Length > 2);
        Assert (Line.Element (2) = Ada.Characters.Latin_1.Ht);
        File.Status := Line.Element (1);
        File.File := Line.Uslice (3, Line.Length);
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
    Cmd : Many_Strings.Many_String;
    Done : Boolean;
    Log_Entry : Log_Entry_Rec;
  begin
    -- Init result
    Log.Delete_List;

    -- Git ls-files
    Cmd.Set ("git");
    Cmd.Cat ("log");
    Cmd.Cat ("--follow");
    Cmd.Cat ("--date=iso");
    Cmd.Cat ("--topo-order");
    Cmd.Cat ("--");
    Cmd.Cat (Path);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git log: " & Err_Flow.Str.Image);
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
    Cmd.Cat (Path);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git log1: " & Err_Flow.Str.Image);
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

  -- List detailed info on a commit
  procedure List_Commit (Hash : in Git_Hash;
                         Date : out Iso_Date;
                         Comment : out Comment_Array;
                         Commit : in out Commit_List) is
    Cmd : Many_Strings.Many_String;
    Dummy_Hash : Git_Hash;
    pragma Unreferenced (Dummy_Hash);
    Dummy_Done : Boolean;
    pragma Unreferenced (Dummy_Done);
  begin
    Cmd.Set ("git");
    Cmd.Cat ("log");
    Cmd.Cat ("--name-status");
    Cmd.Cat ("--date=iso");
    Cmd.Cat ("-n");
    Cmd.Cat ("1");
    Cmd.Cat (Hash);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git log2: " & Err_Flow.Str.Image);
      return;
    end if;

    -- Encode info
    if Out_Flow_1.List.Is_Empty then
      Date := (others => ' ');
      Comment := (others => As.U.Asu_Null);
      Commit.Delete_List;
    else
      Out_Flow_1.List.Rewind;
      Read_Block (Out_Flow_1.List, True, Dummy_Hash, Date,
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
    Cmd.Set ("git");
    Cmd.Cat ("remote");
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git remote: " & Err_Flow.Str.Image);
      return;
    end if;

    -- Encode info
    References.Delete_List;
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
    Cmd.Cat (Hash & ":" & Name);
    Cmd.Cat (">" & File);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      if Log_Error then
        Basic_Proc.Put_Line_Error ("git show: " & Err_Flow.Str.Image);
      end if;
      return False;
    end if;
    return True;
  end Cat;

  -- Launch a diff (asynchronous)
  procedure Launch_Diff (Differator, File_Name : in String) is
  begin
    Utils.Launch ("git difftool -y " & " -x " & Differator
                & " HEAD -- " & File_Name);
  end Launch_Diff;

  -- Launch a diff (asynchronous) from Comp to Ref
  procedure Launch_Delta (Differator, File_Name : in String;
                          Ref_Rev, Comp_Rev : in String) is
  begin
    Utils.Launch ("git difftool -y " & " -x " & Differator
          & " " & Ref_Rev & " " & Comp_Rev & " -- " & File_Name);
  end Launch_Delta;

   -- Launch a revert (checkout) synchronous
  procedure Do_Revert (File : in String) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("checkout");
    Cmd.Cat ("HEAD");
    Cmd.Cat ("--");
    Cmd.Cat (File);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git checkout: " & Err_Flow.Str.Image);
    end if;
  end Do_Revert;

   -- Launch a reset of index synchronous
  procedure Do_Reset (File : in String) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("reset");
    Cmd.Cat ("--");
    Cmd.Cat (File);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Don't handle error because git exits with 1 if some unstaged changes
    --  remain
  end Do_Reset;

   -- Launch a add to index synchronous
  procedure Do_Add (File : in String) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("add");
    Cmd.Cat ("--");
    Cmd.Cat (File);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git add: " & Err_Flow.Str.Image);
    end if;
  end Do_Add;

   -- Launch a rm to index synchronous
  procedure Do_Rm (File : in String) is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("rm");
    Cmd.Cat ("--");
    Cmd.Cat (File);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git rm: " & Err_Flow.Str.Image);
    end if;
  end Do_Rm;

  -- Launch a commit synchronous
  function Do_Commit (Comment : String) return String is
    Cmd : Many_Strings.Many_String;
    Lcomment : As.U.Asu_Us;
  begin
    Lcomment := As.U.Tus (Str_Util.Substit (Comment, """", "\""", True));
    Cmd.Set ("git");
    Cmd.Cat ("commit");
    Cmd.Cat ("-m");
    Cmd.Cat ("""" & Lcomment.Image & """");
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git commit: " & Err_Flow.Str.Image);
      return Err_Flow.Str.Image;
    end if;
    return "";
  end Do_Commit;

  -- Launch a push synchronous
  function Do_Push (Remote : String) return Boolean is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("push");
    Cmd.Cat (Remote);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git push: " & Err_Flow.Str.Image);
      return False;
    end if;
    return True;
  end Do_Push;

  -- Launch a pull synchronous, return True if OK
  function Do_Pull (Remote : String; Branch : String) return Boolean is
    Cmd : Many_Strings.Many_String;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("pull");
    Cmd.Cat (Remote);
    Cmd.Cat (Branch);
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_3'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git pull: " & Err_Flow.Str.Image);
      return False;
    end if;
    return True;
  end Do_Pull;

  -- Get current branch name
  No_Branch : constant String := "(no branch)";
  function Current_Branch return String is
    Cmd : Many_Strings.Many_String;
    Branch : As.U.Asu_Us;
    Moved : Boolean;
  begin
    Cmd.Set ("git");
    Cmd.Cat ("branch");
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git branch: " & Err_Flow.Str.Image);
      return "ERROR.";
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
    return "ERROR.";
  exception
    when others =>
      return "ERROR.";
  end Current_Branch;

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
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git config user.name: " & Err_Flow.Str.Image);
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
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git config user.email: "
                               & Err_Flow.Str.Image);
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
    Command.Execute (Cmd, True, Command.Both,
        Out_Flow_1'Access, Err_Flow'Access, Exit_Code);
    -- Handle error
    if Exit_Code /= 0 then
      Basic_Proc.Put_Line_Error ("git stasj list: " & Err_Flow.Str.Image);
      return;
    end if;
    -- Read lines
    if Out_Flow_1.List.Is_Empty then
      return;
    end if;
    Out_Flow_1.List.Rewind;
    loop
      Out_Flow_1.List.Read (Line, Moved => Moved);
      -- stash@{<Num>}: On <Branch>: <Name>
      begin
       -- "{" <num> "}"
       I1 := Str_Util.Locate (Line.Image, "{");
       I2 := Str_Util.Locate (Line.Image, "}");
       Stash.Num := Stash_Number'Value (Line.Slice (I1 + 1, I2 - 1));
       -- "}: On " <branch> ":"
       I1 := I2 + 5;
       I2 := Str_Util.Locate (Line.Image, ":", I1);
       Stash.Branch := Line.Uslice (I1 + 1, I2 - 1);
       -- ": " <Name>"
       Stash.Name := Line.Uslice (I2 + 2, Line.Length);
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

  -- Stash current context, return True is Ok
  function Add_Stash (Name : String) return Boolean is
  begin
    return True;
  end Add_Stash;

  -- Apply a stash, return True is Ok
  function Apply_Stash (Num : Stash_Number) return Boolean is
  begin
    return True;
  end Apply_Stash;

  -- Pop (apply & delete) a stash, return True is Ok
  function Pop_Stash (Num : Stash_Number) return Boolean is
  begin
    return True;
  end Pop_Stash;

  -- Delete a stash, return True is Ok
  function Del_Stash (Num : Stash_Number) return Boolean is
  begin
    return True;
  end Del_Stash;

end Git_If;

