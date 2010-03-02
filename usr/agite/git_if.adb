with Ada.Exceptions;
with Environ, Sys_Calls, Basic_Proc, Many_Strings, Command, Directory, Dir_Mng,
     String_Mng;
package body Git_If is

  -- Asu
  function Asu_Ts (Str : Asu_Us) return String renames Utils.Asu_Ts;
  function Asu_Tus (Str : String) return Asu_Us renames Utils.Asu_Tus;
  Asu_Null : constant Asu_Us := Utils.Asu_Null;

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
  end Kind_Of;

  -- Current Git version
  function Get_Version return Version_Rec is
    Cmd : Asu_Us;
    D1, D2 : Natural;
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
    -- Remove tailing lin feed - Check and remove heading string
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
    if D1 <= 1 or else D2 <= D1 + 1
    or else D2 = Asu.Length (Out_Flow_3.Str) then
      -- Incorrect format
      Basic_Proc.Put_Line_Error ("git --version: " & Asu_Ts (Out_Flow_3.Str));
      raise No_Git;
    end if;
    Result.Major  := Natural'Value (Asu.Slice (Out_Flow_3.Str, 1, D1 - 1));
    Result.Medium := Natural'Value (Asu.Slice (Out_Flow_3.Str, D1 + 1, D2 - 1));
    Result.Minor  := Natural'Value (Asu.Slice (Out_Flow_3.Str, D2 + 1,
                                               Asu.Length (Out_Flow_3.Str)));
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
      if Asu_Ts (Root) = "/" then
        raise No_Git;
      end if;
      -- Append current Dir to Result, remove it from Path (cd ..)
      Dir := Asu_Tus (Directory.Basename (Asu_Ts (Root)));
      Path := Dir & "/" & Path;
      Asu.Delete (Root, Asu.Length (Root) - Asu.Length (Dir),
                        Asu.Length (Root));
    end loop;
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
    Done : Boolean;
    Kind : Sys_Calls.File_Kind_List;
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
    Many_Strings.Cat (Cmd, "-uno");
    Many_Strings.Cat (Cmd, "--porcelain");
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
        Out_Flow_1.List.Read (Str, Done => Done);
        if Directory.Dirname (Asu_Ts (Str)) = "" then
          File_Entry.Name := Str;
          File_Entry.S2 := ' ';
          File_Entry.S3 := ' ';
          begin
            Kind := Kind_Of (Asu_Ts (Str));
            case Kind is
              when Sys_Calls.File => File_Entry.Kind := ' ';
              when Sys_Calls.Link => File_Entry.Kind := '@';
              -- Normally no dir
              when others         => File_Entry.Kind := '?';
            end case;
          exception
            when others => File_Entry.Kind := '?';
          end;
          Files.Insert (File_Entry);
        end if;
        exit when not Done;
      end loop;
    end if;

    -- Update status of files in result
    if not Out_Flow_2.List.Is_Empty then
      Out_Flow_2.List.Rewind;
      loop
        Out_Flow_2.List.Read (Str, Done => Done);
        File_Entry.S2 := Asu.Element (Str, 1);
        File_Entry.S3 := Asu.Element (Str, 2);
        if File_Entry.S2 /= ' ' or else File_Entry.S3 /= ' ' then
          -- This file is in 2nd or 3rd stage
          -- Remove "XY "
          Asu.Delete (Str, 1, 3);
          if Directory.Dirname (Asu_Ts (Str)) = Current_Path then
            -- This file is in current dir, look for it
            File_Entry.Name := Asu_Tus (Directory.Basename (Asu_Ts (Str)));
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
        exit when not Done;
      end loop;
    end if;

    -- Add directories except "." and ".."
    Dir_Mng.List_Dir (Dir_List, "", "");
    if not Dir_List.Is_Empty then
      if not Files.Is_Empty then
        Files.Rewind;
      end if;
      Dir_List.Rewind;
      loop
        Dir_List.Read (Dir_Entry, Done => Done);
        if Dir_Entry.Kind = Directory.Dir
        and then Dir_Entry.Name (1 .. Dir_Entry.Len) /= "."
        and then Dir_Entry.Name (1 .. Dir_Entry.Len) /= ".." then
          File_Entry.S2 := ' ';
          File_Entry.S3 := ' ';
          File_Entry.Kind := '/';
          File_Entry.Name := Asu_Tus (Dir_Entry.Name (1 .. Dir_Entry.Len));
          Files.Insert (File_Entry);
        end if;
        exit when not Done;
      end loop;
    end if;

    -- Sort the list because of insertion of dirs and deleted
    File_Sort (Files);

    -- Finally insert "." then ".." at head
    if not Files.Is_Empty then
      Files.Rewind;
    end if;
    File_Entry.S2 := ' ';
    File_Entry.S3 := ' ';
    File_Entry.Kind := '/';
    File_Entry.Name := Asu_Tus ("..");
    Files.Insert (File_Entry, File_Mng.Dyn_List.Prev);
    File_Entry.Name := Asu_Tus (".");
    Files.Insert (File_Entry, File_Mng.Dyn_List.Prev);
    Files.Rewind;

  end List_Files;

  -- LOG HISTORY
  -- Git hashing number
  -- subtype Git_Hash is String (1 .. 41);

  -- A date at iso YYYY-MM-DD HH:MM:SS
  -- subtype Iso_Date is String (1 .. 19);

  -- A log entry
  -- type Log_Entry_Rec is record
  --   Hash : Git_Hash;
  --   Date : Iso_Date;
  --   Comment : Asu_Us;
  -- end record;

  -- package Log_Mng is newDynamic_List (Log_Entry_Rec);

  -- subtype Log_List is Log_Mng.Dyn_List.List_Type;

  -- List the log of a dir or file
  procedure List_Log (Path : in String;
                      Log : in out Log_List) is
  begin
    null;
  end List_Log;

  -- COMMIT DETAILS
  -- A commit file entry
  -- type Commit_Entry_Rec is record
  --   Status : Character;
  --   File : Asu_Us;
  -- end record;

  -- package Commit_File_Mng is newDynamic_List (Commit_Entry_Rec);

  -- subtype Commit_List is Commit_File_Mng.Dyn_List.List_Type;

  -- A detailed comment
  -- type Comment_Array is array (1 .. 5) of Asu_Us;

  -- List detailed info on a commit
  procedure List_Commit (Hash : in Git_Hash;
                         Date : out Iso_Date;
                         Comment : out Comment_Array;
                         Commit : in out Commit_List) is
  begin
    null;
  end List_Commit;

end Git_If;

