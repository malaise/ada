with Environ, Sys_Calls, Basic_Proc, Many_Strings, Command, Directory;
package body Git_If is

  -- Asu
  function Asu_Ts (Str : Asu_Us) return String renames Asu.To_String;
  function Asu_Tus (Str : String) return Asu_Us renames Asu.To_Unbounded_String;
  Asu_Null :  constant Asu_Us := Asu.Null_Unbounded_String;

  -- Current relative path to git, empty or "/" appended
  function Get_Path return Asu_Us is
    Git_Dir : Asu_Us;
    Result : Asu_Us;
    Curr_Path : Asu_Us;
    Stat : Sys_Calls.File_Stat_Rec;
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
    Curr_Path := Asu_Tus (Directory.Get_Current) & "/";
    loop
      begin
        Stat := Sys_Calls.File_Stat (Asu_Ts (Curr_Path & Git_Dir));
        -- Found?
        exit when Stat.Kind = Sys_Calls.Dir;
      exception
        when Sys_Calls.Name_Error => null;
        when others => raise No_Git;
      end;
      -- Not found here
      -- Can we get above?
      if Asu_Ts (Curr_Path) = "/" then
        raise No_Git;
      end if;
      -- Append current Dir to Result, remove it from Path (cd ..)
      Asu.Delete (Curr_Path, Asu.Length (Curr_Path), Asu.Length (Curr_Path));
      Dir := Asu_Tus (Directory.Basename (Asu_Ts (Curr_Path)));
      Result := Dir & "/" & Result;
      Asu.Delete (Curr_Path, Asu.Length (Curr_Path) - Asu.Length (Dir) + 1,
                             Asu.Length (Curr_Path));
    end loop;

    return Result;
  end Get_Path;


  -- For Command
  -- Two ouput flows as lists
  Out_Flow_1, Out_Flow_2 : aliased Command.Flow_Rec(Command.List);
  -- One Error flow as string
  Err_Flow: aliased Command.Flow_Rec(Command.Str);
  -- Exit code
  Exit_Code : Command.Exit_Code_Range;

  -- Asu stuff
  -- package Asu renames Ada.Strings.Unbounded;

  -- subtype Asu_Us is Asu.Unbounded_String;

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
    return El1.Name < El2.Name;
  end Less_Than;
  procedure File_Sort is new File_Mng.Dyn_List.Sort (Less_Than);


  -- List the files and status
  procedure List_Files (Current_Path : in String;
                        Files : in out File_List) is
    Cmd : Asu_Us;
    Str : Asu_Us;
    File_Entry : File_Entry_Rec;
    Done : Boolean;
    Found : Boolean;
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

    -- Done if no file
    if Out_Flow_1.List.Is_Empty then
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
    Out_Flow_1.List.Rewind;
    loop
      Out_Flow_1.List.Read (Str, Done => Done);
      if Directory.Basename (Asu_Ts (Str)) = "" then
        File_Entry.Name := Str;
        File_Entry.S2 := ' ';
        File_Entry.S3 := ' ';
      end if;
      exit when not Done;
    end loop;

    -- Done if no status
    if Out_Flow_2.List.Is_Empty then
      return;
    end if;

    -- Update status of files in result
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

    -- Finally sort the list because of insertion of deleted
    File_Sort (Files);
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

