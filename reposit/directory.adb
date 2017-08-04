with Aski, C_Types, Str_Util;
package body Directory is
  use type System.Address;

  subtype Dir_Str is String (1 .. 256);

  Sep_Char : constant Character := '/';
  Sep_Str  : constant String := Sep_Char & "";

  -- For Name_Error else Access_Error
  Enoent  : constant := 2;
  -- For Constraint_Error or else Name_Error else Access_Error
  Erange  : constant := 34;
  -- For Open_Error else Access_Error
  Einval : constant := 22;

  function C_Strlen (S : System.Address) return C_Types.Size_T
    with Import => True, Convention => C, External_Name => "strlen";

  function Str_For_C (Str : String) return String is
    (Str & Aski.Nul);

  -- Returns current working directory
  function C_Getcwd (Buf : System.Address; Size : C_Types.Size_T)
           return System.Address
    with Import => True, Convention => C, External_Name => "getcwd";

  function Get_Current return String is
    Addr : System.Address;
    Result : String (1 .. Max_Dir_Name_Len);
    Len : Natural;
  begin
    Addr := C_Getcwd (Result(Result'First)'Address, Result'Length);
    if Addr = System.Null_Address then
      if Sys_Calls.Errno = Erange then
        -- Buffer too small
        raise Constraint_Error;
      elsif Sys_Calls.Errno = Enoent then
        raise Name_Error;
      else
        raise Access_Error;
      end if;
    end if;
    Len := Natural (C_Strlen (Result(Result'First)'Address));
    return Result (1 .. Len);
  end Get_Current;

  procedure Get_Current (Cur_Dir : in out As.U.Asu_Us) is
  begin
    Cur_Dir := As.U.Tus (Get_Current);
  end Get_Current;

  -- Changes current working directory
  function C_Chdir (Path : System.Address) return C_Types.Int
    with Import => True, Convention => C, External_Name => "chdir";

  procedure Change_Current (New_Dir : in String) is
    C_New_Dir : constant String := Str_For_C(New_Dir);
  begin
    if C_Chdir (C_New_Dir'Address) = -1 then
      if Sys_Calls.Errno = Enoent then
        raise Name_Error;
      else
        raise Access_Error;
      end if;
    end if;
  end Change_Current;

  -- Creates a new directory
  function C_Dir_Create (Path : System.Address) return C_Types.Int
    with Import => True, Convention => C, External_Name =>  "dir_create";

  procedure Create (New_Dir : in String) is
    C_New_Dir : constant String := Str_For_C(New_Dir);
  begin
    if C_Dir_Create (C_New_Dir'Address) = -1 then
      if Sys_Calls.Errno = Enoent then
        raise Name_Error;
      else
        raise Access_Error;
      end if;
    end if;
  end Create;

  -- Removes a new directory
  function C_Rmdir (Path : System.Address) return C_Types.Int
    with Import => True, Convention => C, External_Name => "rmdir";

  procedure Remove (New_Dir : in String) is
    C_New_Dir : constant String := Str_For_C(New_Dir);
  begin
    if C_Rmdir (C_New_Dir'Address) = -1 then
      if Sys_Calls.Errno = Enoent then
        raise Name_Error;
      else
        raise Access_Error;
      end if;
    end if;
  end Remove;

  -- Desc affectation
  procedure Set (Dest : out Dir_Rec; Val : in Dir_Rec) is
  begin
    Dest := Val;
  end Set;

  -- Closes a directory
  procedure C_Closedir (Dir : System.Address)
    with Import => True, Convention => C, External_Name => "closedir";

  -- Desc deallocation
  procedure Finalize (Dest : in out Dir_Rec) is
  begin
    if Dest.Dir_Addr /= System.Null_Address then
      C_Closedir (Dest.Dir_Addr);
    end if;
  end Finalize;

  -- Opens a directory for list of entries
  function C_Opendir (Name : System.Address) return System.Address
    with Import => True, Convention => C, External_Name => "opendir";

  function Open (Dir_Name : in String) return Dir_Desc is
    C_Dir_Name : constant String := Str_For_C(Dir_Name);
    Rec : Dir_Rec;
  begin
    Rec.Dir_Addr := C_Opendir (C_Dir_Name'Address);
    if Rec.Dir_Addr = System.Null_Address then
      if Sys_Calls.Errno = Enoent then
        raise Name_Error;
      else
        raise Access_Error;
      end if;
    end if;
    return (Smart_Desc_Mng.Init (Rec) with null record);
  end Open;
  procedure Open (Desc : in out Dir_Desc; Dir_Name : in String) is
  begin
    Desc := Open (Dir_Name);
  end Open;

  -- Gets next entry of the opened directory
  function C_Readdir (Dir : System.Address; Name : System.Address)
           return C_Types.Int
    with Import => True, Convention => C, External_Name => "read_dir";

  function Get_Rec (Desc : Dir_Desc) return Dir_Rec is
    Rec : Dir_Rec;
  begin
    -- Check dir desc
    if not Desc.Is_Set then
      raise Open_Error;
    end if;
    Desc.Get (Rec);
    if Rec.Dir_Addr = System.Null_Address then
      raise Open_Error;
    end if;
    return Rec;
  end Get_Rec;

  function Next_Entry (Desc : Dir_Desc) return String is
    Len : Integer;
    Dir_Name : Dir_Str;
  begin
    -- Read entry and check validity
    Len := C_Readdir (Get_Rec (Desc).Dir_Addr,
                      Dir_Name(Dir_Name'First)'Address);
    if Len = -1 then
      raise End_Error;
    end if;

    return Dir_Name(1 .. Len);
  end Next_Entry;

  procedure Next_Entry (Desc : in Dir_Desc; Dir_Entry : in out As.U.Asu_Us) is
  begin
    Dir_Entry := As.U.Tus (Next_Entry (Desc));
  end Next_Entry;

  -- Reset entries for the first
  procedure C_Rewinddir (Dir : System.Address)
    with Import => True, Convention => C, External_Name => "rewinddir";

  procedure Rewind (Desc : in Dir_Desc) is
  begin
    C_Rewinddir (Get_Rec (Desc).Dir_Addr);
  end Rewind;

  procedure Close (Desc : in out Dir_Desc) is
    Rec : Dir_Rec := Get_Rec (Desc);
  begin
    C_Closedir (Rec.Dir_Addr);
    Rec.Dir_Addr := System.Null_Address;
    Desc.Set (Rec);
  end Close;

  -- Use Sys_Calls to retrieve file stat
  function File_Kind (File_Name : String) return File_Kind_List is
  begin
    return File_Kind_List(Sys_Calls.File_Stat (File_Name).Kind);
  exception
    when Sys_Calls.Name_Error =>
      raise Name_Error;
    when Sys_Calls.Access_Error =>
      raise Access_Error;
  end File_Kind;

  -- Is it a file, a dir, a symbolic link
  function Is_File (File_Name : String) return Boolean is
    use type Sys_Calls.File_Kind_List;
  begin
    return Sys_Calls.File_Stat (File_Name).Kind = Sys_Calls.File;
  end Is_File;

  function Is_Dir (File_Name : String) return Boolean is
    use type Sys_Calls.File_Kind_List;
  begin
    return Sys_Calls.File_Stat (File_Name).Kind = Sys_Calls.Dir;
  end Is_Dir;

  function Is_Link (File_Name : String) return Boolean is
    use type Sys_Calls.File_Kind_List;
  begin
    return Sys_Calls.File_Stat (File_Name).Kind = Sys_Calls.Link;
  end Is_Link;

  -- Follow a link
  function C_Readlink (Path : System.Address;
                       Buf  : System.Address;
                       Bufsiz : C_Types.Size_T)
           return C_Types.Size_T
    with Import => True, Convention => C, External_Name => "readlink";

  -- May raise Name_Error if File_Name does not exist
  --           Open_Error if File_Name is not a link
  function Read_One_Link (File_Name : String) return String is
    Str : String(1 .. Max_Dir_Name_Len);
    C_File_Name : constant String := Str_For_C(File_Name);
    Res : Integer;
  begin
    Res := Integer (C_Readlink (C_File_Name'Address, Str'Address, Str'Length));
    if Res /= -1 then
      return Str (1 .. Res);
    elsif Sys_Calls.Errno = Enoent then
      -- Enoent : file not found
      raise Name_Error;
    elsif Sys_Calls.Errno = Einval then
      -- Einval : not a symbolic link
      raise Open_Error;
    else
      raise Access_Error;
    end if;
  end Read_One_Link;

  --Internal multi-purpose scan of links
  procedure Scan_Link (File_Name : in String;
                       Recursive : in Boolean;
                       Target : out As.U.Asu_Us;
                       Result : out Link_Result) is
    Orig, Src, Dest, Curr_Dir : As.U.Asu_Us;
    Iter : Positive;
    Threshold : constant := 1024;
    use type As.U.Asu_Us, Sys_Calls.File_Kind_List;

    -- Expand as soon as a path
    procedure Expand (File : in out As.U.Asu_Us) is
    begin
      if Str_Util.Locate (File.Image, Sep_Str) /= 0 then
        if File.Element (1) /= Sep_Char then
          -- Target contains a relative path
          --  => Append current dirname
          File.Prepend (Curr_Dir & Sep_Char);
        end if;
        -- Target contains a path, so cleanup and store new path
        File := As.U.Tus (Normalize_Path (File.Image));
        Curr_Dir := As.U.Tus (Dirname (File.Image));
      end if;
    end Expand;

  begin
    -- Check file_name is a link
    if Sys_Calls.File_Stat (File_Name).Kind /= Sys_Calls.Link then
      Target.Set_Null;
      Result := Link_Open;
      return;
    end if;

    -- Init current dir
    Src := As.U.Tus (File_Name);
    if Src.Element (1) = Sep_Char then
      Curr_Dir := As.U.Tus (Dirname (Src.Image));
    else
      Curr_Dir := As.U.Tus (Get_Current & Sep_Char);
    end if;

    -- Init first dir
    Expand (Src);

    Result := Link_Ok;
    if not Recursive then
      Target := As.U.Tus (Read_One_Link(Src.Image));
      Expand (Target);
      return;
    end if;

    Iter := 1;
    loop
      -- Current is a link, read it,
      Dest := As.U.Tus (Read_One_Link(Src.Image));
      Expand (Dest);
      -- Done when not a link
      exit when Sys_Calls.File_Stat (Dest.Image).Kind /= Sys_Calls.Link;

      -- Detect recursion
      if Dest = Src then
        -- Link points to itself
        raise Recursive_Link;
      elsif Iter = Threshold then
        -- Looks like an infinite loop, store current as Orig
        Orig := Dest;
      elsif Iter > Threshold and then Dest = Orig then
        -- We have looped back to Orig
        raise Recursive_Link;
      end if;

      Iter := Iter + 1;
      Src := Dest;
    end loop;
    -- Ok
    Target := Dest;
    return;
  exception
    -- Open_Error shall not occur because we exit the loop (with success)
    --  if target is not a symlink
    when Name_Error =>
      Target := Dest;
      Result := Link_Name;
    when Access_Error =>
      Target := Dest;
      Result := Link_Access;
    when Recursive_Link =>
      Target := Dest;
      Result := Link_Recursive;
  end Scan_Link;

  -- Symlink API
  function Read_Link (File_Name : String;
                      Recursive : Boolean := True) return String is
    Target : As.U.Asu_Us;
  begin
    Read_Link (File_Name, Target, Recursive);
    return Target.Image;
  end Read_Link;

  procedure Read_Link (File_Name : in String;
                       Target : out As.U.Asu_Us;
                       Recursive : in Boolean := True) is
    Result : Link_Result;
  begin
    Scan_Link (File_Name, Recursive, Target, Result);
    case Result is
      when Link_Ok        => null;
      when Link_Name      => raise Name_Error;
      when Link_Access    => raise Access_Error;
      when Link_Open      => raise Open_Error;
      when Link_Recursive => raise Recursive_Link;
    end case;
  end Read_Link;

  function Scan_Link (File_Name : in String;
                       Target : out As.U.Asu_Us) return Link_Result is
  begin
    return Result : Link_Result do
      Scan_Link (File_Name, True, Target, Result);
    end return;
  end Scan_Link;

  -- Does file name match a pattern
  function C_Fnmatch (Pattern : System.Address;
                      Strings : System.Address;
                      Flags : C_Types.Int)
           return C_Types.Int
    with Import => True, Convention => C, External_Name => "fnmatch";
  File_Matches : constant := 0;
  File_Not_Matches : constant := 1;

  function File_Match (File_Name : String; Template : String) return Boolean is
    C_File_Name : constant String := Str_For_C (File_Name);
    C_Template : constant String := Str_For_C (Template);
    Res : Integer;
  begin
    Res := C_Fnmatch(C_Template'Address, C_File_Name'Address, 0);
    if Res = File_Matches then
      return True;
    elsif Res = File_Not_Matches then
      return False;
    else
      raise Syntax_Error;
    end if;
  end File_Match;


  -- Normalize a path:
  -- - append a "/" if ending by "/." or "/.."
  -- - replace any "//" by "/" recusively
  -- - then remove any "./",
  -- - then recusively replace any "<name>/.." by "" (<name> /= "..")
  -- - then recusively replace any leading "/.." by ""
  function Normalize_Path (Path : String) return String is
    Res : As.U.Asu_Us;
    Start, First, Second, Init : Natural;
  begin
    -- Append a "/" if ending by "/." or "/.."
    Res := As.U.Tus (Path);
    if (Path'Length >= 2
        and then Path(Path'Last - 1 .. Path'Last) = "/.")
    or else (Path'Length >= 3
             and then Path(Path'Last - 2 .. Path'Last) = "/..") then
      Res.Append (Sep_Char);
    end if;

    -- "//" -> "/" recursively
    loop
      Start := Str_Util.Locate (Res.Image, Sep_Char & Sep_Char);
      exit when Start = 0;
      Res.Delete (Start, Start);
    end loop;

    -- "/./" -> "/"
    loop
      Start := Str_Util.Locate (Res.Image, Sep_Char & '.' & Sep_Char);
      exit when Start = 0;
      Res.Delete (Start, Start + 1);
    end loop;

    -- "<name>/../" -> "" recusively
    -- Start at first significant char
    Init := 1;
    if Res.Length > 0 and then Res.Element (1) = Sep_Char then
      Init := Init + 1;
    end if;
    Start := Init;
    loop
      -- Locate next separator
      First := Str_Util.Locate (Res.Image, Sep_Str, Start + 1);
      exit when First = 0;
      -- Locate next separator
      Second := Str_Util.Locate (Res.Image, Sep_Str, First + 1);
      exit when Second = 0;
      if Res.Slice (First + 1, Second - 1) = ".."
      and then Res.Slice (Start, First - 1) /= ".." then
        -- Delete "<name>/../", and restart from beginning
        Res.Delete (Start, Second);
        Start := Init;
      else
        -- Skip
        Start := First + 1;
      end if;
      exit when Start >= Res.Length;
    end loop;

    -- "^/.." -> ""
    while Str_Util.Locate (Res.Image, Sep_Char & "..") = 1 loop
      Res.Delete (Start, 3);
    end loop;

    -- Remove tailing '/' except if only "/"
    if Res.Length > 1 and then Res.Element (Res.Length) = '/' then
      Res.Trail (1);
    end if;

    -- Done
    return Res.Image;

  end Normalize_Path;

   -- Get full path of a path
  function Make_Full_Path (Path : String) return String is
    (Normalize_Path (
        if Path = "" then Get_Current & "/"
        -- Path is already absolute => Normalize
        elsif Path(Path'First) = '/' then Path
        -- Path is relative, prepend current path & Normalize
        else Get_Current & "/" & Path) );

  -- File name manipulation
  Dot :  constant String := ".";
  -- Get dir name (path) from a complete file name (up to the last / included)
  function Dirname (File_Name : String; Strip : Boolean := False)
           return String is
    I : Natural;
  begin
    I := Str_Util.Locate (File_Name, Sep_Str, Forward => False);
    if Strip and then I > 1 then
      I := I - 1;
    end if;
    -- No / in file name => dir name is empty
    return (if I = 0 then "" else File_Name(File_Name'First .. I));
  end Dirname;

   -- If Path Dirname is Ref (or current dir if Path is empty)
  --  then strip the path
  function Reduce_Path (Path : String; Ref : String := "") return String is
    Dir : constant String := Dirname (Path, True);
  begin
    if Dir = (if Ref = "" then Get_Current else Ref) then
      return Path(Path'First + Dir'Length .. Path'Last);
    else
      return Path;
    end if;
  end Reduce_Path;

  -- Get file name from a complete file name (from the last / excluded),
  --  then remove the end of it if it matches Suffix (. is not necessary)
  function Basename (File_Name : String; Suffix : String := "") return String is
    I : Natural;
    Last : constant Natural := File_Name'Last;
  begin
    I := Str_Util.Locate (File_Name, Sep_Str, Forward => False);
    return (if I = 0 then
              -- No / in file name => no dir name
              File_Name
            elsif Suffix = ""
            or else Suffix'Length > Last - I
            or else File_Name(Last - Suffix'Length + 1 .. Last) /= Suffix then
              -- File name does not match suffix
              File_Name(I + 1 .. Last)
            else
              -- End of file name matches suffix
              File_Name(I + 1 .. Last - Suffix'Length));
  end Basename;

  -- Extract the file name, then its prefix (up to the first . excluded)
  function File_Prefix (File_Name : String) return String is
    I : Natural;
    File : constant String := Basename (File_Name);
  begin
    I := Str_Util.Locate (File, Dot);
    return (if I = 0 then
              -- No '.', return full file name
              File
            else
              File (File'First .. I - 1));
  end File_Prefix;

  -- Extract the file name, then its suffix (from the first . included)
  function File_Suffix (File_Name : String) return String is
    I : Natural;
    File : constant String := Basename (File_Name);
  begin
    I := Str_Util.Locate (File, Dot);
    -- No '.', return no suffix
    return (if I = 0 then "" else File (I .. File'Last));
  end File_Suffix;

  -- Build a complete file name
  function Build_File_Name (Dirname : String; File_Prefix, File_Suffix : in String)
           return String is
    function Build_Name return String is
      (if File_Suffix = "" then File_Prefix
       else File_Prefix & Dot & File_Suffix);
  begin
    return (if Dirname = "" then Build_Name
            elsif Dirname(Dirname'Last) = Sep_Char then Dirname & Build_Name
            else Dirname & Sep_Str & Build_Name);
  end Build_File_Name;

end Directory;

