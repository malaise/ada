with Ada.Characters.Latin_1;
with C_Types, Str_Util;
package body Directory is
  use System;

  subtype Dir_Str is String (1 .. 256);

  Separator : constant Character := '/';

  -- For Name_Error else Access_Error
  Enoent  : constant := 2;
  -- For Open_Error else Access_Error
  Einval : constant := 22;

  function C_Strlen (S : System.Address) return C_Types.Size_T;
  pragma Import(C, C_Strlen, "strlen");

  function Str_For_C (Str : String) return String is
  begin
    return Str & Ada.Characters.Latin_1.Nul;
  end Str_For_C;

  -- Returns current working directory
  function C_Getcwd (Buf : System.Address; Size : C_Types.Size_T)
           return System.Address;
  pragma Import(C, C_Getcwd, "getcwd");

  function Get_Current return String is
    Addr : System.Address;
    Result : String (1 .. Max_Dir_Name_Len);
    Len : Natural;
  begin
    Addr := C_Getcwd (Result(Result'First)'Address, Result'Length);
    if Addr = System.Null_Address then
      -- Buffer too small
      raise Constraint_Error;
    end if;
    Len := Natural (C_Strlen (Result(Result'First)'Address));
    return Result (1 .. Len);
  end Get_Current;

  procedure Get_Current (Cur_Dir : in out As.U.Asu_Us) is
  begin
    Cur_Dir := As.U.Tus (Get_Current);
  end Get_Current;

  -- Changes current working directory
  function C_Chdir (Path : System.Address) return C_Types.Int;
  pragma Import(C, C_Chdir, "chdir");

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
  function C_Dir_Create (Path : System.Address) return C_Types.Int;
  pragma Import(C, C_Dir_Create, "dir_create");

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
  function C_Rmdir (Path : System.Address) return C_Types.Int;
  pragma Import(C, C_Rmdir, "rmdir");

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
  procedure Set (Dest : in out Dir_Rec; Val : in Dir_Rec) is
  begin
    Dest := Val;
  end Set;

  -- Opens a directory for list of entries
  function C_Opendir (Name : System.Address) return System.Address;
  pragma Import(C, C_Opendir, "opendir");

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
           return C_Types.Int;
  pragma Import(C, C_Readdir, "read_dir");

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
  procedure C_Rewinddir (Dir : System.Address);
  pragma Import(C, C_Rewinddir, "rewinddir");

  procedure Rewind (Desc : in Dir_Desc) is
  begin
    C_Rewinddir (Get_Rec (Desc).Dir_Addr);
  end Rewind;

  -- Closes a directory
  procedure C_Closedir (Dir : System.Address);
  pragma Import(C, C_Closedir, "closedir");

  procedure Close (Desc : in out Dir_Desc) is
    Rec : Dir_Rec := Get_Rec (Desc);
  begin
    C_Closedir (Rec.Dir_Addr);
    Rec.Dir_Addr := System.Null_Address;
    Desc.Set (Rec);
  end Close;


  -- Follow a link
  function C_Readlink (Path : System.Address;
                       Buf  : System.Address;
                       Bufsiz : C_Types.Size_T)
           return C_Types.Size_T;
  pragma Import(C, C_Readlink, "readlink");

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

  function Read_Link (File_Name : String;
                      Recursive : Boolean := True) return String is
    Orig, Src, Dest : As.U.Asu_Us;
    Iter : Positive;
    Threshold : constant := 1024;
    use type As.U.Asu_Us, Sys_Calls.File_Kind_List;
  begin
    -- Check file_name  is a link
    if Sys_Calls.File_Stat (File_Name).Kind /= Sys_Calls.Link then
      raise Open_Error;
    end if;
    if not Recursive then
      return Read_One_Link(File_Name);
    end if;

    Src := As.U.Tus (Make_Full_Path (File_Name));
    Iter := 1;
    loop
      -- Current is a link, read it,
      Dest := As.U.Tus (Read_One_Link(Src.Image));
      if Dest.Length >= 1 and then Dest.Element (1) /= Separator then
        -- Link is relative, append apth of source
        Dest := As.U.Tus (Dirname (Src.Image)) & Dest;
      end if;
      Dest := As.U.Tus (Make_Full_Path (Dest.Image));
      -- Done when not a link
      exit when Sys_Calls.File_Stat (Dest.Image).Kind /= Sys_Calls.Link;

      -- Detec recursion
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
    return Dest.Image;
  end Read_Link;


  procedure Read_Link (File_Name : in String;
                       Target : in out As.U.Asu_Us;
                       Recursive : in Boolean := True) is
  begin
    Target := As.U.Tus (Read_Link(File_Name, Recursive));
  end Read_Link;


  -- Does file name match a pattern
  function C_Fnmatch (Pattern : System.Address;
                      Strings : System.Address;
                      Flags : C_Types.Int)
           return C_Types.Int;
  pragma Import(C, C_Fnmatch, "fnmatch");
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
    Sep_Char : constant Character := '/';
    Sep_Str : constant String := Sep_Char & "";
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
    loop
      if Str_Util.Locate (Res.Image, Sep_Char & "..") = 1 then
        Res.Delete (Start, 3);
      else
        exit;
      end if;
    end loop;

    -- Done
    return Res.Image;

  end Normalize_Path;

   -- Get full path of a path
  function Make_Full_Path (Path : String) return String is
  begin
    if Path = "" then
      return Normalize_Path (Get_Current & "/");
    elsif Path(Path'First) = '/' then
      -- Path is already absolute => Normalize
      return Normalize_Path (Path);
    else
      -- Path is relative, prepend current path & Normalize
      return Normalize_Path (Get_Current & "/" & Path);
    end if;
  end Make_Full_Path;

  -- File name manipulation
  Sep :  constant String := "" & Separator;
  Dot :  constant String := ".";
  -- Get dir name (path) from a complete file name (up to the last / included)
  function Dirname (File_Name : String) return String is
    I : Natural;
  begin
    I := Str_Util.Locate (File_Name, Sep, Forward => False);
    if I = 0 then
      -- No / in file name => dir name is empty
      return "";
    else
      return File_Name(File_Name'First .. I);
    end if;
  end Dirname;

  -- Get file name from a complete file name (from the last / excluded),
  --  then remove the end of it if it matches Suffix (. is not necessary)
  function Basename (File_Name : String; Suffix : String := "") return String is
    I : Natural;
    Last : constant Natural := File_Name'Last;
  begin
    I := Str_Util.Locate (File_Name, Sep, Forward => False);
    if I = 0 then
      -- No / in file name => no dir name
      return File_Name;
    elsif Suffix = ""
    or else Suffix'Length > Last - I
    or else File_Name(Last - Suffix'Length + 1 .. Last) /= Suffix then
      -- File name does not match suffix
      return File_Name(I + 1 .. Last);
    else
      -- End of file name matches suffix
      return File_Name(I + 1 .. Last - Suffix'Length);
    end if;
  end Basename;

  -- Extract the file name, then its prefix (up to the first . excluded)
  function File_Prefix (File_Name : String) return String is
    I : Natural;
    File : constant String := Basename (File_Name);
  begin
    I := Str_Util.Locate (File, Dot);
    if I = 0 then
      -- No '.', return full file name
      return File;
    else
      return File (File'First .. I - 1);
    end if;
  end File_Prefix;

  -- Extract the file name, then its suffix (from the first . included)
  function File_Suffix (File_Name : String) return String is
    I : Natural;
    File : constant String := Basename (File_Name);
  begin
    I := Str_Util.Locate (File, Dot);
    if I = 0 then
      -- No '.', return no suffix
      return "";
    else
      return File (I .. File'Last);
    end if;
  end File_Suffix;

  -- Build a complete file name
  function Build_File_Name (Dirname : String; File_Prefix, File_Suffix : in String)
           return String is
    function Build_Name return String is
    begin
      if File_Suffix = "" then
        return File_Prefix;
      else
        return File_Prefix & Dot & File_Suffix;
      end if;
    end Build_Name;
  begin
    if Dirname = "" then
      return Build_Name;
    elsif Dirname(Dirname'Last) = Separator then
      return Dirname & Build_Name;
    else
      return Dirname & Sep & Build_Name;
    end if;
  end Build_File_Name;

  -- Use Sys_Calls
  function File_Kind (File_Name : String) return File_Kind_List is
  begin
    return File_Kind_List(Sys_Calls.File_Stat (File_Name).Kind);
  exception
    when Sys_Calls.Name_Error =>
      raise Name_Error;
    when Sys_Calls.Access_Error =>
      raise Access_Error;
  end File_Kind;

end Directory;

