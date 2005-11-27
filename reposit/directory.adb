with Ada.Characters.Latin_1;
with Sys_Calls, Bit_Ops;
with Day_Mng;
package body Directory is
  use System;

  subtype Dir_Str is String (1 .. 256);

  Separator : constant Character := '/';

  -- For Name_Error else Access_Error
  Enoent : constant := 2;
  -- For Open_Error else Access_Error
  Einval : constant := 22;

  function C_Strlen (S : System.Address) return Natural;
  pragma Interface(C, C_Strlen);
  pragma Interface_Name(C_Strlen, "strlen");

  function C_Strcpy (Dest, Src : System.Address) return System.Address;
  pragma Interface(C, C_Strcpy);
  pragma Interface_Name(C_Strcpy, "strcpy");

  function C_Memcpy (Dest, Src : System.Address; Size : Integer)
                    return System.Address;
  pragma Interface(C, C_Memcpy);
  pragma Interface_Name(C_Memcpy, "memcpy");


  function Str_For_C (Str : String) return String is
  begin
    return Str & Ada.Characters.Latin_1.Nul;
  end Str_For_C;

  function C_Getcwd (Buf : System.Address; Size : Integer) return System.Address;
  pragma Interface(C, C_Getcwd);
  pragma Interface_Name(C_Getcwd, "getcwd");

  -- Returns current working directory
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
    Len := C_Strlen (Result(Result'First)'Address);
    return Result (1 .. Len);
  end Get_Current;

  procedure Get_Current (Cur_Dir : in out Text_Handler.Text) is
  begin
    Text_Handler.Set (Cur_Dir, Get_Current);
  end Get_Current;

  function C_Chdir (Path : System.Address) return Integer;
  pragma Interface(C, C_Chdir);
  pragma Interface_Name(C_Chdir, "chdir");

  -- Changes current working directory
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

  function C_Opendir (Name : System.Address) return System.Address;
  pragma Interface(C, C_Opendir);
  pragma Interface_Name(C_Opendir, "opendir");

  -- Opens a directory for list of entries
  function Open (Dir_Name : in String) return Dir_Desc is
    C_Dir_Name : constant String := Str_For_C(Dir_Name);
    Desc : Dir_Desc;
  begin
    if Desc.Dir_Addr /= System.Null_Address then
      raise Open_Error;
    end if;
    Desc.Dir_Addr := C_Opendir (C_Dir_Name'Address);
    if Desc.Dir_Addr = System.Null_Address then
      if Sys_Calls.Errno = Enoent then
        raise Name_Error;
      else
        raise Access_Error;
      end if;
    end if;
    return Desc;
  end Open;


  function C_Readdir (Dir : System.Address; Name : System.Address) return Integer;
  pragma Interface(C, C_Readdir);
  pragma Interface_Name(C_Readdir, "read_dir");

  -- Gets next entry of the opened directory
  function Next_Entry (Desc : Dir_Desc) return String is
    Len : Integer;
    Dir_Name : Dir_Str;
  begin
    -- Check dir desc
    if Desc.Dir_Addr = System.Null_Address then
      raise Open_Error;
    end if;

    -- Read entry and check validity
    Len := C_Readdir (Desc.Dir_Addr, Dir_Name(Dir_Name'First)'Address);
    if Len = -1 then
      raise End_Error;
    end if;

    return Dir_Name(1 .. Len);
  end Next_Entry;

  procedure Next_Entry (Desc : in Dir_Desc; Dir_Entry : in out Text_Handler.Text) is
  begin
    Text_Handler.Set (Dir_Entry, Next_Entry (Desc));
  end Next_Entry;

  procedure C_Rewinddir (Dir : System.Address);
  pragma Interface(C, C_Rewinddir);
  pragma Interface_Name(C_Rewinddir, "rewinddir");

  -- Reset entries for the first
  procedure Rewind (Desc : in Dir_Desc) is
  begin
    -- Check dir desc
    if Desc.Dir_Addr = System.Null_Address then
      raise Open_Error;
    end if;
    C_Rewinddir (Desc.Dir_Addr);
  end Rewind;

  procedure C_Closedir (Dir : System.Address);
  pragma Interface(C, C_Closedir);
  pragma Interface_Name(C_Closedir, "closedir");

  -- Closes a directory
  procedure Close (Desc : in out Dir_Desc) is
  begin
    if Desc.Dir_Addr = System.Null_Address then
      raise Open_Error;
    end if;
    C_Closedir (Desc.Dir_Addr);
    Desc.Dir_Addr := System.Null_Address;
  end Close;


  -- Follow a link
  function C_Readlink (Path : System.Address;
                       Buf : System.Address; Bufsiz : Integer) return Integer;
  pragma Interface(C, C_Readlink);
  pragma Interface_Name(C_Readlink, "readlink");

  -- May raise Name_Error if File_Name does not exist
  --           Open_Error if File_Name is not a link
  function Read_One_Link (File_Name : String) return String is
    Str : String(1 .. Max_Dir_Name_Len);
    C_File_Name : constant String := Str_For_C(File_Name);
    Res : Integer;
  begin
    Res := C_Readlink (C_File_Name'Address, Str'Address, Str'Length);
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


  procedure Extract_Path (From : in String; To : in out Text_Handler.Text) is
  begin
    if From(From'First) /= Separator then
      Text_Handler.Empty(To);
      return;
    end if;
    for I in reverse From'Range loop
      if From(I) = Separator then
        Text_Handler.Set (To, From(From'First .. I - 1));
        exit;
      end if;
    end loop;
  end Extract_Path;

  function Read_Link (File_Name : String;
                      Recursive : Boolean := True) return String is
    Dir, Txt : Text_Handler.Text(Max_Dir_Name_Len);
    use Text_Handler;
    use type Sys_Calls.File_Kind_List;
  begin
    -- Check file_name  is a link
    if Sys_Calls.File_Stat (File_Name).Kind /= Sys_Calls.Link then
      raise Open_Error;
    end if;
    if not Recursive then
      return Read_One_Link(File_Name);
    end if;

    -- Prepend current dir if relative, store path
    Text_Handler.Set (Txt, File_Name);
    if Text_Handler.Value(Txt)(1) /= Separator then
      Get_Current(Dir);
      Text_Handler.Set (Txt, Dir & '/' & Txt);
    end if;
    Extract_Path(Text_Handler.Value(Txt), Dir);

    loop
      -- Current is a link
      Text_Handler.Set (Txt, Read_One_Link(Text_Handler.Value(Txt)));

      -- Prepend path if relative, store path
      if Text_Handler.Value(Txt)(1) /= Separator then
        Text_Handler.Set (Txt, Dir & '/' & Txt);
      end if;
      Extract_Path(Text_Handler.Value(Txt), Dir);

      exit when Sys_Calls.File_Stat (Text_Handler.Value(Txt)).Kind
                /= Sys_Calls.Link;
    end loop;
    return Text_Handler.Value(Txt);
  end Read_Link;


  procedure Read_Link (File_Name : in String;
                       Target : in out Text_Handler.Text;
                       Recursive : in Boolean := True) is
  begin
    Text_Handler.Set (Target, Read_Link(File_Name, Recursive));
  end Read_Link;


  function C_Fnmatch (Pattern : System.Address; Strings : System.Address; Flags : Integer)
           return Integer;
  pragma Interface(C, C_Fnmatch);
  pragma Interface_Name(C_Fnmatch, "fnmatch");

  -- Does file name match a pattern
  function File_Match (File_Name : String; Template : String) return Boolean is
    C_File_Name : constant String := Str_For_C (File_Name);
    C_Template : constant String := Str_For_C (Template);
  begin
    return C_Fnmatch(C_Template'Address, C_File_Name'Address, 0) = 0;
  end File_Match;

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


