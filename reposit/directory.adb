with Sys_Calls, Bit_Ops;
with Day_Mng;
package body Directory is
  use System;

  subtype Dir_Str is String (1 .. 256);

  Separator : constant Character := '/';

  -- For NAME_ERROR else ACCESS_ERROR
  Enoent : constant := 2;
  -- For OPEN_ERROR else ACCESS_ERROR
  Einval : constant := 22;

  type C_Stat_Rec is record
    C_Mode : Integer;
    C_Mtime : Integer;
  end record;

  type My_Tm_T is record
    Tm_Sec  : Integer;
    Tm_Min  : Integer;
    Tm_Hour : Integer;
    Tm_Mday : Integer;
    Tm_Mon  : Integer;
    Tm_Year : Integer;
  end record;

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
    return Str & Ascii.Nul;
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

  function C_Stat (File_Name : System.Address; Stat : System.Address)
                  return Integer;
  pragma Interface(C, C_Stat);
  pragma Interface_Name(C_Stat, "file_stat");

  -- type FILE_KIND_LIST is (FILE, DIR, DEVICE, FIFO_SOCKET);
  procedure File_Stat (File_Name : in String;
                       Kind       : out File_Kind_List;
                       Rights     : out Natural;
                       Modif_Time : out Time_T) is
    C_File_Name : constant String := Str_For_C (File_Name);
    Stat : C_Stat_Rec;
    Res : Integer;
    Mode : Integer;
    use Bit_Ops;
  begin
    Res := C_Stat(C_File_Name(C_File_Name'First)'Address, Stat'Address);
    if Res = -1 then
      if Sys_Calls.Errno = Enoent then
        raise Name_Error;
      else
        raise Access_Error;
      end if;
    end if;
    Mode := Integer(Stat.C_Mode) And 8#00170000#;
    Mode := Shr (Mode, 12);
    case Mode is
      when 8#14# =>
        Kind := Socket;
      when 8#12# =>
        Kind := Link;
      when 8#10# =>
        Kind := File;
      when 8#06# =>
        Kind := Block_Device;
      when 8#04# =>
        Kind := Dir;
      when 8#02# =>
        Kind := Character_Device;
      when 8#01# =>
        Kind := Pipe;
      when others =>
        Kind := Unknown;
    end case;
    Rights := Integer(Stat.C_Mode) And 8#00007777#;
    Modif_Time := Time_T(Stat.C_Mtime);
  end File_Stat;

  function C_Time_To_Tm (Time_P : System.Address;
                         My_Tm_P : System.Address)
           return Integer;
  pragma Interface(C, C_Time_To_Tm);
  pragma Interface_Name(C_Time_To_Tm, "time_to_tm");

  function Time_Of (Time : Time_T) return Calendar.Time is
    My_Tm  : My_Tm_T;
    Result : Integer;
  begin
    Result := C_Time_To_Tm (Time'Address, My_Tm'Address);
    if Result /= 0 then
      raise Constraint_Error;
    end if;
    return Calendar.Time_Of(
      My_Tm.Tm_Year, My_Tm.Tm_Mon, My_Tm.Tm_Mday,
      Day_Mng.Pack (My_Tm.Tm_Hour, My_Tm.Tm_Min, My_Tm.Tm_Sec, 0));
  end Time_Of;

  

  function C_Readlink (Path : System.Address;
                       Buf : System.Address; Bufsiz : Integer) return Integer;
  pragma Interface(C, C_Readlink);
  pragma Interface_Name(C_Readlink, "readlink");

  -- May raise NAME_ERROR if FILE_NAME does not exist
  --           OPEN_ERROR if FILE_NAME is not a link
  function Read_One_Link (File_Name : String) return String is
    Str : String(1 .. Max_Dir_Name_Len);
    C_File_Name : constant String := Str_For_C(File_Name);
    Res : Integer;
  begin
    Res := C_Readlink (C_File_Name'Address, Str'Address, Str'Length);
    if Res /= -1 then
      return Str (1 .. Res);
    elsif Sys_Calls.Errno = Enoent then
      -- ENOENT : file not found
      raise Name_Error;
    elsif Sys_Calls.Errno = Einval then
      -- EINVAL : not a symbolic link
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
    Kind : File_Kind_List;
    Rights : Natural;
    Mtime  : Time_T;
    use Text_Handler;
  begin
    -- Check file_name  is a link
    File_Stat (File_Name, Kind, Rights, Mtime);
    if Kind /= Link then
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
      
      File_Stat(Text_Handler.Value(Txt), Kind, Rights, Mtime);
      exit when Kind /= Link;
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
end Directory;


