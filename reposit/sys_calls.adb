with Interfaces.C.Strings;
with Ada.Characters.Latin_1;
with Day_Mng, Bit_Ops, Basic_Proc;

package body Sys_Calls is

  -- Common utilities
  C_Error  : constant Integer := -1;

  function C_Strlen (S : System.Address) return C_Types.Size_T;
  pragma Import (C, C_Strlen, "strlen");

  function C_Strcpy (Dest, Src : System.Address) return System.Address;
  pragma Import (C, C_Strcpy, "strcpy");

  function C_Memcpy (Dest, Src : System.Address; Size : C_Types.Size_T)
                    return System.Address;
  pragma Import (C, C_Memcpy, "memcpy");

  function Str_For_C (Str : String) return String is
  begin
    return Str & Ada.Characters.Latin_1.Nul;
  end Str_For_C;

  function Str_From_C (Str_Addr : Interfaces.C.Strings.Chars_Ptr) return String is
  begin
    return Interfaces.C.Strings.Value(Str_Addr);
  exception
    when Interfaces.C.Strings.Dereference_Error =>
      return "";
  end Str_From_C;


  -- File/Fd status
  type C_Stat_Rec is record
    C_Mode  : Integer;
    C_Nlink : Integer;
    C_Uid   : Integer;
    C_Gid   : Integer;
    C_Mtime : Integer;
    C_Size  : Long_Integer;
  end record;

  function File_Kind_Of (Mode : Integer) return File_Desc_Kind_List is
    Loc_Mode : Integer;
    Kind : File_Desc_Kind_List;
    use Bit_Ops;
  begin
    Loc_Mode := Mode and 8#00170000#;
    Loc_Mode := Shr (Loc_Mode, 12);
    case Loc_Mode is
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
    return Kind;
  end File_Kind_Of;


  -- Call system
  function Call_System (Command : String) return Integer is
    Command4C : constant String := Str_For_C (Command);
    function C_System (Command : System.Address) return C_Types.Int;
    pragma Import (C, C_System, "system");
  begin
    return C_System (Command4C'Address);
  end Call_System;


  -- Rename/move a file
  function Unlink (File_Name : String) return Boolean is
    function C_Unlink (Pathname: System.Address) return C_Types.Int;
    pragma Import (C, C_Unlink, "unlink");
    File_Name4C : constant String := Str_For_C (File_Name);
    Res : Integer;
  begin
    Res := C_Unlink (File_Name4C'Address);
    return Res = 0;
  end Unlink;

  function Rename (Src, Dest : String) return Boolean is
    function C_Rename (Oldpath, Newpath : System.Address) return C_Types.Int;
    pragma Import (C, C_Rename, "rename");
    Src4C : constant String := Str_For_C (Src);
    Dest4C : constant String := Str_For_C (Dest);
    Res : Integer;
  begin
    Res := C_Rename (Src4C'Address, Dest4C'Address);
    return Res = 0;
  end Rename;

  -- Make a hard or symbolic link: New_Path points to Old_Path.
  -- Raises Name_Error if New_Path already exists
  -- Raises Access_Error if other error
  Eexist : constant := 17;
  procedure Link (Old_Path, New_Path : String; Hard : Boolean) is
    function C_Hard_Link (Old_Path, New_Path : System.Address)
                         return C_Types.Int;
    pragma Import (C, C_Hard_Link, "link");
    function C_Sym_Link (Old_Path, New_Path : System.Address)
                        return C_Types.Int;
    pragma Import (C, C_Sym_Link, "symlink");
    Old4C : constant String := Old_Path & Ada.Characters.Latin_1.Nul;
    New4C : constant String := New_Path & Ada.Characters.Latin_1.Nul;
    Res : Integer;
  begin
    if Hard then
      Res := C_Hard_Link (Old4C'Address, New4C'Address);
    else
      Res := C_Sym_Link (Old4C'Address, New4C'Address);
    end if;
    if Res = 0 then
      return;
    elsif Errno = Eexist then
      raise Name_Error;
    else
      raise Access_Error;
    end if;
  end Link;

  -- Errno and associated string
  function Errno return Integer is
    function C_Get_Errno return C_Types.Int;
    pragma Import (C, C_Get_Errno, "get_errno");
  begin
    return C_Get_Errno;
  end Errno;

  function Str_Error (Err : Integer) return String is
    function C_Strerror (Errnum: C_Types.Int)
             return Interfaces.C.Strings.Chars_Ptr;
    pragma Import (C, C_Strerror, "strerror");
  begin
    return Str_From_C (C_Strerror (Err));
  end Str_Error;

  -- Put line on stdout or stderr
  procedure Put_Output (Str : in String) renames Basic_Proc.Put_Output;
  procedure Put_Output (Char : in Character) renames Basic_Proc.Put_Output;
  procedure New_Line_Output renames Basic_Proc.New_Line_Output;
  procedure Put_Line_Output (Str : in String)
            renames  Basic_Proc.Put_Line_Output;
  procedure Flush_Output renames Basic_Proc.Flush_Output;
  procedure Put_Error (Str : in String) renames Basic_Proc.Put_Error;
  procedure Put_Error (Char : in Character) renames Basic_Proc.Put_Error;
  procedure New_Line_Error renames Basic_Proc.New_Line_Error;
  procedure Put_Line_Error (Str : in String) renames Basic_Proc.Put_Line_Error;
  procedure Flush_Error renames Basic_Proc.Flush_Error;

  -- Basic getenv, raises Env_Unset
  function Getenv (Env_Name : String) return String is
    function C_Getenv (Name : in System.Address) return System.Address;
    pragma Import (C, C_Getenv, "getenv");
    use type System.Address;
    Name4C : constant String := Str_For_C (Env_Name);
    Addr : System.Address;
  begin
    Addr := C_Getenv (Name4C'Address);
    if Addr = System.Null_Address then
      raise Env_Not_Set;
    end if;
    declare
      Result : String (1 .. Integer(C_Strlen(Addr)));
      Dummy_Addr : System.Address;
      pragma Unreferenced (Dummy_Addr);
    begin
      Dummy_Addr := C_Memcpy (Result'Address, Addr, Result'Length);
      return Result;
    end;
  end Getenv;


  -- Getenv and truncates if necessary
  procedure Getenv (Env_Name : in String;
                    Env_Set   : out Boolean;
                    Env_Trunc : out Boolean;
                    Env_Value : out String;
                    Env_Len   : out Natural) is


  begin
    declare
      Str : constant String := Getenv (Env_Name);
    begin
      Env_Set := True;
      if Str'Length <= Env_Value'Length then
        Env_Trunc := False;
        Env_Len := Str'Length;
        Env_Value (Env_Value'First .. Env_Value'First + Str'Length - 1) := Str;
      else
        Env_Trunc := True;
        Env_Len := Env_Value'Length;
        Env_Value := Str (Str'First .. Str'First + Env_Value'Length - 1);
      end if;
    end;
  exception
    when Env_Not_Set =>
      Env_Set := False;
      Env_Trunc := False;
      Env_Len := 0;
  end Getenv;

  -- Cache for nb of elements of Environ_Var
  -- -1 if not known
  Loc_Env_Len : Integer := -1;

  -- Number of variables in env
  function Environ_Len return Natural is
    function C_Env_Len return C_Types.Int;
    pragma Import (C, C_Env_Len, "env_len");
  begin
    if Loc_Env_Len < 0 then
      Loc_Env_Len := C_Env_Len;
    end if;
    return Loc_Env_Len;
  end Environ_Len;

  -- Nth env variable ("name=value" or "")
  function Environ_Val (Index : Positive) return String is
    function C_Env_Val (Index : C_Types.Int) return System.Address;
    pragma Import (C, C_Env_Val, "env_val");
    Str_Addr : System.Address;
    use type System.Address;
  begin
    if Index > Environ_Len
      then return "";
    end if;
    Str_Addr := C_Env_Val (Index);
    if Str_Addr = System.Null_Address then
      return "";
    end if;
    declare
      Str : String (1 .. Integer(C_Strlen (Str_Addr)));
      Dummy_Addr :  System.Address;
      pragma Unreferenced (Dummy_Addr);
    begin
      Dummy_Addr := C_Strcpy (Str(1)'Address, Str_Addr);
      return Str;
    end;
  end Environ_Val;

  -- Setenv / Unsetenv
  procedure Setenv (Env_Name : in String; Env_Value : in String) is
    function C_Setenv (Name : System.Address; Value : System.Address;
                       Overwrite : Integer) return Integer;
    pragma Import (C, C_Setenv, "setenv");
    Name4C : constant String := Str_For_C (Env_Name);
    Value4C : constant String := Str_For_C (Env_Value);
  begin
    if C_Setenv (Name4C'Address, Value4C'Address, 1) /= 0 then
      raise System_Error;
    end if;
  end Setenv;

  procedure Unsetenv (Env_Name : in String) is
    function C_Unsetenv (Name : System.Address) return Integer;
    pragma Import (C, C_Unsetenv, "unsetenv");
    Name4C : constant String := Str_For_C (Env_Name);
  begin
    if C_Unsetenv (Name4C'Address) /= 0 then
      raise System_Error;
    end if;
  end Unsetenv;

  -- Set exit code
  procedure Set_Exit_Code (Code : in Natural) renames Basic_Proc.Set_Exit_Code;
  procedure Set_Ok_Exit_Code renames Basic_Proc.Set_Ok_Exit_Code;
  procedure Set_Error_Exit_Code renames Basic_Proc.Set_Ok_Exit_Code;

  -- Unix File Descriptor

  function File_Desc_Kind (Fd : File_Desc) return File_Desc_Kind_List is
    Fd4C : constant Integer := Integer(Fd);
    Stat4C : C_Stat_Rec;
    function C_Is_A_Tty (Fd : C_Types.Int) return C_Types.Int;
    pragma Import (C, C_Is_A_Tty, "isatty");
    function C_Fd_Stat (Fd : C_Types.Int; Stat : System.Address)
             return C_Types.Int;
    pragma Import (C, C_Fd_Stat, "fd_stat");
  begin
    if C_Is_A_Tty (Fd4C) = 1 then
      return Tty;
    end if;
    if C_Fd_Stat (Fd4C, Stat4C'Address) = -1 then
      return Unknown;
    end if;
    return File_Kind_Of (Stat4C.C_Mode);
  end File_Desc_Kind;

  function Stdin return File_Desc is
  begin
    return 0;
  end Stdin;

  function Stdout return File_Desc is
  begin
    return 1;
  end Stdout;

  function Stderr return File_Desc is
  begin
    return 2;
  end Stderr;


  -- For file exists and stat
  Enoent : constant :=  2;
  function C_File_Stat (File_Name : System.Address; Stat : System.Address)
           return C_Types.Int;
  pragma Import (C, C_File_Stat, "file_stat");

  -- Check if file exists, no exception
  function File_Status (File_Name : String) return File_Status_List is
    File_Name4C : constant String := Str_For_C (File_Name);
    Stat4C : C_Stat_Rec;
    Res : Integer;
  begin
    Res := C_File_Stat(File_Name4C'Address, Stat4C'Address);
    if Res = -1 then
      if Sys_Calls.Errno = Enoent then
        return Not_Found;
      else
        return Error;
      end if;
    else
      return Found;
    end if;
  end File_Status;

  -- Check if file exists, Access_Error if Error
  function File_Check (File_Name : String) return Boolean is
  begin
    case File_Status (File_Name) is
      when Found => return True;
      when Not_Found => return False;
      when Error => raise Access_Error;
    end case;
  end File_Check;

  -- Check if file exists, no exception, True if Found
  function File_Found (File_Name : String) return Boolean is
  begin
    return File_Status (File_Name) = Found;
  end File_Found;


  -- File stat
  function File_Stat (File_Name : String) return File_Stat_Rec is
    File_Name4C : constant String := Str_For_C (File_Name);
    Stat4C : C_Stat_Rec;
    Res : Integer;
    use Bit_Ops;
  begin
    Res := C_File_Stat(File_Name4C'Address, Stat4C'Address);
    if Res = -1 then
      if Sys_Calls.Errno = Enoent then
        raise Name_Error;
      else
        raise Access_Error;
      end if;
    end if;
    return (Kind       => File_Kind_Of (Stat4C.C_Mode),
            Rights     => Stat4C.C_Mode and 8#00007777#,
            Nb_Links   => Stat4C.C_Nlink,
            User_Id    => Stat4C.C_Uid,
            Group_Id   => Stat4C.C_Gid,
            Modif_Time => Time_T(Stat4C.C_Mtime),
            Size       => Stat4C.C_Size);
  end File_Stat;

  -- Set file mode
  procedure Set_Rights (File_Name : in String; Rights : in Natural) is
    function C_Chmod (File_Name : System.Address; Mode : C_Types.Mode_T)
             return C_Types.Int;
    pragma Import (C, C_Chmod, "chmod");
    File_Name4C : constant String := Str_For_C (File_Name);
    Res : Integer;
    use Bit_Ops;
  begin
    Res := C_Chmod (File_Name4C'Address,
                    C_Types.Uint32(Rights and 8#00007777#));
    if Res = -1 then
      if Sys_Calls.Errno = Enoent then
        raise Name_Error;
      else
        raise Access_Error;
      end if;
    end if;
  end Set_Rights;

  -- Convert file time
  type C_Tm_T is record
    Tm_Sec  : Integer;
    Tm_Min  : Integer;
    Tm_Hour : Integer;
    Tm_Mday : Integer;
    Tm_Mon  : Integer;
    Tm_Year : Integer;
  end record;

  function Time_Of (Time : Time_T) return Ada.Calendar.Time is
    Tm4C  : C_Tm_T;
    function C_Time_To_Tm (Time_P  : System.Address;
                           My_Tm_P : System.Address)
             return C_Types.Int;
    pragma Import (C, C_Time_To_Tm, "time_to_tm");
    Res : Integer;
  begin
    Res := C_Time_To_Tm (Time'Address, Tm4C'Address);
    if Res /= 0 then
      raise Constraint_Error;
    end if;
    return Ada.Calendar.Time_Of (
      Tm4C.Tm_Year, Tm4C.Tm_Mon, Tm4C.Tm_Mday,
      Day_Mng.Pack (Tm4C.Tm_Hour, Tm4C.Tm_Min, Tm4C.Tm_Sec, 0));
  end Time_Of;

  -- Get effective user/group Id
  function Get_Effective_User_Id return Natural is
    function C_Geteuid return C_Types.Uid_T;
    pragma Import (C, C_Geteuid, "geteuid");
  begin
    return Natural(C_Geteuid);
  end Get_Effective_User_Id;

  function Get_Effective_Group_Id return Natural is
    function C_Getegid return C_Types.Uid_T;
    pragma Import (C, C_Getegid, "getegid");
  begin
    return Natural(C_Getegid);
  end Get_Effective_Group_Id;

  -- Get user and group names and ids
  function Get_Name_Of_User_Id (User_Id : Natural) return String is
    Str : String (1 .. 1024);
    Res : Integer;
    function C_Get_User_Name_Of_Uid (Uid : C_Types.Int;
                                     Name : System.Address) return C_Types.Int;
    pragma Import (C, C_Get_User_Name_Of_Uid, "get_user_name_of_uid");
  begin
    Res := C_Get_User_Name_Of_Uid (User_Id, Str(Str'First)'Address);
    if Res = C_Error then
      raise System_Error;
    end if;
    return Str (1 .. Res);
  end Get_Name_Of_User_Id;

  function Get_Ids_Of_User_Name (User_Name : String) return Ids_Rec is
    User_Name4C : constant String := Str_For_C (User_Name);
    Res : Integer;
    Ids : Ids_Rec;
    function C_Get_Ids_Of_User_Name (Name : System.Address;
                                     Uid : System.Address;
                                     Gid : System.Address) return C_Types.Int;
    pragma Import (C, C_Get_Ids_Of_User_Name, "get_ids_of_user_name");
  begin
    Res := C_Get_Ids_Of_User_Name (User_Name4C'Address,
                   Ids.User_Id'Address, Ids.Group_Id'Address);
    if Res = C_Error then
      raise System_Error;
    end if;
    return Ids;
  end Get_Ids_Of_User_Name;

  function Get_Name_Of_Group_Id (Group_Id : Natural) return String is
    Str : String (1 .. 1024);
    Res : Integer;
    function C_Get_Group_Name_Of_Gid (Gid : C_Types.Int;
                                      Name : System.Address) return C_Types.Int;
    pragma Import (C, C_Get_Group_Name_Of_Gid, "get_group_name_of_gid");
  begin
    Res := C_Get_Group_Name_Of_Gid (Group_Id, Str(Str'First)'Address);
    if Res = C_Error then
      raise System_Error;
    end if;
    return Str (1 .. Res);
  end Get_Name_Of_Group_Id;

  function Get_Id_Of_Group_Name (Group_Name : String) return Natural is
    Group_Name4C : constant String := Str_For_C (Group_Name);
    Res : Integer;
    Id : Natural;
    function C_Get_Gid_Of_Group_Name (Name : System.Address;
                                     Gid : System.Address) return C_Types.Int;
    pragma Import (C, C_Get_Gid_Of_Group_Name, "get_gid_of_group_name");
  begin
    Res := C_Get_Gid_Of_Group_Name (Group_Name4C'Address, Id'Address);
    if Res = C_Error then
      raise System_Error;
    end if;
    return Id;
  end Get_Id_Of_Group_Name;

  -- Set mode for Stdin
  function Set_Tty_Attr (Fd : File_Desc;
                         Tty_Mode : Tty_Mode_List) return Boolean is
    Tty_Modes_For_C : constant array (Tty_Mode_List) of Integer := (
      Canonical    => 0,
      No_Echo      => 1,
      Char         => 2,
      Char_No_Echo => 3,
      Asynchronous => 4,
      Transparent  => 5);
    function C_Set_Tty_Attr (Fd : C_Types.Int; Mode : C_Types.Int)
                            return C_Types.Int;
    pragma Import (C, C_Set_Tty_Attr, "set_tty_attr");
  begin
    return C_Set_Tty_Attr (Integer(Fd), Tty_Modes_For_C(Tty_Mode)) = 0;
  end Set_Tty_Attr;


  -- Set blocking mode for a non tty
  function Set_Blocking (Fd : File_Desc; Blocking : Boolean) return Boolean is
    function C_Set_Blocking (Fd : C_Types.Int; Blocking : C_Types.Int)
                            return C_Types.Int;
    pragma Import (C, C_Set_Blocking, "set_blocking");
  begin
    if Blocking then
      return C_Set_Blocking (Integer(Fd), 1) = 0;
    else
      return C_Set_Blocking (Integer(Fd), 0) = 0;
    end if;
  end Set_Blocking;


  -- Get char from stdin
  procedure Get_Immediate (Fd : File_Desc;
                           Status : out Get_Status_List;
                           C      : out Character) is
    function C_Get_Immediate (Fd : C_Types.Int) return C_Types.Int;
    pragma Import (C, C_Get_Immediate, "get_immediate");
    C_None   : constant Integer := -2;
    C_Closed : constant Integer := -3;
    Res : Integer;
  begin
    C := Ada.Characters.Latin_1.Nul;
    Status := Error;
    Res := C_Get_Immediate (Integer(Fd));
    if Res >= 0 then
      Status := Got;
      C := Character'Val(Res);
    elsif Res = C_Error then
      Status := Error;
    elsif Res = C_None then
      Status := None;
    elsif Res = C_Closed then
      Status := Closed;
    else
      Status := Error;
    end if;
  end Get_Immediate;

  -- Create
  -- May raise Name_Error
  function Create (Name : String) return File_Desc is
    Name4C : constant String := Name & Ada.Characters.Latin_1.Nul;
    function C_Fd_Create (Path : System.Address) return C_Types.Int;
    pragma Import (C, C_Fd_Create, "fd_create");
    Res : Integer;
  begin
    Res := C_Fd_Create (Name4C'Address);
    if Res = -1 then
      raise Name_Error;
    end if;
    Set_Cloexec (File_Desc(Res), True);
    return File_Desc(Res);
  end Create;

  -- Open
  -- May raise Name_Error
  function Open (Name : String; Mode : File_Mode) return File_Desc is
    Name4C : constant String := Name & Ada.Characters.Latin_1.Nul;
    Modes4C : constant array (File_Mode) of Integer := (
      In_File    => 0,
      Out_File   => 1,
      Inout_File => 2);
    function C_Fd_Open (Path : System.Address; Mode : C_Types.Int)
                       return C_Types.Int;
    pragma Import (C, C_Fd_Open, "fd_open");
    Res : Integer;
  begin
    Res := C_Fd_Open (Name4C'Address, Modes4C(Mode));
    if Res = -1 then
      raise Name_Error;
    end if;
    Set_Cloexec (File_Desc(Res), True);
    return File_Desc(Res);
  end Open;

  -- Read / write on File_Desc
  function Read  (Fd : File_Desc; Buffer : System.Address; Nbytes : Positive)
           return Natural is
    function C_Fd_Int_Read  (Fd     : C_Types.Int;
                             Buffer : System.Address;
                             Nbytes : C_Types.Int)
             return C_Types.Int;
    pragma Import (C, C_Fd_Int_Read, "fd_int_read");
    Res : Integer;
  begin
    Res := C_Fd_Int_Read (Integer(Fd), Buffer, Nbytes);
    if Res >= 0 then
      return Res;
    else
      raise System_Error;
    end if;
  end Read;

  function Write (Fd : File_Desc; Buffer : System.Address; Nbytes : Positive)
           return Natural is
    function C_Fd_Int_Write (Fd     : C_Types.Int;
                             Buffer : System.Address;
                             Nbytes : C_Types.Int)
             return C_Types.Int;
    pragma Import (C, C_Fd_Int_Write, "fd_int_write");
    Res : Integer;
  begin
    Res := C_Fd_Int_Write (Integer(Fd), Buffer, Nbytes);
    if Res >= 0 then
      return Res;
    else
      raise System_Error;
    end if;
  end Write;

  -- Close
  procedure Close (Fd : in File_Desc) is
    function C_Fd_Close (Fd : C_Types.Int) return C_Types.Int;
    pragma Import (C, C_Fd_Close, "fd_close");
  begin
    if C_Fd_Close (Integer(Fd)) /= 0 then
      raise System_Error;
    end if;
  end Close;

  -- Create a pipe
  procedure Pipe (Fd1, Fd2 : out File_Desc) is
    function C_Fd_Pipe (Fd1, Fd2 : in System.Address) return C_Types.Int;
    pragma Import (C, C_Fd_Pipe, "fd_pipe");
  begin
    if C_Fd_Pipe (Fd1'Address, Fd2'Address) /= 0 then
      raise System_Error;
    end if;
    Set_Cloexec (Fd1, True);
    Set_Cloexec (Fd2, True);
  end Pipe;

  -- Duplicate a file descriptor, using smallest or Start_At
  -- May raise System_Error
  function Dup (To_Copy : in File_Desc) return File_Desc is
    function C_Dup (Oldfd : in C_Types.Int) return C_Types.Int;
    pragma Import (C, C_Dup, "dup");
    Res : Integer;
  begin
    Res := C_Dup (Integer (To_Copy));
    if Res = -1 then
      raise System_Error;
    end if;
    return File_Desc (Res);
  end Dup;

  function Dup2 (To_Copy, Set_Fd : in File_Desc) return File_Desc is
   function C_Dup2 (Oldfd : in C_Types.Int; Newfd : C_Types.Int)
                   return C_Types.Int;
    pragma Import (C, C_Dup2, "dup2");
    Res : Integer;
  begin
    Res := C_Dup2 (C_Types.Int (To_Copy), C_Types.Int (Set_Fd));
    if Res = -1 then
      raise System_Error;
    end if;
    Set_Cloexec (File_Desc(Res), True);
    return File_Desc (Res);
  end Dup2;

  -- Set CLOEXEC to true on Fd
  procedure Set_Cloexec (Fd : in File_Desc; On : in Boolean) is
    function C_Fcntl (Fd : in Integer; Cmd : in Integer; Arg : C_Types.Long)
             return C_Types.Int;
    pragma Import (C, C_Fcntl, "fcntl");
    Stat, Res : Integer;
    C_F_Getfd : constant C_Types.Int := 1;
    C_F_Setfd : constant C_Types.Int := 2;
    C_Fd_Cloexec : constant C_Types.Int := 1;
    use Bit_Ops;
  begin
    -- Get status
    Stat := C_Fcntl (Integer(Fd), C_F_Getfd, 0);
    if Stat = C_Error then
      raise System_Error;
    end if;
    -- Change status
    if On then
      Stat := Stat or C_Fd_Cloexec;
    else
      Stat := Stat and not C_Fd_Cloexec;
    end if;
    -- Update status
    Res := C_Fcntl (Integer(Fd), C_F_Setfd, C_Types.Long(Stat));
    if Res = C_Error then
      raise System_Error;
    end if;
  end Set_Cloexec;

  -- Get current / parent pid
  function Get_Pid return Pid is
    function C_Getpid return C_Types.Pid_T;
    pragma Import (C, C_Getpid, "getpid");
  begin
    return Pid(C_Getpid);
  end Get_Pid;

  function Get_Parent_Pid return Pid is
    function C_Getppid return C_Types.Pid_T;
    pragma Import (C, C_Getppid, "getppid");
  begin
    return Pid(C_Getppid);
  end Get_Parent_Pid;

  -- Kill
  procedure Kill (Dest_Pid : in Pid; Signal_No : in Kill_Signal_Range) is
    function C_Kill (Dest_Pid : C_Types.Pid_T; Signal : C_Types.Int)
             return Integer;
    pragma Import (C, C_Kill, "kill");
  begin
    if C_Kill (Integer(Dest_Pid), Signal_No) /= 0 then
      raise System_Error;
    end if;
  end Kill;

  -- Process procreation (fork)
  procedure Procreate (Child : out Boolean; Child_Pid : out Pid) is
    function C_Procreate return C_Types.Int;
    pragma Import (C, C_Procreate, "procreate");
    Res : Integer;
  begin
    Res := C_Procreate;
    if Res > 0 then
      Child := False;
      Child_Pid := Pid(Res);
    elsif Res < 0 then
      Child := True;
      Child_Pid := Pid(- Res);
    else
      raise System_Error;
    end if;
  end Procreate;

  -- Process mutation (exec)
  procedure Mutate (Program : in Many_Strings.Many_String) is
    procedure C_Mutate (Args : in System.Address; Len : in C_Types.Int);
    pragma Import  (C, C_Mutate, "mutate");
    Str4C : constant String := Program.Image & Ada.Characters.Latin_1.Nul;
  begin
    C_Mutate (Str4C'Address, Str4C'Length);
  end Mutate;

  procedure Suicide is
    Suicide_Failed : exception;
    procedure C_Exit (Status : in C_Types.Int);
    pragma Import  (C, C_Exit, "exit");
  begin
    -- Exit 1
    C_Exit (1);
    -- Should not be reached
    raise Suicide_Failed;
  end Suicide;

  -- Process termination
  C_No_More  : constant Integer := 0;
  C_Exited   : constant Integer := 1;
  C_Signaled : constant Integer := 2;
  C_Stopped  : constant Integer := 3;

  function Next_Dead return Death_Rec is
    procedure C_Next_Dead (Cause, Pid, Code : in System.Address);
    pragma Import  (C, C_Next_Dead, "next_dead");
    Cpid, Cause, Code : Integer;
  begin
    C_Next_Dead (Cause'Address, Cpid'Address, Code'Address);
    case Cause is
      when C_Error =>
        raise System_Error;
      when C_No_More =>
        return (Cause => No_Dead);
      when C_Exited =>
        return (Cause => Exited, Exited_Pid => Pid(Cpid), Exit_Code => Code);
      when C_Signaled =>
        return (Cause => Signaled, Signaled_Pid => Pid(Cpid), Signal => Code);
      when C_Stopped =>
        return (Cause => Stopped, Stopped_Pid => Pid(Cpid));
      when others =>
        raise Constraint_Error;
    end case;
  end Next_Dead;

end Sys_Calls;

