with Interfaces.C_Streams;
with Interfaces.C.Strings;
with Ada.Command_Line;
with Day_Mng, Bit_Ops;

package body Sys_Calls is

  -- Common utilities
  function C_Strlen (S : System.Address) return Natural;
  pragma Import (C, C_Strlen, "strlen");

  function C_Strcpy (Dest, Src : System.Address) return System.Address;
  pragma Import (C, C_Strcpy, "strcpy");

  function C_Memcpy (Dest, Src : System.Address; Size : Integer)
  return System.Address;
  pragma Import (C, C_Memcpy, "memcpy");

  function Str_For_C (Str : String) return String is
  begin
    return Str & Ascii.Nul;
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
    C_Mode : Integer;
    C_Mtime : Integer;
  end record;

  function File_Kind_Of (Mode : Integer) return File_Desc_Kind_List is
    Loc_Mode : Integer;
    Kind : File_Desc_Kind_List;
    use Bit_Ops;
  begin
    Loc_Mode := Mode And 8#00170000#;
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

    function C_System (Command : System.Address) return Integer;
    pragma Import (C, C_System, "system");

  begin
    return C_System (Command4C'Address);
  end Call_System;


  -- Rename/move a file
  function Unlink (File_Name : String) return Boolean is
    File_Name4C : constant String := Str_For_C (File_Name);
    Res : Integer;

    function C_Unlink (Pathname: System.Address) return Integer;
    pragma Import (C, C_Unlink, "unlink");

  begin
    Res := C_Unlink (File_Name4C'Address);
    return Res = 0;
  end Unlink;  

  function Rename (Src, Dest : String) return Boolean is
    Src4C : constant String := Str_For_C (Src);
    Dest4C : constant String := Str_For_C (Dest);
    Res : Integer;

    function C_Rename (Oldpath, Newpath : System.Address) return Integer;
    pragma Import (C, C_Rename, "rename");
  begin
    Res := C_Rename (Src4C'Address, Dest4C'Address);
    return Res = 0;
  end Rename;


  -- Errno and associated string
  function Errno return Integer is
    function C_Get_Errno return Integer;
    pragma Import (C, C_Get_Errno, "__get_errno");
  begin
    return C_Get_Errno;
  end Errno;
   
  function Str_Error (Err : Integer) return String is

    function C_Strerror (Errnum: Integer) return Interfaces.C.Strings.Chars_Ptr;
    pragma Import (C, C_Strerror, "strerror");

  begin
    return Str_From_C (C_Strerror (Err));
  end Str_Error;


  -- Put line on stderr
  procedure Put_Error (Str : in String) is
    I : Interfaces.C_Streams.Int;
    C_Str : constant String := Str & Ascii.Nul;
  begin
    I := Interfaces.C_Streams.Fputs (C_Str'Address,
                 Interfaces.C_Streams.Stderr);
  end Put_Error;

  procedure New_Line_Error is
    I : Interfaces.C_Streams.Int;
    C_Str : constant String := Ascii.Lf & Ascii.Nul;
  begin
    I := Interfaces.C_Streams.Fputs (C_Str'Address,
                 Interfaces.C_Streams.Stderr);
  end New_Line_Error;

  procedure Put_Line_Error (Str : in String) is
  begin
    Put_Error (Str);
    New_Line_Error;
  end Put_Line_Error;


  -- Getenv and truncates if necessary
  procedure Getenv (Env_Name : in String;
                    Env_Set   : out Boolean;
                    Env_Trunc : out Boolean;
                    Env_Value : out String;
                    Env_Len   : out Natural) is
    Name4C : constant String := Str_For_C (Env_Name);
    Addr : System.Address;

    function C_Getenv (Name : in System.Address) return System.Address;
    pragma Import (C, C_Getenv, "getenv");

    use System;
  begin
    Addr := C_Getenv (Name4C'Address);
    if Addr = System.Null_Address then
      Env_Set := False;
      Env_Trunc := False;
      Env_Len := 0;
      return;
    end if;

    Env_Set := True;
    declare
      Result : String (1 .. C_Strlen(Addr));
      Dummy_Addr : System.Address;
    begin
      Dummy_Addr := C_Memcpy (Result'Address, Addr, Result'Length);
      if Result'Length <= Env_Value'Length then
        Env_Trunc := False;
        Env_Len := Result'Length;
        Env_Value (Env_Value'First .. Env_Value'First + Result'Length - 1) := Result;
      else
        Env_Trunc := True;
        Env_Len := Env_Value'Length;
        Env_Value := Result (Result'First .. Result'First + Env_Value'Length - 1);
      end if;
    end;
  end Getenv;

  -- Putenv
  function C_Putenv (Str : System.Address) return Integer;
  pragma Import (C, C_Putenv, "putenv");
  
  type Str_Ptr is access String;

  procedure Putenv (Env_Name : in String; Env_Value : in String) is
    Str4C : constant String := Env_Name & "=" & Env_Value & Ascii.Nul;
    -- Voluntary memory leak here
    Ptr : constant Str_Ptr := new String'(Str4C);
    Addr : constant System.Address
         := Ptr.all(Ptr.all'First)'Address;
  begin
    
    if C_Putenv (Addr) /= 0 then
      raise System_Error;
    end if;
  end Putenv;

  -- Set exit code
  procedure Set_Exit_Code (Code : in Natural) is
  begin
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status(Code));
  end Set_Exit_Code;

  -- Set error exit code
  procedure Set_Error_Exit_Code is
  begin
    Set_Exit_Code(1);
  end Set_Error_Exit_Code;



  -- Unix File Descriptor
  function C_Is_A_Tty (Fd : Integer) return Integer;
  pragma Import (C, C_Is_A_Tty, "isatty");

  function C_Fd_Stat (Fd : Integer; Stat : System.Address)
                  return Integer;
  pragma Interface(C, C_Fd_Stat);
  pragma Interface_Name(C_Fd_Stat, "fd_stat");

  function File_Desc_Kind (Fd : File_Desc) return File_Desc_Kind_List is
    C_Fd : constant Integer := Integer(Fd);
    C_Stat : C_Stat_Rec;
  begin
    if C_Is_A_Tty (C_Fd) = 1 then
      return Tty;
    end if;
    if C_Fd_Stat (C_Fd, C_Stat'Address) = -1 then
      return Unknown;
    end if;
    return File_Kind_Of (C_Stat.C_Mode);
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


  -- File status
  function C_File_Stat (File_Name : System.Address; Stat : System.Address)
                  return Integer;
  pragma Interface(C, C_File_Stat);
  pragma Interface_Name(C_File_Stat, "file_stat");

  Enoent : constant :=  2;

  procedure File_Stat (File_Name : in String;
                       Kind       : out File_Kind_List;
                       Rights     : out Natural;
                       Modif_Time : out Time_T) is
    C_File_Name : constant String := Str_For_C (File_Name);
    C_Stat : C_Stat_Rec;
    Res : Integer;
    use Bit_Ops;
  begin
    Res := C_File_Stat(C_File_Name'Address, C_Stat'Address);
    if Res = -1 then
      if Sys_Calls.Errno = Enoent then
        raise Name_Error;
      else
        raise Access_Error;
      end if;
    end if;
    Kind := File_Kind_Of (C_Stat.C_Mode);
    Rights := C_Stat.C_Mode And 8#00007777#;
    Modif_Time := Time_T(C_Stat.C_Mtime);
  end File_Stat;


  -- Convert file time
  function C_Time_To_Tm (Time_P : System.Address;
                         My_Tm_P : System.Address)
           return Integer;
  pragma Interface(C, C_Time_To_Tm);
  pragma Interface_Name(C_Time_To_Tm, "time_to_tm");

  type C_Tm_T is record
    Tm_Sec  : Integer;
    Tm_Min  : Integer;
    Tm_Hour : Integer;
    Tm_Mday : Integer;
    Tm_Mon  : Integer;
    Tm_Year : Integer;
  end record;

  function Time_Of (Time : Time_T) return Ada.Calendar.Time is
    C_Tm  : C_Tm_T;
    Result : Integer;
  begin
    Result := C_Time_To_Tm (Time'Address, C_Tm'Address);
    if Result /= 0 then
      raise Constraint_Error;
    end if;
    return Ada.Calendar.Time_Of(
      C_Tm.Tm_Year, C_Tm.Tm_Mon, C_Tm.Tm_Mday,
      Day_Mng.Pack (C_Tm.Tm_Hour, C_Tm.Tm_Min, C_Tm.Tm_Sec, 0));
  end Time_Of;


  -- Set mode for Stdin
  Tty_Modes_For_C : constant array (Tty_Mode_List) of Integer := (
    Canonical    => 0,
    No_Echo      => 1,
    Asynchronous => 2,
    Transparent  => 3);

  function C_Set_Tty_Attr (Fd : Integer; Mode : Integer) return Integer;
  pragma Import (C, C_Set_Tty_Attr, "set_tty_attr");

  function Set_Tty_Attr (Fd : File_Desc;
                         Tty_Mode : Tty_Mode_List) return Boolean is
  begin
    return C_Set_Tty_Attr (Integer(Fd), Tty_Modes_For_C(Tty_Mode)) = 0;
  end Set_Tty_Attr;


  -- Set blocking mode for a non tty
  function C_Set_Blocking (Fd : Integer; Blocking : Integer) return Integer;
  pragma Import (C, C_Set_Blocking, "set_blocking");

  function Set_Blocking (Fd : File_Desc; Blocking : Boolean) return Boolean is
  begin
    if Blocking then
      return C_Set_Blocking (Integer(Fd), 1) = 0;
    else
      return C_Set_Blocking (Integer(Fd), 0) = 0;
    end if;
  end Set_Blocking;


  -- Get char from stdin
  function C_Get_Immediate (Fd : Integer) return Integer;
  pragma Import (C, C_Get_Immediate, "get_immediate");

  C_Error  : constant Integer := -1;
  C_None   : constant Integer := -2;
  C_Closed : constant Integer := -3;

  procedure Get_Immediate (Fd : File_Desc;
                           Status : out Get_Status_List;
                           C      : out Character) is
    Res : Integer;
  begin
    C := Ascii.Nul;
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

  -- Read / write on File_Desc
  function C_Fd_Int_Read  (Fd     : Integer;
                           Buffer : System.Address;
                           Nbytes : Integer)
                          return Integer;
  pragma Import (C, C_Fd_Int_Read, "fd_int_read");
  function C_Fd_Int_Write (Fd     : Integer;
                           Buffer : System.Address;
                           Nbytes : Integer)
                          return Integer;
  pragma Import (C, C_Fd_Int_Write, "fd_int_write");

  function Read  (Fd : File_Desc; Buffer : System.Address; Nbytes : Positive)
           return Natural is
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
  function C_Fd_Close (Fd : Integer) return Integer;
  pragma Import (C, C_Fd_Close, "fd_close");

  procedure Close (Fd : in File_Desc) is
  begin
    if C_Fd_Close (Integer(Fd)) /= 0 then
      raise System_Error;
    end if;
  end Close;

  -- Create a pipe
  function C_Fd_Pipe (Fd1, Fd2 : in System.Address) return Integer;
  pragma Import (C, C_Fd_Pipe, "fd_pipe");

  procedure Pipe (Fd1, Fd2 : out File_Desc) is
  begin
    if C_Fd_Pipe (Fd1'Address, Fd2'Address) /= 0 then
      raise System_Error;
    end if;
  end Pipe;

  -- Get current / parent pid
  function C_Getpid return Integer;
  pragma Import (C, C_Getpid, "getpid");

  function C_Getppid return Integer;
  pragma Import (C, C_Getppid, "getppid");

  function Get_Pid return Pid is
  begin
    return Pid(C_Getpid);
  end Get_Pid;

  function Get_Parent_Pid return Pid is
  begin
    return Pid(C_Getppid);
  end Get_Parent_Pid;

  -- Process procreation (fork)
  function C_Procreate return Integer;
  pragma Import (C, C_Procreate, "procreate");

  procedure Procreate (Child : out Boolean; Child_Pid : out Pid) is
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
  procedure C_Mutate (Args : in System.Address; Len : in Integer);
  pragma Import  (C, C_Mutate, "mutate");

  procedure Mutate (Program : in String) is
    Str4C : constant String := Program & Ascii.Nul;
  begin
    C_Mutate (Str4C(Str4C'First)'Address, Str4C'Length);
  end Mutate;

  -- Process termination
  C_No_More  : constant Integer := 0;
  C_Exited   : constant Integer := 1;
  C_Signaled : constant Integer := 2;
  C_Stopped  : constant Integer := 3;
  procedure C_Next_Dead (Cause, Pid, Code : in System.Address);
  pragma Import  (C, C_Next_Dead, "next_dead");
  
  function Next_Dead return Death_Rec is
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

