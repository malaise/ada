with System;
with Interfaces.C_Streams;
with Interfaces.C.Strings;
with Ada.Command_Line;

package body Sys_Calls is

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

  function Call_System (Command : String) return Integer is
    Command4C : constant String := Str_For_C (Command);

    function C_System (Command : System.Address) return Integer;
    pragma Import (C, C_System, "system");

  begin
    return C_System (Command4C'Address);
  end Call_System;

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

  procedure Set_Exit_Code (Code : in Natural) is
  begin
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status(Code));
  end Set_Exit_Code;

  -- Set error exit code
  procedure Set_Error_Exit_Code is
  begin
    Set_Exit_Code(1);
  end Set_Error_Exit_Code;

  -- Stdin
  function Stdin return File_Desc is
  begin
    return 0;
  end Stdin;

  -- Set mode for Stdin
  Modes_For_C : constant array (Stdin_Mode_List) of Integer := (
    Canonical    => 0,
    No_Echo      => 1,
    Asynchronous => 2);
  function C_Set_Stdin_Attr (Mode : Integer) return Integer;
  pragma Import (C, C_Set_Stdin_Attr, "set_stdin_attr");

  function Set_Stdin_Attr (Stdin_Mode : in Stdin_Mode_List) return Boolean is
  begin
    return C_Set_Stdin_Attr (Modes_For_C(Stdin_Mode)) = 0;
  end Set_Stdin_Attr;

  -- Get char from stdin
  function C_Get_Immediate_Stdin return Integer;
  pragma Import (C, C_Get_Immediate_Stdin, "get_immediate_stdin");
  procedure Get_Immediate_Stdin (C : out Character; Available : out Boolean) is
    Res : Integer;
  begin
    Res := C_Get_Immediate_Stdin;
    if Res = -1 then
      Available := False;
      C := Ascii.Nul;
    else
      Available := True;
      C := Character'Val(Res);
    end if;
  end Get_Immediate_Stdin;

end Sys_Calls; 

 
