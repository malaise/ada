with System;
with Unchecked_Conversion;

with Ada.Command_Line;
with System.Os_Interface;



package body Sys_Calls is



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

  type Str_Access is access String;

  function Str_For_C (Str : String) return String is
  begin
    return Str & Ascii.Nul;
  end Str_For_C;

  function Str_From_C (Str_Ptr : Str_Access) return String is
    Str_Len : Natural;
  begin
    Str_Len := 0;
    while Str_Ptr.All(Str_Len) /= Ascii.Nul loop
      Str_Len := Str_Len + 1;
    end loop;
    return Str_Ptr.All (1 .. Str_Len);
  end Str_From_C;

  function Call_System (Command : String) return Integer is
    Command4C : constant String := Str_For_C (Command);

    function C_System (Command : System.Address) return Integer;
    pragma Interface(C, C_System);
    pragma Interface_Name(C_System, "system");

  begin
    return C_System (Command4C'Address);
  end Call_System;

  function Unlink (File_Name : String) return Boolean is
    File_Name4C : constant String := Str_For_C (File_Name);
    Res : Integer;

    function C_Unlink (Pathname: System.Address) return Integer;
    pragma Interface(C, C_Unlink);
    pragma Interface_Name(C_Unlink, "unlink");

  begin
    Res := C_Unlink (File_Name4C'Address);
    return Res = 0;
  end Unlink;  

  function Rename (Src, Dest : String) return Boolean is
    Src4C : constant String := Str_For_C (Src);
    Dest4C : constant String := Str_For_C (Dest);
    Res : Integer;

    function C_Rename (Oldpath, Newpath : System.Address) return Integer;
    pragma Interface(C, C_Rename);
    pragma Interface_Name(C_Rename, "rename");
  begin
    Res := C_Rename (Src4C'Address, Dest4C'Address);
    return Res = 0;
  end Rename;

  function Errno return Integer is
  begin

    return Integer(System.Os_Interface.Errno);

  end Errno;
   
 
  function Str_Error (Err : Integer) return String is
    Addr : System.Address;

    function C_Strerror (Errnum: Integer) return System.Address;
    pragma Interface(C, C_Strerror);
    pragma Interface_Name(C_Strerror, "strerror");

    use System;
  begin
    Addr := C_Strerror (Err);
    if Addr = System.Null_Address then
      return "";
    end if;
    declare
      Result : String (1 .. C_Strlen(Addr));
      Dummy_Addr : System.Address;
    begin
      Dummy_Addr := C_Memcpy (Result'Address, Addr, Result'Length);
      return Result;
    end;
  end Str_Error;

  procedure Put_Error (Str : in System.Address);
  pragma Interface(C, Put_Error);
  pragma Interface_Name(Put_Error, "put_error");

  procedure Put_Error (Str : in String) is
    C_Str : constant String := Str & Ascii.Nul;
  begin
    Put_Error (C_Str'Address);
  end Put_Error;

  procedure New_Line_Error is
    C_Str : constant String := Ascii.Lf & Ascii.Nul;
  begin
    Put_Error (C_Str'Address);
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
    pragma Interface(C, C_Getenv);
    pragma Interface_Name(C_Getenv, "getenv");

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

  procedure Set_Error_Exit_Code is
  begin
    Set_Exit_Code(1);
  end Set_Error_Exit_Code;

  function Stdin return File_Desc is
  begin
    return 0;
  end Stdin;

end Sys_Calls; 

 
