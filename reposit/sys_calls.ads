with System;
with Ada.Calendar;
with C_Types;
package Sys_Calls is

  -- Call system
  function Call_System (Command : String) return Integer;

  -- Unlink a file
  function Unlink (File_Name : String) return Boolean;

  -- Rename/move a file (same filesystem)
  function Rename (Src, Dest : String) return Boolean;

  -- Make a hard or symbolic link: New_Path will point to Old_Path.
  -- Raises Name_Error if New_Path already exists
  -- Raises Access_Error if other error
  procedure Link (Old_Path, New_Path : String; Hard : Boolean);

  -- Errno and associated string
  function Errno return Integer;
  function Str_Error (Err : Integer) return String;

  -- Put line on stdout or stderr
  procedure Put_Output (Str : in String);
  procedure Put_Line_Output (Str : in String);
  procedure New_Line_Output;
  procedure Flush_Output;
  procedure Put_Error (Str : in String);
  procedure Put_Line_Error (Str : in String);
  procedure New_Line_Error;
  procedure Flush_Error;

  -- Basic getenv, raises Env_Not_Set
  function Getenv (Env_Name : String) return String;
  Env_Not_Set : exception;
  -- Getenv and truncates if necessary
  procedure Getenv (Env_Name : in String;
                    Env_Set   : out Boolean;
                    Env_Trunc : out Boolean;
                    Env_Value : out String;
                    Env_Len   : out Natural);

  -- Number of variables in env
  function Environ_Len return Natural;
  -- Nth env variable ("name=value" or "")
  function Environ_Val (Index : Positive) return String;

  -- Putenv (causes a memory leak)
  -- May raise System_Error
  procedure Putenv (Env_Name : in String; Env_Value : in String);

  -- Set exit code
  procedure Set_Exit_Code (Code : in Natural);
  -- Set ok or error exit code
  procedure Set_Ok_Exit_Code;
  procedure Set_Error_Exit_Code;

  -- Unix File Descriptor
  type File_Desc is new C_Types.Int range 0 .. C_Types.Int'Last;
  type File_Desc_Kind_List is (Tty,
    File, Dir, Link, Block_Device, Character_Device, Pipe, Socket, Unknown);
  function File_Desc_Kind (Fd : File_Desc) return File_Desc_Kind_List;
  function Stdin  return File_Desc;
  function Stdout return File_Desc;
  function Stderr return File_Desc;

  -- Result of file check
  type File_Status_List is (Found, Not_Found, Error);
  -- Check if file exists, no exception
  function File_Status (File_Name : String) return File_Status_List;
  -- Check if file exists, Access_Error if Error
  function File_Check (File_Name : String) return Boolean;
  -- Check if file exists, no exception, True if Found
  function File_Found (File_Name : String) return Boolean;


  -- File kind (not tty)
  subtype File_Kind_List is File_Desc_Kind_List range File .. Unknown;
  -- File modif time
  type Time_T is private;
  -- File Rights are :
  --  1st bit OX  2nd bit OW  3rd bit OR
  --  4th bit GX  5th bit GW  6th bit GR
  --  7th bit UX  8th bit UW  9th bit UR
  -- 10th bit ST (sticky)
  -- 11th bit GS (set GID)
  -- 12th bit US (set UID)
  subtype Size_T is Long_Integer;
  type File_Stat_Rec is record
    Kind       : File_Kind_List;
    Rights     : Natural;
    Nb_Links   : Natural;
    User_Id    : Natural;
    Group_Id   : Natural;
    Modif_Time : Time_T;
    Size       : Size_T;
  end record;

  -- File stat infos
  -- May raise Name_Error or Access_Error
  function File_Stat (File_Name : String) return File_Stat_Rec;

  -- Set file mode
  -- May raise Name_Error or Access_Error
  procedure Set_Rights (File_Name : in String; Rights : in Natural);

  -- Convert file time
  function Time_Of (Time : Time_T) return Ada.Calendar.Time;

  -- Get effective user/group Id
  function Get_Effective_User_Id return Natural;
  function Get_Effective_Group_Id return Natural;

  -- Get user name from uid and get uid and gid from user name
  -- May raise System_Error
  function Get_Name_Of_User_Id (User_Id : Natural) return String;
  type Ids_Rec is record
    User_Id  : Natural;
    Group_Id : Natural;
  end record;
  function Get_Ids_Of_User_Name (User_Name : String) return Ids_Rec;

  -- Get group name from gid and get gid from group name */
  -- May raise System_Error
  function Get_Name_Of_Group_Id (Group_Id : Natural) return String;
  function Get_Id_Of_Group_Name (Group_Name : String) return Natural;

  -- Modes for a tty. Return True if success
  type Tty_Mode_List is (
    Canonical,   -- Wait for Cr, Echo, Blocking
    No_Echo,     -- Wait for Cr, No echo, Blocking
    Asynchronous,-- No wait, Echo, Not Blocking
    Transparent  -- No wait, No echo, Not Blocking
  );
  function Set_Tty_Attr (Fd : File_Desc;
                         Tty_Mode : Tty_Mode_List) return Boolean;

  -- Blocking mode for a non tty. Return True if success
  function Set_Blocking (Fd : File_Desc; Blocking : Boolean) return Boolean;

  -- Non blocking get of a character
  -- (from Asynchronous tty or non blocking fd)
  -- If Status is Got then C is the got character
  -- Else Status = None if no character available
  --      Status = Closed if connection is closed (read -> 0)
  --      Status = Error  in case of other error
  type Get_Status_List is (Got, None, Closed, Error);
  procedure Get_Immediate (Fd : in File_Desc;
                           Status : out Get_Status_List;
                           C      : out Character);
  -- Read / write on File_Desc
  -- May raise System_Error (C read/write returned -1)
  function Read  (Fd : File_Desc; Buffer : System.Address; Nbytes : Positive)
           return Natural;
  function Write (Fd : File_Desc; Buffer : System.Address; Nbytes : Positive)
           return Natural;

  -- Open / Create
  -- Fd has CLOEXEC set
  -- May raise Name_Error or System_Error
  type File_Mode is (In_File, Inout_File, Out_File);
  function Create (Name : String) return File_Desc;
  function Open   (Name : String; Mode : File_Mode) return File_Desc;

  -- Close
  -- May raise System_Error (not open)
  procedure Close (Fd : in File_Desc);

  -- Create a pipe
  -- Fds have CLOEXEC set
  -- May raise System_Error
  procedure Pipe (Fd1, Fd2 : out File_Desc);

  -- Duplicate a file descriptor, using smallest or Start_At
  -- Fd has CLOEXEC set
  -- May raise System_Error
  function Dup (To_Copy : in File_Desc) return File_Desc;
  function Dup2 (To_Copy, Set_Fd : in File_Desc) return File_Desc;

  -- Set CLOEXEC to true on Fd
  -- May raise System_Error
  procedure Set_Cloexec (Fd : in File_Desc; On : in Boolean);

  type Pid is new Integer;

  -- Get current / parent pid
  function Get_Pid return Pid;
  function Get_Parent_Pid return Pid;

  -- Kill a process
  -- May raise System_Error
  procedure Kill (Dest_Pid : in Pid; Signal_No : in Natural);

  -- Process procreation (fork)
  -- May raise System_Error
  procedure Procreate (Child : out Boolean; Child_Pid : out Pid);

  -- Process mutation (exec)
  -- Program_name and arguments have to follow Many_Strings format
  procedure Mutate (Program : in String);

  -- When mutation failed, child process shall suicide by using this
  procedure Suicide;

  -- Process termination information
  type Death_Cause_List is (No_Dead, Exited, Signaled, Stopped);
  type Death_Rec (Cause : Death_Cause_List := No_Dead) is record
    case Cause is
      when No_Dead =>
        null;
      when Exited =>
        Exited_Pid : Pid;
        Exit_Code : Integer;
      when Signaled =>
        Signaled_Pid : Pid;
        Signal : Positive;
      when Stopped =>
        Stopped_Pid : Pid;
    end case;
  end record;
  -- May raise System_Error
  function Next_Dead return Death_Rec;

  -- Exceptions (of File_Stat, Open, Create, Link)
  Name_Error   : exception;
  Access_Error : exception;

  -- Exception (of read, write, pipe...)
  System_Error : exception;

private

  type Time_T is new C_Types.Time_T;

end Sys_Calls;

