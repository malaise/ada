with System;
with Ada.Calendar;
with Interfaces.C.Strings;
with C_Types, Many_Strings, Basic_Proc;
package Sys_Calls is

  -- Remove, rename, link a file
  ------------------------------
  -- Unlink (remove) a file, returns True if done
  function Unlink (File_Name : String) return Boolean;
  -- Unlink a file if possible, no error
  procedure Unlink (File_Name : String);

  -- Rename/move a file (same filesystem)
  function Rename (Src, Dest : String) return Boolean;

  -- Make a hard or symbolic link: New_Path will point to Old_Path.
  -- Raises Name_Error if New_Path already exists
  -- Raises Access_Error if other error
  procedure Link (Old_Path, New_Path : String; Hard : Boolean);


  -- Errno
  --------
  -- Return last Errno and associated string
  function Errno return Integer;
  function Str_Error (Err : Integer) return String;


  -- Output on stdout and stderr
  ------------------------------
  -- Put line on stdout or stderr
  Io_Error : exception renames Basic_Proc.Io_Error;
  procedure Put_Output (Str : in String);
  procedure Put_Output (Char : in Character);
  procedure Put_Line_Output (Str : in String);
  procedure New_Line_Output;
  procedure Flush_Output;
  procedure Put_Error (Str : in String);
  procedure Put_Error (Char : in Character);
  procedure Put_Line_Error (Str : in String);
  procedure New_Line_Error;
  procedure Flush_Error;


  -- C strings
  ------------
  function Str_From_C (Str_Addr : Interfaces.C.Strings.Chars_Ptr) return String;


  -- Environment
  --------------
  -- Basic getenv, raises Env_Not_Set
  Env_Not_Set : exception;
  function Getenv (Env_Name : String) return String;
  -- Getenv and truncates if necessary, no exception
  procedure Getenv (Env_Name : in String;
                    Env_Set   : out Boolean;
                    Env_Trunc : out Boolean;
                    Env_Value : out String;
                    Env_Len   : out Natural);

  -- Number of variables in env
  function Environ_Len return Natural;
  -- Nth env variable ("name=value" or "")
  function Environ_Val (Index : Positive) return String;

  -- Setenv / Unsetenv
  -- May raise System_Error
  procedure Setenv (Env_Name : in String; Env_Value : in String);
  procedure Unsetenv (Env_Name : in String);


  -- Exit code
  ------------
  -- Set exit code
  procedure Set_Exit_Code (Code : in Natural);
  -- Set ok or error exit code
  procedure Set_Ok_Exit_Code;
  procedure Set_Error_Exit_Code;


  -- Unix File Descriptor
  -----------------------
  type File_Desc is new C_Types.Int range 0 .. C_Types.Int'Last;
  -- File kind
  type File_Desc_Kind_List is (Tty,
    File, Dir, Link, Block_Device, Character_Device, Pipe, Socket, Unknown);
  function File_Desc_Kind (Fd : File_Desc) return File_Desc_Kind_List;
  function Stdin  return File_Desc;
  function Stdout return File_Desc;
  function Stderr return File_Desc;


  -- File exists?
  ---------------
  -- Result of file check
  type File_Status_List is (Found, Not_Found, Error);

  -- Check if file exists, no exception
  function File_Status (File_Name : String) return File_Status_List;
  -- Check if file exists, raises Access_Error if Error
  function File_Check (File_Name : String) return Boolean;
  -- Check if file exists, no exception, True if Found
  function File_Found (File_Name : String) return Boolean;


  -- File status
  --------------
  -- Kind of file on disk (not tty)
  subtype File_Kind_List is File_Desc_Kind_List range File .. Unknown;
  -- File modif time, in GMT time
  type Time_T is new C_Types.Time_T;
  -- File access rights are :
  --  1st bit OX  2nd bit OW  3rd bit OR
  --  4th bit GX  5th bit GW  6th bit GR
  --  7th bit UX  8th bit UW  9th bit UR
  -- 10th bit ST (sticky)
  -- 11th bit GS (set GID)
  -- 12th bit US (set UID)
  subtype Off_T is C_Types.Off_T;
  type File_Stat_Rec is record
    Kind       : File_Kind_List;
    Rights     : Natural;
    Nb_Links   : Natural;
    User_Id    : Natural;
    Group_Id   : Natural;
    Modif_Time : Time_T;
    Size       : Off_T;
  end record;

  -- File stat infos
  -- May raise Name_Error or Access_Error
  function File_Stat (File_Name : String) return File_Stat_Rec;

  -- Set file access rights
  -- May raise Name_Error or Access_Error
  procedure Set_Rights (File_Name : in String; Rights : in Natural);

  -- Time
  -------
  -- Convert Time_T time
  function Tm_To_Time (Time : Time_T) return Ada.Calendar.Time;
  function Time_To_Tm (Time : Ada.Calendar.Time) return Time_T;

  -- Get offset of local time versus a given GMT time
  -- Add this offset to a GMT time to get the corresponding local time
  Now_Time_T : constant Time_T := -1;
  function Gmt_Offset (At_Time : Time_T := Now_Time_T) return Time_T;

  -- Unix users
  -------------
  -- Get effective user/group Id of current process
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


  -- TTYs
  -------
  -- Modes for a tty. Return True if success
  type Tty_Mode_List is (
    Canonical,   -- Wait for Cr, Echo, Blocking
    No_Echo,     -- Wait for Cr, No echo, Blocking
    Char,        -- No wait, Echo, Blocking
    Char_No_Echo,-- No wait, No echo, Blocking
    Asynchronous,-- No wait, Echo, Not Blocking
    Transparent  -- No wait, No echo, Not Blocking
  );
  function Set_Tty_Attr (Fd : File_Desc;
                         Tty_Mode : Tty_Mode_List) return Boolean;

  -- Blocking mode for a non tty. Return True if success
  function Set_Blocking (Fd : File_Desc; Blocking : Boolean) return Boolean;

  -- Is blocking (for tty or not)
  -- May raise System_Error (not open)
  function Is_Blocking (Fd : File_Desc) return Boolean;

  -- Non blocking get of a character
  -- (from Asynchronous tty or non blocking fd)
  -- If Status is Got then C is the got character
  -- Else Status = None if no character available
  --      Status = Closed if connection/tty is closed (read -> 0)
  --      Status = Error  in case of other error
  type Get_Status_List is (Got, None, Closed, Error);
  procedure Get_Immediate (Fd : in File_Desc;
                           Status : out Get_Status_List;
                           C      : out Character);

  -- Fd operations
  ----------------
  -- Open / Create
  -- Fd has CLOEXEC set
  -- May raise Name_Error or System_Error
  type File_Mode is (In_File, Inout_File, Out_File);
  function Create (Name : String) return File_Desc;
  function Open   (Name : String; Mode : File_Mode) return File_Desc;

  -- Close
  -- May raise System_Error (not open)
  procedure Close (Fd : in File_Desc);

  -- Read / write on File_Desc
  -- May raise System_Error (C read/write returned -1)
  function Read  (Fd : File_Desc; Buffer : System.Address; Nbytes : Positive)
           return Natural;
  function Write (Fd : File_Desc; Buffer : System.Address; Nbytes : Positive)
           return Natural;

  -- Create a pipe
  -- Fds have CLOEXEC set
  -- May raise System_Error
  procedure Pipe (Fd1, Fd2 : out File_Desc);

  -- Duplicate the file descriptor To_Copy, using smallest Fd possible
  -- Fd has CLOEXEC set
  -- May raise System_Error
  function Dup (To_Copy : in File_Desc) return File_Desc;
  -- Same but returns Set_Fd (closing it before duplicating if necessary)
  function Dup2 (To_Copy, Set_Fd : in File_Desc) return File_Desc;

  -- Set or reset CLOEXEC on Fd
  -- May raise System_Error
  procedure Set_Cloexec (Fd : in File_Desc; On : in Boolean);


  -- Fork and Exec
  ----------------
  -- Process Id
  type Pid is new Integer;

  -- Get current / parent pid
  function Get_Pid return Pid;
  function Get_Parent_Pid return Pid;

  -- Send a signal to a process
  -- May raise System_Error
  subtype Sent_Signal_Range is Natural range 0 .. 64;
  subtype Signal_Range is Sent_Signal_Range range 1 .. Sent_Signal_Range'Last;
  procedure Send_Signal (Dest_Pid : in Pid; Signal_No : in Sent_Signal_Range);

  -- Block or unblock a signal
  procedure Allow_Signal (Signal_No : in Signal_Range; Allow : in Boolean);

  -- Process procreation (fork)
  -- May raise System_Error
  procedure Procreate (Child : out Boolean; Child_Pid : out Pid);

  -- Process mutation (exec)
  -- Program_name and arguments have to follow Many_Strings format
  procedure Mutate (Program : in Many_Strings.Many_String);

  -- When mutation failed, child process shall suicide by using this
  procedure Suicide;

  -- Process termination information, dummy No_Dead first
  type Death_Info_List is (No_Dead, Exited, Signaled, Stopped);
  subtype Death_Cause_List is Death_Info_List range Exited .. Signaled;
  type Death_Rec (Cause : Death_Info_List := No_Dead) is record
    case Cause is
      when No_Dead =>
        null;
      when Exited =>
        Exited_Pid : Pid;
        Exit_Code : Integer;
      when Signaled =>
        Signaled_Pid : Pid;
        Signal : Signal_Range;
      when Stopped =>
        Stopped_Pid : Pid;
    end case;
  end record;
  -- May raise System_Error
  function Next_Dead return Death_Rec;

  -- Call system (execute UNIX command synchronously), returns the exit code
  function Call_System (Command : String) return Integer;


  -- Exceptions
  -------------
  -- Exceptions (of File_Stat, Open, Create, Link)
  Name_Error   : exception;
  Access_Error : exception;

  -- Exception (of read, write, pipe...)
  System_Error : exception;

end Sys_Calls;

