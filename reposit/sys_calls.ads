with System;
with Ada.Calendar;
with Many_Strings;
package Sys_Calls is

  -- Call system
  function Call_System (Command : String) return Integer;

  -- Unlink a file
  function Unlink (File_Name : String) return Boolean;

  -- Rename/move a file
  function Rename (Src, Dest : String) return Boolean;

  -- Errno and associated string
  function Errno return Integer;
  function Str_Error (Err : Integer) return String;

  -- Put line on stderr
  procedure Put_Error (Str : in String);
  procedure Put_Line_Error (Str : in String);
  procedure New_Line_Error;

  -- Getenv and truncates if necessary
  procedure Getenv (Env_Name : in String;
                    Env_Set   : out Boolean;
                    Env_Trunc : out Boolean;
                    Env_Value : out String;
                    Env_Len   : out Natural);

  -- Putenv
  procedure Putenv (Env_Name : in String; Env_Value : in String);

  -- Set exit code
  procedure Set_Exit_Code (Code : in Natural);
  -- Set error exit code
  procedure Set_Error_Exit_Code;

  -- Unix File Descriptor
  type File_Desc is new Natural;
  type File_Desc_Kind_List is (Tty, 
    File, Dir, Link, Block_Device, Character_Device, Pipe, Socket, Unknown);
  function File_Desc_Kind (Fd : File_Desc) return File_Desc_Kind_List;
  function Stdin  return File_Desc;
  function Stdout return File_Desc;
  function Stderr return File_Desc;

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
  -- File status
  -- May raise Name_Error or Access_Error
  subtype Size_T is Long_Integer;
  procedure File_Stat (File_Name : in String;
                       Kind       : out File_Kind_List;
                       Rights     : out Natural;
                       Modif_Time : out Time_T;
                       Size       : out Size_T);

  -- Convert file time
  function Time_Of (Time : Time_T) return Ada.Calendar.Time;

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
  function Read  (Fd : File_Desc; Buffer : System.Address; Nbytes : Positive)
           return Natural;
  function Write (Fd : File_Desc; Buffer : System.Address; Nbytes : Positive)
           return Natural;

  -- Close
  procedure Close (Fd : in File_Desc);

  -- Create a pipe
  procedure Pipe (Fd1, Fd2 : out File_Desc);


  type Pid is new Positive;

  -- Get current / parent pid
  function Get_Pid return Pid;
  function Get_Parent_Pid return Pid;

  -- Kill a process
  procedure Kill (Dest_Pid : in Pid; Signal_No : in Natural);
 
  -- Process procreation (fork)
  procedure Procreate (Child : out Boolean; Child_Pid : out Pid);

  -- Process mutation (exec)
  -- Program_name and arguments have to follow Many_Strings format
  procedure Mutate (Program : in String);

  -- Process termination
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
  function Next_Dead return Death_Rec;

  -- Exceptions (of File_Stat)
  Name_Error   : exception;
  Access_Error : exception;

  -- Exception (of read, write, pipe...)
  System_Error : exception;

private

  type Time_T is new Integer;

end Sys_Calls; 

 
