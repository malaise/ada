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

  -- Set exit code
  procedure Set_Exit_Code (Code : in Natural);
  -- Set error exit code
  procedure Set_Error_Exit_Code;

  -- Unix File Descriptor
  type File_Desc is new Natural;
  function Stdin return File_Desc;

  -- Modes for Stdin. Return True if success
  type Stdin_Mode_List is (
    Canonical,   -- Wait for CR, Echo, Blocking
    No_Echo,     -- Wait for CR, No echo, Blocking
    Asynchronous -- No wait, No echo, Not Blocking
  );
  function Set_Stdin_Attr (Stdin_Mode : in Stdin_Mode_List) return Boolean;

  -- Non blocking get of a character on stdin (in Asynchronous mode)
  procedure Get_Immediate_Stdin (C : out Character; Available : out Boolean);

end Sys_Calls; 

 
