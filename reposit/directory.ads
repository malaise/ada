with System;
with Ada.Calendar;
with Text_Handler, Sys_Calls;
package Directory is

  Max_Dir_Name_Len : constant := 1024;

  -- Returns current working directory
  function Get_Current return String;
  procedure Get_Current (Cur_Dir : in out Text_Handler.Text);

  -- Changes current working directory
  procedure Change_Current (New_Dir : in String);
  -- May raise Name_Error


  type Dir_Desc is private;

  -- Opens a directory for list of entries
  function Open (Dir_Name : in String) return Dir_Desc;
  -- May raise Open_Error if dir desc is already open
  -- May raise Name_Error if not found
  -- May raise Access_Error 

  -- Gets next entry of the opened directory
  function Next_Entry (Desc : Dir_Desc) return String;
  procedure Next_Entry (Desc : in Dir_Desc; Dir_Entry : in out Text_Handler.Text);
  -- May raise Open_Error if dir desc is not open
  -- Will raise End_Error if no more entry

  -- Reset entries for the first 
  procedure Rewind (Desc : in Dir_Desc);
  -- May raise Open_Error if dir desc is not open

  -- Closes a directory
  procedure Close (Desc : in out Dir_Desc);
  -- May raise Open_Error if dir desc is not open

  function Read_Link (File_Name : String; Recursive : Boolean := True)
                      return String;
  procedure Read_Link (File_Name : in String;
                       Target : in out Text_Handler.Text;
                       Recursive : in Boolean := True);
  -- May raise Name_Error if File_Name does not exist
  --           Access_Error if File_Name cannot be read
  --           Open_Error if File_Name is not a link

  -- Does file name match a pattern
  function File_Match (File_Name : String; Template : String) return Boolean;


  -- File status
  type File_Kind_List is new Sys_Calls.File_Kind_List;
  type Time_T is new Sys_Calls.Time_T;
  type Size_T is new Sys_Calls.Size_T range 0 .. Sys_Calls.Size_T'Last;
  procedure File_Stat (File_Name : in String;
                       Kind       : out File_Kind_List;
                       Rights     : out Natural;
                       Modif_Time : out Time_T;
                       Size       : out Size_T);


  -- Exceptions
  Name_Error   : exception renames Sys_Calls.Name_Error;
  Open_Error   : exception;
  Access_Error : exception renames Sys_Calls.Access_Error;
  End_Error    : exception;

  
private

  type Dir_Desc is record
    Dir_Addr : System.Address := System.Null_Address;
  end record;

end Directory;


