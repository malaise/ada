with System;
with Calendar;
with Text_Handler;
package Directory is

  Max_Dir_Name_Len : constant := 1024;

  -- Returns current working directory
  function Get_Current return String;
  procedure Get_Current (Cur_Dir : in out Text_Handler.Text);

  -- Changes current working directory
  procedure Change_Current (New_Dir : in String);
  -- May raise NAME_ERROR


  type Dir_Desc is private;

  -- Opens a directory for list of entries
  function Open (Dir_Name : in String) return Dir_Desc;
  -- May raise OPEN_ERROR if dir desc is already open
  -- May raise NAME_ERROR if not found
  -- May raise ACCESS_ERROR 

  -- Gets next entry of the opened directory
  function Next_Entry (Desc : Dir_Desc) return String;
  procedure Next_Entry (Desc : in Dir_Desc; Dir_Entry : in out Text_Handler.Text);
  -- May raise OPEN_ERROR if dir desc is not open
  -- Will raise END_ERROR if no more entry

  -- Reset entries for the first 
  procedure Rewind (Desc : in Dir_Desc);
  -- May raise OPEN_ERROR if dir desc is not open

  -- Closes a directory
  procedure Close (Desc : in out Dir_Desc);
  -- May raise OPEN_ERROR if dir desc is not open

  type File_Kind_List is (File, Dir, Link,
           Block_Device, Character_Device, Pipe, Socket, Unknown);
  type Time_T is private;
  -- RIGHTS are :
  --  1st bit OX
  --  2nd bit OW
  --  3rd bit OR
  --  4th bit GX
  --  5th bit GW
  --  6th bit GR
  --  7th bit UX
  --  8th bit UW
  --  9th bit UR
  -- 10th bit ST (sticky)
  -- 11th bit GS (set GID)
  -- 12th bit US (set UID))
  procedure File_Stat (File_Name : in String;
                       Kind       : out File_Kind_List;
                       Rights     : out Natural;
                       Modif_Time : out Time_T);
  -- May raise NAME_ERROR or ACCESS_ERROR

  function Time_Of (Time : Time_T) return Calendar.Time;
  
  function Read_Link (File_Name : String; Recursive : Boolean := True)
                      return String;
  procedure Read_Link (File_Name : in String;
                       Target : in out Text_Handler.Text;
                       Recursive : in Boolean := True);
  -- May raise NAME_ERROR if FILE_NAME does not exist
  --           ACCESS_ERROR if FILE_NAME cannot be read
  --           OPEN_ERROR if FILE_NAME is not a link

  -- Does file name match a pattern
  function File_Match (File_Name : String; Template : String) return Boolean;

  Name_Error   : exception;
  Open_Error   : exception;
  Access_Error : exception;
  End_Error    : exception;

  
private

  type Dir_Desc is record
    Dir_Addr : System.Address := System.Null_Address;
  end record;

  type Time_T is new Integer;
end Directory;


