with System;
with As.U, Sys_Calls;
package Directory is

  Max_Dir_Name_Len : constant := 1024;

  -- Returns current working directory
  function Get_Current return String;
  procedure Get_Current (Cur_Dir : in out As.U.Asu_Us);

  -- Changes current working directory
  procedure Change_Current (New_Dir : in String);
  -- May raise Name_Error or Access_Error


  type Dir_Desc is private;

  -- Opens a directory for list of entries
  function Open (Dir_Name : in String) return Dir_Desc;
  -- May raise Open_Error if dir desc is already open
  -- May raise Name_Error if not found
  -- May raise Access_Error

  -- Gets next entry of the opened directory
  function Next_Entry (Desc : Dir_Desc) return String;
  procedure Next_Entry (Desc : in Dir_Desc; Dir_Entry : in out As.U.Asu_Us);
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
                       Target : in out As.U.Asu_Us;
                       Recursive : in Boolean := True);
  -- May raise Name_Error if File_Name does not exist
  --           Access_Error if File_Name cannot be read
  --           Open_Error if File_Name is not a link
  --           Recursive_Link if Recursive and if links points on itself
  --            (indirectly or not)


  -- Does file name match a pattern
  -- May raise Syntax_Error
  function File_Match (File_Name : String; Template : String) return Boolean;

  -- Normalize a path: remove "name/..", "./" ....
  function Normalize_Path (Path : String) return String;

  -- Get full path of a path
  function Make_Full_Path (Path : String) return String;

  -- Get dir name (path) from a complete file name (up to the last / included)
  function Dirname (File_Name : String) return String;
  -- Get file name from a complete file name (from the last / excluded),
  --  then remove the end of it if it matches Suffix (. is not necessary)
  function Basename (File_Name : String; Suffix : String := "") return String;
  -- Extract the file name, then its prefix (up to the first . excluded)
  function File_Prefix (File_Name : String) return String;
  -- Extract the file name, then its suffix (from the first . included)
  function File_Suffix (File_Name : String) return String;

  -- Build a complete file name
  function Build_File_Name (Dirname : String;
                            File_Prefix, File_Suffix : in String)
           return String;

  -- File kind and complete status
  type File_Kind_List is new Sys_Calls.File_Kind_List;
  function File_Kind (File_Name : String) return File_Kind_List;

  -- Exceptions
  Name_Error     : exception renames Sys_Calls.Name_Error;
  Open_Error     : exception;
  Access_Error   : exception renames Sys_Calls.Access_Error;
  End_Error      : exception;
  Syntax_Error   : exception;
  Recursive_Link : exception;

private

  type Dir_Desc is record
    Dir_Addr : System.Address := System.Null_Address;
  end record;

end Directory;

