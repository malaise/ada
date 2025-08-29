-- Management of directory and file names
private with System;
private with Smart_Reference;
with As.U, Sys_Calls;
package Directory is

  Max_Dir_Name_Len : constant := 1024;

  -- Returns current working directory
  -- May raise Constraint_Error (name too long), Name_Error or Access_Error
  function Get_Current return String;
  procedure Get_Current (Cur_Dir : in out As.U.Asu_Us);

  -- Changes current working directory
  -- May raise Name_Error or Access_Error
  procedure Change_Current (New_Dir : in String);

  -- Create a directory
  -- May raise Name_Error or Access_Error
  procedure Create (New_Dir : in String);

  -- Remove a directory
  -- May raise Name_Error or Access_Error
  procedure Remove (New_Dir : in String);

  type Dir_Desc is tagged private;

  -- Opens a directory for list of entries
  -- May raise Open_Error if dir desc is already open
  -- May raise Name_Error if not found
  -- May raise Access_Error
  function Open (Dir_Name : in String) return Dir_Desc;
  procedure Open (Desc : in out Dir_Desc; Dir_Name : in String);

  -- Gets next entry of the opened directory
  -- May raise Open_Error if dir desc is not open
  -- Will raise End_Error if no more entry
  -- May raise Access_Error
  function Next_Entry (Desc : Dir_Desc) return String;
  procedure Next_Entry (Desc : in Dir_Desc; Dir_Entry : in out As.U.Asu_Us);

  -- Reset entries for the first
  -- May raise Open_Error if dir desc is not open
  procedure Rewind (Desc : in Dir_Desc);

  -- Closes a directory
  -- May raise Open_Error if dir desc is not open
  procedure Close (Desc : in out Dir_Desc);

  -- File kind
  -- May raise Name_Error or Access_Error
  type File_Kind_List is new Sys_Calls.File_Kind_List;
  function File_Kind (File_Name : String) return File_Kind_List;

  -- Is it a file, a directory, a symbolic link
  -- May raise Name_Error or Access_Error
  function Is_File (File_Name : String) return Boolean;
  function Is_Dir  (File_Name : String) return Boolean;
  function Is_Link (File_Name : String) return Boolean;

  -- Read the target of a symbolic link
  --  if Recursive then follow the successive links
  -- Target becomes an absolute path as soon as File_Name or one followed link
  --  contains a path
  -- May raise Name_Error if File_Name does not exist
  --           Access_Error if File_Name cannot be read
  --           Open_Error if File_Name is not a link
  --           Recursive_Link if Recursive and if links points on itself
  --            (indirectly or not)
  function Read_Link (File_Name : String; Recursive : Boolean := True)
                      return String;
  procedure Read_Link (File_Name : in String;
                       Target : out As.U.Asu_Us;
                       Recursive : in Boolean := True);

  -- Recursively follow links until sucess or error
  -- Set Target on the final or faulty element
  type Link_Result is (Link_Ok, Link_Name, Link_Access, Link_Open,
                       Link_Recursive);
  function Scan_Link (File_Name : in String;
                      Target : out As.U.Asu_Us) return Link_Result;

  -- Does file name match a pattern
  -- May raise Syntax_Error
  function File_Match (File_Name : String; Template : String) return Boolean;

  -- Normalize a path: remove "name/..", "./" ....
  function Normalize_Path (Path : String) return String;

  -- Get full path of a path
  function Make_Full_Path (Path : String) return String;

  -- If Path Dirname is Ref (or current dir if Path is empty)
  --  then strip the path
  function Reduce_Path (Path : String; Ref : String := "") return String;

  -- Get dir name (path) from a complete file name (up to the last / included)
  -- If Strip then remove the trailing '/' except on "/"
  function Dirname (File_Name : String; Strip : Boolean := False) return String;
  -- Get file name from a complete file name (from the last / excluded),
  --  then remove the end of it if it matches Suffix (. is not necessary)
  function Basename (File_Name : String; Suffix : String := "") return String;
  -- Extract the file name, then its prefix (up to the last . excluded)
  function File_Prefix (File_Name : String) return String;
  -- Extract the file name, then its suffix (from the last . included)
  function File_Suffix (File_Name : String) return String;

  -- Build a complete file name
  -- [ Dirname / ] File_Prefix [ . File_Suffix]
  function Build_File_Name (Dirname : String;
                            File_Prefix, File_Suffix : in String)
           return String;

  -- Transform '\' sequences "\n", "\b", "\t", "\\",
  --   \xyz" (octal) into bytes
  function To_Bytes (Str : String) return String;
  -- and reverse
  function To_Sequence (Str : String) return String;

  -- Exceptions
  Name_Error     : exception renames Sys_Calls.Name_Error;
  Open_Error     : exception;
  Access_Error   : exception renames Sys_Calls.Access_Error;
  End_Error      : exception;
  Syntax_Error   : exception;
  Recursive_Link : exception;

private

  type Dir_Rec is record
    Dir_Addr : System.Address := System.Null_Address;
  end record;

  procedure Set (Dest : out Dir_Rec; Val : in Dir_Rec);
  procedure Finalize (Dest : in out Dir_Rec);
  package Smart_Desc_Mng is new Smart_Reference (
    Object => Dir_Rec, Set => Set, Finalize => Finalize);

  type Dir_Desc is new Smart_Desc_Mng.Handle with null record;

end Directory;

