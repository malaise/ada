with Ada.Strings.Unbounded, Ada.Characters.Latin_1;
with Sys_Calls;
package Text_Line is

  -- The file type
  type File_Type is private;

  -- The End_Of_Line character
  New_line : constant Character := Ada.Characters.Latin_1.Lf;

  -- Associate a file desc to a Txt_Line file
  -- May raise Status_Error if File is already open
  procedure Open (File : in out File_Type; Fd : in Sys_Calls.File_Desc);

  -- Dissociate a file desc from a Txt_Line file
  -- May raise Status_Error if File is not open
  procedure Close (File : in out File_Type);

  -- Returns if a file is open
  function Is_Open (File : File_Type) return Boolean;

  -- Returns the associated file desc
  -- May raise Status_Error if File is not open
  function Get_Fd (File : File_Type) return Sys_Calls.File_Desc;

  -- Read next text line from File
  -- Reads characters up to a New_Line (that is appended)
  --  or up to the end of file.
  -- So, either the returned string ends with a New_Line and
  --   another get can be performed,
  --  Or the string does not end with New_Line (or is empty) and
  --   the end of file has been reached.
  -- Further calls after the end of file will return an empty string.
  -- May raise Status_Error if File is not open
  -- May raise Read_Error if IO error
  function Get (File : File_Type) return String;
  function Get (File : File_Type) return Ada.Strings.Unbounded.Unbounded_String;

  Status_Error : exception;
  Read_Error : exception;

private
  -- A cache of read characters
  Buffer_Size : constant := 1024;
  subtype Buffer_Index_Range is Natural range 0 .. Buffer_Size;
  subtype Buffer_Array is String (1 .. Buffer_Size);
  type File_Type_Rec is record
    Fd : Sys_Calls.File_Desc;
    Buffer_Len : Buffer_Index_Range;
    Buffer_Index : Buffer_Index_Range;
    Buffer : Buffer_Array;
  end record;

  type File_Type is access File_Type_Rec;

end Text_Line;

