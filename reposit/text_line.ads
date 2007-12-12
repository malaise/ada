with Ada.Strings.Unbounded, Ada.Characters.Latin_1, Ada.Finalization;
with Sys_Calls;
package Text_Line is

  -- The file type
  type File_Type is limited private;

  -- The End_Of_Line character
  Line_Feed : constant Character := Ada.Characters.Latin_1.Lf;

  -- The File mode
  type File_Mode is (In_File, Out_File);

  -- Associate a file desc to a Txt_Line file
  -- May raise Status_Error if File is already open
  procedure Open (File : in out File_Type;
                  Mode : in File_Mode;
                  Fd : in Sys_Calls.File_Desc);

  -- Dissociate a file desc from a Txt_Line file
  -- May raise Status_Error if File is not open
  -- May raise Io_Error if IO error (flushing Out_File)
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
  -- May raise Status_Error if File is not open or not In_File
  -- May raise Io_Error if IO error
  function Get (File : File_Type) return String;
  function Get (File : File_Type) return Ada.Strings.Unbounded.Unbounded_String;

  -- Put some text in file
  -- This text will either be flushed explicitely
  --  or on close (or each N characters)
  -- May raise Status_Error if File is not open or not Out_File
  -- May raise Io_Error if IO error
  procedure Put (File : in File_Type; Text : in String);

  -- Put_Line some text
  -- Same as Put (Text & New_Line)
  procedure Put_Line (File : in File_Type; Text : in String);

  -- Put a New_Line
  -- Same as Put_Line ("")
  procedure New_Line (File : in File_Type);

  -- Flush the remaining of text put on file
  -- Does nothing on a In_File file
  -- May raise Io_Error if IO error
  procedure Flush (File : in File_Type);

  Status_Error : exception;
  Io_Error : exception;

private
  -- A cache of read characters
  Buffer_Size : constant := 1024;
  subtype Buffer_Index_Range is Natural range 0 .. Buffer_Size;
  subtype Buffer_Array is String (1 .. Buffer_Size);
  type File_Type_Rec is record
    Fd : Sys_Calls.File_Desc;
    Mode : File_Mode;
    Buffer_Len : Buffer_Index_Range;
    Buffer_Index : Buffer_Index_Range;
    Buffer : Buffer_Array;
  end record;

  type Rec_Access is access File_Type_Rec;
  type File_Type is limited new Ada.Finalization.Limited_Controlled with record
    Acc : Rec_Access;
  end record;

  procedure Finalize (File : in out File_Type);

end Text_Line;

