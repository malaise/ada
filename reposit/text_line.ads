-- Get lines (until Lf) of text from file
with Ada.Characters.Latin_1;
with As.U, Sys_Calls;
package Text_Line is

  -- The file type
  type File_Type is tagged limited private;

  -- The End_Of_Line sequence
  Line_Feed_Char : constant Character := Ada.Characters.Latin_1.Lf;
  Line_Feed_Str : constant String := Line_Feed_Char & "";
  Max_Line_Feed_Len : constant := 10;

  -- The File mode
  type File_Mode is (In_File, Inout_File, Out_File);

  -- Associate a file desc to a Txt_Line file
  -- May raise Status_Error if File is already open
  procedure Open (File : in out File_Type;
                  Mode : in File_Mode;
                  Fd : in Sys_Calls.File_Desc);

  -- Dissociate a file desc from a Txt_Line file
  -- May raise Status_Error if File is not open
  procedure Close (File : in out File_Type);

  -- Returns if a file is open
  function Is_Open (File : File_Type) return Boolean;

  -- Returns the associated file desc
  -- May raise Status_Error if File is not open
  function Get_Fd (File : File_Type) return Sys_Calls.File_Desc;

  -- Set and get Line_Feed sequence
  -- If Line_Feed is empty, then Get will return the complete flow or file
  -- May raise Status_Error if File is not open
  -- May raise Constraint_Error if Str is too long
  procedure Set_Line_Feed (File : in out File_Type; Str : in String);
  function Get_Line_Feed (File : File_Type) return String;

  -- Read next text line from File
  -- Reads characters up to Line_Feed (that is appended)
  --  or up to the end of file.
  -- So, either the returned string ends with Line_Feed and
  --   another get can be performed,
  --  Or the string does not end with Line_Feed (or is empty) and
  --   the end of file has been reached.
  -- Further calls after the end of file will return an empty string.
  -- May raise Status_Error if File is not open or Out_File
  -- May raise Io_Error if IO error
  function Get (File : in out File_Type) return String;
  function Get (File : in out File_Type) return As.U.Asu_Us;

  -- Put some text in file
  -- This text will either be flushed explicitely
  --  or on close (or each N characters)
  -- May raise Status_Error if File is not open or In_File
  -- May raise Io_Error if IO error
  procedure Put (File : in out File_Type; Text : in String);

  -- Put_Line some text
  -- Same as Put (Text & Line_Feed)
  procedure Put_Line (File : in out File_Type; Text : in String);

  -- Put a Line_Feed
  -- Same as Put_Line ("")
  procedure New_Line (File : in out File_Type);

  -- Flush the remaining of text put on file
  -- Does nothing on a In_File file
  -- May raise Io_Error if IO error
  procedure Flush (File : in out File_Type);

  -- If Line ends with Line_Feed_Char then delete it
  procedure Trim (Line : in out As.U.Asu_Us;
                  Line_Feed : in String := Line_Feed_Str);
  function Trim (Line : String;
                 Line_Feed : in String := Line_Feed_Str) return String;

  -- Shortcuts to open/close the fd and the file together

  -- Open the fd associated to File_Name (use stdin/stdout depending
  --  on Mode if empty File_Name) and open File to it
  -- May raise Name_Error or Io_Error if error opening File_Name
  -- May raise Status_Error if File is already open
  -- May raise Mode_Error if empty File_Name and Mode = Inout_File
  procedure Open_All (File : in out File_Type;
                      Mode : in File_Mode;
                      File_Name : in String := "");

  -- Create (Mode Out_File)
  procedure Create_All (File : in out File_Type;
                        File_Name : in String);

  -- Close the file then the fd (if not stdin/stdout/stderr)
  -- May raise Status_Error if File is not open
  procedure Close_All (File : in out File_Type);


  Name_Error : exception;
  Mode_Error : exception;
  Status_Error : exception;
  Io_Error : exception;

private
  -- A cache of read characters
  Buffer_Size : constant := 1024;
  subtype Buffer_Index_Range is Natural range 0 .. Buffer_Size;
  subtype Buffer_Array is String (1 .. Buffer_Size);
  type File_Type is tagged limited record
    Open : Boolean := False;
    Fd : Sys_Calls.File_Desc;
    Mode : File_Mode;
    Line_Feed : As.U.Asu_Us;
    Buffer_Len : Buffer_Index_Range;
    Buffer_Index : Buffer_Index_Range;
    Buffer : Buffer_Array;
  end record;

end Text_Line;

