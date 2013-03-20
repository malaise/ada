-- Get/unget chars from a file
with As.U, Sys_Calls, Text_Line;
package Text_Char is

  -- The file type
  type File_Type is tagged limited private;

  -- Associate a file desc to a Txt_Char file
  -- May raise Status_Error if File is already open
  procedure Open (File : in out File_Type;
                  Fd : in Sys_Calls.File_Desc);

  -- Dissociate a file desc from a Txt_Char file
  -- May raise Status_Error if File is not open
  procedure Close (File : in out File_Type);

  -- Returns if a file is open
  function Is_Open (File : File_Type) return Boolean;

  -- Returns the associated file desc
  -- May raise Status_Error if File is not open
  function Get_Fd (File : File_Type) return Sys_Calls.File_Desc;

  -- Read next char from File
  -- May raise Status_Error if File is not open
  -- May raise End_Error if end of file is reached
  -- May raise Io_Error if IO error
  function Get (File : in out File_Type) return Character;
  procedure Get (File : in out File_Type; Char : out Character);

  -- Unget a char so that it will be the next got
  -- May raise Status_Error if File is not open
  procedure Unget (File : in out File_Type; Char : in Character);

  -- Returns if the end of file is reached
  -- May raise Status_Error if File is not open
  function End_Of_File (File : in out File_Type) return Boolean;

  -- Shortcuts to open/close the fd and the file together

  -- Open the fd associated to File_Name (use stdin if empty File_Name)
  --  and open File to it
  -- May raise Name_Error or Io_Error if error opening File_Name
  -- May raise Status_Error if File is already open
  procedure Open_All (File : in out File_Type;
                      File_Name : in String := "");


  -- Close the file then the fd (if not stdin)
  -- May raise Status_Error if File is not open
  procedure Close_All (File : in out File_Type);

  Name_Error : exception;
  Status_Error : exception;
  End_Error : exception;
  Io_Error : exception;

private
  type File_Type is tagged limited record
    Line_File : Text_Line.File_Type;

    -- Line got from Text_Line file and index in Line_Got of last char got
    -- If Get_Index is not 0 and Line_Got is empty, this means
    --  that end of Text_Line file has been reached
    -- If Get_Index is 0 and Line_Got is empty, this means that
    --  a new line must be read from Text_Line file
    -- If Line_Got is not empty, then Line_Got(Get_Index+1) can be read,
    --  there is never Get_Index = Length(Line_Got), except 0
    Line_Got : As.U.Asu_Us;
    Get_Index : Natural := 0;

    -- Ungot chars appended one after the other
    Ungot_Chars : As.U.Asu_Us;
  end record;

end Text_Char;


