with Ada.Strings.Unbounded, Ada.Finalization;
with Sys_Calls, Text_Line;
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
  function Get (File : File_Type) return Character;
  procedure Get (File : in File_Type; Char : out Character);

  -- Unget a char so that it will be the next got
  -- May raise Status_Error if File is not open
  procedure Unget (File : in File_Type; Char : in Character);

  -- Returns if the end of file is reached
  -- May raise Status_Error if File is not open
  function End_Of_File (File : in File_Type) return Boolean;

  Status_Error : exception;
  End_Error : exception;
  Io_Error : exception;

private
  type File_Type_Rec is record
    Line_File : Text_Line.File_Type;

    -- Line got from Text_Line file and index in Line_Got of last char got
    -- If Get_Index is not 0 and Line_Got is empty, this means
    --  that end of Text_Line file has been reached
    -- If Get_Index is 0 and Line_Got is empty, this means that
    --  a new line must be read from Text_Line file
    -- If Line_Got is not empty, then Line_Got(Get_Index+1) can be read,
    --  there is never Get_Index = Length(Line_Got), except 0
    Line_Got : Ada.Strings.Unbounded.Unbounded_String;
    Get_Index : Natural := 0;

    -- Ungot chars appended one after the other
    Ungot_Chars : Ada.Strings.Unbounded.Unbounded_String;
  end record;

  type Rec_Access is access File_Type_Rec;
  type File_Type is limited new Ada.Finalization.Limited_Controlled with record
    Acc : Rec_Access;
  end record;

  overriding procedure Finalize (File : in out File_Type);

end Text_Char;

