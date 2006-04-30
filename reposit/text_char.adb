with Dyn_Data;
package body Text_Char is

  package File_Data is new Dyn_Data (File_Type_Rec, File_Type);
  package Asu renames Ada.Strings.Unbounded;

  -- Associate a file desc to a Txt_Char file
  -- May raise Status_Error if File is already open
  procedure Open (File : in out File_Type;
                  Fd : in Sys_Calls.File_Desc) is
  begin
    if Is_Open (File) then
      raise Status_Error;
    end if;
    -- Allocate file and open Text_Line file
    File := File_Data.Allocate;
    Text_Line.Open (File.Line_File, Text_Line.In_File, Fd);
  end Open;

  -- Dissociate a file desc from a Txt_Char file
  -- May raise Status_Error if File is not open
  procedure Close (File : in out File_Type) is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    -- Close Text_Line file and free file
    Text_Line.Close (File.Line_File);
    File_Data.Free (File);
  end Close;

  -- Returns if a file is open
  function Is_Open (File : File_Type) return Boolean is
  begin
    return File /= null;
  end Is_Open;

  -- Returns the associated file desc
  -- May raise Status_Error if File is not open
  function Get_Fd (File : File_Type) return Sys_Calls.File_Desc is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    return Text_Line.Get_Fd (File.Line_File);
  end Get_Fd;

  -- Try to get another text line
  -- This sets Get_Index to /= 0 if end of Text_Line file is reached
  procedure Read_Line (File : in File_Type) is
  begin
    -- Read next line
    File.Line_Got := Text_Line.Get (File.Line_File);
    -- Check end of file
    if Asu.Length (File.Line_Got) = 0 then
      -- End of file
      File.Get_Index := 1;
    else
      -- Ready to get chars of this line
      File.Get_Index := 0;
    end if;
  end Read_Line;

  -- Read next char from File
  -- May raise Status_Error if File is not open
  -- May raise End_Error if end of file is reached
  -- May raise Io_Error if IO error
  function Get (File : File_Type) return Character is
    C : Character;
  begin
    Get (File, C);
    return C;
  end Get;

  procedure Get (File : in File_Type; Char : out Character) is
    Len : Natural;

    -- Read a char from Line_Got, update Get_Index
    procedure Read_Char is
    begin
      File.Get_Index := File.Get_Index + 1;
      Char := Asu.Element (File.Line_Got, File.Get_Index);
      if File.Get_Index = Len then
        -- Reset if end of read line
        File.Line_Got := Asu.Null_Unbounded_String;
        Len := 0;
        File.Get_Index := 0;
      end if;
    end Read_Char;

  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    -- Check if there are ungot chars
    if File.Unget_Index /= 0 then
      Char := Asu.Element (File.Ungot_Chars, File.Unget_Index);
      -- Check if this is the last char to unget
      if File.Unget_Index = Asu.Length (File.Ungot_Chars) then
        -- Reset ungot chars
        File.Ungot_Chars := Asu.Null_Unbounded_String;
        File.Unget_Index := 0;
      else
        File.Unget_Index := File.Unget_Index + 1;
      end if;
      return;
    end if;
    -- Check if there are read chars to get
    Len := Asu.Length (File.Line_Got);
    if Len /= 0 then
      -- There are chars to read in Line_Got
      Read_Char;
    elsif File.Get_Index /= 0 then
      -- End of file already reached
      raise End_Error;
    else
      -- Line_Got is empty and Get_Index = 0
      -- Try to get another text line
      -- This sets Get_Index to /= 0 if end of Text_Line file is reached
      Read_Line (File);
      -- Check if end of file reached
      if File.Get_Index /= 0 then
        raise End_Error;
      end if;
      Len := Asu.Length (File.Line_Got);
      Read_Char;
    end if;
  end Get;

  -- Unget a char so that it will be the next got
  -- May raise Status_Error if File is not open
  procedure Unget (File : in File_Type; Char : in Character) is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    -- Just append char to the string of ungot chars
    Asu.Append (File.Ungot_Chars, Char);
    -- Init Unget_Index if first char to unget
    if File.Unget_Index = 0 then
      File.Unget_Index := 1;
    end if;
  end Unget;

  -- Returns if the end of file is reached
  -- May raise Status_Error if File is not open
  function End_Of_File (File : in File_Type) return Boolean is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    -- Check if there are ungot chars
    if File.Unget_Index /= 0 then
      return False;
    end if;
    -- Check if there are read chars to get
    if Asu.Length (File.Line_Got) /= 0 then
      return False;
    end if;
    -- Check if end of file was already reached
    if File.Get_Index /= 0 then
      return True;
    end if;
    -- Try to get another text line
    -- This sets Get_Index to /= 0 if end of Text_Line file is reached
    Read_Line (File);
    -- Check if end of file reached
    return File.Get_Index /= 0;
  end End_Of_File;

end Text_Char;

