with Unchecked_Deallocation;
package body Text_Char is

  procedure Free is new Unchecked_Deallocation (File_Type_Rec, Rec_Access);
  package Asu renames Ada.Strings.Unbounded;

  -- Associate a file desc to a Txt_Char file
  -- May raise Status_Error if File is already open
  procedure Open (File : in out File_Type;
                  Fd : in Sys_Calls.File_Desc) is
  begin
    if Is_Open (File) then
      raise Status_Error;
    end if;
    -- Open Text_Line file
    File.Acc := new File_Type_Rec;
    Text_Line.Open (File.Acc.Line_File, Text_Line.In_File, Fd);
  end Open;

  -- Dissociate a file desc from a Txt_Char file
  -- May raise Status_Error if File is not open
  procedure Close (File : in out File_Type) is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    -- Close Text_Line file and free file
    Text_Line.Close (File.Acc.Line_File);
    File.Acc.Line_Got := Ada.Strings.Unbounded.Null_Unbounded_String;
    File.Acc.Ungot_Chars := Ada.Strings.Unbounded.Null_Unbounded_String;
    Free (File.Acc);
    File.Acc := null;
  end Close;

  -- Returns if a file is open
  function Is_Open (File : File_Type) return Boolean is
  begin
    return File.Acc /= null;
  end Is_Open;

  -- Returns the associated file desc
  -- May raise Status_Error if File is not open
  function Get_Fd (File : File_Type) return Sys_Calls.File_Desc is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    return Text_Line.Get_Fd (File.Acc.Line_File);
  end Get_Fd;

  -- Try to get another text line
  -- This sets Get_Index to /= 0 if end of Text_Line file is reached
  procedure Read_Line (File : in File_Type) is
  begin
    -- Read next line
    File.Acc.Line_Got := Text_Line.Get (File.Acc.Line_File);
    -- Check end of file
    if Asu.Length (File.Acc.Line_Got) = 0 then
      -- End of file
      File.Acc.Get_Index := 1;
    else
      -- Ready to get chars of this line
      File.Acc.Get_Index := 0;
    end if;
  exception
    when Text_Line.Io_Error =>
      raise Io_Error;
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
      File.Acc.Get_Index := File.Acc.Get_Index + 1;
      Char := Asu.Element (File.Acc.Line_Got, File.Acc.Get_Index);
      if File.Acc.Get_Index = Len then
        -- Reset if end of read line
        File.Acc.Line_Got := Asu.Null_Unbounded_String;
        Len := 0;
        File.Acc.Get_Index := 0;
      end if;
    end Read_Char;

  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    -- Check if there are ungot chars
    Len := Asu.Length (File.Acc.Ungot_Chars);
    if Len /= 0 then
      Char := Asu.Element (File.Acc.Ungot_Chars, Len);
      -- Delete this last char
      Asu.Delete (File.Acc.Ungot_Chars, Len, Len);
      return;
    end if;
    -- Check if there are read chars to get
    Len := Asu.Length (File.Acc.Line_Got);
    if Len /= 0 then
      -- There are chars to read in Line_Got
      Read_Char;
    elsif File.Acc.Get_Index /= 0 then
      -- End of file already reached
      raise End_Error;
    else
      -- Line_Got is empty and Get_Index = 0
      -- Try to get another text line
      -- This sets Get_Index to /= 0 if end of Text_Line file is reached
      Read_Line (File);
      -- Check if end of file reached
      if File.Acc.Get_Index /= 0 then
        raise End_Error;
      end if;
      Len := Asu.Length (File.Acc.Line_Got);
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
    Asu.Append (File.Acc.Ungot_Chars, Char);
  end Unget;

  -- Returns if the end of file is reached
  -- May raise Status_Error if File is not open
  function End_Of_File (File : in File_Type) return Boolean is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    -- Check if there are ungot chars
    if Asu.Length (File.Acc.Ungot_Chars) /= 0 then
      return False;
    end if;
    -- Check if there are read chars to get
    if Asu.Length (File.Acc.Line_Got) /= 0 then
      return False;
    end if;
    -- Check if end of file was already reached
    if File.Acc.Get_Index /= 0 then
      return True;
    end if;
    -- Try to get another text line
    -- This sets Get_Index to /= 0 if end of Text_Line file is reached
    Read_Line (File);
    -- Check if end of file reached
    return File.Acc.Get_Index /= 0;
  end End_Of_File;

  overriding procedure Finalize (File : in out File_Type) is
  begin
    if Is_Open (File) then
      Close (File);
    end if;
  end Finalize;

end Text_Char;

