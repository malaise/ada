package body Text_Char is

  -- Associate a file desc to a Txt_Char file
  -- May raise Status_Error if File is already open
  procedure Open (File : in out File_Type;
                  Fd : in Sys_Calls.File_Desc) is
  begin
    if Is_Open (File) then
      raise Status_Error;
    end if;
    -- Open Text_Line file
    Text_Line.Open (File.Line_File, Text_Line.In_File, Fd);
    File.Line_Got.Set_Null;
    File.Get_Index := 0;
    File.Ungot_Chars.Set_Null;
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
    File.Line_Got.Set_Null;
    File.Ungot_Chars.Set_Null;
  end Close;

  -- Returns if a file is open
  function Is_Open (File : File_Type) return Boolean is
  begin
    return File.Line_File.Is_Open;
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
  procedure Read_Line (File : in out File_Type) is
  begin
    -- Read next line
    File.Line_Got := Text_Line.Get (File.Line_File);
    -- Check end of file
    File.Get_Index := (if File.Line_Got.Is_Null then 1 else 0);
  exception
    when Text_Line.Io_Error =>
      raise Io_Error;
  end Read_Line;

  -- Read next char from File
  -- May raise Status_Error if File is not open
  -- May raise End_Error if end of file is reached
  -- May raise Io_Error if IO error
  function Get (File : in out File_Type) return Character is
    C : Character;
  begin
    Get (File, C);
    return C;
  end Get;

  procedure Get (File : in out File_Type; Char : out Character) is
    Len : Natural;

    -- Read a char from Line_Got, update Get_Index
    procedure Read_Char is
    begin
      File.Get_Index := File.Get_Index + 1;
      Char := File.Line_Got.Element (File.Get_Index);
      if File.Get_Index = Len then
        -- Reset if end of read line
        File.Line_Got.Set_Null;
        Len := 0;
        File.Get_Index := 0;
      end if;
    end Read_Char;

  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    -- Check if there are ungot chars
    Len := File.Ungot_Chars.Length;
    if Len /= 0 then
      Char := File.Ungot_Chars.Element (Len);
      -- Delete this last char
      File.Ungot_Chars.Delete (Len, Len);
      return;
    end if;
    -- Check if there are read chars to get
    Len := File.Line_Got.Length;
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
      Len := File.Line_Got.Length;
      Read_Char;
    end if;
  end Get;

  -- Unget a char so that it will be the next got
  -- May raise Status_Error if File is not open
  procedure Unget (File : in out File_Type; Char : in Character) is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    -- Just append char to the string of ungot chars
    File.Ungot_Chars.Append (Char);
  end Unget;

  -- Returns if the end of file is reached
  -- May raise Status_Error if File is not open
  function End_Of_File (File : in out File_Type) return Boolean is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    -- Check if there are ungot chars
    if not File.Ungot_Chars.Is_Null then
      return False;
    end if;
    -- Check if there are read chars to get
    if not File.Line_Got.Is_Null then
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

  -- Open the fd associated to File_Name (stdin if empty) for reading
  --  and open File to it
  procedure Open_All (File : in out File_Type;
                      File_Name : in String := "") is
    Fd : Sys_Calls.File_Desc;
  begin
    if Is_Open (File) then
      raise Status_Error;
    end if;
    if File_Name /= "" then
      begin
        Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
      exception
        when Sys_Calls.Name_Error =>
          raise Name_Error;
        when Sys_Calls.System_Error =>
          raise Io_Error;
      end;
    else
      Fd := Sys_Calls.Stdin;
    end if;
    Open (File, Fd);
  end Open_All;

  -- Close the file then the fd (if not stdin)
  -- May raise Status_Error if File is not open
  procedure Close_All (File : in out File_Type) is
    Fd : Sys_Calls.File_Desc;
    use type Sys_Calls.File_Desc;
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    Fd := File.Line_File.Get_Fd;
    if Fd /= Sys_Calls.Stdin then
      begin
        Sys_Calls.Close (Fd);
      exception
        when others =>
          null;
      end;
    end if;
    Close (File);
  end Close_All;

end Text_Char;

