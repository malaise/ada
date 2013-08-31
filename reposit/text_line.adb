package body Text_Line is

  -- Associate a file desc to a Txt_Line file
  -- May raise Status_Error if File is already open
  procedure Open (File : in out File_Type;
                  Mode : in File_Mode;
                  Fd : in Sys_Calls.File_Desc) is
  begin
    if Is_Open (File) then
      raise Status_Error;
    end if;
    File.Open := True;
    File.Fd := Fd;
    File.Mode := Mode;
    File.Line_Feed := As.U.Tus (Line_Feed_Str);
    File.Buffer_Len := 0;
    File.Buffer_Index := 0;
    File.Buffer := (others => Ada.Characters.Latin_1.Nul);
  end Open;

  -- Dissociate a file desc from a Txt_Line file
  -- May raise Status_Error if File is not open
  procedure Close (File : in out File_Type) is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    if File.Mode = Out_File or else File.Mode = Inout_File then
      Flush (File);
    end if;
    File.Open := False;
  exception
    when Io_Error =>
      -- May be raised by Flush, for example if close is called following
      --  an Io_Error...
      null;
  end Close;

  -- Returns if a file is open
  function Is_Open (File : File_Type) return Boolean is
  begin
    return File.Open;
  end Is_Open;

  -- Returns the associated file desc
  -- May raise Status_Error if File is not open
  function Get_Fd (File : File_Type) return Sys_Calls.File_Desc is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    return File.Fd;
  end Get_Fd;

  procedure Set_Line_Feed (File : in out File_Type; Str : in String) is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    if Str'Length > Max_Line_Feed_Len then
      raise Constraint_Error;
    end if;
    File.Line_Feed := As.U.Tus (Str);
  end Set_Line_Feed;

  function Get_Line_Feed (File : in File_Type) return String is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    return File.Line_Feed.Image;
  end Get_Line_Feed;

  -- Read next text line from File
  -- Reads characters up to a newline (that is appended)
  --  or up to the end of file.
  -- So, either the returned string ends with a Latin_1.Nl and
  --  another get can be performed,
  -- Or the strings does not end with Latin_Nl (empty or not) and
  --  the end of file has been reached.
  -- May raise Status_Error if File is not open of Out_File
  -- May raise Read_Error if IO error
  function Get (File : in out File_Type) return String is
  begin
    return Get (File).Image;
  end Get;

  -- Internal procedure that reads a buffer (or up to end of file)
  procedure Read (File : in out File_Type; Done : out Boolean) is
    Read_Len : Buffer_Index_Range;
  begin
    Read_Len := Sys_Calls.Read (
           File.Fd,
           File.Buffer(File.Buffer_Index + 1)'Address,
           Buffer_Size - File.Buffer_Index);
    Done := Read_Len = 0;
    File.Buffer_Len := File.Buffer_Len + Read_Len;
    File.Buffer_Index := 0;
  exception
    when Sys_Calls.System_Error =>
      raise Io_Error;
  end Read;


  function Get (File : in out File_Type) return As.U.Asu_Us is
    Str : As.U.Asu_Us;
    Stop_Index : Buffer_Index_Range;
    Done : Boolean;
  begin
    -- Check file is open and in read mode
    if not Is_Open (File) or else File.Mode = Out_File then
      raise Status_Error;
    end if;

    -- Specif case of no Line_Feed, read all
    if File.Line_Feed.Is_Null then
      loop
        Read (File, Done);
        -- Done when read -> 0
        exit when Done;
        -- Append read chars to Str
        Str.Append (File.Buffer(1 .. File.Buffer_Len));
        File.Buffer_Len := 0;
        File.Buffer_Index := 0;
      end loop;
      return Str;
    end if;

    -- Locate next Line_Feed
    declare
      Loc_Line_Feed : constant String := File.Line_Feed.Image;
      Loc_Line_Len : constant Natural := Loc_Line_Feed'Length;
    begin
      loop
        -- Fill buffer if needed
        if File.Buffer_Len < Loc_Line_Len then
           Read (File, Done);
           -- Done when read -> 0
           if Done then
              -- Cat remaining of buffer
              Str.Append (File.Buffer(1 .. File.Buffer_Len));
              File.Buffer_Index := 0;
              File.Buffer_Len := 0;
              exit;
           end if;
        end if;
        -- Locate next newline sequence in buffer
        Stop_Index := 0;
        for I in File.Buffer_Index + 1
              .. File.Buffer_Len - Loc_Line_Len + 1 loop
          if File.Buffer(I .. I + Loc_Line_Len - 1) = Loc_Line_Feed then
            Stop_Index := I;
            exit;
          end if;
        end loop;
        if Stop_Index /= 0 then
          -- A newline sequence is found: append it and return
          Str.Append (File.Buffer(File.Buffer_Index + 1
                                   .. Stop_Index + Loc_Line_Len - 1));
          File.Buffer_Index := Stop_Index + Loc_Line_Len - 1;
          exit;
        else
          -- No newline was found: append buffer and go on reading
          Str.Append (File.Buffer(File.Buffer_Index + 1
                                   .. File.Buffer_Len - Loc_Line_Len + 1));
          File.Buffer(1 .. Loc_Line_Len - 1) :=
              File.Buffer(File.Buffer_Len - Loc_Line_Len + 2
                           .. File.Buffer_Len);
          File.Buffer_Len := Loc_Line_Len - 1;
          File.Buffer_Index := Loc_Line_Len - 1;
        end if;
      end loop;
    end;
    -- Done
    return Str;
  end Get;

  -- Put some text in file
  -- This text will either be flushed explicitely
  --  or on close (or each N characters)
  -- May raise Status_Error if File is not open or In_File
  -- May raise Io_Error if IO error
  procedure Put (File : in out File_Type; Text : in String) is
    Tmp : Natural;
  begin
    -- Check file is open and in write mode
    if not Is_Open (File) or else File.Mode = In_File then
      raise Status_Error;
    end if;

    -- Check that there is enough room in buffer
    if Buffer_Size - File.Buffer_Len >= Text'Length then
      Tmp := File.Buffer_Len + 1;
      File.Buffer_Len := File.Buffer_Len + Text'Length;
      File.Buffer (Tmp .. File.Buffer_Len) := Text;
      return;
    end if;

    -- Need to flush the buffer
    Flush (File);
    if Buffer_Size >= Text'Length then
      -- The text can be stored in buffer
      File.Buffer_Len := Text'Length;
      File.Buffer (1 .. Text'Length) := Text;
      return;
    end if;

    -- The text is longer than the buffer, flush it
    Tmp := Sys_Calls.Write (File.Fd,
                            Text'Address,
                            Text'Length);
    if Tmp /= Text'Length then
      raise Io_Error;
    end if;
  end Put;

  -- Put_Line some text
  -- Same as Put (Ada.Characters.Latin_1.Lf)
  procedure Put_Line (File : in out File_Type; Text : in String) is
  begin
    -- Check file is open and in write mode
    if not Is_Open (File) or else File.Mode = In_File then
      raise Status_Error;
    end if;
    Put (File, Text & File.Line_Feed.Image);
  end Put_Line;

  -- Put a New_Line
  -- Same as Put_Line ("")
  procedure New_Line (File : in out File_Type) is
  begin
    -- Check file is open and in write mode
    if not Is_Open (File) or else File.Mode = In_File then
      raise Status_Error;
    end if;
    Put (File, File.Line_Feed.Image);
  end New_Line;

  -- Flush the remaining of text put on file
  -- Does nothing on a In_File file
  -- May raise Io_Error if IO error
  procedure Flush (File : in out File_Type) is
    Result : Natural;
  begin
    -- File must be open, Out_File or inout_File, and buffer not empty
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    if File.Mode = In_File or else File.Buffer_Len = 0 then
      return;
    end if;
    -- Write and reset size
    Result := Sys_Calls.Write (File.Fd,
                               File.Buffer'Address,
                               File.Buffer_Len);
    if Result /= File.Buffer_Len then
      raise Io_Error;
    end if;
    File.Buffer_Len := 0;
  exception
    when Sys_Calls.System_Error =>
      raise Io_Error;
  end Flush;

 -- If Line ends with Line_Feed_Char then delete it
  procedure Trim (Line : in out As.U.Asu_Us;
                  Line_Feed : in String := Line_Feed_Str) is
  begin
    if Line_Feed'Length = 1 then
      -- Line_Feed is a char => optim
      if Line.Length > 1 then
        Line.Delete (Line.Length, Line.Length);
      end if;
    else
      -- Line_Feed is a string => optim
      if Line.Length >= Line_Feed'Length
      and then Line.Slice (Line.Length - Line_Feed'Length + 1, Line.Length)
             = Line_Feed then
        Line.Delete (Line.Length - Line_Feed'Length + 1, Line.Length);
      end if;
    end if;
  end Trim;

  function Trim (Line : String;
                 Line_Feed : in String := Line_Feed_Str) return String is
  begin
    if Line_Feed'Length = 1 then
      -- Line_Feed is a char => optim
      if Line'Length > 1 then
        return Line(Line'First .. Line'Last - 1);
      end if;
    else
      -- Line_Feed is a string => optim
      if Line'Length >= Line_Feed'Length
      and then Line (Line'Last - Line_Feed'Length + 1 .. Line'Last)
             = Line_Feed then
        return Line(Line'First .. Line'Last - Line_Feed'Length);
      end if;
    end if;
    return Line;
  end Trim;

  -- Open the fd associated to File_Name (stdin if empty) for reading
  --  and open File to it
  procedure Open_All (File : in out File_Type;
                      Mode : in File_Mode;
                      File_Name : in String := "") is
    Fd : Sys_Calls.File_Desc;
  begin
    if Is_Open (File) then
      raise Status_Error;
    end if;
    if File_Name /= "" then
      begin
        Fd := Sys_Calls.Open (File_Name,
            (case Mode is
               when In_File    => Sys_Calls.In_File,
               when Out_File   => Sys_Calls.Out_File,
               when Inout_File => Sys_Calls.Inout_File));
      exception
        when Sys_Calls.Name_Error =>
          raise Name_Error;
        when Sys_Calls.System_Error =>
          raise Io_Error;
      end;
    else
      case Mode is
        when In_File => Fd := Sys_Calls.Stdin;
        when Out_File => Fd := Sys_Calls.Stdout;
        when Inout_File => raise Mode_Error;
      end case;
    end if;
    Open (File, Mode, Fd);
  end Open_All;

  -- Create (Mode Out_File)
  procedure Create_All (File : in out File_Type;
                        File_Name : in String) is
    Fd : Sys_Calls.File_Desc;
  begin
    if Is_Open (File) then
      raise Status_Error;
    end if;
    Fd := Sys_Calls.Create (File_Name);
    Open (File, Out_File, Fd);
  end Create_All;

  -- Close the file then the fd (if not stdin)
  -- May raise Status_Error if File is not open
  procedure Close_All (File : in out File_Type) is
    Fd : Sys_Calls.File_Desc;
    use type Sys_Calls.File_Desc;
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    Fd := File.Fd;
    Close (File);
    if Fd /= Sys_Calls.Stdin
    and then Fd /= Sys_Calls.Stdout
    and then Fd /= Sys_Calls.Stderr then
      begin
        Sys_Calls.Close (Fd);
      exception
        when others =>
          null;
      end;
    end if;
  end Close_All;

  overriding procedure Finalize (File : in out File_Type) is
  begin
    if File.Is_Open then
      Close_All (File);
    end if;
  end Finalize;

end Text_Line;

