with Unchecked_Deallocation;
package body Text_Line is


  procedure Free is new Unchecked_Deallocation (File_Type_Rec, Rec_Access);

  -- Associate a file desc to a Txt_Line file
  -- May raise Status_Error if File is already open
  procedure Open (File : in out File_Type;
                  Mode : in File_Mode;
                  Fd : in Sys_Calls.File_Desc) is
  begin
    if Is_Open (File) then
      raise Status_Error;
    end if;
    File.Acc := new File_Type_Rec'(
       Fd => Fd,
       Mode => Mode,
       Line_Feed => Asu_Tus (Line_Feed_Str),
       Buffer_Len => 0,
       Buffer_Index => 0,
       Buffer => (others => Ada.Characters.Latin_1.Nul) );
  end Open;

  -- Dissociate a file desc from a Txt_Line file
  -- May raise Status_Error if File is not open
  procedure Close (File : in out File_Type) is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    if File.Acc.Mode = Out_File then
      Flush (File);
    end if;
    Free (File.Acc);
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
    if File.Acc = null then
      raise Status_Error;
    end if;
    return File.Acc.Fd;
  end Get_Fd;

  procedure Set_Line_Feed (File : in out File_Type; Str : in String) is
  begin
    if File.Acc = null then
      raise Status_Error;
    end if;
    if Str'Length > Max_Line_Feed_Len then
      raise Status_Error;
    end if;
    File.Acc.Line_Feed := Asu_Tus (Str);
  end Set_Line_Feed;

  function Get_Line_Feed (File : in File_Type) return String is
  begin
    if File.Acc = null then
      raise Status_Error;
    end if;
    return Asu_Ts (File.Acc.Line_Feed);
  end Get_Line_Feed;

  -- Read next text line from File
  -- Reads characters up to a newline (that is appended)
  --  or up to the end of file.
  -- So, either the returned string ends with a Latin_1.Nl and
  --  another get can be performed,
  -- Or the strings does not end with Latin_Nl (empty or not) and
  --  the end of file has been reached.
  -- May raise Status_Error if File is not open
  -- May raise Read_Error if IO error
  function Get (File : File_Type) return String is
  begin
    return Asu_Ts (Get (File));
  end Get;

  -- Internal procedure that reads a buffer (or up to end of file)
  procedure Read (File : in File_Type; Done : out Boolean) is
    Read_Len : Buffer_Index_Range;
  begin
    Read_Len := Sys_Calls.Read (
           File.Acc.Fd,
           File.Acc.Buffer(File.Acc.Buffer_Index + 1)'Address,
           Buffer_Size - File.Acc.Buffer_Index);
    Done := Read_Len = 0;
    File.Acc.Buffer_Len := File.Acc.Buffer_Len + Read_Len;
    File.Acc.Buffer_Index := 0;
  exception
    when Sys_Calls.System_Error =>
      raise Io_Error;
  end Read;


  function Get (File : File_Type)
                return Asu_Us is
    Str : Asu_Us;
    Stop_Index : Buffer_Index_Range;
    Done : Boolean;
  begin
    -- Check file is open and in read mode
    if File.Acc = null or else File.Acc.Mode /= In_File then
      raise Status_Error;
    end if;

    -- Specif case of no Line_Feed, read all
    if Asu_Is_Null (File.Acc.Line_Feed) then
      loop
        Read (File, Done);
        -- Done when read -> 0
        exit when Done;
        -- Append read chars to Str
        Asu.Append (Str, File.Acc.Buffer(1 .. File.Acc.Buffer_Len));
        File.Acc.Buffer_Len := 0;
        File.Acc.Buffer_Index := 0;
      end loop;
      return Str;
    end if;

    -- Locate next Line_Feed
    declare
      Loc_Line_Feed : constant String := Asu.To_String (File.Acc.Line_Feed);
      Loc_Line_Len : constant Natural := Loc_Line_Feed'Length;
    begin
      loop
        -- Fill buffer if needed
        if File.Acc.Buffer_Len < Loc_Line_Len then
           Read (File, Done);
           -- Done when read -> 0
           if Done then
              -- Cat remaining of buffer
              Asu.Append (Str, File.Acc.Buffer(1 .. File.Acc.Buffer_Len));
              File.Acc.Buffer_Index := 0;
              File.Acc.Buffer_Len := 0;
              exit;
           end if;
        end if;
        -- Locate next newline sequence in buffer
        Stop_Index := 0;
        for I in File.Acc.Buffer_Index + 1
              .. File.Acc.Buffer_Len - Loc_Line_Len + 1 loop
          if File.Acc.Buffer(I .. I + Loc_Line_Len - 1) = Loc_Line_Feed then
            Stop_Index := I;
            exit;
          end if;
        end loop;
        if Stop_Index /= 0 then
          -- A newline sequence is found: append it and return
          Asu.Append (Str, File.Acc.Buffer(File.Acc.Buffer_Index + 1
                                   .. Stop_Index + Loc_Line_Len - 1));
          File.Acc.Buffer_Index := Stop_Index + Loc_Line_Len - 1;
          exit;
        else
          -- No newline was found: append buffer and go on reading
          Asu.Append (Str, File.Acc.Buffer(File.Acc.Buffer_Index + 1
                               .. File.Acc.Buffer_Len - Loc_Line_Len + 1));
          File.Acc.Buffer(1 .. Loc_Line_Len - 1) :=
              File.Acc.Buffer(File.Acc.Buffer_Len - Loc_Line_Len + 2
                           .. File.Acc.Buffer_Len);
          File.Acc.Buffer_Len := Loc_Line_Len - 1;
          File.Acc.Buffer_Index := Loc_Line_Len - 1;
        end if;
      end loop;
    end;
    -- Done
    return Str;
  end Get;

  -- Put some text in file
  -- This text will either be flushed explicitely
  --  or on close (or each N characters)
  -- May raise Status_Error if File is not open or not Out_File
  -- May raise Io_Error if IO error
  procedure Put (File : in File_Type; Text : in String) is
    Tmp : Natural;
  begin
    -- Check file is open and in write mode
    if File.Acc = null or else File.Acc.Mode /= Out_File then
      raise Status_Error;
    end if;

    -- Check that there is enough room in buffer
    if Buffer_Size - File.Acc.Buffer_Len >= Text'Length then
      Tmp := File.Acc.Buffer_Len + 1;
      File.Acc.Buffer_Len := File.Acc.Buffer_Len + Text'Length;
      File.Acc.Buffer (Tmp .. File.Acc.Buffer_Len) := Text;
      return;
    end if;

    -- Need to flush the buffer
    Flush (File);
    if Buffer_Size >= Text'Length then
      -- The text can be stored in buffer
      File.Acc.Buffer_Len := Text'Length;
      File.Acc.Buffer (1 .. Text'Length) := Text;
      return;
    end if;

    -- The text is longer than the buffer, flush it
    Tmp := Sys_Calls.Write (File.Acc.Fd,
                            Text'Address,
                            Text'Length);
    if Tmp /= Text'Length then
      raise Io_Error;
    end if;
  end Put;

  -- Put_Line some text
  -- Same as Put (Ada.Characters.Latin_1.Lf)
  procedure Put_Line (File : in File_Type; Text : in String) is
  begin
    -- Check file is open and in write mode
    if File.Acc = null or else File.Acc.Mode /= Out_File then
      raise Status_Error;
    end if;
    Put (File, Text & Asu_Ts (File.Acc.Line_Feed));
  end Put_Line;

  -- Put a New_Line
  -- Same as Put_Line ("")
  procedure New_Line (File : in File_Type) is
  begin
    -- Check file is open and in write mode
    if File.Acc = null or else File.Acc.Mode /= Out_File then
      raise Status_Error;
    end if;
    Put (File, Asu_Ts (File.Acc.Line_Feed));
  end New_Line;

  -- Flush the remaining of text put on file
  -- Does nothing on a In_File file
  -- May raise Io_Error if IO error
  procedure Flush (File : in File_Type) is
    Result : Natural;
  begin
    -- File must be open, Out_File and buffer not empty
    if File.Acc = null then
      raise Status_Error;
    end if;
    if File.Acc.Mode /= Out_File or else File.Acc.Buffer_Len = 0 then
      return;
    end if;
    -- Write and reset size
    Result := Sys_Calls.Write (File.Acc.Fd,
                               File.Acc.Buffer'Address,
                               File.Acc.Buffer_Len);
    if Result /= File.Acc.Buffer_Len then
      raise Io_Error;
    end if;
    File.Acc.Buffer_Len := 0;
  exception
    when Sys_Calls.System_Error =>
      raise Io_Error;
  end Flush;

  overriding procedure Finalize (File : in out File_Type) is
  begin
    if Is_Open (File) then
      Close (File);
    end if;
  end Finalize;

end Text_Line;

