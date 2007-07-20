with Dyn_Data;
pragma Elaborate (Dyn_Data);
package body Text_Line is

  package File_Data is new Dyn_Data (File_Type_Rec, File_Type);

  -- Associate a file desc to a Txt_Line file
  -- May raise Status_Error if File is already open
  procedure Open (File : in out File_Type;
                  Mode : in File_Mode;
                  Fd : in Sys_Calls.File_Desc) is
  begin
    if Is_Open (File) then
      raise Status_Error;
    end if;
    File := File_Data.Allocate (
      (Fd => Fd,
       Mode => Mode,
       Buffer_Len => 0,
       Buffer_Index => 0,
       Buffer => (others => Ada.Characters.Latin_1.Nul)) );
  end Open;

  -- Dissociate a file desc from a Txt_Line file
  -- May raise Status_Error if File is not open
  procedure Close (File : in out File_Type) is
  begin
    if not Is_Open (File) then
      raise Status_Error;
    end if;
    if File.Mode = Out_File then
      Flush (File);
    end if;
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
    if File = null then
      raise Status_Error;
    end if;
    return File.Fd;
  end Get_Fd;

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
    return Ada.Strings.Unbounded.To_String (Get (File));
  end Get;

  -- Internal procedure that reads a buffer (or up to end of file)
  procedure Read (File : in File_Type) is
  begin
    File.Buffer_Len := Sys_Calls.Read (File.Fd,
                                       File.Buffer'Address,
                                       Buffer_Size);
    File.Buffer_Index := 0;
  exception
    when Sys_Calls.System_Error =>
      raise Io_Error;
  end Read;


  function Get (File : File_Type)
                return Ada.Strings.Unbounded.Unbounded_String is
    package Asu renames Ada.Strings.Unbounded;
    Str : Asu.Unbounded_String;
    Stop_Index : Buffer_Index_Range;
  begin
    -- Check file is open and in read mode
    if File = null or else File.Mode /= In_File then
      raise Status_Error;
    end if;
    -- Locate next newline
    loop
      -- Fill buffer if needed
      if File.Buffer_Len = 0 then
         Read (File);
         exit when File.Buffer_Len = 0;
      end if;
      -- Locate next newline in buffer
      Stop_Index := 0;
      for I in File.Buffer_Index + 1 .. File.Buffer_Len loop
        if File.Buffer(I) = Line_Feed then
          Stop_Index := I;
          exit;
        end if;
      end loop;
      if Stop_Index /= 0 then
        -- A newline is found: append it and return
        Asu.Append (Str, File.Buffer(File.Buffer_Index + 1 .. Stop_Index));
        File.Buffer_Index := Stop_Index;
        exit;
      else
        -- No newline was found: append buffer and go on reading
        Asu.Append (Str, File.Buffer(File.Buffer_Index + 1 .. File.Buffer_Len));
        File.Buffer_Len := 0;
      end if;
    end loop;
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
    if File = null or else File.Mode /= Out_File then
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
  procedure Put_Line (File : in File_Type; Text : in String) is
  begin
    Put (File, Text & Line_Feed);
  end Put_Line;

  -- Put a New_Line
  -- Same as Put_Line ("")
  procedure New_Line (File : in File_Type) is
  begin
    Put (File, "" & Line_Feed);
  end New_Line;


  -- Flush the remaining of text put on file
  -- Does nothing on a In_File file
  -- May raise Io_Error if IO error
  procedure Flush (File : in File_Type) is
    Result : Natural;
  begin
    -- File must be open, Out_File and buffer not empty
    if File = null then
      raise Status_Error;
    end if;
    if File.Mode /= Out_File or else File.Buffer_Len = 0 then
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
  end Flush;

end Text_Line;

