with As.U, Argument, Sys_Calls, Text_Line;
package body File_Mng is

  -- Reports an error
  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
        & " ERROR: " & Msg & ".");
  end Error;

  File_Name_Str : As.U.Asu_Us;

  -- Open the file of files
  -- May raise Open_Error
  File : Text_Line.File_Type;
  procedure Open (File_Name : in String) is
    Fd : Sys_Calls.File_Desc;
  begin
    if File_Name = Stdin then
      Fd := Sys_Calls.Stdin;
    else
      Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
    end if;
    File.Open (Text_Line.In_File, Fd);
    File_Name_Str := As.U.Tus (File_Name);
  exception
    when others =>
      Error ("Cannot open file of files " & File_Name);
      raise Open_Error;
  end Open;

  procedure Close is
    Fd : Sys_Calls.File_Desc;
    use type Sys_Calls.File_Desc;
  begin
    if not File.Is_Open then
      return;
    end if;
    Fd := File.Get_Fd;
    File.Close;
    if Fd /= Sys_Calls.Stdin then
      Sys_Calls.Close (Fd);
    end if;
  exception
    when others => null;
  end Close;

  -- Get next file name from file
  -- May raise End_Error or Io_Error (and closes file)
  function Get_Next_File return String is
    Str : As.U.Asu_Us;
    Len : Natural;
  begin
    Str := File.Get;
    Len :=  Str.Length;
    if Len /= 0
    and then Str.Element (Len)
           = Text_Line.Line_Feed_Char then
      -- Normal lines (all but last) of Text_Line end with Line_Feed
      -- Explicitly remove it
      Str.Delete (Len, Len);
      Len := Len - 1;
    end if;
    if Len = 0 then
      -- Normal end of file
      Close;
      raise End_Error;
    end if;
    return Str.Image;
  exception
    when End_Error =>
      raise;
    when others =>
      -- Error
      Error ("processing file of files " & File_Name_Str.Image);
      Close;
      raise Io_Error;
  end Get_Next_File;

end File_Mng;

