with Ada.Strings.Unbounded;
with Argument, Sys_Calls, Text_Line;
package body File_Mng is

  -- Reports an error
  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
        & " ERROR: " & Msg & ".");
  end Error;

  File_Name_Str : Ada.Strings.Unbounded.Unbounded_String;

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
    File_Name_Str := Ada.Strings.Unbounded.To_Unbounded_String (File_Name);
  exception
    when others =>
      Error ("Cannot open file of files " & File_Name);
      raise Open_Error;
  end Open;

  procedure Close is
    Fd : Sys_Calls.File_Desc;
    use type Sys_Calls.File_Desc;
  begin
    if not Text_Line.Is_Open (File) then
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
    Str : Ada.Strings.Unbounded.Unbounded_String;
    Len : Natural;
  begin
    Str := Text_Line.Get (File);
    Len :=  Ada.Strings.Unbounded.Length (Str);
    if Len /= 0
    and then Ada.Strings.Unbounded.Element (Str, Len)
           = Text_Line.Line_Feed_Char then
      -- Normal lines (all but last) of Text_Line end with Line_Feed
      -- Explicitly remove it
      Ada.Strings.Unbounded.Delete (Str, Len, Len);
      Len := Len - 1;
    end if;
    if Len = 0 then
      -- Normal end of file
      Close;
      raise End_Error;
    end if;
    return Ada.Strings.Unbounded.To_String (Str);
  exception
    when End_Error =>
      raise;
    when others =>
      -- Error
      Error ("processing file of files "
           & Ada.Strings.Unbounded.To_String(File_Name_Str));
      Close;
      raise Io_Error;
  end Get_Next_File;

end File_Mng;

