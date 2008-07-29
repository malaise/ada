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
    Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
    Text_Line.Open (File, Text_Line.In_File, Fd);
    File_Name_Str := Ada.Strings.Unbounded.To_Unbounded_String (File_Name);
  exception
    when others =>
      Error ("Cannot open file of files " & File_Name);
      raise Open_Error;
  end Open;

  procedure Close is
    Fd : Sys_Calls.File_Desc;
  begin
    if not Text_Line.Is_Open (File) then
      return;
    end if;
    Fd := Text_Line.Get_Fd (File);
    Text_Line.Close (File);
    Sys_Calls.Close (Fd);
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
    if Len = 0 then
      -- Normal end of file
      Close;
      raise End_Error;
    end if;
    if Ada.Strings.Unbounded.Element (Str, Len) = Text_Line.Line_Feed_Char then
      -- Normal lines of Text_Line end with Line_Feed explicitly
      -- remove it
      return Ada.Strings.Unbounded.Slice (Str, 1, Len - 1);
    else
      -- Last line may not have a Line_Feed
      return Ada.Strings.Unbounded.To_String (Str);
    end if;
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

