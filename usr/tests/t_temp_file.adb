with Ada.Text_Io;
with Sys_Calls, Temp_File, Directory;
procedure T_Temp_File is
  Temp_Dir : constant String := "data";
  type String_Access is access String;
  Names : array (1 .. 1000) of String_Access;
  Desc : Directory.Dir_Desc;
  Dummy : Boolean;
begin
  Ada.Text_Io.Put_Line ("Creating " & Names'Last'Img & " temp files in "
                      & Temp_Dir & ":");
  for I in Names'Range loop
    Names(I) := new String'(Temp_File.Create (Temp_Dir));
    Ada.Text_Io.Put_Line ("Created temp file " & Names(I).all);
  end loop;
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Checking " & Temp_Dir & " directory content:");
  Desc := Directory.Open (Temp_Dir);
  begin
    loop
      Ada.Text_Io.Put_Line (Directory.Next_Entry (Desc));
    end loop;
  exception
    when Directory.End_Error =>
      Directory.Close (Desc);
  end;
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Checking exception on yet another temp file:");
  begin
    declare
      N3 : constant String := Temp_File.Create (Temp_Dir);
    begin
      null;
    end;
  exception
    when Temp_File.No_More_Temp =>
      Ada.Text_Io.Put_Line ("Raises No_More_Temp OK");
  end;
  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Checking exception on invalid directory:");
  begin
    declare
      N3 : constant String := Temp_File.Create ("DataDir_That_Does_NOT_Exist");
    begin
      null;
    end;
  exception
    when Temp_File.Invalid_Dir =>
      Ada.Text_Io.Put_Line ("Raises Invalid_Dir OK");
  end;
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Cleaning temp files.");
  for I in Names'Range loop
    Dummy := Sys_Calls.Unlink (Names(I).all);
  end loop;
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Done.");

end T_Temp_File;

