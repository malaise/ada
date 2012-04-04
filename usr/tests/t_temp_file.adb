with Sys_Calls, Temp_File, Directory;
procedure T_Temp_File is
  Temp_Dir : constant String := "data";
  type String_Access is access String;
  Names : array (1 .. 1000) of String_Access;
  Desc : Directory.Dir_Desc;
begin
  Sys_Calls.Put_Line_Output ("Creating " & Names'Last'Img & " temp files in "
                      & Temp_Dir & ":");
  for I in Names'Range loop
    Names(I) := new String'(Temp_File.Create (Temp_Dir));
    Sys_Calls.Put_Line_Output ("Created temp file " & Names(I).all);
  end loop;
  Sys_Calls.New_Line_Output;

  Sys_Calls.Put_Line_Output ("Checking " & Temp_Dir & " directory content:");
  Desc := Directory.Open (Temp_Dir);
  begin
    loop
      Sys_Calls.Put_Line_Output (Directory.Next_Entry (Desc));
    end loop;
  exception
    when Directory.End_Error =>
      Directory.Close (Desc);
  end;
  Sys_Calls.New_Line_Output;

  Sys_Calls.Put_Line_Output ("Checking exception on yet another temp file:");
  begin
    declare
      N3 : constant String := Temp_File.Create (Temp_Dir);
      pragma Unreferenced (N3);
    begin
      null;
    end;
  exception
    when Temp_File.No_More_Temp =>
      Sys_Calls.Put_Line_Output ("Raises No_More_Temp OK");
  end;
  Sys_Calls.New_Line_Output;
  Sys_Calls.Put_Line_Output ("Checking exception on invalid directory:");
  begin
    declare
      N3 : constant String := Temp_File.Create ("DataDir_That_Does_NOT_Exist");
      pragma Unreferenced (N3);
    begin
      null;
    end;
  exception
    when Temp_File.Invalid_Dir =>
      Sys_Calls.Put_Line_Output ("Raises Invalid_Dir OK");
  end;
  Sys_Calls.New_Line_Output;

  Sys_Calls.Put_Line_Output ("Cleaning temp files.");
  for I in Names'Range loop
    Sys_Calls.Unlink (Names(I).all);
  end loop;
  Sys_Calls.New_Line_Output;

  Sys_Calls.Put_Line_Output ("Done.");

end T_Temp_File;

