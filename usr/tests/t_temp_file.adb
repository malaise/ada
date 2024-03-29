-- Check creation of several Tmp files in directory "data"
-- Fixed sequence of operations
with Sys_Calls, Temp_File, Directory;
procedure T_Temp_File is
  Temp_Dir : constant String := "data";
  type String_Access is access String;
  Names : array (1 .. 1000) of String_Access;
  Desc : Directory.Dir_Desc;
begin
  Sys_Calls.Put_Line_Output ("Creating " & Names'Last'Img & " temp files in "
                      & Temp_Dir & ":");
  for Name of Names loop
    Name := new String'(Temp_File.Create (Temp_Dir, "tmp"));
    Sys_Calls.Put_Line_Output ("Created temp file " & Name.all);
  end loop;
  Sys_Calls.New_Line_Output;

  Sys_Calls.Put_Line_Output ("Checking " & Temp_Dir & " directory content:");
  Desc.Open (Temp_Dir);
  begin
    loop
      Sys_Calls.Put_Line_Output (Desc.Next_Entry);
    end loop;
  exception
    when Directory.End_Error =>
      Desc.Close;
  end;
  Sys_Calls.New_Line_Output;

  Sys_Calls.Put_Line_Output ("Checking exception on yet another temp file:");
  begin
    declare
      N3 : constant String := Temp_File.Create (Temp_Dir, "tmp");
    begin
      Sys_Calls.Put_Line_Output ("NOK, got " & N3);
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
  for Name of Names loop
    Sys_Calls.Unlink (Name.all);
  end loop;
  Sys_Calls.New_Line_Output;

  Sys_Calls.Put_Line_Output ("Done.");

end T_Temp_File;

