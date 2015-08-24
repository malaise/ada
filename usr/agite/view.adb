with Temp_File;
with Utils, Config;
procedure View (Path : in String;
                Hash : in Git_If.Git_Hash) is
  Ok : Boolean;
begin
  -- Create temporaty file
  declare
    Tmp_Name : constant String := Temp_File.Create ("/tmp");
  begin
    -- Cat file in it
    Ok := Git_If.Cat (Path, Hash, Tmp_Name, Log_Error => False);
    if not Ok then
      -- Try Hash^ (file might be deleted by the commit of hash)
      Ok := Git_If.Cat (Path, Hash & "^", Tmp_Name);
    end if;
    -- Launch viewer
    if Ok then
      Utils.Launch (Config.Viewer & " '" & Tmp_Name
                  & "' ; sleep 5; rm '" & Tmp_Name & "'");
    end if;
  end;
exception
  when Temp_File.Invalid_Dir | Temp_File.No_More_Temp =>
    null;
end View;

