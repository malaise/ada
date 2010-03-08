with Afpx, Temp_File;
with Utils, Config;
procedure View (Path : in String;
                Hash : in Git_If.Git_Hash) is
begin
  -- Create temporaty file
  declare
    Tmp_Name : constant String := Temp_File.Create ("/tmp");
  begin
    -- Cat file in it
    Afpx.Suspend;
    begin
      Git_If.Cat (Path, Hash, Tmp_Name);
      Afpx.Resume;
    exception
      when others =>
        Afpx.Resume;
        raise;
    end;
    -- Launch viewer
    Utils.Launch (Config.Viewer & " " & Tmp_Name
                & "; sleep 5; rm " & Tmp_Name);
  end;
    exception
      when Temp_File.Invalid_Dir | Temp_File.No_More_Temp =>
        null;
end View;

