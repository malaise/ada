-- Use Select_File to rename files
with Ada.Text_Io;
with Text_Handler, Argument, Select_File, Sys_Calls;

procedure Renamer is

  Read : Boolean;
  Ok : Boolean;
  File, Prev_File : Text_Handler.Text(500);

  function My_Select_File is
           new Select_File (Read_Title  => "Select file to rename",
                            Write_Title => "Enter new name and Ret");

  function Me return String renames Argument.Get_Program_Name;

  Access_Error : exception;
  function File_Exists (File_Name : String) return Boolean is
  begin
    return Sys_Calls.File_Check (File_Name);
  exception
    when Sys_Calls.Access_Error =>
      raise Access_Error;
  end File_Exists;

begin
  -- Start reading file to rename
  Read := True;

  loop
    -- Get (orig or new) file name
    File.Set (My_Select_File (1, File.Value, Read));
    exit when File.Is_Empty;
    Ok := True;

    -- Check if no /
    if File.Locate ('/') /= 0 then
      Ada.Text_Io.Put_Line (Me & ": File name contains '/'. Skipping.");
      Ok := False;
      Read := True;
      File.Empty;
    end if;

    -- Save original name or check new name is new
    if Ok then
      begin
        if Read then
          -- Check file exists and is accessible
          if File_Exists (File.Value) then
            -- Save original file name
            Prev_File.Set(File);
            Read := False;
            Ok := False;
          else
            Ada.Text_Io.Put_Line (Me & ": File not found "
                                     & File.Value
                                     & ". Skipping.");
          end if;
        else
          -- Check file does not exist and is accessible
          if File_Exists (File.Value) then
            Ada.Text_Io.Put_Line (Me & ": New name "
                                     & File.Value
                                     & " already exists. Skipping.");
            Ok := False;
          elsif Text_Handler."=" (Prev_File, File) then
            Ada.Text_Io.Put_Line (Me & ": New name "
                                     & File.Value
                                     & " is prev name. Skipping.");
            Ok := False;
          end if;
          Read := True;
        end if;
      exception
        when Access_Error =>
          Ada.Text_Io.Put_Line (Me & ": Cannot access file "
                                   & File.Value
                                   & ". Skipping.");
          Ok := False;
          Read := True;
      end;
    end if;

    -- Rename
    if Ok then
      Ok := Sys_Calls.Rename (Prev_File.Value, File.Value);
      if Ok then
        Ada.Text_Io.Put_Line (Me & ": " & Prev_File.Value &
                              " renamed to " & File.Value);
      else
        Ada.Text_Io.Put_Line (Me & ": Failed to rename " & Prev_File.Value &
                              " to " & File.Value);
      end if;
      File.Empty;
    end if;
  end loop;

  Ada.Text_Io.Put_Line (Me & ": Done.");
end Renamer;

