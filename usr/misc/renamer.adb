-- Use Select_File to rename files
with Ada.Text_Io;
with As.B, Argument, Select_File, Sys_Calls;

procedure Renamer is

  Read : Boolean;
  Ok : Boolean;
  File, Prev_File : As.B.Asb_Bs(500);

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
    File.Set (My_Select_File (1, File.Image, Read, True));
    exit when File.Is_Null;
    Ok := True;

    -- Check if no /
    if File.Locate ("/") /= 0 then
      Ada.Text_Io.Put_Line (Me & ": File name contains '/'. Skipping.");
      Ok := False;
      Read := True;
      File.Set_Null;
    end if;

    -- Save original name or check new name is new
    if Ok then
      begin
        if Read then
          -- Check file exists and is accessible
          if File_Exists (File.Image) then
            -- Save original file name
            Prev_File.Set(File);
            Read := False;
            Ok := False;
          else
            Ada.Text_Io.Put_Line (Me & ": File not found "
                                     & File.Image
                                     & ". Skipping.");
          end if;
        else
          -- Check file does not exist and is accessible
          if File_Exists (File.Image) then
            Ada.Text_Io.Put_Line (Me & ": New name "
                                     & File.Image
                                     & " already exists. Skipping.");
            Ok := False;
          elsif As.B."=" (Prev_File, File) then
            Ada.Text_Io.Put_Line (Me & ": New name "
                                     & File.Image
                                     & " is prev name. Skipping.");
            Ok := False;
          end if;
          Read := True;
        end if;
      exception
        when Access_Error =>
          Ada.Text_Io.Put_Line (Me & ": Cannot access file "
                                   & File.Image
                                   & ". Skipping.");
          Ok := False;
          Read := True;
      end;
    end if;

    -- Rename
    if Ok then
      Ok := Sys_Calls.Rename (Prev_File.Image, File.Image);
      if Ok then
        Ada.Text_Io.Put_Line (Me & ": " & Prev_File.Image &
                              " renamed to " & File.Image);
      else
        Ada.Text_Io.Put_Line (Me & ": Failed to rename " & Prev_File.Image &
                              " to " & File.Image);
      end if;
      File.Set_Null;
    end if;
  end loop;

  Ada.Text_Io.Put_Line (Me & ": Done.");
end Renamer;

