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
    Text_Handler.Set (File, My_Select_File (1, Text_Handler.Value(File), Read));
    exit when Text_Handler.Empty(File);
    Ok := True;

    -- Check if no /
    if Text_Handler.Locate (File, '/') /= 0 then
      Ada.Text_Io.Put_Line (Me & ": File name contains '/'. Skipping.");
      Ok := False;
      Read := True;
      Text_Handler.Empty (File);
    end if;

    -- Save original name or check new name is new
    if Ok then
      begin
        if Read then
          -- Check file exists and is accessible
          if File_Exists (Text_Handler.Value(File)) then
            -- Save original file name
            Text_Handler.Set(Prev_File, File);
            Read := False;
            Ok := False;
          else
            Ada.Text_Io.Put_Line (Me & ": File not found "
                                     & Text_Handler.Value(File)
                                     & ". Skipping.");
          end if;
        else
          -- Check file does not exist and is accessible
          if File_Exists (Text_Handler.Value(File)) then
            Ada.Text_Io.Put_Line (Me & ": New name "
                                     & Text_Handler.Value(File)
                                     & " already exists. Skipping.");
            Ok := False;
          elsif Text_Handler."=" (Prev_File, File) then
            Ada.Text_Io.Put_Line (Me & ": New name "
                                     & Text_Handler.Value(File)
                                     & " is prev name. Skipping.");
            Ok := False;
          end if;
          Read := True;
        end if;
      exception
        when Access_Error =>
          Ada.Text_Io.Put_Line (Me & ": Cannot access file "
                                   & Text_Handler.Value(File)
                                   & ". Skipping.");
          Ok := False;
          Read := True;
      end;
    end if;

    -- Rename
    if Ok then
      Ok := Sys_Calls.Rename (Text_Handler.Value(Prev_File),
                              Text_Handler.Value(File));
      if Ok then
        Ada.Text_Io.Put_Line (Me & ": " & Text_Handler.Value(Prev_File) &
                              " renamed to " & Text_Handler.Value(File));
      else
        Ada.Text_Io.Put_Line (Me & ": Failed to rename " & Text_Handler.Value(Prev_File) &
                              " to " & Text_Handler.Value(File));
      end if;
      Text_Handler.Empty(File);
    end if;
  end loop;

  Ada.Text_Io.Put_Line (Me & ": Done.");
end Renamer;

