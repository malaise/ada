-- Use Select_File to rename files
with Ada.Text_Io;
with Text_Handler, Argument, Select_File, Sys_Calls;

procedure Renamer is

  Read : Boolean;
  OK : Boolean;
  File, Prev_File : Text_Handler.Text(500);

  function My_Select_File is
           new Select_File (Read_Title  => "Select file to rename",
                            Write_Title => "Enter new name and Ret");

  function Me return String renames Argument.Get_Program_Name;

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
      if Read then
        -- Save original file name
        Text_Handler.Set(Prev_File, File);
        Read := False;
        Ok := False;
      else
        if Text_Handler."=" (Prev_File, File) then
          Ada.Text_Io.Put_Line (Me & ": New name " & Text_Handler.Value(File) &
                                " is prev name. Skipping.");
          Ok := False;
        end if;
        Read := True;
      end if;
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

