-- Use Select_File (Afpx) to rename files
with As.B, Argument,Sys_Calls, Basic_Proc, Directory,
     Afpx, Afpx_Xref, Select_File;
procedure Renamer is

  Read : Boolean;
  Ok : Boolean;
  File, Prev_File : As.B.Asb_Bs(500);

  package Msf is
          new Select_File (Afpx_Xref.File_Selection.Dscr_Num,
                           Read_Title  => "Select file to rename",
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
  Afpx.Use_Descriptor (Afpx_Xref.File_Selection.Dscr_Num);

  -- Optional argument: a directory
  if Argument.Get_Nbre_Arg = 1 then
    begin
      if Directory.Dirname (Argument.Get_Parameter) /= "" then
        Directory.Change_Current (Directory.Make_Full_Path (
            Argument.Get_Parameter));
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error (Me & ": Invalid directory path.");
        Basic_Proc.Set_Error_Exit_Code;
        return;
    end;
  elsif Argument.Get_Nbre_Arg = 2 then
    Basic_Proc.Put_Line_Error (Me
        & ": Invalid argument. Only one optional path supported.");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;


  -- Start reading file to rename
  Read := True;

  loop
    -- Protect list when getting new name
    Afpx.Set_Field_Protection (Afpx.List_Field_No, not Read);
    if Read then
      Afpx.Clear_Field  (16);
      Afpx.Encode_Field (16, (1, 2), "EXIT");
    else
      Afpx.Reset_Field (16, Reset_Colors => False);
    end if;
    -- Get (orig or new) file name
    File.Set (Msf.Get_File (File.Image, Read, True));

    -- CANCEL / EXIT button?
    Ok := not File.Is_Null;
    -- Exit
    exit when not Ok and then Read;

    -- Check if no /
    if Ok and then File.Locate ("/") /= 0 then
      Basic_Proc.Put_Line_Error (Me & ": File name contains '/'. Skipping.");
      Msf.Report_Error ("File name contains '/'");
      Ok := False;
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
          else
            Basic_Proc.Put_Line_Error (Me & ": File not found "
                                    & File.Image
                                    & ". Skipping.");
            Msf.Report_Error ("File not found");
            Ok := False;
          end if;
        else
          -- Check file is different, does not exist and is accessible
          if As.B."=" (Prev_File, File) then
            Basic_Proc.Put_Line_Error (Me & ": New name "
                                    & File.Image
                                    & " is prev name. Skipping.");
            Msf.Report_Error ("Same file name");
            Ok := False;
          elsif File_Exists (File.Image) then
            Basic_Proc.Put_Line_Error (Me & ": New name "
                                    & File.Image
                                    & " already exists. Skipping.");
            Msf.Report_Error ("File exists");
            Ok := False;
          end if;
        end if;
      exception
        when Access_Error =>
          Basic_Proc.Put_Line_Error (Me & ": Cannot access file "
                                  & File.Image
                                  & ". Skipping.");
          Msf.Report_Error ("Access error");
          Ok := False;
      end;
    end if;

    -- Rename
    if Ok and then not Read then
      Ok := Sys_Calls.Rename (Prev_File.Image, File.Image);
      if Ok then
        Basic_Proc.Put_Line_Output (Me & ": " & Prev_File.Image &
                              " renamed to " & File.Image);
      else
        Basic_Proc.Put_Line_Error (Me & ": Failed to rename "
                                & Prev_File.Image & " to " & File.Image);
        Msf.Report_Error ("Rename error");
      end if;
      File.Set_Null;
    end if;

    if Ok then
      Read := not Read;
    else
      -- Restart on error
      Read := True;
    end if;
  end loop;

  Basic_Proc.Put_Line_Output (Me & ": Done.");
exception
  when Msf.Exit_Requested =>
    Basic_Proc.Put_Line_Output (Me & ": Aborted.");
end Renamer;

