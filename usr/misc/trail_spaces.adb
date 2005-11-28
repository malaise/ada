with Ada.Text_Io, Ada.Exceptions, Ada.Strings.Unbounded, Ada.Characters.Latin_1;
with Argument, Sys_Calls, Temp_File, Text_Line;
procedure Trail_Spaces is

  -- Process one file
  procedure Do_File (In_File_Name : in String) is

    -- Close a file with no exception
    procedure Close  (File : in out Text_Line.File_Type) is
      use type  Sys_Calls.File_Desc;
    begin
      if Text_Line.Is_Open (File) then
        Sys_Calls.Close (Text_Line.Get_Fd(File));
        Text_Line.Close (File);
      end if;
    exception
      when others => null;
    end Close;

    package Asu renames Ada.Strings.Unbounded;

    -- Temporary out file name
    Out_File_Name : constant String := Temp_File.Create(".");
    -- In and out file desc
    In_File : Text_Line.File_Type;
    Out_File : Ada.Text_Io.File_Type;
    -- Line of input
    Line : Ada.Strings.Unbounded.Unbounded_String;
    -- Is file modified
    Modified : Boolean;
    -- Dummy result for Sys calls
    Dummy : Boolean;

    use type Asu.Unbounded_String;

  begin
    -- Open in-file read only and associate in Text_Line
    declare
      Fd : Sys_Calls.File_Desc := Sys_Calls.Stdin;
    begin
      Fd := Sys_Calls.Open (In_File_Name, Sys_Calls.In_File);
      Text_Line.Open (In_File, Fd);
    exception
      when Sys_Calls.Name_Error =>
        Sys_Calls.Put_Line_Error ("Error. File "
             & In_File_Name & " not found, skipping.");
        return;
      when Error:others =>
        Sys_Calls.Put_Line_Error ("Error. Cannot open file "
             & In_File_Name & " due to "
             & Ada.Exceptions.Exception_Name (Error) & ", skipping.");
        return;
    end;
    -- Create out_file
    begin
      Ada.Text_Io.Create (Out_File, Ada.Text_Io.Out_File, Out_File_Name);
    exception
      when Error:others =>
        Sys_Calls.Put_Line_Error ("Error. Cannot create out file "
             & Out_File_Name & " due to "
             & Ada.Exceptions.Exception_Name (Error) & ", skipping.");
        Close (In_File);
        return;
    end;

    -- Process input file
    Modified := False;
    loop
      -- Read a line until the end
      Line := Text_Line.Get (In_File);
      exit when Asu.Length (Line) = 0;
      -- Append a New_Line if missing
      if Asu.Element (Line, Asu.Length (Line)) /= Text_Line.New_Line then
        Asu.Append (Line, Text_Line.New_Line);
        Modified := True;
      end if;
      -- Replace horiz tabs by spaces
      for I in 1 .. Asu.Length (Line) - 1 loop
        if Asu.Element (Line, I) = Ada.Characters.Latin_1.Ht then
          Asu.Replace_Element (Line, I, ' ');
          Modified := True;
        end if;
      end loop;
      if Asu.Length (Line) = 1 then
        -- Redisplay empty lines
        Ada.Text_Io.New_Line (Out_File);
      else
        -- Trail spaces of line
        for I in reverse 1 .. Asu.Length (Line) - 1 loop
          if Asu.Element (Line, I) /= ' ' then
            Ada.Text_Io.Put_Line (Out_File, Asu.Slice (Line, 1, I));
            if I /= Asu.Length (Line) - 1 then
              -- File has been modified
              Modified := True;
            end if;
            exit;
          end if;
        end loop;
      end if;
    end loop;

    -- Close files
    Close (In_File);
    Ada.Text_Io.Close (Out_File);

    -- Move files if modified
    if Modified then
      -- Put modified file name
      Ada.Text_Io.Put_Line (In_File_Name);
      -- Rename out file as in file
      if not Sys_Calls.Rename (Out_File_Name, In_File_Name) then
        Sys_Calls.Put_Line_Error ("Error. Cannot rename out file "
               & Out_File_Name & " as " & In_File_Name & ", skipping.");
        -- At least try to remove tmp file
        Dummy := Sys_Calls.Unlink (Out_File_Name);
      end if;
    else
      -- Leave unchanged source file
      if not Sys_Calls.Unlink (Out_File_Name) then
        Sys_Calls.Put_Line_Error ("Warning. Cannot remove unused out file "
               & Out_File_Name & ".");
      end if;
    end if;

  exception
    when others =>
      -- Close files and delete temp file
      Close (In_File);
      if Ada.text_Io.Is_Open (Out_File) then
        begin
          Ada.Text_Io.Close (Out_File);
        exception
          when others =>
            null;
        end;
      end if;
      Dummy := Sys_Calls.Unlink (Out_File_Name);
      raise;
  end Do_File;

begin

   -- Process all arguments as file names
   for I in 1 .. Argument.Get_Nbre_Arg loop
     Do_File (Argument.Get_Parameter (I));
   end loop;

end Trail_Spaces;

