-- Replace each Htab by a space
-- Replace Cr+Lf by Lf
-- Remove trailing spaces (speces preceeding a Lf)
-- Append a Lf and end of file if last char is not a Lf
with Ada.Exceptions, Ada.Characters.Latin_1;
with As.U, Argument, Sys_Calls, Temp_File, Text_Line;
procedure Trail_Spaces is

  -- This is the exit code. Like diff:
  -- An exit status of 0 means no change,
  --  1 means some files have been modified,
  --  and 2 means trouble on at least one file
  All_Unchanged : constant Natural := 0;
  Some_Modified : constant Natural := 1;
  Problem : constant Natural := 2;
  Exit_Code : Natural := All_Unchanged;

  -- Process one file
  procedure Do_File (In_File_Name : in String) is

    -- Close a file with no exception
    procedure Close  (File : in out Text_Line.File_Type) is
      use type Sys_Calls.File_Desc;
    begin
      if Text_Line.Is_Open (File) then
        -- Flush before closing!
        Text_Line.Flush (File);
        Sys_Calls.Close (Text_Line.Get_Fd(File));
        Text_Line.Close (File);
      end if;
    exception
      when others => null;
    end Close;

    -- Temporary out file name
    Out_File_Name : constant String := Temp_File.Create(".");
    -- In and out file desc
    In_File : Text_Line.File_Type;
    Out_File : Text_Line.File_Type;
    -- Line of input
    Line : As.U.Asu_Us;
    -- Is file modified
    Modified : Boolean;
    -- Dummy result for Sys calls
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
    use type As.U.Asu_Us;
  begin
    -- Open in-file read only and associate in Text_Line
    declare
      Fd : Sys_Calls.File_Desc := Sys_Calls.Stdin;
    begin
      Fd := Sys_Calls.Open (In_File_Name, Sys_Calls.In_File);
      Text_Line.Open (In_File, Text_Line.In_File, Fd);
    exception
      when Sys_Calls.Name_Error =>
        Sys_Calls.Put_Line_Error ("Error. File "
             & In_File_Name & " not found, skipping.");
        Dummy := Sys_Calls.Unlink (Out_File_Name);
        Exit_Code := Problem;
        return;
      when Error:others =>
        Sys_Calls.Put_Line_Error ("Error. Cannot open file "
             & In_File_Name & " due to "
             & Ada.Exceptions.Exception_Name (Error) & ", skipping.");
        Dummy := Sys_Calls.Unlink (Out_File_Name);
        Exit_Code := Problem;
        return;
    end;
    -- Create out_file and associate in Text_Line
    declare
      Fd : Sys_Calls.File_Desc := Sys_Calls.Stdin;
    begin
      Fd := Sys_Calls.Create (Out_File_Name);
      Text_Line.Open (Out_File, Text_Line.Out_File, Fd);
    exception
      when Sys_Calls.Name_Error =>
        Sys_Calls.Put_Line_Error ("Error. Cannot create out file "
             & Out_File_Name & ", skipping.");
        Close (In_File);
        Dummy := Sys_Calls.Unlink (Out_File_Name);
        Exit_Code := Problem;
        return;
    end;

    -- Process input file
    Modified := False;
    loop
      -- Read a line until the end
      Line := Text_Line.Get (In_File);
      exit when Line.Length = 0;
      -- Append a Line_Feed if missing
      if Line.Element (Line.Length) /= Text_Line.Line_Feed_Char then
        Line.Append (Text_Line.Line_Feed_Char);
        Modified := True;
      end if;
      -- Replace horiz tabs by spaces
      for I in 1 .. Line.Length - 1 loop
        if Line.Element (I) = Ada.Characters.Latin_1.Ht then
          Line.Replace_Element (I, ' ');
          Modified := True;
        end if;
      end loop;
      -- Remove Cr if Cr then Lf
      if Line.Length > 1
      and then Line.Element (Line.Length - 1)
         = Ada.Characters.Latin_1.Cr then
        Line := Line.Slice (1, Line.Length - 2)
              & As.U.Tus (Text_Line.Line_Feed_Char & "");
        Modified := True;
      end if;
      -- Trail spaces of line
      if Line.Length = 1 then
        -- Redisplay empty lines
        Text_Line.New_Line (Out_File);
      else
        -- Trail spaces of line
        for I in reverse 1 .. Line.Length - 1 loop
          if Line.Element (I) /= ' ' then
            Text_Line.Put_Line (Out_File, Line.Slice (1, I));
            if I /= Line.Length - 1 then
              -- File has been modified
              Modified := True;
            end if;
            exit;
          elsif I = 1 then
            -- Line is made of spaces, replace it by an empty line
            Text_Line.New_Line (Out_File);
            Modified := True;
          end if;
        end loop;
      end if;
    end loop;

    -- Close files
    Close (In_File);
    Close (Out_File);

    -- Move files if modified
    if Modified then
      -- Put modified file name
      Sys_Calls.Put_Line_Output (In_File_Name);
      -- Rename out file as in file
      if Sys_Calls.Rename (Out_File_Name, In_File_Name) then
        if Exit_Code = All_Unchanged then
          Exit_Code := Some_Modified;
        end if;
      else
        Sys_Calls.Put_Line_Error ("Error. Cannot rename out file "
               & Out_File_Name & " as " & In_File_Name & ", skipping.");
        -- At least try to remove tmp file
        Dummy := Sys_Calls.Unlink (Out_File_Name);
        Exit_Code := Problem;
      end if;
    else
      -- Leave unchanged source file
      if not Sys_Calls.Unlink (Out_File_Name) then
        Sys_Calls.Put_Line_Error ("Warning. Cannot remove unused out file "
               & Out_File_Name & ".");
      end if;
    end if;

  exception
    when Error:others =>
      Sys_Calls.Put_Line_Error ("Error. Exception "
             & Ada.Exceptions.Exception_Name (Error)
             & " raised while processing file "
             & In_File_Name & ", skipping.");
      -- Close files and delete temp file
      Close (In_File);
      Close (Out_File);
      Dummy := Sys_Calls.Unlink (Out_File_Name);
      Exit_Code := Problem;
      raise;
  end Do_File;

begin

   -- Process all arguments as file names
   for I in 1 .. Argument.Get_Nbre_Arg loop
     Do_File (Argument.Get_Parameter (I));
   end loop;

   Sys_Calls.Set_Exit_Code (Exit_Code);

end Trail_Spaces;

