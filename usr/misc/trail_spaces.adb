with Ada.Text_Io, Ada.Exceptions, Ada.Strings.Unbounded, Ada.Characters.Latin_1;
with Argument, Sys_Calls;
procedure Trail_Spaces is

  -- Process one file
  procedure Do_File (In_File_Name : in String) is

    -- Build temp file name in /tmp from In_File_Name
    function Make_Out_File_Name return String is
    begin
      return "Trailed_Spaces_" & In_File_Name;
    end Make_Out_File_Name;

    -- Close a file with no exception
    procedure Close  (File : in out Ada.Text_Io.File_Type) is
    begin
      Ada.Text_Io.Close (File);
    exception
      when others => null;
    end Close;

    -- In and out file desc
    In_File : Ada.Text_Io.File_Type;
    Out_File : Ada.Text_Io.File_Type;
    -- Line of input
    Line : String (1 .. 1024);
    Len : Natural;
    -- Concat of temprary lines
    Unb_Line : Ada.Strings.Unbounded.Unbounded_String;
    -- Is line put due to parsing
    Put : Boolean;
    -- Is file modified
    Modified : Boolean;
    -- Append a last newline (always but when last line is spaces)
    Append_New_Line : Boolean;
    -- Dummy result for Sys calls
    Dummy : Boolean;

    use type Ada.Strings.Unbounded.Unbounded_String;

  begin
    -- Open in-file read only
    begin
      Ada.Text_Io.Open (In_File, Ada.Text_Io.In_File, In_File_Name);
    exception
      when Ada.Text_Io.Name_Error =>
        Sys_Calls.Put_Line_Error ("Error. File "
             & In_File_Name & " not found, skipping.");
        return;
      when Error:Others =>
        Sys_Calls.Put_Line_Error ("Error. Cannot open file "
             & In_File_Name & " due to "
             & Ada.Exceptions.Exception_Name (Error) & ", skipping.");
        return;
    end;
    -- Create out_file
    declare
      Out_File_Name : constant String := Make_Out_File_Name;
    begin
      Ada.Text_Io.Create (Out_File, Ada.Text_Io.Out_File, Out_File_Name);
    exception
      when Error:Others =>
        Sys_Calls.Put_Line_Error ("Error. Cannot create out file "
             & Out_File_Name & " due to "
             & Ada.Exceptions.Exception_Name (Error) & ", skipping.");
        return;
    end;

    -- Process input file
    Modified := False;
    loop
      Append_New_Line := True;
      exit when Ada.Text_Io.End_Of_File (In_File);
      Ada.Text_Io.Get_Line (In_File, Line, Len);
      -- Replace horiz tabs by spaces
      for I in 1 .. Len loop
        if Line(I) = Ada.Characters.Latin_1.Ht then
          Modified := True;
          Line(I) := ' ';
        end if;
      end loop;
      if Len < Line'Last then
        -- This is the end of the line: trail space and put remaining
        Put := False;
        if Unb_Line = Ada.Strings.Unbounded.Null_Unbounded_String then
          -- No line saved so far
          -- Normal case where input line fits
          -- Search and parse trailing spaces
          for I in reverse 1 .. Len loop
            if Line(I) /= ' ' then
              Ada.Text_Io.Put_Line (Out_File, Line(1 .. I));
              -- Line has been put
              Put := True;
              if I /= Len then
                -- File has been modified
                Modified := True;
              end if;
              exit;
            end if;
          end loop;
        else
          -- This is the end of a long long line...
          Ada.Strings.Unbounded.Append (Unb_Line, Line(1 .. Len));
          Len := Ada.Strings.Unbounded.Length (Unb_Line);
          for I in reverse 1 .. Len loop
            if Ada.Strings.Unbounded.Element (Unb_Line, I) /= ' ' then
              Ada.Text_Io.Put_Line (Out_File,
                Ada.Strings.Unbounded.Slice (Unb_Line, 1, I));
              Unb_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
              -- Line has been put
              Put := True;
              if I /= Len then
                -- File has been modified
                Modified := True;
              end if;
              exit;
            end if;
          end loop;
        end if;
        if not Put then
          -- Line was full of spaces or empty
          Ada.Text_Io.New_Line (Out_File);
          if Len /= 0 then
            -- Line was full of spaces
            Modified := True;
            Append_New_Line := False;
          end if;
        end if;
      elsif Unb_Line = Ada.Strings.Unbounded.Null_Unbounded_String
      and then Line(Line'Last) /= ' ' then
        -- Nothing saved so far and this line does not en with spaces
        -- We can put (no newline) this data
        Ada.Text_Io.Put (Out_File, Line);
      else
        -- Input line does not fit: save it in an unbounded string
        Ada.Strings.Unbounded.Append (Unb_Line, Line);
      end if;
    end loop;

    -- Append last newline Close files
    Ada.Text_Io.Close (In_File);
    if Append_New_Line then
      Ada.Text_Io.New_Line (Out_File);
    end if;
    Ada.Text_Io.Close (Out_File);

    -- Rename out file as in file
    if not Sys_calls.Rename (Make_Out_File_Name, In_File_Name) then
      Sys_Calls.Put_Line_Error ("Error. Cannot rename out file "
             & Make_Out_File_Name & " as " & In_File_Name & ", skipping.");
      -- At least try to remove tmp file
      Dummy := Sys_calls.Unlink (Make_Out_File_Name);
      return;
    end if;

    -- Put modified file name
    if Modified then
      Ada.Text_Io.Put_Line (In_File_Name);
    end if;

  exception
    when others =>
      -- Close files and delete temp file
      if Ada.Text_Io.Is_Open (In_File) then
        Close (In_File);
      end if;
      if Ada.Text_Io.Is_Open (Out_File) then
        Close (Out_File);
      end if;
      Dummy := Sys_Calls.Unlink (Make_Out_File_Name);
      raise;
  end Do_File;

begin

   -- Process all arguments as file names
   for I in 1 .. Argument.Get_Nbre_Arg loop
     Do_File (Argument.Get_Parameter (I));
   end loop;

end Trail_Spaces;
  
