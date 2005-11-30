with Ada.Text_Io, Ada.Strings.Unbounded;
with Argument, Sys_Calls, Text_Line, Temp_File, Dynamic_List,
     Regular_Expressions;
with Search_Pattern;
package body Substit is
  Debug : Boolean := True;

  package Asu renames Ada.Strings.Unbounded;

  -- List of strings
  package Line_List_Dyn_Mng is new Dynamic_List (Asu.Unbounded_String);
  package Line_List_Mng renames Line_List_Dyn_Mng.Dyn_List;
  Line_List : Line_List_Mng.List_Type;
  subtype Str_Access is Line_List_Mng.Element_Access;

  -- File names
  In_File_Name : Asu.Unbounded_String;
  Out_File_Name : Asu.Unbounded_String;

  -- Files
  In_File : Text_Line.File_Type;
  Out_File : Ada.Text_Io.File_Type;

  -- We work on stdin/stdout?
  Is_Stdin : Boolean;

  -- Display error and raise File_Error
  procedure Error (Msg : in String);

  -- Close files
  procedure Close is
  begin
    if not Is_Stdin then
      -- Close in file
      if Text_Line.Is_Open (In_File) then
        begin
          Sys_Calls.Close (Text_Line.Get_Fd(In_File));
        exception
          when others => null;
        end;
        begin
          Text_Line.Close (In_File);
        exception
          when others => null;
        end;
      end if;
      -- Close Out file
      Ada.Text_Io.Set_Output (Ada.Text_Io.Standard_Output);
      if Ada.Text_Io.Is_Open (Out_File) then  
        begin
          Ada.Text_Io.Close (Out_File);
        exception
          when others => null;
        end;
      end if;
    end if;
  end Close;

  -- Remove Out file
  procedure Clean is
    Rec : Sys_Calls.File_Stat_Rec;
    Dummy : Boolean;
  begin
    if Asu.Length (Out_File_Name) /= 0 then
      Rec := Sys_Calls.File_Stat (Asu.To_String (Out_File_Name));
      -- File exists => remove
      Dummy := Sys_Calls.Unlink (Asu.To_String (Out_File_Name));
    end if;
  exception
    when others => null;
  end Clean;

  procedure Comit is
    Result : Boolean;
  begin
    Result := Sys_Calls.Rename (Asu.To_String (In_File_Name),
                                Asu.To_String (Out_File_Name));
    if not Result then
      Error ("Cannot move " & Asu.To_String (Out_File_Name)
           & " to " & Asu.To_String (In_File_Name));
      Clean;
    end if;
  end Comit;

  -- Open Files
  procedure Open (File_Name : in String) is
    In_Fd : Sys_Calls.File_Desc;
  begin
    In_File_Name := Asu.To_Unbounded_String (File_Name);
    Is_Stdin := File_Name = Std_in_Out;
    -- Open In fd and Out file if not stdin
    if Is_Stdin then
      In_Fd := Sys_Calls.Stdin;
      Out_File_Name := Asu.Null_Unbounded_String;
    else
      begin
        In_Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
      exception
        when Sys_Calls.Name_Error =>
          Error ("Cannot open input file " & File_Name);
      end;
      Out_File_Name := Asu.To_Unbounded_String (Temp_File.Create ("."));
      begin
        Ada.Text_Io.Create (Out_File, Ada.Text_Io.Out_File,
                            Asu.To_String (Out_File_Name));
      exception
        when others =>
          Error ("Cannot create temp file " & Asu.To_String (Out_File_Name));
      end;
      Ada.Text_Io.Set_Output (Out_File);
    end if;
    -- Associate In fd to In file
    Text_Line.Open (In_File, In_fd);
  end Open;

  -- Reports an error
  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
        & " ERROR: " & msg & ".");
    Close;
    Clean;
    raise File_Error;
  end Error;

  -- Read the number of lines and New_Lines requested
  Trail_New_Line : Boolean := False;
  function Read (Nb_Pattern : Positive) return Boolean is
    Nb_To_Read : Natural;
    Line : Asu.Unbounded_String;
    Len : Natural;
  begin
    -- Move to end
    begin
      Line_List_Mng.Rewind (Line_List, Line_List_Mng.Prev);
    exception
      when Line_List_Mng.Empty_List =>
        null;
    end;
    -- Compute amount to fill buffer (Nb lines)
    Nb_To_Read := Nb_Pattern - Line_List_Mng.List_Length (Line_List);
    -- Append trailing new line if any
    if Trail_New_Line then
      Line_List_Mng.Insert (Line_List,
                Asu.To_Unbounded_String (Text_Line.New_Line & ""));
      Trail_New_Line := False;
      Nb_To_Read := Nb_To_Read - 1;
    end if;

    -- Read and append remaining amount, save trailing newline
    while Nb_To_Read /= 0 loop
      Line := Text_Line.Get (In_File);
      Len := Asu.Length (Line);
      if Debug then
        Sys_Calls.Put_Line_Error ("Read > " &  Asu.To_String (Line) & "<");
      end if;
      if Len = 0 then
        -- We reached the end of file
        return False;
      end if;
      -- There are either one or two items to push
      if Len > 1 and then Asu.Element(Line, Len) = Text_Line.New_Line then
        -- Line + Nl
        -- Insert line (without Nl)
        Line_List_Mng.Insert (Line_List,
              Asu.To_Unbounded_String (Asu.Slice (Line, 1, Len-1)));
        Nb_To_Read := Nb_To_Read - 1;
        if Nb_To_Read = 0 then
          -- Nl remains for next read
          Trail_New_Line := True;
          return True;
        else
          -- Insert Nl
          Line_List_Mng.Insert (Line_List,
                Asu.To_Unbounded_String (Text_Line.New_Line & ""));
          Nb_To_Read := Nb_To_Read - 1;
        end if;
      else
        -- Line without Nl, or a Nl
        -- Insert it
        Line_List_Mng.Insert (Line_List, Line);
        Nb_To_Read := Nb_To_Read - 1;
      end if;
    end loop;
    return True;
  end Read;

  -- Process one file (stdin -> stdout if File_Name is Std_In_Out)
  function Subst_Lines (Nb_Pattern : in Positive) return Boolean;
  procedure Do_One_File (File_Name : in String; Nb_Pattern : in Positive) is
    Modified : Boolean;
  begin
    -- Open files
    Open (File_Name);

    -- Init buffer of lines
    Line_List_Mng.Delete_List (Line_List);
    Trail_New_Line := False;
    -- Init substitution by reading Nb_Pattern lines and Newlines
    Modified := False;
    -- Loop on substit
    loop
      -- Done when the amount of lines cannot be read
      exit when not Read (Nb_Pattern);
      -- Process these lines
      if Subst_Lines (Nb_Pattern) then
        -- These lines have been substituted
        Modified := True;
      end if;
    end loop;
    
    -- Close and cleanup files
    Close;
    -- After close (stdout restored to standard output)
    --  put name of modified file
    if not Is_Stdin and then Modified then
      Comit;
      Ada.Text_Io.Put_Line (File_Name);
    else
      Clean;
    end if;
  exception
    when others =>
      Close;
      raise;
  end Do_One_File;

  -- Handle multiple substitutions within one line
  function Subst_One_Line (Str : Str_Access) return Boolean is
  begin
   -- @@@
    Ada.text_Io.Put_Line (Asu.To_String (Str.all));
    return False;
  end Subst_One_Line;

  -- Check current list of lines vs search patterns
  function Subst_Lines (Nb_Pattern : in Positive) return Boolean is
    Match_Res : Regular_Expressions.Match_Cell;
    Line : Str_Access;
    Start, Stop : Positive;
    Matches : Boolean;
  begin
    -- Rewind read lines
    Line_List_Mng.Rewind (Line_List);
    -- Handle multiple substitutions if one pattern
    if Nb_Pattern = 1 then
      Matches := Subst_One_Line (Line_List_Mng.Access_Current (Line_List));
      Line_List_Mng.Delete_List (Line_List, False);
    end if;
    Matches := True;
    for I in 1 .. Nb_Pattern loop
      -- Check this read line
      Line := Line_List_Mng.Access_Current (Line_List);
      Match_Res := Search_Pattern.Check (Asu.To_String (Line.all), I);
      if Match_Res.Start_Offset <= 0
      or else Match_Res.End_Offset <= 0 then
        -- This one does not match
        Matches := False;
        exit;
      end if;
      -- Keep pos of start of first match and stop of last match
      if I = 1 then
        Start := Match_Res.Start_Offset;
      elsif I = Nb_Pattern then
        Stop := Match_Res.End_Offset;
      end if;
    end loop;
    if not Matches then
      -- If not match, put first line and delete it
      Line_List_Mng.Rewind (Line_List);
      Line := Line_List_Mng.Access_Current (Line_List);
      Ada.Text_Io.Put (Asu.To_String (Line.all));
      Line_List_Mng.Delete (Line_List);
    else
      -- If Match, substit, put result and delete all
      -- @@@
      Matches := False;
      Line_List_Mng.Delete_List (Line_List, False);
    end if;
    return Matches;
  end Subst_Lines;

end Substit;

