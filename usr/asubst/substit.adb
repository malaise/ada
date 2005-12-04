with Ada.Text_Io, Ada.Strings.Unbounded, Copy_File, Directory;
with Argument, Sys_Calls, Text_Line, Temp_File, Dynamic_List,
     Regular_Expressions, Debug;
with Search_Pattern, Replace_Pattern;
package body Substit is

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
  Out_File : Text_Line.File_Type;

  -- We work on stdin/stdout?
  Is_Stdin : Boolean;

  -- Number of patterns and can it be multiple
  Nb_Pattern : Positive;
  Is_Multiple : Boolean;

  -- Display error and raise Substit_Error
  procedure Error (Msg : in String);

  -- Close files
  procedure Close is
  begin
    -- Close in file
    if Text_Line.Is_Open (In_File) then
      if not Is_Stdin then
        begin
          Sys_Calls.Close (Text_Line.Get_Fd(In_File));
        exception
          when others => null;
        end;
      end if;
      begin
        Text_Line.Close (In_File);
      exception
        when others => null;
      end;
    end if;
    -- Flush and close Out file
    if Text_Line.Is_Open (Out_File) then
      Text_Line.Flush (Out_File);
      if not Is_Stdin then
        begin
          Sys_Calls.Close (Text_Line.Get_Fd(Out_File));
        exception
          when others => null;
        end;
      end if;
      begin
        Text_Line.Close (Out_File);
      exception
        when others => null;
      end;
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

  procedure Comit (Backup : in Boolean) is
    Result : Boolean;
  begin
    if Backup then
      -- Copy in file as .asu if Backup
      Result := Copy_File (Asu.To_String (In_File_Name),
                           Asu.To_String (In_File_Name) & ".asu");
      if not Result then
        Error ("Cannot copy " & Asu.To_String (In_File_Name)
             & " to " & Asu.To_String (In_File_Name) & ".asu");
        Clean;
        return;
      end if;
    end if;
    -- Rename out file as in file
    Result := Sys_Calls.Rename (Asu.To_String (Out_File_Name),
                                Asu.To_String (In_File_Name));
    if not Result then
      Error ("Cannot move " & Asu.To_String (Out_File_Name)
           & " to " & Asu.To_String (In_File_Name));
      Clean;
    end if;
  end Comit;

  -- Open Files
  procedure Open (File_Name : in String) is
    In_Fd, Out_Fd : Sys_Calls.File_Desc;
    File_Dir : Asu.Unbounded_String;
    use type Asu.Unbounded_String;
  begin
    In_File_Name := Asu.To_Unbounded_String (File_Name);
    Is_Stdin := File_Name = Std_In_Out;
    -- Open In fd and Out file if not stdin/stdout
    if Is_Stdin then
      In_Fd := Sys_Calls.Stdin;
      Out_Fd := Sys_Calls.Stdout;
      Out_File_Name := Asu.Null_Unbounded_String;
    else
      begin
        In_Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
      exception
        when Sys_Calls.Name_Error =>
          Error ("Cannot open input file " & File_Name);
      end;
      -- Build out file name
      File_Dir := Asu.To_Unbounded_String (Directory.DirName (File_Name));
      if File_Dir /= Asu.Null_Unbounded_String then
        -- Remove trailing /
        File_Dir := Asu.To_Unbounded_String (
          Asu.Slice (File_Dir, 1, Asu.Length(File_Dir) - 1));
      else
        File_Dir := Asu.To_Unbounded_String (".");
      end if;
      Out_File_Name := Asu.To_Unbounded_String (
                       Temp_File.Create (Asu.To_String (File_Dir)));
      begin
        Out_Fd := Sys_Calls.Create (Asu.To_String (Out_File_Name));
      exception
        when Sys_Calls.Name_Error =>
          Error ("Cannot create temp file " & Asu.To_String (Out_File_Name));
      end;
    end if;
    -- Associate fds to files
    Text_Line.Open (In_File,  Text_Line.In_File,  In_Fd);
    Text_Line.Open (Out_File, Text_Line.Out_File, Out_Fd);
    -- Get Search pattern characteristics
    Nb_Pattern := Search_Pattern.Number;
    Is_Multiple := Search_Pattern.Multiple;
  end Open;

  -- Reports an error
  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
        & " ERROR: " & Msg & ".");
    Close;
    Clean;
    raise Substit_Error;
  end Error;

  -- Read the number of lines and New_Lines requested
  Trail_New_Line : Boolean := False;
  function Read return Boolean is
    Nb_To_Read : Natural;
    Line : Asu.Unbounded_String;
    Len : Natural;
  begin
    -- Move to end
    if not Line_List_Mng.Is_Empty (Line_List) then
      Line_List_Mng.Rewind (Line_List, Line_List_Mng.Prev);
    end if;
    -- Compute amount to fill buffer (Nb lines)
    Nb_To_Read := Nb_Pattern - Line_List_Mng.List_Length (Line_List);
    -- Append trailing new line if any
    if Trail_New_Line then
      Line_List_Mng.Insert (Line_List,
                Asu.To_Unbounded_String (Text_Line.Line_Feed & ""));
      Trail_New_Line := False;
      Nb_To_Read := Nb_To_Read - 1;
    end if;

    -- Read and append remaining amount, save trailing newline
    while Nb_To_Read /= 0 loop
      Line := Text_Line.Get (In_File);
      Len := Asu.Length (Line);
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Read >" &  Asu.To_String (Line) & "<");
      end if;
      if Len = 0 then
        -- We reached the end of file
        return False;
      end if;
      -- There are either one or two items to push
      if Len > 1 and then Asu.Element(Line, Len) = Text_Line.Line_Feed then
        -- Line and Line_Feed
        -- Insert line (without Lf)
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
                Asu.To_Unbounded_String (Text_Line.Line_Feed & ""));
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
  procedure Flush_Lines;
  function Subst_Lines return Boolean;
  procedure Do_One_File (File_Name : in String; Backup : in Boolean) is
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
      exit when not Read;
      -- Process these lines
      if Subst_Lines then
        -- These lines have been substituted
        Modified := True;
      end if;
    end loop;
    -- Put remaining linesA (read but not matching)
    Flush_Lines;
    -- Close and cleanup files
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Done.");
    end if;
    Close;
    -- After close (stdout restored to standard output)
    --  put name of modified file
    if not Is_Stdin and then Modified then
      Comit (Backup);
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
  function Subst_One_Line (Line : Str_Access) return Boolean is
    Current : Positive;
    Matches : Boolean;
    Match_Res : Regular_Expressions.Match_Cell;
  begin
    -- Multiple substitutions in one line
    Current := 1;
    Matches := False;
    loop
      -- Search a Match from Current to Last
      Match_Res := Search_Pattern.Check (
         Asu.Slice (Line.all, Current, Asu.Length(Line.all)),
         1);
      exit when Match_Res.Start_Offset <= 0
      or else Match_Res.End_Offset <= 0;
      -- Found a match
      Matches := True;
      -- Get substituting string
      declare
        Replacing : constant String
                  := Replace_Pattern.Replace (Asu.Slice (Line.all,
                                            Match_Res.Start_Offset,
                                            Match_Res.End_Offset));
      begin
        -- Substitute from start to stop
        Asu.Replace_Slice (Line.all,
                           Match_Res.Start_Offset,
                           Match_Res.End_Offset,
                           Replacing);
        -- Next search index is the next char after the replaced string
        Current := Match_Res.Start_Offset + Replacing'Length;
        exit when Current > Asu.Length(Line.all);
      end;
    end loop;
    -- Put the (modified) line
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Putting >" & Asu.To_String (Line.all) & "<");
    end if;
    Text_Line.Put (Out_File, Asu.To_String (Line.all));
    -- Delete all
    Line_List_Mng.Delete_List (Line_List, False);
    return Matches;
  exception
    when Constraint_Error =>
      Error ("String too long substituting " & Asu.To_String (Line.all));
      return False;
  end Subst_One_Line;

  -- Put and flush lines that have been read but not match
  procedure Flush_Lines is
  begin
    if Line_List_Mng.Is_Empty (Line_List) then
      return;
    end if;
    Line_List_Mng.Rewind (Line_List);
    while not Line_List_Mng.Is_Empty (Line_List) loop
      Text_Line.Put (Out_File,
               Asu.To_String (Line_List_Mng.Access_Current (Line_List).all));
      if Debug.Set then
        Sys_Calls.Put_Line_Error (
            "Flushing >"
          & Asu.To_String (Line_List_Mng.Access_Current (Line_List).all)
          & "<");
      end if;
      Line_List_Mng.Delete (Line_List);
    end loop;
  end Flush_Lines;

  -- Check current list of lines vs search patterns
  function Subst_Lines return Boolean is
    Match_Res : Regular_Expressions.Match_Cell;
    Line, First_Line, Last_Line : Str_Access;
    Start, Stop : Positive;
    Matches : Boolean;
    Str_To_Replace : Asu.Unbounded_String;
  begin
    -- Rewind read lines
    Line_List_Mng.Rewind (Line_List);
    if Is_Multiple then
      -- Handle separately multiple substitutions if one pattern
      return Subst_One_Line (Line_List_Mng.Access_Current (Line_List));
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
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Not match " & I'Img
                  & " with >" & Asu.To_String (Line.all) & "<");
        end if;
        exit;
      end if;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Match " & I'Img
                & " with >" & Asu.To_String (Line.all) & "<");
      end if;
      -- Keep pos of start of first match and stop of last match
      if I = 1 then
        Start := Match_Res.Start_Offset;
      elsif I = Nb_Pattern then
        Stop := Match_Res.End_Offset;
      end if;
      -- Move to next input line
      if I /= Nb_Pattern then
        Line_List_Mng.Move_To (Line_List);
      end if;
    end loop;
    if not Matches then
      -- If not match, put first line and delete it
      Line_List_Mng.Rewind (Line_List);
      Line := Line_List_Mng.Access_Current (Line_List);
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Putting >" & Asu.To_String (Line.all) & "<");
      end if;
      Text_Line.Put (Out_File, Asu.To_String (Line.all));
      Line_List_Mng.Delete (Line_List);
    else
      -- Match, build string to replace: end of first line
      --  + all lines but last + start of last line
      Line_List_Mng.Rewind (Line_List);
      First_Line := Line_List_Mng.Access_Current (Line_List);
      Asu.Append (Str_To_Replace,
        Asu.Slice (First_Line.all, Start, Asu.Length (First_Line.all)));
      for I in 2 .. Nb_Pattern - 1 loop
        Line_List_Mng.Move_To (Line_List);
        Line := Line_List_Mng.Access_Current (Line_List);
        Asu.Append (Str_To_Replace, Line.all);
      end loop;
      Line_List_Mng.Move_To (Line_List);
      Last_Line := Line_List_Mng.Access_Current (Line_List);
      Asu.Append (Str_To_Replace, Asu.Slice (Last_Line.all, 1, Stop));
      -- Make replacing string
      declare
        Replacing : constant String
                  := Replace_Pattern.Replace (Asu.To_String (Str_To_Replace));
      begin
        -- Put result: beginning of first line + replacing + end of last line
        if Debug.Set then
          Sys_Calls.Put_Line_Error (
            "Putting >"
            & Asu.Slice (First_Line.all, 1, Start - 1)
            & Replacing
            & Asu.Slice (Last_Line.all, Stop + 1, Asu.Length (Last_Line.all))
            & "<");
        end if;
        Text_Line.Put (Out_File,
              Asu.Slice (First_Line.all, 1, Start - 1)
            & Replacing
            & Asu.Slice (Last_Line.all, Stop + 1, Asu.Length (Last_Line.all)));
      end;
      -- Delete all
      Line_List_Mng.Delete_List (Line_List, False);
    end if;
    return Matches;
   end Subst_Lines;

end Substit;

