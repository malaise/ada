with Ada.Text_Io, Ada.Strings.Unbounded;
with Argument, Sys_Calls, Text_Line, Temp_File, Dynamic_List,
     Regular_Expressions, Directory, Copy_File, File_Access, Mixed_Str,
     Int_Image;
with Search_Pattern, Replace_Pattern, Debug;
package body Substit is

  package Asu renames Ada.Strings.Unbounded;

  -- List of strings
  package Line_List_Dyn_Mng is new Dynamic_List (Asu.Unbounded_String);
  package Line_List_Mng renames Line_List_Dyn_Mng.Dyn_List;
  Line_List : Line_List_Mng.List_Type;
  subtype Str_Access is Line_List_Dyn_Mng.Element_Access;

  -- File names
  In_File_Name : Asu.Unbounded_String;
  Out_File_Name : Asu.Unbounded_String;

  -- Files
  In_File : Text_Line.File_Type;
  Out_File : Text_Line.File_Type;

  -- We work on stdin/stdout?
  Is_Stdin : Boolean;

  -- Number of patterns and is it iterative
  Nb_Pattern : Positive;
  Is_Iterative : Boolean;

  -- Line or block separator
  Delimiter : Asu.Unbounded_String;

  -- Current line number
  Line_No : Long_Long_Natural;
  function Line_Image is new Int_Image (Long_Long_Natural);

  -- Display error. If Give_Up then also cleanup and raise Substit_Error
  procedure Error (Msg : in String; Give_Up : in Boolean := True);

  -- Check that this is a file we can read and write
  procedure Check_File (File_Name : in String) is
    Path_Name : constant String := Directory.Dirname (File_Name);
    use type Sys_Calls.File_Kind_List;
    -- Get dir name of file, following symbolic links
    function Dir_Name return String is
      Stat : Sys_Calls.File_Stat_Rec;
    begin
      if Path_Name = "" then
        return "./";
      else
        Stat := Sys_Calls.File_Stat (Path_Name);
        if Stat.Kind = Sys_Calls.Link then
          -- Resolve sym link recursively
          return Directory.Read_Link (Path_Name);
        else
          -- Kind is checked later
          return Path_Name;
        end if;
      end if;
    end Dir_Name;
    Stat : Sys_Calls.File_Stat_Rec;
    Can_Read, Can_Write, Can_Exec : Boolean;
  begin
    -- Directory must be a dir with wrx access
    Stat := Sys_Calls.File_Stat (Dir_Name);
    if Stat.Kind /= Sys_Calls.Dir then
      Error ("Directory " & Dir_Name & " is of incorrect kind: "
           & Mixed_Str (Stat.Kind'Img));
    end if;
    File_Access (Sys_Calls.Get_Effective_User_Id, Sys_Calls.Get_Effective_Group_Id,
                 Stat.User_Id, Stat.Group_Id, Stat.Rights,
                 Can_Read, Can_Write, Can_Exec);
    if not (Can_Read and then Can_Write and then Can_Exec) then
      Error ("Directory " & Dir_Name & " has incorrect access rights");
    end if;
    -- This must be a file, with rw access
    begin
      Stat := Sys_Calls.File_Stat (File_Name);
    exception
      when others =>
        Error ("Cannot access file " & File_Name);
    end;
    if Stat.Kind /= Sys_Calls.File then
      Error ("File " & File_Name & " is of incorrect kind: "
           & Mixed_Str (Stat.Kind'Img));
    end if;
    File_Access (Sys_Calls.Get_Effective_User_Id, Sys_Calls.Get_Effective_Group_Id,
                 Stat.User_Id, Stat.Group_Id, Stat.Rights,
                 Can_Read, Can_Write, Can_Exec);
    if not (Can_Read and then Can_Write) then
      Error ("File " & File_Name & " has incorrect access rights");
    end if;
  end Check_File;

  -- Close files
  procedure Close is
  begin
    -- Close in file
    if Text_Line.Is_Open (In_File) then
      if not Is_Stdin then
        begin
          Sys_Calls.Close (Text_Line.Get_Fd (In_File));
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
          Sys_Calls.Close (Text_Line.Get_Fd (Out_File));
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
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    if Asu.Length (Out_File_Name) /= 0
    and then Sys_Calls.File_Check (Asu.To_String (Out_File_Name)) then
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

    -- Propagate access rights from In_File to Out_File
    begin
      Sys_Calls.Set_Rights (Asu.To_String (Out_File_Name),
          Sys_Calls.File_Stat (Asu.To_String (In_File_Name)).Rights);
    exception
      when others =>
        Error ("Cannot propagate rights of " & Asu.To_String (In_File_Name)
             & " to " & Asu.To_String (Out_File_Name));
    end;

    -- Rename out file as in file
    Result := Sys_Calls.Rename (Asu.To_String (Out_File_Name),
                                Asu.To_String (In_File_Name));
    if not Result then
      -- Rename failed, perhaps not the same file system
      -- Try a Copy then Delete
      Result := Copy_File (Asu.To_String (Out_File_Name),
                                Asu.To_String (In_File_Name));
      if not Result then
        Error ("Cannot move " & Asu.To_String (Out_File_Name)
             & " to " & Asu.To_String (In_File_Name));
      end if;
      -- This should work, anyway not a real problem if it fails
      Result := Sys_Calls.Unlink (Asu.To_String (Out_File_Name));
      if not Result then
        Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
          & " WARNING: Cannot remove temporary file "
          & Asu.To_String (Out_File_Name) & ".");
      end if;
    end if;

  end Comit;

  -- Open Files
  procedure Open (File_Name : in String;
                  Tmp_Dir   : in String) is
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
      -- Check access rights (rw) of this file
      Check_File (File_Name);
      begin
        In_Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
      exception
        when Sys_Calls.Name_Error =>
          Error ("Cannot open input file " & File_Name);
      end;
      -- Build out file dir
      if Tmp_Dir = "" then
        -- Current dir
        File_Dir := Asu.To_Unbounded_String (Directory.Dirname (File_Name));
        if File_Dir /= Asu.Null_Unbounded_String then
          -- Remove trailing /
          File_Dir := Asu.To_Unbounded_String (
            Asu.Slice (File_Dir, 1, Asu.Length(File_Dir) - 1));
        else
          File_Dir := Asu.To_Unbounded_String (".");
        end if;
      else
        -- Tmp dir specified as argument
        File_Dir := Asu.To_Unbounded_String (Tmp_Dir);
      end if;
      -- Create out file
      begin
        Out_File_Name := Asu.To_Unbounded_String (
                         Temp_File.Create (Asu.To_String (File_Dir)));
      exception
        when others =>
          Error ("Cannot create temp file in """ & Asu.To_String (File_Dir)
               & """");
      end;
      begin
        Out_Fd := Sys_Calls.Open (Asu.To_String (Out_File_Name),
                                  Sys_Calls.Out_File);
      exception
        when Sys_Calls.Name_Error =>
          Error ("Cannot open temp file " & Asu.To_String (Out_File_Name));
      end;
    end if;
    -- Associate fds to files
    Text_Line.Open (In_File,  Text_Line.In_File,  In_Fd);
    -- Set specific delimited of In_File
    Text_Line.Set_Line_Feed (In_File, Search_Pattern.Get_Delimiter);
    Text_Line.Open (Out_File, Text_Line.Out_File, Out_Fd);
    -- Get Search pattern characteristics
    Nb_Pattern := Search_Pattern.Number;
    Is_Iterative := Search_Pattern.Iterative;
    -- Init number of line
    Line_No := 0;
  end Open;

  -- Reports an error
  procedure Error (Msg : in String; Give_Up : in Boolean := True) is
  begin
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
        & " ERROR: " & Msg & ".");
    if Give_Up then
      Close;
      Clean;
      raise Substit_Error;
    end if;
  end Error;

  -- Read the number of lines and New_Lines needed to fill Line_List
  Trail_Line_Feed : Boolean := False;
  function Read return Boolean is
    Nb_To_Read : Natural;
    Line : Asu.Unbounded_String;
    Len : Natural;
    Line_Feed : constant Asu.Unbounded_String
              :=  Asu.To_Unbounded_String (Text_Line.Get_Line_Feed (In_File));
  begin
    -- Move to end
    if not Line_List_Mng.Is_Empty (Line_List) then
      Line_List_Mng.Rewind (Line_List, Line_List_Mng.Prev);
    end if;
    -- Compute amount to fill buffer (Nb lines)
    Nb_To_Read := Nb_Pattern - Line_List_Mng.List_Length (Line_List);
    -- Append trailing new line if any
    if Trail_Line_Feed then
      Line_List_Mng.Insert (Line_List, Line_Feed);
      Trail_Line_Feed := False;
      Nb_To_Read := Nb_To_Read - 1;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Read added trailing line feed");
      end if;
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
      if Len >= 1
      and then Asu.Element(Line, Len) = Text_Line.Line_Feed_Char then
        -- Line (possibly empty) and Line_Feed
        -- Insert line (without Lf)
        Line_List_Mng.Insert (Line_List,
              Asu.To_Unbounded_String (Asu.Slice (Line, 1, Len-1)));
        Nb_To_Read := Nb_To_Read - 1;
        Line_No := Line_No + 1;
        if Nb_To_Read = 0 then
          -- Nl remains for next read
          Trail_Line_Feed := True;
        else
          -- Insert Nl
          Line_List_Mng.Insert (Line_List, Line_Feed);
          Nb_To_Read := Nb_To_Read - 1;
        end if;
      else
        -- Line without Nl (last line)
        -- Insert it
        Line_List_Mng.Insert (Line_List, Line);
        Nb_To_Read := Nb_To_Read - 1;
        -- Last line is without Nl
        Line_No := Line_No + 1;
      end if;
    end loop;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Read line no " &  Line_No'Img);
    end if;
    return True;
  end Read;

  -- Process one file (stdin -> stdout if File_Name is Std_In_Out)
  procedure Flush_Lines;
  function Subst_Lines (Max_Subst : Long_Long_Natural;
                        Verbose   : Boolean;
                        Grep      : Boolean;
                        Line_Nb   : Boolean;
                        Test    : Boolean) return Long_Long_Natural;

  function Do_One_File (File_Name : String;
                        Tmp_Dir   : String;
                        Delimiter : String;
                        Max_Subst : Long_Long_Natural;
                        Backup    : Boolean;
                        Verbose   : Boolean;
                        Grep      : Boolean;
                        Line_Nb   : Boolean;
                        Test      : Boolean) return Long_Long_Natural is
    Total_Subst : Long_Long_Natural;
    Remain_Subst : Long_Long_Natural;
    Nb_Subst : Long_Long_Natural;
    Do_Verbose : Boolean;
  begin
    -- Open files
    Open (File_Name, Tmp_Dir);
    -- Verbose if requested and not stdin
    Do_Verbose := Verbose and then not Is_Stdin;

    -- Init buffer of lines
    Line_List_Mng.Delete_List (Line_List);
    Trail_Line_Feed := False;
    Substit.Delimiter := Asu.To_Unbounded_String (Delimiter);
    -- Init substitution by reading Nb_Pattern lines and Newlines
    -- Loop on substit
    Total_Subst := 0;
    Nb_Subst := 0;
    Remain_Subst := Max_Subst;
    loop
      -- Done when the amount of lines cannot be read
      exit when not Read;
      -- If grep is iterative with a replace and got something,
      --  then append a line feed
      if Grep and then Is_Iterative and then Nb_Subst /= 0
      and then not Replace_Pattern.Is_Empty then
        Ada.Text_Io.New_Line;
      end if;
      -- Process these lines
      Nb_Subst := Subst_Lines (Remain_Subst, Do_Verbose, Grep, Line_Nb, Test);
      Total_Subst := Total_Subst + Nb_Subst;
      -- Done when amount of substitutions reached
      if Max_Subst /= 0 then
        exit when Max_Subst = Total_Subst;
        Remain_Subst := Max_Subst - Total_Subst;
      end if;
    end loop;
    -- Put remaining lines (read but not matching, or not read)
    if not Grep then
      Flush_Lines;
    end if;
    -- Close and cleanup files
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Done.");
    end if;
    Close;
    -- After close, comit or clean
    if not Is_Stdin
    and then Total_Subst /= 0
    and then not Test then
      Comit (Backup);
    else
      Clean;
    end if;
    return Total_Subst;
  exception
    when Replace_Pattern.Command_Error =>
      -- Rollback on this file
      Close;
      Clean;
      return 0;
    when others =>
      -- Rollback and stop
      Close;
      Clean;
      raise;
  end Do_One_File;

  -- Handle multiple substitutions within one line
  function Subst_One_Line (Line      : Str_Access;
                           Max_Subst : Long_Long_Natural;
                           Verbose   : Boolean;
                           Grep      : Boolean;
                           Line_Nb   : Boolean;
                           Test      : Boolean) return Long_Long_Natural is
    Current : Positive;
    Nb_Match : Long_Long_Natural;
    Match_Res : Regular_Expressions.Match_Cell;
  begin
    -- Multiple substitutions in one line
    Current := 1;
    Nb_Match := 0;
    loop
      -- Search a Match from Current to Last
      -- Exit when no (more) match
      exit when not Search_Pattern.Check (Asu.To_String (Line.all),
                        Current, Search => True, Regex_Index => 1);

      -- Found a match
      Match_Res := Search_Pattern.Str_Indexes;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Match in end of line >"
           & Asu.Slice (Line.all, Current, Asu.Length(Line.all))
           & "< from" & Match_Res.First_Offset'Img
           & " to" & Match_Res.Last_Offset_Stop'Img);
      end if;

      -- Check if this matching patterm matches the exclusion rule
      if Search_Pattern.Check (
          Asu.Slice (Line.all, Match_Res.First_Offset,
                               Match_Res.Last_Offset_Stop),
          Start => Match_Res.First_Offset,
          Search => False, Regex_Index => 1) then
        -- Str matches the find criteria but also the exclude criteria: skip
        if Debug.Set then
          Sys_Calls.Put_Line_Error
              ("Match >" & Asu.Slice (Line.all, Match_Res.First_Offset,
                                      Match_Res.Last_Offset_Stop)
               & "< discarded because matching exclusion");
        end if;
        Current := Match_Res.First_Offset + 1;
        exit when Current > Asu.Length(Line.all);
      else
        -- Str matches the find criteria and does not match the exclude
        --  criteria: OK
        -- Get substituting string
        declare
          Replacing : constant String := Replace_Pattern.Replace;
        begin
          Nb_Match := Nb_Match + 1;
          -- Display verbose substitution
          if Verbose then
            Ada.Text_Io.Put_Line (
                Line_No'Img & " : "
              & Asu.Slice (Line.all, Match_Res.First_Offset,
                                     Match_Res.Last_Offset_Stop)
              & " -> " & Replacing);
          elsif Grep then
            if Nb_Match = 1 and then not Is_Stdin then
              Ada.Text_Io.Put (Asu.To_String (In_File_Name) & ":");
              if Line_Nb then
                Ada.Text_Io.Put (Line_Image(Line_No) & ":");
              end if;
            end if;
            if Replace_Pattern.Is_Empty then
              -- Display once each matching line
              Ada.Text_Io.Put_Line (Asu.To_String (Line.all));
              exit;
            else
              -- Display each replaced (line feed is handled in Do_One_File)
              Ada.Text_Io.Put (Replacing);
            end if;
          end if;
          if not Test then
            -- Substitute from start to stop
            if Debug.Set then
              Sys_Calls.Put_Line_Error ("Replacing by "
                & Asu.Slice (Line.all, Match_Res.First_Offset,
                                       Match_Res.Last_Offset_Stop)
                & " -> " & Replacing);
            end if;
            Asu.Replace_Slice (Line.all,
                               Match_Res.First_Offset,
                               Match_Res.Last_Offset_Stop,
                               Replacing);
            -- Next search index is the next char after the replaced string
            Current := Match_Res.First_Offset + Replacing'Length;
          else
            Current := Match_Res.Last_Offset_Stop + 1;
          end if;
          exit when Current > Asu.Length(Line.all);
        end;
      end if;
      -- Exit when number of subtitution is reached
      --  (Max_Subst may be 0 for infinite)
      exit when Nb_Match = Max_Subst;
    end loop;

    if not Grep then
      -- Put the (modified) line
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Putting >" & Asu.To_String (Line.all) & "<");
      end if;
      Text_Line.Put (Out_File, Asu.To_String (Line.all));
    end if;
    -- Delete all
    Line_List_Mng.Delete_List (Line_List, False);
    return Nb_Match;
  exception
    when Constraint_Error =>
      Error ("String too long substituting " & Asu.To_String (Line.all));
      return 0;
  end Subst_One_Line;

  -- Put and flush lines that have been read but not match,
  --  then lines that have not been read
  procedure Flush_Lines is
  begin
    -- Put and flush lines that have been read but not match,
    if not Line_List_Mng.Is_Empty (Line_List) then
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
    end if;
    --  Put lines that have not been read
    loop
      declare
        Str : constant String := Text_Line.Get (In_File);
      begin
        exit when Str = "";
        Text_Line.Put (Out_File, Str);
      end;
    end loop;
  end Flush_Lines;

  -- Check current list of lines vs search patterns
  function Subst_Lines (Max_Subst : Long_Long_Natural;
                        Verbose   : Boolean;
                        Grep      : Boolean;
                        Line_Nb   : Boolean;
                        Test      : Boolean) return Long_Long_Natural is
    Match_Res : Regular_Expressions.Match_Cell;
    Line, First_Line, Last_Line : Str_Access;
    Matches, Excluded : Boolean;
    -- Put matching text, complete lines text or just the matching text
    procedure Put_Match (Complete : in Boolean) is
      use type Str_Access;
    begin
      if Last_Line = First_Line then
        -- Handle specific case of only one line
        if Complete then
          Ada.Text_Io.Put (Asu.To_String (Last_Line.all));
        else
          Ada.Text_Io.Put (Asu.Slice (Last_Line.all,
                           Match_Res.First_Offset,
                           Match_Res.Last_Offset_Stop));
        end if;
        return;
      end if;

      if Complete then
        Ada.Text_Io.Put (Asu.To_String (First_Line.all));
      else
        Ada.Text_Io.Put (Asu.Slice (First_Line.all,
                                    Match_Res.First_Offset,
                                    Asu.Length (First_Line.all)));
      end if;
      Line_List_Mng.Rewind (Line_List);
      for I in 2 .. Nb_Pattern - 1 loop
        Line_List_Mng.Move_To (Line_List);
        Line := Line_List_Mng.Access_Current (Line_List);
        Ada.Text_Io.Put (Asu.To_String (Line.all));
      end loop;
      if Complete then
        Ada.Text_Io.Put (Asu.To_String (Last_Line.all));
      else
        Ada.Text_Io.Put (Asu.Slice (Last_Line.all,
                        1, Match_Res.Last_Offset_Stop));
      end if;
    end Put_Match;
  begin
    -- Rewind read lines
    Line_List_Mng.Rewind (Line_List);
    if Is_Iterative then
      -- Handle separately multiple substitutions if one pattern
      return Subst_One_Line (Line_List_Mng.Access_Current (Line_List),
                             Max_Subst, Verbose, Grep, Line_Nb, Test);
    end if;

    -- Check all patterns until one does not match
    for I in 1 .. Nb_Pattern loop
      -- Check this read line
      Line := Line_List_Mng.Access_Current (Line_List);
      Matches := Search_Pattern.Check (Asu.To_String (Line.all), 1,
                 Search => True, Regex_Index => I);
      if not Matches then
        -- This one does not match
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Not match " & I'Img
                  & " with >" & Asu.To_String (Line.all) & "<");
        end if;
        exit;
      end if;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Line >" & Asu.To_String (Line.all)
                                & "< matches pattern No" & I'Img);
      end if;
      -- Move to next input line
      if I /= Nb_Pattern then
        Line_List_Mng.Move_To (Line_List);
      end if;
    end loop;

    if Matches then
      -- Check if it matches all exclusion rules
      Excluded := True;
      Match_Res := Search_Pattern.Str_Indexes;
      Line_List_Mng.Rewind (Line_List);
      for I in 1 .. Nb_Pattern loop
        -- Check this read line
        Line := Line_List_Mng.Access_Current (Line_List);
        if I = 1 then
          Matches := Search_Pattern.Check (Asu.To_String (Line.all),
                     Start =>  Match_Res.First_Offset,
                     Search => False, Regex_Index => I);
        elsif I /= Nb_Pattern then
          Matches := Search_Pattern.Check (Asu.To_String (Line.all),
                     Start => 1,
                     Search => False, Regex_Index => I);
        else
          Matches := Search_Pattern.Check (
                     Asu.Slice (Line.all, 1, Match_Res.Last_Offset_Stop),
                     Start => 1,
                     Search => False, Regex_Index => I);
        end if;
        if not Matches then
          -- This one does not match this exclusion: OK
          Excluded := False;
          exit;
        end if;
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Line >" & Asu.To_String (Line.all)
                                  & "< matches exclusion No" & I'Img);
        end if;
        -- Move to next input line
        if I /= Nb_Pattern then
          Line_List_Mng.Move_To (Line_List);
        end if;
      end loop;
      if Excluded then
        if Debug.Set then
          Line_List_Mng.Rewind (Line_List);
          Line := Line_List_Mng.Access_Current (Line_List);
          Sys_Calls.Put_Line_Error ("Line >" & Asu.To_String (Line.all)
                                  & "< is excluded");
        end if;
        Matches := False;
      else
        Matches := True;
      end if;
    end if;
    if Matches then
      -- Match, build string to replace:
      Match_Res := Search_Pattern.Str_Indexes;
      -- Get access to first and last lines of input
      Line_List_Mng.Rewind (Line_List);
      First_Line := Line_List_Mng.Access_Current (Line_List);
      Line_List_Mng.Rewind (Line_List, Line_List_Mng.Prev);
      Last_Line := Line_List_Mng.Access_Current (Line_List);
      -- Result string is -> Start of first line + Replacing
      --                   + End of last line
      declare
        Str_Replacing : constant String := Replace_Pattern.Replace;
        Str_Replaced : Asu.Unbounded_String;
        use type Asu.Unbounded_String;
      begin
        -- Set result: beginning of first line + replacing + end of last line
        Str_Replaced := Asu.To_Unbounded_String (
                 Asu.Slice (First_Line.all, 1, Match_Res.First_Offset - 1))
               & Str_Replacing;
        if Match_Res.Last_Offset_Stop < Asu.Length (Last_Line.all) then
          -- This would raise Constraint_Error if Stop = Length
          Asu.Append (Str_Replaced,
              Asu.Slice (Last_Line.all,
                         Match_Res.Last_Offset_Stop + 1,
                         Asu.Length (Last_Line.all)));
        end if;
        if Verbose then
          -- Display verbose substitution
          Ada.Text_Io.Put (
              Long_Long_Natural'Image(Line_No
                                    - Long_Long_Natural(Nb_Pattern) / 2)
            & " : ");
          Put_Match (False);
          Ada.Text_Io.Put_Line (" -> " & Str_Replacing);
        elsif Grep then
          -- Display grep result
          if not Is_Stdin then
            Ada.Text_Io.Put (Asu.To_String (In_File_Name & ":"));
            if Line_Nb then
              Ada.Text_Io.Put (Line_Image(Line_No) & ":");
            end if;
          end if;
          if Replace_Pattern.Is_Empty then
            Put_Match (True);
            Ada.Text_Io.New_Line;
          else
            Ada.Text_Io.Put_Line (Str_Replacing);
          end if;
        end if;
        if not Test then
          -- Write result
          if Debug.Set then
            Sys_Calls.Put_Line_Error ("Putting >" & Asu.To_String (Str_Replaced) & "<");
          end if;
          Text_Line.Put (Out_File, Asu.To_String (Str_Replaced));
          -- Delete all
          Line_List_Mng.Delete_List (Line_List, False);
        end if;
      end;
    end if;
    if not Matches or else Test then
      -- If not match or test, put first line and delete it
      Line_List_Mng.Rewind (Line_List);
      Line := Line_List_Mng.Access_Current (Line_List);
      if not Grep then
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Putting >" & Asu.To_String (Line.all) & "<");
        end if;
        Text_Line.Put (Out_File, Asu.To_String (Line.all));
      end if;
      Line_List_Mng.Delete (Line_List);
    end if;
    -- Return number of subtitutions performed
    if Matches then
      return 1;
    else
      return 0;
    end if;
   end Subst_Lines;

end Substit;

