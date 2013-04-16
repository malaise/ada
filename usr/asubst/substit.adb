with As.U.Utils;
with Argument, Sys_Calls, Text_Line, Temp_File, Regular_Expressions, Directory,
     Copy_File, File_Access, Mixed_Str, Images, Basic_Proc;
with Search_Pattern, Replace_Pattern, Debug;
package body Substit is

  -- List of strings
  package Line_List_Mng renames As.U.Utils.Asu_Dyn_List_Mng;
  Line_List : Line_List_Mng.List_Type;
  subtype Str_Access is As.U.Utils.Asu_Us_Access;

  -- File names
  In_File_Name : As.U.Asu_Us;
  Out_File_Name : As.U.Asu_Us;

  -- Files
  In_File : Text_Line.File_Type;
  Out_File : Text_Line.File_Type;

  -- We work on stdin/stdout?
  Is_Stdin : Boolean;

  -- Number of patterns and is it iterative
  Nb_Pattern : Positive;
  Is_Iterative : Boolean;

  -- Line or block separator
  Delimiter : As.U.Asu_Us;

  -- Current line number
  Line_No : Long_Long_Natural;
  function Line_Image is new Images.Int_Image (Long_Long_Natural);

  -- Display error. If Give_Up then also cleanup and raise Substit_Error
  procedure Error (Msg : in String; Give_Up : in Boolean := True);

  -- Check that this is a file we can read and write
  procedure Check_File (File_Name : in String;
                        For_Write : in Boolean) is
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
    exception
      when others =>
        Error ("Cannot access directory " & Path_Name);
        raise Substit_Error;
    end Dir_Name;
    Stat : Sys_Calls.File_Stat_Rec;
    Can_Read, Can_Write, Can_Exec : Boolean;
  begin
    -- Directory must be a dir with (w)rx access
    Stat := Sys_Calls.File_Stat (Dir_Name);
    if Stat.Kind /= Sys_Calls.Dir then
      Error ("Directory " & Dir_Name & " is of incorrect kind: "
           & Mixed_Str (Stat.Kind'Img));
    end if;
    File_Access (Sys_Calls.Get_Effective_User_Id, Sys_Calls.Get_Effective_Group_Id,
                 Stat.User_Id, Stat.Group_Id, Stat.Rights,
                 Can_Read, Can_Write, Can_Exec);
    -- Read => rx, Write => rwx
    if not Can_Read
    or else not Can_Exec
    or else (For_Write and then not Can_Write) then
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
    -- Read => r, Write => rw
    if not Can_Read
    or else (For_Write and then not Can_Write) then
      Error ("File " & File_Name & " has incorrect access rights");
    end if;
  end Check_File;

  -- Close files
  procedure Close is
  begin
    -- Close in file
    if In_File.Is_Open then
      if not Is_Stdin then
        begin
          Sys_Calls.Close (In_File.Get_Fd);
        exception
          when others => null;
        end;
      end if;
      begin
        In_File.Close;
      exception
        when others => null;
      end;
    end if;
    -- Flush and close Out file
    if Out_File.Is_Open then
      Out_File.Flush;
      if not Is_Stdin then
        begin
          Sys_Calls.Close (Out_File.Get_Fd);
        exception
          when others => null;
        end;
      end if;
      begin
        Out_File.Close;
      exception
        when others => null;
      end;
    end if;
  end Close;

  -- Remove Out file
  procedure Clean is
  begin
    if Out_File_Name.Length /= 0
    and then Sys_Calls.File_Check (Out_File_Name.Image) then
      -- File exists => remove
      Sys_Calls.Unlink (Out_File_Name.Image);
    end if;
  exception
    when others => null;
  end Clean;

  procedure Comit (Backup : in Boolean) is
    Result : Boolean;
  begin
    if Backup then
      -- Copy in file as .asu if Backup
      Result := Copy_File (In_File_Name.Image,
                           In_File_Name.Image & ".asu");
      if not Result then
        Error ("Cannot copy " & In_File_Name.Image
             & " to " & In_File_Name.Image & ".asu");
        Clean;
        return;
      end if;
    end if;

    -- Propagate access rights from In_File to Out_File
    begin
      Sys_Calls.Set_Rights (Out_File_Name.Image,
          Sys_Calls.File_Stat (In_File_Name.Image).Rights);
    exception
      when others =>
        Error ("Cannot propagate rights of " & In_File_Name.Image
             & " to " & Out_File_Name.Image);
    end;

    -- Rename out file as in file
    Result := Sys_Calls.Rename (Out_File_Name.Image,
                                In_File_Name.Image);
    if not Result then
      -- Rename failed, perhaps not the same file system
      -- Try a Copy then Delete
      Result := Copy_File (Out_File_Name.Image,
                           In_File_Name.Image);
      if not Result then
        Error ("Cannot move " & Out_File_Name.Image
             & " to " & In_File_Name.Image);
      end if;
      -- This should work, anyway not a real problem if it fails
      Result := Sys_Calls.Unlink (Out_File_Name.Image);
      if not Result then
        Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
          & " WARNING: Cannot remove temporary file "
          & Out_File_Name.Image & ".");
      end if;
    end if;

  end Comit;

  -- Open Files
  procedure Open (File_Name : in String;
                  Tmp_Dir   : in String;
                  For_Write : in Boolean) is
    In_Fd, Out_Fd : Sys_Calls.File_Desc;
    File_Dir : As.U.Asu_Us;
  begin
    In_File_Name := As.U.Tus (File_Name);
    Is_Stdin := File_Name = Std_In_Out;
    -- Open In fd and Out file if not stdin/stdout
    if Is_Stdin then
      In_Fd := Sys_Calls.Stdin;
      Out_Fd := Sys_Calls.Stdout;
      Out_File_Name.Set_Null;
    else
      -- Check access rights (rw) of this file
      Check_File (File_Name, For_Write);
      begin
        In_Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
      exception
        when Sys_Calls.Name_Error =>
          Error ("Cannot open input file " & File_Name);
      end;
      if For_Write then
        -- Build out file dir
        if Tmp_Dir = "" then
          -- Current dir
          File_Dir := As.U.Tus (Directory.Dirname (File_Name));
          if not File_Dir.Is_Null then
            -- Remove trailing /
            File_Dir := As.U.Tus (File_Dir.Slice (1, File_Dir.Length - 1));
          else
            File_Dir := As.U.Tus (".");
          end if;
        else
          -- Tmp dir specified as argument
          File_Dir := As.U.Tus (Tmp_Dir);
        end if;
        -- Create out file
        begin
          Out_File_Name := As.U.Tus (Temp_File.Create (File_Dir.Image));
        exception
          when others =>
            Error ("Cannot create temp file in """ & File_Dir.Image
                 & """");
        end;
        begin
          Out_Fd := Sys_Calls.Open (Out_File_Name.Image, Sys_Calls.Out_File);
        exception
          when Sys_Calls.Name_Error =>
            Error ("Cannot open temp file " & Out_File_Name.Image);
        end;
      end if;
    end if;
    -- Associate fds to files
    In_File.Open (Text_Line.In_File, In_Fd);
    -- Set specific delimited of In_File
    In_File.Set_Line_Feed (Search_Pattern.Get_Delimiter);
    if For_Write then
      Out_File.Open (Text_Line.Out_File, Out_Fd);
    end if;
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
    Line : As.U.Asu_Us;
    Len : Natural;
    Line_Feed : constant As.U.Asu_Us := As.U.Tus (In_File.Get_Line_Feed);
    Feed_Len : constant Natural := Line_Feed.Length;
    use type As.U.Asu_Us;
  begin
    -- Move to end
    Line_List.Rewind (False, Line_List_Mng.Prev);
    -- Compute amount to fill buffer (Nb lines)
    Nb_To_Read := Nb_Pattern - Line_List.List_Length;
    -- Append trailing new line if any
    if Trail_Line_Feed then
      Line_List.Insert (Line_Feed);
      Trail_Line_Feed := False;
      Nb_To_Read := Nb_To_Read - 1;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Read added trailing line feed");
      end if;
    end if;

    -- Read and append remaining amount, save trailing newline
    while Nb_To_Read /= 0 loop
      Line := In_File.Get;
      Len := Line.Length;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Read >" &  Line.Image & "<");
      end if;
      if Len = 0 then
        -- We reached the end of file
        return False;
      end if;
      -- There are either one or two items to push
      if Feed_Len /= 0
      and then Len >= Feed_Len
      and then Line.Slice(Len - Feed_Len + 1, Len) = Line_Feed then
        -- Line (possibly empty) and Line_Feed
        -- Insert line (without Lf)
        Line_List.Insert (As.U.Tus (Line.Slice (1, Len - Feed_Len)));
        Nb_To_Read := Nb_To_Read - 1;
        Line_No := Line_No + 1;
        if Nb_To_Read = 0 then
          -- Nl remains for next read
          Trail_Line_Feed := True;
        else
          -- Insert Nl
          Line_List.Insert (Line_Feed);
          Nb_To_Read := Nb_To_Read - 1;
        end if;
      else
        -- Line without Lf (last line)
        -- Insert it
        Line_List.Insert (Line);
        Nb_To_Read := Nb_To_Read - 1;
        -- Last line is without Nl
        Line_No := Line_No + 1;
      end if;
    end loop;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Read up to line no " &  Line_No'Img
                 & (if Trail_Line_Feed then " & trail" else ""));
    end if;
    return True;
  end Read;

  -- Process one file (stdin -> stdout if File_Name is Std_In_Out)
  procedure Flush_Lines;
  procedure Subst_Lines (Match_Range    : in String;
                         Verbose        : in Boolean;
                         Grep           : in Boolean;
                         Grep_List      : in Boolean;
                         Grep_File_Name : in Boolean;
                         Grep_Line_Nb   : in Boolean;
                         Grep_Invert    : in Boolean;
                         Test           : in Boolean;
                         Nb_Match       : in out Long_Long_Natural;
                         Loc_Subst      : out Long_Long_Natural;
                         Done_File      : out Boolean);

  function Do_One_File (File_Name      : String;
                        Tmp_Dir        : String;
                        Delimiter      : String;
                        Match_Range    : String;
                        Backup         : Boolean;
                        Verbose        : Boolean;
                        Grep           : Boolean;
                        Grep_List      : Boolean;
                        Grep_File_Name : Boolean;
                        Grep_Line_Nb   : Boolean;
                        Grep_Invert    : Boolean;
                        Test           : Boolean) return Long_Long_Natural is
    Done_File : Boolean;
    Nb_Subst : Long_Long_Natural;
    Nb_Match : Long_Long_Natural;
    Loc_Subst : Long_Long_Natural;
    Do_Verbose : Boolean;
  begin
    -- Open files: test is set if no need to write
    Open (File_Name, Tmp_Dir, not Grep);
    -- Verbose if requested and not stdin
    Do_Verbose := Verbose and then not Is_Stdin;

    -- Init buffer of lines
    Line_List.Delete_List;
    Trail_Line_Feed := False;
    Substit.Delimiter := As.U.Tus (Delimiter);
    -- Init substitution by reading Nb_Pattern lines and Newlines
    -- Loop on substit
    Done_File := False;
    Nb_Subst := 0;
    Nb_Match := 0;
    Loc_Subst := 0;
    loop
      -- Done when file is already put in List mode
      --   or when the amount of lines cannot be read
      exit when Done_File or else not Read;
      -- If grep is iterative with a replace and got something,
      --  then append a line feed
      if Grep and then Is_Iterative and then Loc_Subst /= 0
      and then not Replace_Pattern.Is_Empty then
        Basic_Proc.New_Line_Output;
      end if;
      -- Process these lines
      Subst_Lines (Match_Range, Do_Verbose, Grep, Grep_List, Grep_File_Name,
                   Grep_Line_Nb, Grep_Invert, Test, Nb_Match, Loc_Subst,
                   Done_File);
      Nb_Subst := Nb_Subst + Loc_Subst;
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
    and then Nb_Subst /= 0
    and then not Test then
      Comit (Backup);
    else
      Clean;
    end if;
    return Nb_Subst;
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

  -- Handle multiple substitutions of a char by a char within one line
  procedure Subst_Chars (Line         : access As.U.Asu_Us;
                         Match_Range  : in String;
                         Search_Char  : in Character;
                         Replace_Char : in Character;
                         Verbose      : in Boolean;
                         Test         : in Boolean;
                         Nb_Match     : in out Long_Long_Natural;
                         Loc_Subst    : out Long_Long_Natural) is
  begin
    Loc_Subst := 0;
    for I in 1 .. Line.all.Length loop
      if Line.all.Element (I) = Search_Char then
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Match in line >"
             & Line.all.Image & "< at" & I'Img);
        end if;
        Nb_Match := Nb_Match + 1;
        -- Check match range
        if not Subst_Match.Matches (Nb_Match, Match_Range) then
          if Debug.Set then
            Sys_Calls.Put_Line_Error (
               "Match discarded because out of matching range");
          end if;
        else
          -- Ok, matches all citeria
          Loc_Subst := Loc_Subst + 1;
          if Verbose then
            Basic_Proc.Put_Line_Output (
                Line_No'Img & " : "
              & Search_Char & " -> " & Replace_Char);
          end if;
          if not Test then
            -- Real substitution
            Line.all.Replace_Element (I, Replace_Char);
          end if;
        end if;
      end if;
    end loop;

    if not Test and then Loc_Subst /= 0 then
      if Debug.Set then
        -- A substitution has occured
        Sys_Calls.Put_Line_Error ("Replacing by " & Line.all.Image);
      end if;
    end if;

    -- Put the (modified) line
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Putting >" & Line.all.Image & "<");
    end if;
    Out_File.Put (Line.all.Image);
    -- Delete all
    Line_List.Delete_List (False);
  end Subst_Chars;

  -- Handle multiple substitutions within one line
  procedure Subst_One_Line (Line           : access As.U.Asu_Us;
                            Match_Range    : in String;
                            Verbose        : in Boolean;
                            Grep           : in Boolean;
                            Grep_List      : in Boolean;
                            Grep_File_Name : in Boolean;
                            Grep_Line_Nb   : in Boolean;
                            Grep_Invert    : in Boolean;
                            Test           : in Boolean;
                            Nb_Match       : in out Long_Long_Natural;
                            Loc_Subst      : out Long_Long_Natural;
                            Done_File      : out Boolean) is

    Current : Positive;
    Match_Res : Regular_Expressions.Match_Cell;
    Matches : Boolean;
    use type Regular_Expressions.Match_Cell;
  begin
    Done_File := False;
    -- Optimization if simple substit of a char by a char, no regex
    if not Search_Pattern.Search_Regex
    and then not Grep
    and then not Search_Pattern.Has_Exclude
    and then Search_Pattern.Number = 1 then
      declare
        Search_Str : constant String := Search_Pattern.Get_Pattern (1);
        Replace_Str : constant String := Replace_Pattern.Get;
      begin
        if Search_Str'Length = 1 and then Replace_Str'Length = 1 then
          Subst_Chars (Line, Match_Range,
                       Search_Str(Search_Str'First),
                       Replace_Str(Replace_Str'First),
                       Verbose, Test, Nb_Match, Loc_Subst);
          return;
        end if;
      end;
    end if;

    -- Multiple substitutions in one line
    Current := 1;
    Loc_Subst := 0;
    loop
      -- Search a Match from Current to Last
      -- Exit when no (more) match
      Matches := Search_Pattern.Check (Line.all.Image,
                        Current, Search => True, Regex_Index => 1);

      -- Found a match
      if Matches then
        Match_Res := Search_Pattern.Str_Indexes;
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Match in end of line >"
             & Line.all.Slice (Current, Line.all.Length)
             & "< from" & Match_Res.First_Offset'Img
             & " to" & Match_Res.Last_Offset_Stop'Img);
        end if;
      else
        Match_Res := Regular_Expressions.No_Match;
      end if;

      -- Check if this matching pattern matches the exclusion rule
      if Matches then
        if Search_Pattern.Check (
            Line.all.Slice (Match_Res.First_Offset,
                            Match_Res.Last_Offset_Stop),
            Start => Match_Res.First_Offset,
            Search => False, Regex_Index => 1) then
          -- Str matches the find criteria but also the exclude criteria: skip
          if Debug.Set then
            Sys_Calls.Put_Line_Error
                ("Match >" & Line.all.Slice (Match_Res.First_Offset,
                                             Match_Res.Last_Offset_Stop)
                 & "< discarded because matching exclusion");
          end if;
          Matches := False;
        end if;
      end if;

      -- Check if this matching pattern matches match range
      if Matches then
        Nb_Match := Nb_Match + 1;
        if not Subst_Match.Matches (Nb_Match, Match_Range) then
          if Debug.Set then
            Sys_Calls.Put_Line_Error
                ("Match >" & Line.all.Slice (Match_Res.First_Offset,
                                             Match_Res.Last_Offset_Stop)
                 & "< discarded because out of matching range");
          end if;
          Matches := False;
        end if;
      end if;

      -- Invert matching
      if Grep_Invert then
        if Line.all.Image = In_File.Get_Line_Feed then
          Matches := False;
          Match_Res := Regular_Expressions.No_Match;
          if Debug.Set then
            Sys_Calls.Put_Line_Error
                ("Match delimiter discarded because grep inversion");
          end if;
        elsif Line.all.Is_Null then
          Matches := False;
          Match_Res := Regular_Expressions.No_Match;
          if Debug.Set then
            Sys_Calls.Put_Line_Error
                ("Match empty discarded because grep inversion");
          end if;
        elsif Matches then
          -- Line matching becomes not matching
          Matches := False;
          if Debug.Set then
            Sys_Calls.Put_Line_Error
                ("Match >" & Line.all.Slice (Match_Res.First_Offset,
                                             Match_Res.Last_Offset_Stop)
                 & "< discarded because grep inversion");
          end if;
          Match_Res := Regular_Expressions.No_Match;
        else
          -- Line not matching becomes full matching
          Matches := True;
          Match_Res.First_Offset := 1;
          Match_Res.Last_Offset_Stop := Line.all.Length;
          if Debug.Set then
            Sys_Calls.Put_Line_Error
                ("Match >" & Line.all.Slice (Match_Res.First_Offset,
                                             Match_Res.Last_Offset_Stop)
                 & "< matches because grep inversion");
          end if;
        end if;
      end if;

      if not Matches then
        -- Search next match if any
        exit when Match_Res = Regular_Expressions.No_Match;
        Current := Match_Res.First_Offset + 1;
        exit when Current > Line.all.Length;
      else
        -- Str matches the find criteria and does not match the exclude
        --  criteria, and...: OK
        -- Get substituting string
        declare
          Replacing : constant String := Replace_Pattern.Replace;
        begin
          Loc_Subst := Loc_Subst + 1;
          -- Display verbose substitution
          if Verbose then
            Basic_Proc.Put_Line_Output (
                Line_No'Img & " : "
              & Line.all.Slice (Match_Res.First_Offset,
                                Match_Res.Last_Offset_Stop)
              & " -> " & Replacing);
          elsif Grep then
            if Loc_Subst = 1
            and then not Is_Stdin
            and then (Grep_List or else Grep_File_Name) then
              Basic_Proc.Put_Output (In_File_Name.Image);
              if Grep_List then
                -- Done with this file
                Basic_Proc.New_Line_Output;
                Done_File := True;
                exit;
              end if;
              Basic_Proc.Put_Output (":");
              if Grep_Line_Nb then
                Basic_Proc.Put_Output (Line_Image(Line_No) & ":");
              end if;
            elsif Grep_List then
              Done_File := True;
              exit;
            end if;
            if Replace_Pattern.Is_Empty then
              -- Display once each matching line
              Basic_Proc.Put_Line_Output (Line.all.Image);
              -- Done with this line
              exit;
            else
              -- Display each replaced (line feed is handled in Do_One_File)
              Basic_Proc.Put_Output (Replacing);
            end if;
          end if;
          if not Test then
            -- Substitute from start to stop
            if Debug.Set then
              Sys_Calls.Put_Line_Error ("Replacing by "
                & Line.all.Slice (Match_Res.First_Offset,
                                       Match_Res.Last_Offset_Stop)
                & " -> " & Replacing);
            end if;
            Line.all.Replace (Match_Res.First_Offset,
                              Match_Res.Last_Offset_Stop,
                              Replacing);
            -- Next search index is the next char after the replaced string
            Current := Match_Res.First_Offset + Replacing'Length;
          else
            Current := Match_Res.Last_Offset_Stop + 1;
          end if;
          exit when Current > Line.all.Length;
        end;

      end if;
    end loop;

    if not Grep then
      -- Put the (modified) line
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Putting >" & Line.all.Image & "<");
      end if;
      Out_File.Put (Line.all.Image);
    end if;
    -- Delete all
    Line_List.Delete_List (False);
  exception
    when Constraint_Error =>
      Error ("String too long substituting " & Line.all.Image);
      return;
  end Subst_One_Line;

  -- Put and flush lines that have been read but not match,
  --  then lines that have not been read
  procedure Flush_Lines is
  begin
    -- Put and flush lines that have been read but not match,
    if not Line_List.Is_Empty then
      Line_List.Rewind;
      while not Line_List.Is_Empty loop
        Out_File.Put (Line_List.Access_Current.all.Image);
        if Debug.Set then
          Sys_Calls.Put_Line_Error (
             "Flushing >" & Line_List.Access_Current.all.Image & "<");
        end if;
        Line_List.Delete;
      end loop;
    end if;
    --  Put lines that have not been read
    loop
      declare
        Str : constant String := In_File.Get;
      begin
        exit when Str = "";
        Out_File.Put (Str);
      end;
    end loop;
  end Flush_Lines;

  -- Check current list of lines vs search patterns
  procedure Subst_Lines (Match_Range    : in String;
                         Verbose        : in Boolean;
                         Grep           : in Boolean;
                         Grep_List      : in Boolean;
                         Grep_File_Name : in Boolean;
                         Grep_Line_Nb   : in Boolean;
                         Grep_Invert    : in Boolean;
                         Test           : in Boolean;
                         Nb_Match       : in out Long_Long_Natural;
                         Loc_Subst      : out Long_Long_Natural;
                         Done_File      : out Boolean) is
    Match_Res : Regular_Expressions.Match_Cell;
    Line, First_Line, Last_Line : access As.U.Asu_Us;
    Matches, Excluded : Boolean;

    -- Put matching text, complete line or just the matching text
    procedure Put_Match (Complete : in Boolean) is
      use type Str_Access;
    begin
      if Last_Line = First_Line then
        -- Handle specific case of only one line
        if Complete then
          Basic_Proc.Put_Output (Last_Line.all.Image);
        else
          Basic_Proc.Put_Output (Last_Line.all.Slice (
                           Match_Res.First_Offset,
                           Match_Res.Last_Offset_Stop));
        end if;
        return;
      end if;

      if Complete then
        Basic_Proc.Put_Output (First_Line.all.Image);
      else
        Basic_Proc.Put_Output (First_Line.all.Slice (
                                    Match_Res.First_Offset,
                                    First_Line.all.Length));
      end if;
      Line_List.Rewind;
      for I in 2 .. Nb_Pattern - 1 loop
        Line_List.Move_To;
        Line := Line_List.Access_Current;
        Basic_Proc.Put_Output (Line.all.Image);
      end loop;
      if Complete then
        Basic_Proc.Put_Output (Last_Line.all.Image);
      else
        Basic_Proc.Put_Output (Last_Line.all.Slice (
                        1, Match_Res.Last_Offset_Stop));
      end if;
    end Put_Match;
  begin
    -- Rewind read lines
    Line_List.Rewind;
    if Is_Iterative then
      -- Handle separately multiple substitutions if one pattern
      Subst_One_Line (Line_List.Access_Current,
                      Match_Range, Verbose, Grep, Grep_List, Grep_File_Name,
                      Grep_Line_Nb, Grep_Invert, Test, Nb_Match, Loc_Subst,
                      Done_File);
      return;
    end if;

    Done_File := False;
    Loc_Subst := 0;
    -- Check all patterns until one does not match
    for I in 1 .. Nb_Pattern loop
      -- Check this read line
      Line := Line_List.Access_Current;
      Matches := Search_Pattern.Check (Line.all.Image, 1,
                 Search => True, Regex_Index => I);
      if not Matches then
        -- This one does not match
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Not match " & I'Img
                  & " with >" & Line.all.Image & "<");
        end if;
        exit;
      end if;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Line >" & Line.all.Image
                                & "< matches pattern No" & I'Img);
      end if;
      -- Move to next input line
      if I /= Nb_Pattern then
        Line_List.Move_To;
      end if;
    end loop;

    if Matches then
      -- Check if it matches all exclusion rules
      Excluded := True;
      Match_Res := Search_Pattern.Str_Indexes;
      Line_List.Rewind;
      for I in 1 .. Nb_Pattern loop
        -- Check this read line
        Line := Line_List.Access_Current;
        if I = 1 then
          Matches := Search_Pattern.Check (Line.all.Image,
                     Start =>  Match_Res.First_Offset,
                     Search => False, Regex_Index => I);
        elsif I /= Nb_Pattern then
          Matches := Search_Pattern.Check (Line.all.Image,
                     Start => 1,
                     Search => False, Regex_Index => I);
        else
          Matches := Search_Pattern.Check (
                     Line.all.Slice (1, Match_Res.Last_Offset_Stop),
                     Start => 1,
                     Search => False, Regex_Index => I);
        end if;
        if not Matches then
          -- This one does not match this exclusion: OK
          Excluded := False;
          exit;
        end if;
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Line >" & Line.all.Image
                                  & "< matches exclusion No" & I'Img);
        end if;
        -- Move to next input line
        if I /= Nb_Pattern then
          Line_List.Move_To;
        end if;
      end loop;

      if Excluded then
        if Debug.Set then
          Line_List.Rewind;
          Line := Line_List.Access_Current;
          Sys_Calls.Put_Line_Error ("Line >" & Line.all.Image
                                  & "< is excluded");
        end if;
        Matches := False;
      else
        Matches := True;
      end if;
    end if;

    if Matches then
      -- Check if it matches match range
      Nb_Match := Nb_Match + 1;
      Matches := Subst_Match.Matches (Nb_Match, Match_Range);

      if not Matches then
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Line >" & Line.all.Image
               & "< discarded because out of matching range");
        end if;
      end if;
    end if;

    -- Invert matching
    if Grep_Invert then
      Matches := not Matches;
      if Debug.Set then
        if not Matches then
          Sys_Calls.Put_Line_Error ("Line >" & Line.all.Image
               & "< discarded because grep inversion");
        else
          Sys_Calls.Put_Line_Error ("Line >" & Line.all.Image
               & "< matches because grep inversion");
        end if;
      end if;
    end if;

    if Matches then
      Loc_Subst := 1;
      -- Match, build string to replace:
      Match_Res := Search_Pattern.Str_Indexes;
      -- Get access to first and last lines of input
      Line_List.Rewind;
      First_Line := Line_List.Access_Current;
      Line_List.Rewind (True, Line_List_Mng.Prev);
      Last_Line := Line_List.Access_Current;
      -- Result string is -> Start of first line + Replacing
      --                   + End of last line (if not overlap)
      declare
        Str_Replacing : constant String := Replace_Pattern.Replace;
        Str_Replaced : As.U.Asu_Us;
        Tail : As.U.Asu_Us;
        use type As.U.Asu_Us;
      begin
        -- Set result: beginning of first line + replacing + end of last line
        Str_Replaced := As.U.Tus (
                 First_Line.all.Slice (1, Match_Res.First_Offset - 1))
               & Str_Replacing;
        if Match_Res.Last_Offset_Stop < Last_Line.all.Length then
          -- This would raise Constraint_Error if Stop = Length
          Tail := Last_Line.all.Uslice (Match_Res.Last_Offset_Stop + 1,
                                        Last_Line.all.Length);
        end if;
        if not Search_Pattern.Overlaps then
          -- If overlap then keep tail for next search
          Str_Replaced.Append (Tail);
        end if;
        if Verbose then
          -- Display verbose substitution
          Basic_Proc.Put_Output (
              Long_Long_Natural'Image(Line_No
                                    - Long_Long_Natural(Nb_Pattern) / 2)
            & " : ");
          Put_Match (False);
          Basic_Proc.Put_Line_Output (" -> " & Str_Replacing);
        elsif Grep then
          -- Display grep result
          if not Is_Stdin and then (Grep_List or else Grep_File_Name) then
            Basic_Proc.Put_Output (In_File_Name.Image);
            if Grep_List then
              Basic_Proc.New_Line_Output;
              Done_File := True;
              return;
            end if;
            Basic_Proc.Put_Output (":");
            if Grep_Line_Nb then
              Basic_Proc.Put_Output (Line_Image(Line_No) & ":");
            end if;
          elsif Grep_List then
            Done_File := True;
            return;
          end if;
          if Replace_Pattern.Is_Empty then
            Put_Match (True);
            Basic_Proc.New_Line_Output;
          else
            Basic_Proc.Put_Output (Str_Replacing);
            if Search_Pattern.Number = 1 then
              Basic_Proc.New_Line_Output;
            end if;
          end if;
        end if;
        if not Test then
          -- Match and not test
          -- Write result
          if Debug.Set then
            Sys_Calls.Put_Line_Error ("Putting >" & Str_Replaced.Image & "<");
          end if;
          Out_File.Put (Str_Replaced.Image);
          -- Delete all, and re-insert tail if overlap
          Line_List.Delete_List (False);
          if Search_Pattern.Overlaps 
          and then not Tail.Is_Null then
            Line_List.Insert (Tail);
          end if;
        end if;
      end;
    end if;
    if not Matches or else Test then
      -- If not match or test, put first line and delete it
      Line_List.Rewind;
      Line := Line_List.Access_Current;
      if not Grep then
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Putting >" & Line.all.Image & "<");
        end if;
        Out_File.Put (Line.all.Image);
      end if;
      Line_List.Delete;
    end if;
  end Subst_Lines;

end Substit;

