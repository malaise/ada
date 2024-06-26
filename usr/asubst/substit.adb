with As.U.Utils;
with Argument, Sys_Calls.File_Access, Text_Line, Temp_File, Reg_Exp,
     Directory, Copy_File, Mixed_Str, Basic_Proc;
with Search_Pattern, Replace_Pattern, Log;
package body Substit is

  -- User and group IDs
  User_Id, Group_Id : Natural;
  procedure Init is
  begin
    User_Id  := Sys_Calls.Get_Effective_User_Id;
    Group_Id := Sys_Calls.Get_Effective_Group_Id;
  end Init;

  -- List of strings
  package Line_List_Mng renames As.U.Utils.Asu_Dyn_List_Mng;
  Line_List : Line_List_Mng.List_Type;

  -- File names
  In_File_Name : As.U.Asu_Us;
  Out_File_Name : As.U.Asu_Us;

  -- Files
  In_File : Text_Line.File_Type;
  Out_File : Text_Line.File_Type;

  -- We work on stdin/stdout?
  Is_Stdin : Boolean;

  -- Number of patterns and is it iterative
  Nb_Pattern : Search_Pattern.Ll_Positive;
  Is_Iterative : Boolean;

  -- Current line number
  Line_No : Arbitrary.Number;
  function Line_Image (A : Arbitrary.Number)
           return String renames Arbitrary.Basic_Image;

  -- Display error. If Give_Up then also cleanup and raise Substit_Error
  procedure Error (Msg : in String; Give_Up : in Boolean := True);

  -- Check that this is a file we can read and write
  -- Optim : cache of previous dir name, in order to skip access check
  Prev_Path_Name : As.U.Asu_Us;
  procedure Check_File (File_Name : in String;
                        For_Write : in Boolean) is
    Path_Name : constant String := Directory.Dirname (File_Name);
    use type As.U.Asu_Us, Sys_Calls.File_Kind_List;
    -- Set name and stat of path of file, following symbolic links
    Dir_Name : As.U.Asu_Us;
    Dir_Stat : Sys_Calls.File_Stat_Rec;
    procedure Set_Dir is
    begin
      if Path_Name = "" then
        Dir_Name := As.U.Tus ( "./");
        Dir_Stat := Sys_Calls.File_Stat (Dir_Name.Image);
      else
        Dir_Stat := Sys_Calls.File_Stat (Path_Name);
        if Dir_Stat.Kind = Sys_Calls.Link then
          -- Resolve sym link recursively
          Dir_Name := As.U.Tus (Directory.Read_Link (Path_Name));
        else
          -- Kind is checked later
          Dir_Name := As.U.Tus (Path_Name);
        end if;
      end if;
    exception
      when others =>
        Error ("Cannot access directory " & Path_Name);
        raise Substit_Error;
    end Set_Dir;

    File_Stat : Sys_Calls.File_Stat_Rec;
    Can_Read, Can_Write, Can_Exec : Boolean;
  begin
    if Path_Name /= Prev_Path_Name then
      Prev_Path_Name := As.U.Tus (Path_Name);
      -- Directory must be a dir with (w)rx access
      Set_Dir;
      if Dir_Stat.Kind /= Sys_Calls.Dir then
        Error ("Directory " & Dir_Name.Image & " is of incorrect kind: "
             & Mixed_Str (Dir_Stat.Kind'Img));
      end if;
      Sys_Calls.File_Access.Has_Access (User_Id, Group_Id, Dir_Stat,
                                        Can_Read, Can_Write, Can_Exec);
      -- Read => rx, Write => rwx
      if not Can_Read
      or else not Can_Exec
      or else (For_Write and then not Can_Write) then
        Error ("Directory " & Dir_Name.Image & " has incorrect access rights");
      end if;
    end if;

    -- This must be a file, with rw access
    begin
      File_Stat := Sys_Calls.File_Stat (File_Name);
    exception
      when others =>
        Error ("Cannot access file " & File_Name);
    end;
    if File_Stat.Kind /= Sys_Calls.File then
      Error ("File " & File_Name & " is of incorrect kind: "
           & Mixed_Str (File_Stat.Kind'Img));
    end if;
    Sys_Calls.File_Access.Has_Access (User_Id, Group_Id, File_Stat,
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
    Line_No.Set_Null;
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
    Nb_To_Read : Search_Pattern.Ll_Natural;
    Line : As.U.Asu_Us;
    Len : Natural;
    Line_Feed : constant As.U.Asu_Us := As.U.Tus (In_File.Get_Line_Feed);
    Feed_Len : constant Natural := Line_Feed.Length;
    use type As.U.Asu_Us, Search_Pattern.Ll_Natural;
  begin
    -- Move to end
    Line_List.Rewind (Line_List_Mng.Prev, False);
    -- Compute amount to fill buffer (Nb lines)
    Nb_To_Read := Nb_Pattern
                - Search_Pattern.Ll_Natural (Line_List.List_Length);
    -- Append trailing new line if any
    if Trail_Line_Feed then
      Line_List.Insert (Line_Feed);
      Trail_Line_Feed := False;
      Nb_To_Read := Nb_To_Read - 1;
      Log.Sub ("Read added trailing line feed");
    end if;

    -- Read and append remaining amount, save trailing newline
    while Nb_To_Read /= 0 loop
      Line := In_File.Get;
      Len := Line.Length;
      Log.Sub ("Read >" &  Line.Image & "<");
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
        Line_No.Incr;
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
        Line_No.Incr;
      end if;
    end loop;
    Log.Sub ("Read up to line no " &  Line_Image (Line_No)
             & (if Trail_Line_Feed then " & trail" else ""));
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
                         Nb_Match       : in out Subst_Natural;
                         Loc_Subst      : out Subst_Natural;
                         Done_File      : out Boolean);

  function Do_One_File (File_Name      : String;
                        Tmp_Dir        : String;
                        Match_Range    : String;
                        Backup         : Boolean;
                        Verbose        : Boolean;
                        Grep           : Boolean;
                        Grep_List      : Boolean;
                        Grep_File_Name : Boolean;
                        Grep_Line_Nb   : Boolean;
                        Grep_Invert    : Boolean;
                        Test           : Boolean) return Subst_Natural is
    Done_File : Boolean;
    Nb_Subst : Subst_Natural;
    Nb_Match : Subst_Natural;
    Loc_Subst : Subst_Natural;
    Do_Verbose : Boolean;
    use type Arbitrary.Number;
  begin
    -- Open files: test is set if no need to write
    Open (File_Name, Tmp_Dir, not Grep);
    -- Verbose if requested and not stdin
    Do_Verbose := Verbose and then not Is_Stdin;

    -- Init buffer of lines
    Line_List.Delete_List;
    Trail_Line_Feed := False;
    -- Init substitution by reading Nb_Pattern lines and Newlines
    -- Loop on substit
    Done_File := False;
    Nb_Subst.Set_Null;
    Nb_Match.Set_Null;
    Loc_Subst.Set_Null;
    -- Done when file is already put in List mode
    --   or when the amount of lines cannot be read
    while not Done_File and then Read loop
      -- If grep is iterative with a replace and got something,
      --  then append a line feed
      if Grep and then Is_Iterative and then not Loc_Subst.Is_Null
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
    Log.Sub ("Done.");
    Close;
    -- After close, comit or clean
    if not Is_Stdin
    and then not Nb_Subst.Is_Null
    and then not Test then
      Comit (Backup);
    else
      Clean;
    end if;
    return Nb_Subst;
  exception
    when Replace_Pattern.Command_Error
       | Search_Pattern.Regex_Error =>
      -- Rollback on this file
      Close;
      Clean;
      return Arbitrary.Zero;
    when others =>
      -- Rollback and stop
      Close;
      Clean;
      raise;
  end Do_One_File;

  -- Handle multiple substitutions of a char by a char within one line
  procedure Subst_Chars (Line         : not null access As.U.Asu_Us;
                         Match_Range  : in String;
                         Search_Char  : in Character;
                         Replace_Char : in Character;
                         Verbose      : in Boolean;
                         Test         : in Boolean;
                         Nb_Match     : in out Subst_Natural;
                         Loc_Subst    : out Subst_Natural) is
  begin
    Loc_Subst.Set_Null;
    for I in 1 .. Line.all.Length loop
      if Line.all.Element (I) = Search_Char then
        Log.Sub ("Match in line >" & Line.all.Image & "< at" & I'Img);
        Nb_Match.Incr;
        -- Check match range
        if not Subst_Match.Matches (Nb_Match, Match_Range) then
          Log.Sub ("Match discarded because out of matching range");
        else
          -- Ok, matches all citeria
          Loc_Subst.Incr;
          if Verbose then
            Basic_Proc.Put_Line_Output (
                Line_Image (Line_No) & " : "
              & Search_Char & " -> " & Replace_Char);
          end if;
          if not Test then
            -- Real substitution
            Line.all.Replace_Element (I, Replace_Char);
          end if;
        end if;
      end if;
    end loop;

    if not Test and then not Loc_Subst.Is_Null then
      -- A substitution has occured
      Log.Sub ("Replacing by " & Line.all.Image);
    end if;

    -- Put the (modified) line
    Log.Sub ("Putting >" & Line.all.Image & "<");
    Out_File.Put (Line.all.Image);
    -- Delete all
    Line_List.Delete_List (False);
  end Subst_Chars;

  -- Handle multiple substitutions within one line
  procedure Subst_One_Line (Line           : not null access As.U.Asu_Us;
                            Match_Range    : in String;
                            Verbose        : in Boolean;
                            Grep           : in Boolean;
                            Grep_List      : in Boolean;
                            Grep_File_Name : in Boolean;
                            Grep_Line_Nb   : in Boolean;
                            Grep_Invert    : in Boolean;
                            Test           : in Boolean;
                            Nb_Match       : in out Subst_Natural;
                            Loc_Subst      : out Subst_Natural;
                            Done_File      : out Boolean) is

    Current : Positive;
    Match_Res : Reg_Exp.Match_Cell;
    Matches : Boolean;
    use type Reg_Exp.Match_Cell, Search_Pattern.Ll_Natural, Subst_Natural;
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
    Loc_Subst.Set_Null;
    loop
      -- Search a Match from Current to Last
      -- Exit when no (more) match
      Matches := Search_Pattern.Check (Line.all.Image,
                        Current, Search => True, Regex_Index => 1);

      -- Found a match
      if Matches then
        Match_Res := Search_Pattern.Str_Indexes;
        Log.Sub ("Match in end of line >"
               & Line.all.Slice (Current, Line.all.Length)
               & "< from" & Match_Res.First_Offset'Img
               & " to" & Match_Res.Last_Offset_Stop'Img);
      else
        Match_Res := Reg_Exp.No_Match;
      end if;

      -- Check if this matching pattern matches the exclusion rule
      if Matches then
        if Search_Pattern.Check (
            Line.all.Slice (Match_Res.First_Offset,
                            Match_Res.Last_Offset_Stop),
            Start => Match_Res.First_Offset,
            Search => False, Regex_Index => 1) then
          -- Str matches the find criteria but also the exclude criteria: skip
          Log.Sub ("Match >" & Line.all.Slice (Match_Res.First_Offset,
                                                 Match_Res.Last_Offset_Stop)
                 & "< discarded because matching exclusion");
          Matches := False;
        end if;
      end if;

      -- Check if this matching pattern matches match range
      if Matches then
        Nb_Match.Incr;
        if not Subst_Match.Matches (Nb_Match, Match_Range) then
          Log.Sub ("Match >" & Line.all.Slice (Match_Res.First_Offset,
                                               Match_Res.Last_Offset_Stop)
                 & "< discarded because out of matching range");
          Matches := False;
        end if;
      end if;

      -- Invert matching
      if Grep_Invert then
        if Line.all.Image = In_File.Get_Line_Feed then
          Matches := False;
          Match_Res := Reg_Exp.No_Match;
          Log.Sub ("Match delimiter discarded because grep inversion");
        elsif Line.all.Is_Null then
          Matches := False;
          Match_Res := Reg_Exp.No_Match;
          Log.Sub ("Match empty discarded because grep inversion");
        elsif Matches then
          -- Line matching becomes not matching
          Matches := False;
          Log.Sub ("Match >" & Line.all.Slice (Match_Res.First_Offset,
                                               Match_Res.Last_Offset_Stop)
                   & "< discarded because grep inversion");
          Match_Res := Reg_Exp.No_Match;
        else
          -- Line not matching becomes full matching
          Matches := True;
          Match_Res.First_Offset := 1;
          Match_Res.Last_Offset_Stop := Line.all.Length;
          Log.Sub ("Match >" & Line.all.Slice (Match_Res.First_Offset,
                                               Match_Res.Last_Offset_Stop)
                   & "< matches because grep inversion");
        end if;
      end if;

      if not Matches then
        -- Search next match if any
        exit when Match_Res = Reg_Exp.No_Match;
        Current := Match_Res.First_Offset + 1;
        exit when Current > Line.all.Length;
      else
        -- Str matches the find criteria and does not match the exclude
        --  criteria, and...: OK
        -- Get substituting string
        declare
          Replacing : constant String := Replace_Pattern.Replace;
        begin
          Loc_Subst.Incr;
          -- Display verbose substitution
          if Verbose then
            Basic_Proc.Put_Line_Output (
                Line_Image (Line_No) & " : "
              & Line.all.Slice (Match_Res.First_Offset,
                                Match_Res.Last_Offset_Stop)
              & " -> " & Replacing);
          elsif Grep then
            if Loc_Subst = Arbitrary.One
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
                Basic_Proc.Put_Output (Line_Image (Line_No) & ":");
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
            Log.Sub ("Replacing by "
                    & Line.all.Slice (Match_Res.First_Offset,
                                      Match_Res.Last_Offset_Stop)
                    & " -> " & Replacing);
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
      Log.Sub ("Putting >" & Line.all.Image & "<");
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
        Log.Sub ("Flushing >" & Line_List.Access_Current.all.Image & "<");
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
                         Nb_Match       : in out Subst_Natural;
                         Loc_Subst      : out Subst_Natural;
                         Done_File      : out Boolean) is
    Match_Res : Reg_Exp.Match_Cell;
    Line, First_Line, Last_Line : access As.U.Asu_Us;
    Matches, Excluded : Boolean;

    -- Put matching text, complete line or just the matching text
    procedure Put_Match (Complete : in Boolean) is
      use type Search_Pattern.Ll_Natural;
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

     use type Search_Pattern.Ll_Natural;
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
    Loc_Subst.Set_Null;
    -- Check all patterns until one does not match
    for I in 1 .. Nb_Pattern loop
      -- Check this read line
      Line := Line_List.Access_Current;
      Matches := Search_Pattern.Check (Line.all.Image, 1,
                 Search => True, Regex_Index => I);
      if not Matches then
        -- This one does not match
        Log.Sub ("Not match " & I'Img
               & " with >" & Line.all.Image & "<");
        exit;
      end if;
      Log.Sub ("Line >" & Line.all.Image
             & "< matches pattern No" & I'Img);
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
        Log.Sub ("Line >" & Line.all.Image
               & "< matches exclusion No" & I'Img);
        -- Move to next input line
        if I /= Nb_Pattern then
          Line_List.Move_To;
        end if;
      end loop;

      if Excluded then
        if Log.Sub_Debug then
          Line_List.Rewind;
          Line := Line_List.Access_Current;
          Log.Sub ("Line >" & Line.all.Image & "< is excluded");
        end if;
        Matches := False;
      else
        Matches := True;
      end if;
    end if;

    if Matches then
      -- Check if it matches match range
      Nb_Match.Incr;
      Matches := Subst_Match.Matches (Nb_Match, Match_Range);

      if not Matches then
        Log.Sub ("Line >" & Line.all.Image
               & "< discarded because out of matching range");
      end if;
    end if;

    -- Invert matching
    if Grep_Invert then
      Matches := not Matches;
      if not Matches then
        Log.Sub ("Line >" & Line.all.Image
               & "< discarded because grep inversion");
      else
        Log.Sub ("Line >" & Line.all.Image
               & "< matches because grep inversion");
      end if;
    end if;

    if Matches then
      Loc_Subst := Arbitrary.One;
      -- Match, build string to replace:
      Match_Res := Search_Pattern.Str_Indexes;
      -- Get access to first and last lines of input
      Line_List.Rewind;
      First_Line := Line_List.Access_Current;
      Line_List.Rewind (Line_List_Mng.Prev);
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
          Basic_Proc.Put_Output (Line_Image (Line_No) & " : ");
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
              Basic_Proc.Put_Output (Line_Image (Line_No) & ":");
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
          Log.Sub ("Putting >" & Str_Replaced.Image & "<");
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
        Log.Sub ("Putting >" & Line.all.Image & "<");
        Out_File.Put (Line.all.Image);
      end if;
      Line_List.Delete;
    end if;
  end Subst_Lines;

end Substit;

