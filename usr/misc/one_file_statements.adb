with Normal, Sys_Calls, Text_Line, As.U, My_Math;

-- Count Ada statements
package body One_File_Statements is

  type Metrics is record
    Statements : Natural := 0;
    Comments : Natural := 0;
    Lines : Positive := 1;
  end record;

  -- Total Nb of statements
  Total : Metrics;
  File_Error : exception;
  function Count_Statements_Of_File (File_Name : String) return Metrics is

    File  : Text_Line.File_Type;
    -- Current and prev chars
    C, Prev_C : Character := ' ';
    -- Character read head
    Next_C  : Character := ' ';
    Next_Set : Boolean := False;
    -- Current Nb of statements
    Current : Metrics;
    -- Parentheses
    Levels : Natural := 0;

    -- Cache of line
    Line : As.U.Asu_Us;
    Len : Natural := 0;
    Index : Positive := 1;

    End_Error : exception;

    procedure Get (C : out Character) is
    begin
      if Index > Len then
        Line := File.Get;
        if Line.Is_Null then
          raise End_Error;
        end if;
        Len := Line.Length;
        Index := 1;
      end if;

      C := Line.Element (Index);
      Index := Index + 1;

      if C = Text_Line.Line_Feed_Char then
        Current.Lines := Current.Lines + 1;
      end if;
   exception
      when End_Error =>
        raise;
      when others =>
        Sys_Calls.Put_Line_Error ("Exception when reading line "
          & Current.Lines'Img & " of file " & File_Name);
        raise File_Error;
    end Get;

    procedure Skip_Line is
    begin
      if Index <= Len then
        Current.Lines := Current.Lines + 1;
        -- Force reading of next line
        Len := 0;
        Index := 1;
      end if;
    end Skip_Line;

    procedure Close is
    begin
      File.Close_All;
    exception
      when others =>
        null;
    end Close;

    procedure Skip_Until (Upto : in Character) is
    begin
      loop
        Get (C);
        exit when C = Upto;
      end loop;
    end Skip_Until;

    function Line_No return Positive is
    begin
      return Current.Lines;
    end Line_No;

  begin

    begin
      File.Open_All (Text_Line.In_File, File_Name);
    exception
      when others =>
        Sys_Calls.Put_Line_Error ("Exception when opening file "
                                & File_Name);
        raise File_Error;
    end;

    loop
      -- Has a char been read ahead with previous char
      if Next_Set then
        C := Next_C;
        Next_Set := False;
      else
        Get (C);
      end if;

      -- Check for comment on the line
      if C = '-' then
        Get (C);
        -- Which is signaled by the '-' following a '-'
        if C = '-' then
          -- Then just skip the rest of the line and go to the next
          Skip_Line;
          Current.Comments := Current.Comments + 1;
        else
          -- Restore char
          Next_C := C;
          Next_Set := True;
        end if;

      elsif C = ';' then
        -- Any ';' that can be found at this point after all exclusions
        -- must be a valid "statement terminator"
        if Levels = 0 then
          -- Skip parentheses cause every ';' within is a formal parameter list
          Current.Statements := Current.Statements + 1;
        end if;
      elsif  C = '(' then
        -- Count the number of levels of parentheses
        Levels := Levels + 1;
      elsif  C = ')' then
        if Levels /= 0 then
          Levels := Levels - 1;
        else
          Sys_Calls.Put_Line_Error (
                  "Warning: Reaching negative parenthesis level"
                & " at line" & Line_No'Img);
        end if;
      elsif C = '"' or else C = '%' then
        -- Now, check for string brackets of either kind, " or %
        -- This works even if there is '""' in string
        Skip_Until (C);
      elsif C = ''' then
        -- Character literals are just three characters long including '
        -- Attributes are skipped the same way because longer than one char
        Get (Prev_C);
        Get (C);
        -- Handle specific Qualifier'(...
        if C /= ''' then
          -- This is not a char literal
          -- Should restore Prev_C and C but in fact, Prev_C can only
          --  be '(' or an attribute or a separator.
          -- So handle Prev_C = '(' and restore C
          if Prev_C = '(' then
            -- Qualifier'(...
            Levels := Levels + 1;
          end if;
          Next_C := C;
          Next_Set := True;
        end if;
      end if;
    end loop;

  exception
    when End_Error =>
      Close;
      if Levels /= 0 then
        Sys_Calls.Put_Line_Error ("Warning: Ending file with parenthesis level"
                            & Levels'Img & ".");
      end if;
      if Current.Lines >= 2 then
        Current.Lines := Current.Lines - 1;
      end if;
      return Current;
    when File_Error =>
      Close;
      raise;
    when others =>
      Close;
      Sys_Calls.Put_Line_Error ("Exception when processing line "
          & Current.Lines'Img & " of file " & File_Name);
      raise;
  end Count_Statements_Of_File;


  Max_Dig : constant := 9;
  Gap : constant String := "  ";
  procedure Put_Vals (File : in out Text_Line.File_Type;
                      Vals : Metrics) is
    Percent : Natural;
    Lines : Natural;
    use type My_Math.Real;
  begin
    if Vals.Statements = 0 then
      Percent := 0;
      Lines := 0;
    else
      Percent := Natural (My_Math.Round (My_Math.Real (Vals.Comments) * 100.0
                                       / My_Math.Real (Vals.Statements)));
      Lines := Vals.Lines;
    end if;
    Text_Line.Put_Line (File, Gap & Normal(Vals.Statements, Max_Dig)
                            & Gap & Normal(Percent, 4)
                            & Gap & Normal(Lines, Max_Dig));
  end Put_Vals;

  -- If File_Name is empty, put total so far and reset it
  procedure Print_Statements_Of_File (
             File_Name : String;
             Put_It : in Boolean := True) is

    File : Text_Line.File_Type;
    File_Name_Len : constant Natural := File_Name'Length;
    Current : Metrics;
    Ok : Boolean;
    Max_Tab : constant := 68;
  begin

    Text_Line.Open (File, Text_Line.Out_File, Sys_Calls.Stdout);
    if File_Name = "" then
      -- Summary so far
      if Put_It then
        -- Put formatted output
        for I in Integer range 1 .. Max_Tab + 1
                   + Gap'Length + Max_Dig + Gap'Length + 4 + Gap'Length + Max_Dig loop
          Text_Line.Put (File, "-");
        end loop;
        Text_Line.New_Line (File);

        declare
          Total_Str : constant String := "TOTAL statements %comments lines";
        begin
          Text_Line.Put (File, Total_Str);
          for I in Integer range Total_Str'Length .. Max_Tab loop
            Text_Line.Put (File, " ");
          end loop;
        end;
        Put_Vals (File, Total);
      else
        -- Just put number of statements
        Text_Line.Put (File, Normal(Total.Statements, Max_Dig));
      end if;
      Total := (others => <>);

    else
      -- Statements of file
      begin
        Ok := True;
        Current := One_File_Statements.Count_Statements_Of_File(File_Name);
      exception
        when others =>
          Ok := False;
      end;

      if Put_It then
        -- Put formatted output
        Text_Line.Put (File, File_Name);
        if File_Name_Len < Max_Tab then
          Text_Line.Put (File, " ");
          for I in File_Name_Len + 1 .. Max_Tab loop
            Text_Line.Put (File, ".");
          end loop;
        elsif File_Name_Len > Max_Tab then
          Text_Line.New_Line (File);
          for I in Integer range 1 .. Max_Tab + 1 loop
            Text_Line.Put (File, ".");
          end loop;
        end if;

        if Ok then
          Put_Vals (File, Current);
        else
          Text_Line.Put_Line (File, Gap & " SKIPPED");
        end if;
      end if;

      if Ok then
        Total.Statements := Total.Statements + Current.Statements;
        Total.Comments := Total.Comments + Current.Comments;
        Total.Lines := Total.Lines + Current.Lines;
      end if;

    end if;

    Text_Line.Close (File);
  end Print_Statements_Of_File;

end One_File_Statements;

