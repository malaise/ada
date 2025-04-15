with Normal, Basic_Proc, Sys_Calls, Text_Line, Text_Char, My_Math, As.U;
-- Count Ada statements
package body One_File_Statements is

  File_Error : exception;
  function Count_Statements_Of_File (File_Name : String;
                                     Java_Syntax : Boolean) return Metrics is

    File  : Text_Char.File_Type;
    -- Comment "--" or "//"
    Comment_Char : Character;
    -- Current Nb of statements
    Current : Metrics;
    -- Parentheses
    Levels : Natural := 0;

    -- Current and prev chars
    C, Prev_C : Character;
    procedure Get is
    begin
      Prev_C := C;
      C := File.Get;
      if C = Text_Line.Line_Feed_Char then
        Current.Lines := Current.Lines + 1;
      end if;
   exception
      when Text_Char.End_Error =>
        raise;
      when others =>
        Basic_Proc.Put_Line_Error ("Exception when reading line "
          & Current.Lines'Img & " of file " & File_Name);
        raise File_Error;
    end Get;

    procedure Unget is
    begin
      if C = Text_Line.Line_Feed_Char then
        Current.Lines := Current.Lines - 1;
      end if;
      File.Unget (C);
      C := Prev_C;
    end Unget;

    procedure Skip_Line is
    begin
      loop
        Get;
        exit when C = Text_Line.Line_Feed_Char;
      end loop;
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
        Get;
        exit when C = Upto;
      end loop;
    end Skip_Until;

    function Line_No return Positive is (Current.Lines);

  begin

    begin
      File.Open_All (File_Name);
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Exception when opening file "
                                & File_Name);
        raise File_Error;
    end;
    if Java_Syntax then
      Comment_Char := '/';
    else
      Comment_Char := '-';
    end if;
    C := ' ';

    loop
      Get;

      -- Check for comment on the line
      if C = Comment_Char then
        Get;
        -- Which is signaled by the '-' following a '-'
        if C = Comment_Char then
          -- Then just skip the rest of the line and go to the next
          Skip_Line;
          Current.Comments := Current.Comments + 1;
        else
          -- Restore char
          Unget;
        end if;
      elsif Java_Syntax and then C = '*' and then Prev_C = '/' then
        -- Java comment "/*", skip until "*/"
        loop
          Skip_Until ('/');
          exit when Prev_C = '*';
        end loop;
        Current.Comments := Current.Comments + 1;
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
          Basic_Proc.Put_Line_Error (
                  "Warning: Reaching negative parenthesis level"
                & " at line" & Line_No'Img);
        end if;
      elsif not Java_Syntax and then (C = '"' or else C = '%') then
        -- Check for string brackets of either kind, " or %
        -- This works even if there is '""' in string
        Skip_Until (C);
      elsif Java_Syntax and then C = '"' and then Prev_C /= '\' then
        -- Check for string bracket '"' not preceeded by '\'
        loop
          Skip_Until ('"');
          exit when Prev_C /= '\';
        end loop;
      elsif C = ''' then
        if Java_Syntax then
          -- Get 'C' or '\C'
          Get;
          if C = '\' then
             Get;
           end if;
           Get;
        else
          -- Character literals are just three characters long including '
          -- Attributes are skipped the same way because longer than one char
          Get;
          Get;
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
            Unget;
          end if;
        end if;
      end if;
    end loop;

  exception
    when Text_Char.End_Error =>
      -- Normal end of file
      Close;
      if Levels /= 0 then
        Basic_Proc.Put_Line_Error ("Warning: Ending file with parenthesis level"
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
      Basic_Proc.Put_Line_Error ("Exception when processing line "
          & Current.Lines'Img & " of file " & File_Name);
      raise;
  end Count_Statements_Of_File;

  File : Text_Line.File_Type;

  -- Statments in a output file
  procedure Open_File is
  begin
    if not File.Is_Open then
      File.Open (Text_Line.Out_File, Sys_Calls.Stdout);
    end if;
  end Open_File;


  -- Formating info
  Max_Dig : constant := 9;
  Gap : constant String := "  ";

  -- Put a full line of '-'
  procedure Put_Delimiter (Width : in Positive) is
  begin
    for I in Integer range 1 ..
          Width
          + Gap'Length + Max_Dig
          + Gap'Length + 4 + Gap'Length + Max_Dig loop
        Text_Line.Put (File, "-");
    end loop;
    Text_Line.New_Line (File);
  end Put_Delimiter;

  -- Put formated line:
  -- <name> [ ' ' { '.' } ] <gap> <statmts> <gap> <%cmts> <gap> <lines>
  procedure Put_Line (Name  : in String;
                      Dot   : in Character;
                      Statmts, Cmts, Lines : in String;
                      Width : in Positive) is
    Name_Len : Natural := Name'Length;
  begin
    -- Put Name
    Text_Line.Put (File, Name);
    if Name_Len > Width then
      -- New line
      Text_Line.New_Line (File);
      Name_Len := 0;
    end if;
    -- Complete up to first '.'
    if Name_Len = 0 then
      Text_Line.Put (File, Dot & "");
      Name_Len := 1;
    elsif Name_Len /= Width then
      Text_Line.Put (File, " ");
      Name_Len := Name_Len + 1;
    end if;
    -- Complete up to Width
    for I in Name_Len + 1 .. Width loop
      Text_Line.Put (File, Dot & "");
    end loop;
    -- Gap Statmt Gap Cmts Gap Lines
     Text_Line.Put_Line (File, Gap & Statmts & Gap & Cmts & Gap & Lines);
  end Put_Line;

  -- Put a metric
  procedure Put_Line (Name   : in String;
                      Dot    : in Character;
                      Metric : in Metrics;
                      Width  : in Positive) is
    Percent : Natural;
    Lines : Natural;
    use type My_Math.Real;
  begin
    if Metric.Statements = 0 then
      Percent := 0;
      Lines := 0;
    else
      Percent := Natural (My_Math.Round (
            My_Math.Real (Metric.Comments) * 100.0
          / My_Math.Real (Metric.Statements)));
        Lines := Metric.Lines;
    end if;
    Put_Line (Name, Dot, Normal (Metric.Statements, Max_Dig),
              Normal (Percent, 4), Normal (Lines, Max_Dig), Width);
  end Put_Line;

  procedure Put_Header (Width : in Positive := Default_Width) is
  begin
    Open_File;
    Put_Line ("File", ' ', " Statemts", "%Cmt", "    Lines", Width);
    Put_Delimiter (Width);
  end Put_Header;

   -- Put the total
  procedure Put_Total (Metric  : in Metrics;
                       Summary : in Boolean;
                       Width   : in Positive := Default_Width) is

    Total_Str : constant String
                  := "TOTAL (statms, %cmt, lines)";
    Short_Str : constant String := "TOTAL:";
    Title : As.U.Asu_Us;

  begin
    Open_File;
    -- Summary so far
    if Summary then
      -- Put formated output
      Put_Delimiter (Width);
      if Total_Str'Length <= Width then
        Title.Set (Total_Str);
      elsif Short_Str'Length <= Width then
        Title.Set (Short_Str);
      else
        Title.Set_Null;
      end if;
      Put_Line (Title.Image, ' ', Metric, Width);
    else
      -- Just put number of statements
      Text_Line.Put (File, Normal(Metric.Statements, Max_Dig));
    end if;
    -- Close output file
    Text_Line.Close (File);
  end Put_Total;

  -- Put metrics of a file
  procedure Put_File (
             File_Name : in String;
             Metric    : in Metrics;
             Width     : in Positive := Default_Width) is
  begin
    -- Put formatted output
    Open_File;
    Put_Line (File_Name, '.', Metric, Width);
  end Put_File;

end One_File_Statements;

