with Text_Io;
with Normal;

package body One_File_Statements is

  Total : Natural := 0;
  File_Error : exception;
  function Count_Statements_Of_File (File_Name : String) return Natural is

    File  : Text_Io.File_Type;
    C     : Character := ' ';
    Statements : Natural := 0;
    Levels : Natural := 0;

    procedure Get (File : in Text_Io.File_Type; C : out Character) is
    begin
      Text_Io.Get (File, C);
    exception
      when Text_Io.End_Error =>
        raise;
      when others =>
        Text_Io.Put_Line ("Exception raised when reading line "
                        & Text_Io.Positive_Count'Image(Text_Io.Line(File))
                        & " of file " & File_Name);
        raise File_Error;
    end Get;

    procedure Close (File : in out Text_Io.File_Type) is
    begin
      Text_Io.Close (File);
    exception
      when others => null;
    end Close;

  begin

    begin
      Text_Io.Open (File, Text_Io.In_File, File_Name);
    exception
      when others =>
        Text_Io.Put_Line ("Exception raised when opening file " & File_Name);
        raise File_Error;
    end;

    loop

      Get (File, C);

      -- Check for comment on the line
      if C = '-' then
        Get (File, C);
        -- Which is signaled by the '-' following a '-'
        if C = '-' then
          -- Then just skip the rest of the line and go to the next
          Text_Io.Skip_Line (File);
        end if;
      end if;

      -- Check for one of the characters which introduce code constructs
      -- like string or character litteral or formal parameter list
      -- within which a ';' does not terminate a "line of code"

      if C = '(' or C = '"' or C = '%' or C = ''' then

        -- Check for opening parentheses
        -- Every ';' within is a formal parameter list
        if  C = '(' then
          -- Count the number of levels of parentheses
          Levels := Levels + 1;

          -- Read ahead until the whole construct is closed, Level = 0
          while Levels > 0 loop
            Get (File, C);
            if C = '(' then
              -- Increase the level if another '(' is found
              Levels := Levels + 1;
            elsif C = ')' then
              -- Decrease the leval if a ')' is found
              Levels := Levels - 1;
            end if;
          end loop;

        -- Now, check for string brackets of either kind, " or %
        elsif C = '"' or C = '%' then
          -- Treat them in parallel, one must lead off
          if C = '"' then
            loop
              Get (File, C);
              -- Loop until  the close comes
              -- If there is a doubled character it just starts again
              exit when C = '"';
            end loop;
          elsif C = '%' then
            -- The '%' is handled exactly the same way as '"'
            loop
              Get (File, C);
              exit when C = '%';
            end loop;
          end if;

        elsif C = ''' then
          -- Character literals are just three characters long including '
          Get (File, C);
          Get (File, C);
        end if;

    elsif C = ';' then
      -- Any ';' that can be found at this point after all exclusions
      -- must be a valid "line of code terminator"
      Statements := Statements + 1;
    end if;
  end loop;

  exception
    when Text_Io.End_Error =>
      Close (File);
      return Statements;
    when File_Error =>
      Close (File);
      raise;
    when others =>
      Close (File);
      Text_Io.Put_Line ("Exception raised when processing line "
                      & Text_Io.Positive_Count'Image(Text_Io.Line(File))
                      & " of file " & File_Name);
      raise;
  end Count_Statements_Of_File;

  -- If File_Name is empty, put total so far and reset it
  procedure Print_Statements_Of_File (
             File_Name : String;
             Put_It : in Boolean := True) is

    File_Name_Len : constant Natural := File_Name'Length;
    Count : Integer range -1 ..Integer'Last;
    Max_Tab : constant := 60;
    Max_Dig : constant := 8;
    Gap : constant String := "  ";
  begin

    if File_Name = "" then
      if Put_It then
        for I in Integer range 1 .. Max_Tab + Gap'Length + Max_Dig + 1 loop
          Text_Io.Put ("-");
        end loop;
        Text_Io.New_Line;

        declare
          Total_Str : constant String := "TOTAL statements";
        begin
          Text_Io.Put (Total_Str);
          for I in Integer range Total_Str'Length .. Max_Tab loop
            Text_Io.Put (" ");
          end loop;
        end;
        Text_Io.Put_Line (Gap & Normal(Total, Max_Dig));
      end if;
      Total := 0;

    else 

      begin
        Count := One_File_Statements.Count_Statements_Of_File(File_Name);
      exception
        when others =>
          Count := -1;
      end;

      if Put_It then
        Text_Io.Put (File_Name);
        if File_Name_Len < Max_Tab then
          Text_Io.Put (" ");
          for I in File_Name_Len+2 .. Max_Tab loop
            Text_Io.Put (".");
          end loop;
        elsif File_Name_Len > Max_Tab then
          Text_Io.New_Line;
          for I in Integer range 1 .. Max_Tab loop
            Text_Io.Put (".");
          end loop;
        end if;

        if Count >= 0 then
          Text_Io.Put_Line (Gap & Normal(Count, Max_Dig));
        else
          Text_Io.Put_Line (Gap & " SKIPPED");
        end if;
      end if;

      if Count >= 0 then
        Total := Total + Count;
      end if;

    end if;

  end Print_Statements_Of_File;

end One_File_Statements;

