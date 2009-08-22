with Ada.Text_Io;
with Normal, Sys_Calls, Text_Line;

package body One_File_Statements is

  Total : Natural := 0;
  File_Error : exception;
  function Count_Statements_Of_File (File_Name : String) return Natural is

    File  : Ada.Text_Io.File_Type;
    C, Prev_C, Next_C  : Character := ' ';
    Next_Set : Boolean := False;
    Statements : Natural := 0;
    Levels : Natural := 0;

    procedure Get (File : in Ada.Text_Io.File_Type; C : out Character) is
    begin
      Ada.Text_Io.Get (File, C);
    exception
      when Ada.Text_Io.End_Error =>
        raise;
      when others =>
        Sys_Calls.Put_Line_Error ("Exception when reading line "
                       & Ada.Text_Io.Positive_Count'Image(Ada.Text_Io.Line(File))
                       & " of file " & File_Name);
        raise File_Error;
    end Get;

    procedure Close (File : in out Ada.Text_Io.File_Type) is
    begin
      Ada.Text_Io.Close (File);
    exception
      when others => null;
    end Close;

    procedure Skip_Until (Upto : in Character) is
    begin
      loop
        Get (File, C);
        exit when C = Upto;
      end loop;
    end Skip_Until;

    function Line_No return Ada.Text_Io.Positive_Count is
    begin
      return Ada.Text_Io.Line (File);
    end Line_No;

  begin

    begin
      Ada.Text_Io.Open (File, Ada.Text_Io.In_File, File_Name);
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
        Get (File, C);
      end if;

      -- Check for comment on the line
      if C = '-' then
        Get (File, C);
        -- Which is signaled by the '-' following a '-'
        if C = '-' then
          -- Then just skip the rest of the line and go to the next
          Ada.Text_Io.Skip_Line (File);
        else
          -- Restore char
          Next_C := C;
          Next_Set := True;
        end if;

      elsif C = ';' then
        -- Any ';' that can be found at this point after all exclusions
        -- must be a valid "line of code terminator"
        if Levels = 0 then
          -- Skip parentheses cause every ';' within is a formal parameter list
          Statements := Statements + 1;
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
        Skip_Until (C);
      elsif C = ''' then
        -- Character literals are just three characters long including '
        -- Attributes are skipped the same way because longer than one char
        Get (File, Prev_C);
        Get (File, C);
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
    when Ada.Text_Io.End_Error =>
      Close (File);
      if Levels /= 0 then
        Sys_Calls.Put_Line_Error ("Warning: Ending file with parenthesis level"
                            & Levels'Img & ".");
      end if;
      return Statements;
    when File_Error =>
      Close (File);
      raise;
    when others =>
      Close (File);
      Sys_Calls.Put_Line_Error ("Exception when processing line "
                      & Ada.Text_Io.Positive_Count'Image(Ada.Text_Io.Line(File))
                      & " of file " & File_Name);
      raise;
  end Count_Statements_Of_File;

  -- If File_Name is empty, put total so far and reset it
  procedure Print_Statements_Of_File (
             File_Name : String;
             Put_It : in Boolean := True) is

    File : Text_Line.File_Type;
    File_Name_Len : constant Natural := File_Name'Length;
    Count : Integer range -1 ..Integer'Last;
    Max_Tab : constant := 60;
    Max_Dig : constant := 8;
    Gap : constant String := "  ";
  begin

    Text_Line.Open (File, Text_Line.Out_File, Sys_Calls.Stdout);
    if File_Name = "" then
      if Put_It then
        for I in Integer range 1 .. Max_Tab + Gap'Length + Max_Dig + 1 loop
          Text_Line.Put (File, "-");
        end loop;
        Text_Line.New_Line (File);

        declare
          Total_Str : constant String := "TOTAL statements";
        begin
          Text_Line.Put (File, Total_Str);
          for I in Integer range Total_Str'Length .. Max_Tab loop
            Text_Line.Put (File, " ");
          end loop;
        end;
        Text_Line.Put_Line (File, Gap & Normal(Total, Max_Dig));
      else
        Text_Line.Put (File, Normal(Total, Max_Dig));
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
        Text_Line.Put (File, File_Name);
        if File_Name_Len < Max_Tab then
          Text_Line.Put (File, " ");
          for I in File_Name_Len+1 .. Max_Tab loop
            Text_Line.Put (File, ".");
          end loop;
        elsif File_Name_Len > Max_Tab then
          Text_Line.New_Line (File);
          for I in Integer range 1 .. Max_Tab loop
            Text_Line.Put (File, ".");
          end loop;
        end if;

        if Count >= 0 then
          Text_Line.Put_Line (File, Gap & Normal(Count, Max_Dig));
        else
          Text_Line.Put_Line (File, Gap & " SKIPPED");
        end if;
      end if;

      if Count >= 0 then
        Total := Total + Count;
      end if;

    end if;

    Text_Line.Close (File);
  end Print_Statements_Of_File;

end One_File_Statements;

