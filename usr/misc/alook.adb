-- Make a ADA 83 sources look like a Ada 95 one.
-- Each upper case character, if preceeded by an upper case
--  is set to lower case.
-- Strings and comments are not modified.
with Ada.Text_Io, Ada.Direct_Io, Ada.Exceptions;

with Argument, Lower_Char;

procedure Look_95 is

  package Char_Io is new Ada.Direct_Io (Character);

  -- Process one file
  procedure Do_One (File_Name : in String) is
    -- Current file
    File : Char_Io.File_Type;
    -- Current index in file
    Index : Char_Io.Positive_Count;

    -- Current and prev character
    Char, Prev_Char : Character;
    -- Are they upper case
    Prev_Is_Upper, Curr_Is_Upper : Boolean;

    -- Are we in a comment, in a string
    In_Comment, In_String : Boolean;

    -- Do we proceed current character
    Proceed : Boolean;

    -- End of line/comment
    New_Line : constant Character := Ascii.Lf;

    -- Is Char an upper case
    function Is_Upper (Char : Character) return Boolean is
    begin
      return Char in 'A' .. 'Z';
    end Is_Upper;

    use type Char_Io.Positive_Count;

  begin
    -- Open file
    begin
      Char_Io.Open (File, Char_Io.InOut_File, File_Name);
    exception
      when Char_Io.Name_Error =>
        Ada.Text_Io.Put_Line ("Error. Cannot open file " & File_Name
           & " skipping.");
        return;
      when Error : others =>
        Ada.Text_Io.Put_Line ("Error. Cannot open file " & File_Name
           & " Exception " & Ada.Exceptions.Exception_Name (Error)
           & " skipping.");
        return;
    end;

    -- Init
    Prev_Is_Upper := False;
    In_String := False;
    In_Comment := False;
    Prev_Char := Ascii.Nul;
    Index := 1;

    -- Conversion loop:
    -- If upper_case and previous also upper_case, write lower_case
    while not Char_Io.End_Of_File (File) loop

      -- Read char
      Char_Io.Read (File, Char);

      -- Check in comment. Set Proceed.
      if In_Comment then
        Proceed := False;
        if Char = New_Line then
          -- End of line: end of comment
          In_Comment := False;
        end if;
      else
        Proceed := True;
        if Char = '-' and then Prev_Char = '-' then
          In_Comment := True;
          Proceed := False;
        end if;
      end if;

      -- Check in string. Update Proceed
      if Proceed and then Char = '"' then
        In_String := not In_String;
      end if;
      if In_String then
        Proceed := False;
      end if;

      if Proceed then
        Curr_Is_Upper := Is_Upper (Char);

        -- Convert?
        if Prev_Is_Upper and then Curr_Is_Upper then
          Char_Io.Write (File, Lower_Char (Char), Index);
        end if;

        -- Prepare for next char
        Prev_Is_Upper := Curr_Is_Upper;
        Prev_Char := Char;
      else
        Prev_Is_Upper := False;
        Prev_Char := Ascii.Nul;
      end if;
      Index := Index + 1;

    end loop;
      
    -- Close
    Char_Io.Close (File);

  exception
    when Error : others =>
      Ada.Text_Io.Put_Line (
           "Error. While processing file " & File_Name
           & " at offset " & Char_Io.Positive_Count'Image (Index)
           & " Exception " & Ada.Exceptions.Exception_Name (Error)
           & " skipping.");
      Char_Io.Close (File);
  end Do_One;

begin
  -- Process all arguments (file names)
  for I in 1 .. Argument.Get_Nbre_Arg loop
    Ada.Text_Io.Put_Line (Argument.Get_Parameter (I));
    Do_One (Argument.Get_Parameter (I));
  end loop;
end Look_95;

