-- Make a ADA 83 sources look like a Ada 95 one.
-- Each upper case character, if preceeded by an upper case
--  is set to lower case.
-- Strings, comments and based literals are not modified.
with Ada.Text_Io, Ada.Exceptions;

with Argument, Lower_Char, Bloc_Io;

procedure Look_95 is

  package Reading is
    procedure Open(File_Name : in String);
    Name_Error : exception;

    -- Closes and raises End_Of_File;
    function Next_Char return Character;
    End_Of_File : exception;

    -- Forced close
    procedure Close;

    -- Modify last read char
    procedure Update_Char(New_Char : in Character);
  end Reading;

  package body Reading is

    package Char_Io is new Bloc_Io (Character);
    File : Char_Io.File_Type;

    Bloc_Size : constant Char_Io.Count := 1024;

    File_Size : Char_Io.Count;
    Nb_Bloc : Char_Io.Count;

    Bloc : Char_Io.Element_Array(1 .. Bloc_Size);
    Curr_Bloc : Char_Io.Count;

    Sub_Index : Char_Io.Count;
    Last_Index : Char_Io.Count;
    Modified : Boolean;

    procedure Open(File_Name : in String) is
      use type Char_Io.Count;
    begin
      -- Open and init file metrics
      Char_Io.Open(File, Char_Io.Inout_File, File_Name);
      File_Size := Char_Io.Size(File);
      Nb_Bloc := File_Size / Bloc_Size;
      if File_Size rem Bloc_Size /= 0 then
        Nb_Bloc := Nb_Bloc + 1;
      end if;

      -- Current indexes
      Curr_Bloc := 0;
      Modified := False;
      Last_Index := 0;
      -- Simulate end of previous bloc (for reading new)
      Sub_Index := Last_Index;
    exception
      when Char_Io.Name_Error =>
        raise Name_Error;
      when others =>
        begin
          Close;
        exception
          when others =>
            null;
        end;
        raise;
    end Open;

    procedure Close is
    begin
      Char_Io.Close(File);
    end Close;

    function Next_Char return Character is
      use type Char_Io.Count;
    begin
      if Sub_Index = Last_Index then
        -- End of bloc
        if Modified then
          -- Need to write before read
          Char_Io.Write(File, Bloc(1 .. Last_Index),
                        (Curr_Bloc - 1) * Bloc_Size + 1);
          Modified := False;
        end if;
        if Curr_Bloc = Nb_Bloc then
          -- No more bloc
          Close;
          raise End_Of_File;
        end if;
        -- Read next bloc
        Curr_Bloc := Curr_Bloc + 1;
        if Curr_Bloc = Nb_Bloc then
          if File_Size rem Bloc_Size = 0 then
            Last_Index := Bloc_Size;
          else
            Last_Index := File_Size rem Bloc_Size;
          end if;
        else
          Last_Index := Bloc_Size;
        end if;
        Char_Io.Read(File, Bloc(1 .. Last_Index));
        Sub_Index := 1;
      else
        Sub_Index := Sub_Index + 1;
      end if;
      return Bloc(Sub_Index);
    end Next_Char;

    procedure Update_Char(New_Char : in Character) is
    begin
      Bloc(Sub_Index) := New_Char;
      Modified := True;
    end Update_Char;

  end Reading;

  -- Process one file
  function Do_One(File_Name : in String) return Boolean is

    -- Current and prev character
    Char, Prev_Char : Character;
    -- Are they upper case
    Prev_Is_Upper, Curr_Is_Upper : Boolean;

    -- Are we in a comment, in a string, in a literal
    In_Comment, In_String, In_Literal : Boolean;

    -- Do we proceed current character
    Proceed : Boolean;

    -- End of line/comment
    New_Line : constant Character := Ascii.Lf;

    -- File has been changed
    Modified : Boolean;

    -- Is Char an upper case
    function Is_Upper (Char : Character) return Boolean is
    begin
      return Char in 'A' .. 'Z';
    end Is_Upper;

  begin
    -- Open file
    begin
      Reading.Open(File_Name);
    exception
      when Reading.Name_Error =>
        Ada.Text_Io.Put_Line("Error. Cannot open file " & File_Name
           & " skipping.");
        return False;
      when Error : others =>
        Ada.Text_Io.Put_Line("Error. Cannot open file " & File_Name
           & " Exception " & Ada.Exceptions.Exception_Name (Error)
           & " skipping.");
        return False;
    end;

    -- Init
    Prev_Is_Upper := False;
    In_String := False;
    In_Comment := False;
    In_Literal := False;
    Prev_Char := Ascii.Nul;
    Modified := False;

    -- Conversion loop:
    -- If upper_case and previous also upper_case, write lower_case
    loop

      -- Read char
      begin
        Char := Reading.Next_Char;
      exception
        when Reading.End_Of_File =>
          exit;
      end;

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
        if not In_String and then Prev_Char /= ''' then
          In_String := True;
        elsif In_String then
          In_String := False;
        end if;
      end if;
      if In_String then
        Proceed := False;
      end if;

      -- Check in literal. Update Proceed
      if Proceed and then Char = '#' and then Prev_Char /= ''' then
        In_Literal := not In_Literal;
      end if;
      if In_Literal then
        Proceed := False;
      end if;

      if Proceed then
        Curr_Is_Upper := Is_Upper(Char);

        -- Convert?
        if Prev_Is_Upper and then Curr_Is_Upper then
          Modified := True;
          Reading.Update_Char(Lower_Char(Char));
        end if;

        -- Prepare for next char
        Prev_Is_Upper := Curr_Is_Upper;
        Prev_Char := Char;
      else
        Prev_Is_Upper := False;
        Prev_Char := Ascii.Nul;
      end if;

    end loop;

    return Modified;

  exception
    when Error : others =>
      Ada.Text_Io.Put_Line (
           "Error. While processing file " & File_Name
           & " Exception " & Ada.Exceptions.Exception_Name (Error)
           & " skipping.");
      Reading.Close;
      return Modified;
  end Do_One;

  First_File : Positive;
  Verbose    : Boolean;

begin

  -- Help
  if Argument.Get_Parameter = "-h" then
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                                    & " [ -v ] [ { <file> } ]");
    return;
  end if;

  -- Verbose optional flag
  First_File := 1;
  Verbose := False;
  if Argument.Get_Parameter = "-v" then
    Verbose := True;
    First_File := 2;
  end if;


  -- Process all remaining arguments (file names)
  for I in First_File .. Argument.Get_Nbre_Arg loop
    if Do_One (Argument.Get_Parameter (I)) then
      -- Always trace altered files
      Ada.Text_Io.Put_Line (Argument.Get_Parameter (Occurence => I) & " *");
    elsif Verbose then
      -- Trace unaltered files if verbose
      Ada.Text_Io.Put_Line (Argument.Get_Parameter (Occurence => I) & " =");
    end if;
  end loop;
end Look_95;

