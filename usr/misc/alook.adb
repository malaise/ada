-- Make a ADA 83 sources look like a Ada 95 one.
-- Reserved words are in lower_case and others in Mixed_Case.
-- Strings, comments and based literals are not modified.
--
-- Verbose mode lists all processed files with a "=" (not modified)
--  or a "*" (modified).
-- Silent mode does not list any file.
-- Default mode lists all the modified files (with no "*").
-- Test mode as well but does not modify.
--
-- Warnings can be activated (no correction is done):
--  when last character is not a New_Line,
--  when two consecutive upper case are found in comment.
--
-- Debug displays the modified words.

with Ada.Text_Io, Ada.Exceptions;

with Argument, Lower_Char, Bloc_Io, Text_Handler, Ada_Words,
     Lower_Str, Mixed_Str;

procedure Look_95 is

  Debug : Boolean := False;

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

    -- Current index in file: 0 if closed or nothing read
    --  1 after reading first char...
    package Char_Io is new Bloc_Io (Character);
    subtype Count is Char_Io.Count;
    function Curr_Index return Count;

    -- Update a string at a given offset
    subtype Positive_Count is Char_Io.Positive_Count;
    procedure Update_Str (Str : in String; At_Index : in Positive_Count);
  end Reading;

  package body Reading is

    File : Char_Io.File_Type;

    Bloc_Size : constant Char_Io.Count := 1024;

    File_Size : Char_Io.Count;
    Nb_Bloc : Char_Io.Count;
    Curr_Count : Char_Io.Count := 0;

    subtype Bloc_Type is Char_Io.Element_Array(1 .. Bloc_Size);
    Curr_Bloc : Bloc_Type;
    Prev_Bloc : Bloc_Type;
    Curr_Bloc_No : Char_Io.Count;

    Sub_Index : Char_Io.Count;
    Last_Index : Char_Io.Count;
    Modified : Boolean;
    Prev_Modified : Boolean;

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
      Curr_Bloc_No := 0;
      Modified := False;
      Prev_Modified := False;
      Last_Index := 0;
      Curr_Count := 0;
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
      Curr_Count := 0;
    end Close;

    function Next_Char return Character is
      use type Char_Io.Count;
    begin
      if Sub_Index = Last_Index then
        -- End of bloc
        if Prev_Modified then
          -- Need to write before read
          Char_Io.Write(File, Prev_Bloc(1 .. Bloc_Size),
                        (Curr_Bloc_No - 2) * Bloc_Size + 1);
        end if;
        -- Shift blocks by one
        Prev_Bloc(1 .. Last_Index) := Curr_Bloc(1 .. Last_Index);
        Prev_Modified := Modified;
        if Curr_Bloc_No = Nb_Bloc then
          -- No more bloc. Save last bloc
          Char_Io.Write(File, Curr_Bloc(1 .. Last_Index),
                        (Curr_Bloc_No - 1) * Bloc_Size + 1);
          Close;
          raise End_Of_File;
        end if;
        -- Read next bloc
        Curr_Bloc_No := Curr_Bloc_No + 1;
        if Curr_Bloc_No = Nb_Bloc then
          if File_Size rem Bloc_Size = 0 then
            Last_Index := Bloc_Size;
          else
            Last_Index := File_Size rem Bloc_Size;
          end if;
        else
          Last_Index := Bloc_Size;
        end if;
        Modified := False;
        Char_Io.Read(File, Curr_Bloc(1 .. Last_Index),
                     (Curr_Bloc_No - 1) * Bloc_Size + 1);
        Sub_Index := 1;
      else
        Sub_Index := Sub_Index + 1;
      end if;
      Curr_Count := Curr_Count + 1;
      return Curr_Bloc(Sub_Index);
    end Next_Char;

    procedure Update_Char(New_Char : in Character) is
    begin
      Curr_Bloc(Sub_Index) := New_Char;
      Modified := True;
    end Update_Char;

    function Curr_Index return Count is
    begin
      return Curr_Count;
    end Curr_Index;

    -- Update a string at a given offset
    Bloc_Error : exception;
    procedure Update_Str (Str : in String; At_Index : in Positive_Count) is
      -- Index of first char of current bloc
      Bloc_Index : Positive_Count;
      -- Index of first char of the sub-string in current bloc
      Start_Index : Positive_Count;
      -- Index of last char of Str
      Last_Index : Positive_Count;
      -- Nb of chars to copy in bloc
      Nb_Char : Natural;
      use type Char_Io.Count;

      procedure Cp2Bloc (Str : in String;
                            From_Id : Positive;
                            Nb : Natural;
                            Bloc : in out Char_Io.Element_Array;
                            To_Index : in Positive_Count) is
        I : Natural;
        J : Positive_Count;
      begin
        I := 0;
        J := To_Index;
        while I < Nb loop
          Bloc(J) := Str(From_Id + I);
          I := I + 1;
          exit when I = Nb;
          J := J + 1;
        end loop;
      end Cp2Bloc;
         
      
    begin
      -- Sanity checks raising anonymous Bloc_Error
      -- Something read and index before current
      if At_Index > Curr_Count then
        raise Bloc_Error;
      end if;
      -- Index not before previous block
      if Curr_Bloc_No > 2
      and then At_Index <= (Curr_Bloc_No - 2) * Bloc_Size then
        raise Bloc_Error;
      end if;
      -- Index of last char of Str
      Last_Index := At_Index + Str'Length - 1;
      -- End of Str not after Curr_Count
      if Last_Index > Curr_Count then
        raise Bloc_Error;
      end if;

      -- Discard empty string
      if Str'Length = 0 then
        return;
      end if;

      -- Index of first char of current bloc
      Bloc_Index := ((Curr_Count - 1) / Bloc_Size) * Bloc_Size + 1;

      -- Write prev if needed, compute rest of chars to write and where
      if At_Index < Bloc_Index then
        Nb_Char := Natural(Bloc_Index - At_Index);
        Cp2Bloc (Str, Str'First, Nb_Char,
                 Prev_Bloc, Bloc_Size - Count(Nb_Char) + 1);
        Prev_Modified := True;
        Nb_Char := Str'Length - Nb_Char;
        Start_Index := Bloc_Index;
      else
        Nb_Char :=  Str'Length;
        Start_Index := At_Index;
      end if;

      -- Save remaining in current bloc
      if Last_Index >= Bloc_Index then
        -- Index in Curr_Block of start of string
        Start_Index := (Start_Index - 1) rem  Bloc_Size + 1;
        Cp2Bloc (Str, Str'Last - Nb_Char + 1, Nb_Char,
                 Curr_Bloc, Start_Index);
        Modified := True;
      end if;

    end Update_Str;

  end Reading;

  -- These ones are static because they are big
  -- Current line for warnings
  Line : Text_Handler.Text (Text_Handler.Max_Len_Range'Last);
  -- Current word and exception when it is full
  Word_Error : exception;
  Word : Text_Handler.Text (Text_Handler.Max_Len_Range'Last);

  -- Process one file
  function Do_One(File_Name : in String;
                  Do_It : in Boolean;
                  Warn_Comment, Warn_Newline : in Boolean) return Boolean is

    -- Current and prev character
    Char, Prev_Char : Character;

    -- Last character when eod of file
    Last_Char : Character;

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

    -- Current line number
    Line_No : Positive;

    -- At least one warning has been detected on current line
    Warnings : Boolean;

    -- Absolute index of word start in file
    Word_Index : Reading.Positive_Count;

    -- Significant character (not newline nor string)
    --  before current word was '
    Prev_Tick : Boolean;

    -- Is Char an upper case
    function Is_Upper (Char : Character) return Boolean is
    begin
      return Char in 'A' .. 'Z';
    end Is_Upper;

    -- Check if word case is correct
    procedure Check_Word is
      Str : constant String := Text_Handler.Value (Word);
      Is_Keyword : Boolean;
      procedure Change_Word (New_Str : in String) is
      begin
        if Do_It then
          Reading.Update_Str (New_Str, Word_Index);
        end if;
        Modified := True;
        if Debug then
          Ada.Text_Io.Put_Line ("In file " & File_Name
                              & " at line" & Line_No'Img
                              & ": " & Str
                              & "->" & New_Str);
        end if;
      end Change_Word;

    begin
      if Text_Handler.Empty (Word) then
        return;
      end if;
      case Ada_Words.Check_Keyword (Str) is
        when Ada_Words.Is_Keyword =>
          Is_Keyword := True;
        when Ada_Words.May_Be_Keyword =>
          Is_Keyword := not Prev_Tick;
        when Ada_Words.Is_Not_Keyword =>
          Is_Keyword := False;
      end case;
      if Is_Keyword and then Str /= Lower_Str (Str) then
        Change_Word (Lower_Str (Str));
        return;
      end if;
      if not Is_Keyword and then Str /= Mixed_Str (Str) then
        Change_Word (Mixed_Str (Str));
        return;
      end if;

    end Check_Word;

    -- Check if warning to display and clear line
    procedure Check_Line is
    begin
      if Warnings then
        Ada.Text_Io.Put_Line("Warning. In file " & File_Name
                           & " at line" & Line_No'Img);
        Ada.Text_Io.Put_Line("--> " & Text_Handler.Value (Line));
        Warnings := False;
      end if;
      Text_Handler.Empty (Line);
      Text_Handler.Empty (Word);
    end Check_Line;

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
    Last_Char := Ascii.Nul;
    Modified := False;
    Warnings := False;
    Line_No := 1;
    Prev_Tick := False;

    -- Conversion loop:
    -- If upper_case and previous also upper_case, write lower_case
    loop

      -- Read char
      begin
        Char := Reading.Next_Char;
        Last_Char := Char;
      exception
        when Reading.End_Of_File =>
          -- Done: Check warnings
          Check_Line;
          if Last_Char /= New_Line and then Warn_Newline then
            Ada.Text_Io.Put_Line("Warning. Missing last Newline in file "
                               & File_Name
                               & " at line" & Line_No'Img);
          end if;
          exit;
      end;

      -- Init Proceed
      Proceed := True;


      -- Check end of line or store char
      if Char = New_Line then
        -- End of line (and end of comment)
        In_Comment := False;
        Proceed := False;
        Text_Handler.Empty (Word);
        -- Check warnings
        Check_Line;
        Line_No := Line_No + 1;
      end if;

      -- Check in comment
      if Proceed
      and then not In_Comment
      and then not In_String
      and then Char = '-'
      and then Prev_Char = '-' then
        -- Entering comment
        In_Comment := True;
        Proceed := False;
        Text_Handler.Empty (Word);
      end if;
      if In_Comment then
        Proceed := False;
      end if;

      -- Check in string. Update Proceed
      if Proceed and then Char = '"' then
        if not In_String and then Prev_Char /= ''' then
          -- Entering string
          In_String := True;
          Text_Handler.Empty (Word);
        elsif In_String then
          -- Leaving String
          In_String := False;
          Proceed := False;
        end if;
      end if;
      if In_String then
        Proceed := False;
      end if;

      -- Check in literal. Update Proceed
      if Proceed and then Char = '#' and then Prev_Char /= ''' then
        if not In_Literal then
          -- Entering literal
          Text_Handler.Empty (Word);
        end if;
        In_Literal := not In_Literal;
      end if;
      if In_Literal then
        Proceed := False;
      end if;

      if Proceed or else In_Comment then
        Curr_Is_Upper := Is_Upper(Char);
      end if;

      -- Update line char for warnings if possible
      if Char /= New_Line then
        begin
          Text_Handler.Append (Line, Char);
        exception
          when Constraint_Error =>
            -- Line is too to big for Line text!
            null;
        end;
      end if;

      -- Check word
      if not In_Comment
      and then not In_String
      and then not In_Literal then
        if Ada_Words.Is_Separator(Char) 
        or else Ada_Words.Is_Delimiter(Char) then
          -- End of word, check it
          -- The tricky way to avoid checking character literal is
          --  to dicard a word of one char with Prev_Tick set!
          if Text_Handler.Length (Word) /= 1 or else not Prev_Tick then
            Check_Word;
          end if;
          -- Not in word
          Text_Handler.Empty (Word);
          Prev_Tick := Char = ''';
        else
          -- In word: append if possible
          if Text_Handler.Empty (Word) then
            Word_Index := Reading.Curr_Index;
          end if;
          begin
            Text_Handler.Append (Word, Char);
          exception
            when Constraint_Error =>
              raise Word_Error;
          end;
        end if;
      end if;

      -- Warning in comments
      if Warn_Comment
      and then In_Comment
      and then Prev_Is_Upper and then Curr_Is_Upper then
        Warnings := True;
      end if;

        -- Prepare for next char
      if Proceed or else In_Comment then
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
      Text_Handler.Empty (Line);
      Text_Handler.Empty (Word);
      return Modified;
  end Do_One;

  type Verbose_Level_List is (Normal, Silent, Verbose, Test);
  Verbose_Level : Verbose_Level_List;
  Warn_Comment, Warn_Newline : Boolean;

begin

  -- Help
  if Argument.Get_Nbre_Arg = 0
  or else Argument.Get_Parameter = "-h" then
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
         & " [ { -v | -s | -t | -n | -C | -N <file> } ]");
    Ada.Text_Io.Put_Line ("Verbose levels (exclusive): " &
                          "Verbose, Silent, Normal or Test");
    Ada.Text_Io.Put_Line ("Warnings: on upper_Case in comments");
    Ada.Text_Io.Put_Line ("          or missing last New_Line");
    return;
  end if;

  Verbose_Level := Normal;
  Warn_Comment := False;
  Warn_Newline := False;

  -- Process all remaining arguments (file names)
  for I in 1 .. Argument.Get_Nbre_Arg loop

    -- Change verbose level?
    if Argument.Get_Parameter (I) = "-d" then
      Debug := True;
    elsif Argument.Get_Parameter (I) = "-v" then
      Verbose_Level := Verbose; 
    elsif Argument.Get_Parameter (I) = "-s" then
      Verbose_Level := Silent;
    elsif Argument.Get_Parameter (I) = "-n" then
      Verbose_Level := Normal;
    elsif Argument.Get_Parameter (I) = "-t" then
      Verbose_Level := Test;
    elsif Argument.Get_Parameter (I) = "-C" then
      Warn_Comment := True;
    elsif Argument.Get_Parameter (I) = "-N" then
      Warn_Newline := True;
    else
      -- Process file
      if Do_One (Argument.Get_Parameter (I),
                 Verbose_Level /= Test,
                 Warn_Comment, Warn_Newline) then
        -- Trace altered files if not silent
        if Verbose_Level /= Silent then
          Ada.Text_Io.Put (Argument.Get_Parameter (Occurence => I));
          if Verbose_Level = Verbose then
            Ada.Text_Io.Put_Line (" *");
          else
            Ada.Text_Io.New_Line;
          end if;
        end if;
      elsif Verbose_Level = Verbose then
        -- Trace unaltered files if verbose
        Ada.Text_Io.Put_Line (Argument.Get_Parameter (Occurence => I) & " =");
      end if;
    end if;
  end loop;
end Look_95;

