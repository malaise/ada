-- Make ADA 83 sources, or badly cased but valid Ada sources,
--  look like a Ada 95, 05 and 2012 one.
-- Reserved words are set in lower_case and other identifiers in Mixed_Case
--  (e.g. Ident4Good).
-- Numeric literals (extended digits and exponent) are set in UPPERCASE (e.g.
--  16#FF#).
-- Strings and comments are not modified.
-- A Line_Feed is appended to the file when the last char is not a Line_Feed.
--
-- Verbose mode lists all processed files with a "=" (not modified)
--  or a "*" (modified).
-- Silent mode does not list any file.
-- Default mode lists all the modified files (with no "*").
-- Test mode as well but does not modify.
--
-- Warnings can be activated (no correction is done):
--  when two consecutive upper case are found in comment.
--
-- Debug displays the modified words.

with Ada.Direct_Io, Ada.Exceptions;
with Aski, As.U, Argument, Bloc_Io, Ada_Words.Keywords,
     Lower_Str, Mixed_Str, Upper_Str, Basic_Proc, Trace.Loggers;
procedure Alook is

  Logger : Trace.Loggers.Logger;

  package Reading is

    -- Open file for Next_Char
    procedure Open(File_Name : in String; Read_Only : in Boolean);
    Name_Error : exception;

    -- Closes and raises End_Of_File;
    function Next_Char return Character;
    End_Of_File : exception;

    -- Forced close
    procedure Close;

    -- Current index in file: 0 if closed or nothing read
    --  1 after reading first char...
    package Char_Io is new Bloc_Io (Character);
    subtype Count is Char_Io.Count;
    function Curr_Index return Count;

    -- Update a string at a given offset
    subtype Positive_Count is Char_Io.Positive_Count;
    procedure Update_Str (Str : in String; At_Index : in Positive_Count);

    -- End of line/comment
    Line_Feed : Character renames Aski.Lf;
    -- Words might end by and Ada separator or delimiter
    --  or by a Line_Feed or (DOS format) a Carriage_Return
    Carriage_Return : Character renames Aski.Cr;

    -- Append a line feed in file (which has to be and is left closed)
    procedure Append_Line_Feed (File_Name : in String);
  end Reading;

  package body Reading is
    package Dir_Io is new Ada.Direct_Io (Character);

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

    procedure Open(File_Name : in String; Read_Only : in Boolean) is
      use type Char_Io.Count;
    begin
      -- Open and init file metrics
      if Read_Only then
        Char_Io.Open(File, Char_Io.In_File, File_Name);
      else
        Char_Io.Open(File, Char_Io.Inout_File, File_Name);
      end if;
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
        if Curr_Bloc_No = Nb_Bloc then
          -- No more bloc. Save last bloc
          if Modified then
            Char_Io.Write(File, Curr_Bloc(1 .. Last_Index),
                          (Curr_Bloc_No - 1) * Bloc_Size + 1);
          end if;
          Close;
          raise End_Of_File;
        end if;
        -- Shift blocks by one
        Prev_Bloc(1 .. Last_Index) := Curr_Bloc(1 .. Last_Index);
        Prev_Modified := Modified;
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
        Char_Io.Read(File, Curr_Bloc(1 .. Last_Index),
                     (Curr_Bloc_No - 1) * Bloc_Size + 1);
        Modified := False;
        Sub_Index := 1;
      else
        Sub_Index := Sub_Index + 1;
      end if;
      Curr_Count := Curr_Count + 1;
      return Curr_Bloc(Sub_Index);
    end Next_Char;

    function Curr_Index return Count is (Curr_Count);

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

    procedure Append_Line_Feed (File_Name : in String) is
      Dir_File : Dir_Io.File_Type;
      use type Dir_Io.Count;
    begin
      if Char_Io.Is_Open (File) then
        raise Char_Io.Status_Error;
      end if;
      Dir_Io.Open (Dir_File,  Dir_Io.Inout_File, File_Name);
      Dir_Io.Write (Dir_File, Line_Feed, Dir_Io.Size (Dir_File) + 1);
      Dir_Io.Close (Dir_File);
    end Append_Line_Feed;

  end Reading;

  -- This is the exit code. Like diff:
  -- An  exit status of 0 means no change,
  --  1 means some files have been (or would be if test) modified,
  --  and 2 means trouble on at least one file
  All_Unchanged : constant Natural := 0;
  Some_Modified : constant Natural := 1;
  Problem : constant Natural := 2;
  Exit_Code : Natural := All_Unchanged;

  -- Ada language version (for keywords)
  Version : Ada_Words.Keywords.Language_Versions
          := Ada_Words.Keywords.Default_Version;

  -- Process one file
  function Do_One(File_Name : in String;
                  Do_It : in Boolean;
                  Warn_Comment : in Boolean) return Boolean is

    -- Current, prev and prev of prev characters
    Char, Prev_Char, Prev_Prev_Char : Character;
    Nul : Character renames Aski.Nul;

    -- Are they upper case
    Prev_Is_Upper, Curr_Is_Upper : Boolean;

    -- Are we in a comment, in a string
    In_Comment, In_String : Boolean;

    -- Do we proceed current character
    Proceed : Boolean;

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

    -- End of file reached
    End_Of_File : Boolean;

    -- Current line for warnings
    Line : As.U.Asu_Us;

    -- Current word and exception when it is filled
    Word_Error : exception;
    Word : As.U.Asu_Us;

    -- Is Char an upper case
    function Is_Upper (Char : Character) return Boolean is (Char in 'A' .. 'Z');

    -- Check if word case is correct
    procedure Check_Word is
      Str : constant String := Word.Image;
      Is_Keyword : Boolean;
      procedure Change_Word (New_Str : in String) is
      begin
        if Do_It then
          Reading.Update_Str (New_Str, Word_Index);
        end if;
        Modified := True;
        Logger.Log_Debug ("In file " & File_Name
                        & " at line" & Line_No'Img
                        & ": " & Str
                        & "->" & New_Str);
      end Change_Word;

    begin
      -- Log at lower debug level
      Logger.Log (16#20#, Str);
      if Str(1) >= '0' and then Str(1) <= '9' then
        -- Convert numeric in upper case
        if Str /= Upper_Str (Str) then
          Change_Word (Upper_Str (Str));
        end if;
        return;
      end if;
      -- Identifier or reserved word
      case Ada_Words.Keywords.Check_Keyword (Str, Version) is
        when Ada_Words.Keywords.True =>
          Is_Keyword := True;
        when Ada_Words.Keywords.Maybe =>
          Is_Keyword := not Prev_Tick;
        when Ada_Words.Keywords.False =>
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
        Basic_Proc.Put_Line_Output ("Warning. In file " & File_Name
                                  & " at line" & Line_No'Img);
        Basic_Proc.Put_Line_Output ("--> " & Line.Image);
        Warnings := False;
      end if;
      Line.Set_Null;
      Word.Set_Null;
    end Check_Line;

  begin
    -- Open file
    begin
      Reading.Open(File_Name, not Do_It);
    exception
      when Reading.Name_Error =>
        Basic_Proc.Put_Line_Error ("Error. Cannot open file " & File_Name
           & " for writting. Skipping.");
        Exit_Code := Problem;
        return False;
      when Error : others =>
        Basic_Proc.Put_Line_Error ("Error. Cannot open file " & File_Name
           & " for writting, exception "
           & Ada.Exceptions.Exception_Name (Error)
           & ". Skipping.");
        Exit_Code := Problem;
        return False;
    end;

    -- Init
    Prev_Is_Upper := False;
    In_String := False;
    In_Comment := False;
    Prev_Char := Nul;
    Prev_Prev_Char := Nul;
    Modified := False;
    Warnings := False;
    Line_No := 1;
    Prev_Tick := False;
    End_Of_File := False;
    Line.Set_Null;
    Word.Set_Null;

    -- Conversion loop:
    -- If upper_case and previous also upper_case, write lower_case
    loop

      -- Init Proceed
      Proceed := True;

      -- Read char
      begin
        Char := Reading.Next_Char;
      exception
        when Reading.End_Of_File =>
          End_Of_File := True;
      end;

      -- Check end of line
      if Char = Reading.Line_Feed or else End_Of_File then
        -- End of line or of file (and end of comment, string)
        In_Comment := False;
        In_String := False;
        Proceed := False;
      end if;

      -- Update line char for warnings if possible
      if Char /= Reading.Line_Feed and then not End_Of_File then
        begin
          Line.Append (Char);
        exception
          when Constraint_Error =>
            -- Line is too to big for Line text!
            null;
        end;
      end if;

      -- Check word or append char to word
      if not In_Comment
      and then not In_String then
        if Ada_Words.Is_Separator (Char)
        or else Ada_Words.Is_Delimiter (Char)
        or else Char = Reading.Line_Feed
        or else Char = Reading.Carriage_Return
        or else End_Of_File then
          -- End of word, check it
          if Word.Is_Null
          -- Avoid checking character literal
          or else (Word.Length = 1
                   and then Prev_Prev_Char = ''' and then Char = ''' )then
            null;
          else
            -- Check word
            Check_Word;
            -- Prev_Tick is set until the end of a word
            Prev_Tick := False;
          end if;
          -- Not in word
          Word.Set_Null;
          -- Store tick if not in character literal
          if not Prev_Tick then
            Prev_Tick := Char = ''' and then Prev_Prev_Char /= ''';
          end if;
        else
          -- In word: append if possible
          if Word.Is_Null then
            Word_Index := Reading.Curr_Index;
          end if;
          begin
            Word.Append (Char);
          exception
            when Constraint_Error =>
              raise Word_Error;
          end;
        end if;
      end if;

      -- Check in comment. Update Proceed
      if Proceed
      and then not In_Comment
      and then not In_String
      and then Char = '-'
      and then Prev_Char = '-' then
        -- Entering comment
        In_Comment := True;
        Proceed := False;
        Word.Set_Null;
      end if;

      -- Check in string. Update Proceed
      if Proceed and then Char = '"' then
        if not In_String and then Prev_Char /= ''' then
          -- Entering string
          In_String := True;
          Word.Set_Null;
        elsif In_String then
          -- Leaving String
          In_String := False;
          Proceed := False;
        end if;
      end if;
      if In_String then
        Proceed := False;
      end if;

      -- Store if upper char
      if In_Comment then
        Curr_Is_Upper := Is_Upper(Char);
      end if;

      -- Warning in comments
      if Warn_Comment
      and then In_Comment
      and then Prev_Is_Upper and then Curr_Is_Upper then
        Warnings := True;
      end if;

      -- Show warnings at line level if end of line
      if Char = Reading.Line_Feed or else End_Of_File then
        -- Check warnings
        Check_Line;
        Line_No := Line_No + 1;
      end if;

      -- Done at end of file
      exit when End_Of_File;

      -- Prepare for next char
      if Proceed or else In_Comment then
        if In_Comment then
          Prev_Is_Upper := Curr_Is_Upper;
        end if;
        Prev_Prev_Char := Prev_Char;
        Prev_Char := Char;
      else
        Prev_Is_Upper := False;
        Prev_Prev_Char := Nul;
        Prev_Char := Nul;
      end if;

    end loop;

    -- Done: Check that last line ends with Line_Feed
    if Char /= Reading.Line_Feed and then Do_It then
      Reading.Append_Line_Feed (File_Name);
      Modified := True;
      Logger.Log_Debug ("In file " & File_Name
                      & " at line" & Line_No'Img
                      & ": Line_Feed appended");
    end if;

    return Modified;

  exception
    when Error : others =>
      Basic_Proc.Put_Line_Error(
           "Error. While processing file " & File_Name
           & ", exception " & Ada.Exceptions.Exception_Name (Error)
           & ". Skipping.");
      Reading.Close;
      Line.Set_Null;
      Word.Set_Null;
      Exit_Code := Problem;
      return Modified;
  end Do_One;

  type Verbose_Level_List is (Normal, Silent, Verbose, Test);
  Verbose_Level : Verbose_Level_List;
  Warn_Comment : Boolean;

  procedure Put_Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
         & " [ { -v | -s | -t | -n | -C | -c | <file> } ]");
    Basic_Proc.Put_Line_Output ("Verbose levels (exclusive): " &
                          "Verbose, Silent, Normal or Test");
    Basic_Proc.Put_Line_Output ("Warnings on comments (on/off): if upper case");
  end Put_Usage;

  Arg : As.U.Asu_Us;
begin

  Logger.Init;
  -- Help
  if Argument.Get_Nbre_Arg = 1
  and then Argument.Get_Parameter = "-h" then
    Put_Usage;
    return;
  end if;

  Verbose_Level := Normal;
  Warn_Comment := False;

  -- Process all remaining arguments (file names)
  for I in 1 .. Argument.Get_Nbre_Arg loop

    Argument.Get_Parameter (Arg, I);
    -- Change verbose level?
    if Arg.Image = "-d" then
      Logger.Add_Mask (Trace.Debug);
    elsif Arg.Image = "-v" then
      Verbose_Level := Verbose;
    elsif Arg.Image = "-s" then
      Verbose_Level := Silent;
    elsif Arg.Image = "-n" then
      Verbose_Level := Normal;
    elsif Arg.Image = "-t" then
      Verbose_Level := Test;
    elsif Arg.Image = "-C" then
      Warn_Comment := True;
    elsif Arg.Image = "-c" then
      Warn_Comment := False;
    elsif Arg.Length >= 7
    and then Arg.Slice (1, 5) = "--Ada" then
      -- Change Ada language version
      begin
        Version := Ada_Words.Keywords.Language_Versions'Value(
            Arg.Slice (3, Arg.Length));
      exception
        when others =>
          Basic_Proc.Put_Line_Error ("Invalid Argument");
          raise;
      end;
    else
      -- Process file
      if Do_One (Argument.Get_Parameter (I),
                 Verbose_Level /= Test,
                 Warn_Comment) then
        -- Trace altered files if not silent
        if Verbose_Level /= Silent then
          Basic_Proc.Put_Output (Argument.Get_Parameter (Occurence => I));
          if Verbose_Level = Verbose then
            Basic_Proc.Put_Line_Output (" *");
          else
            Basic_Proc.New_Line_Output;
          end if;
        end if;
        if Exit_Code /= Problem then
          Exit_Code := Some_Modified;
        end if;
      elsif Verbose_Level = Verbose then
        -- Trace unaltered files if verbose
        Basic_Proc.Put_Line_Output (Argument.Get_Parameter (Occurence => I)
                                  & " =");
      end if;
    end if;
  end loop;

  Basic_Proc.Set_Exit_Code (Exit_Code);
exception
  when others =>
    Basic_Proc.Set_Exit_Code (Problem);
    raise;
end Alook;

