-- Search for words matching criteria (au:o.obile) or regexp (au.*bile)
-- Or search anagrams
with As.U.Utils, Argument, Con_Io, Afpx.Utils, Basic_Proc, Language,
     Str_Util, Lower_Str, Mixed_Str, Environ, Images, Event_Mng, Afpx_Xref,
     Mutexes, Protected_Var, Trilean, Rounds, Normal, Reg_Exp, Long_Longs;
with Database, Cmd, Analist, Icon;
procedure Xwords is

  -- Name of ENV variable for database
  Words_Env_Name : constant String := "DICTIO_WORDS_FILE";
  Nouns_Env_Name : constant String := "DICTIO_NOUNS_FILE";

  procedure Error is
  begin
    Basic_Proc.Put_Line_Error ("Usage : "
                             & Argument.Get_Program_Name & " [ -l ]");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Log option
  Log : Boolean := False;

  -- Afpx stuff
  Get_Handle : Afpx.Get_Handle_Rec;
  Ptg_Result : Afpx.Result_Rec;
  Afpx_Item  : Afpx.Line_Rec;
  type Status_List is (Found, Ok, Error);
  Status     : Status_List;

  -- Fields
  List_Fld : constant Afpx.Absolute_Field_Range := Afpx.List_Field_No;
  Clear_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Clear;
  Recall_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Recall;
  Nouns_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Nouns;
  Get_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Get;
  Scroll_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Scroll;
  Topnum_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Topnum;
  Topof_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Topof;
  Percent_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Percent;
  Anamode_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Anamode;
  Anagrams_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Anagrams;
  Ananouns_Title_Fld : constant Afpx.Field_Range
                     := Afpx_Xref.Main.Ananouns_Title;
  Ananouns_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Ananouns;
  Ananame_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Ananame;
  Search_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Search;
  Research_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Re_Search;
  Add_Word_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Add_Word;
  Add_Noun_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Add_Noun;
  Del_Word_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Del_Word;
  Del_Noun_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Del_Noun;
  History_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.History;
  Clear_List_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Clear_List;
  Lmng_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.List_Mng;
  Exit_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Quit;

  -- History of search requests
  History_List : Cmd.Res_List;
  Moved : Boolean;

  -- A line of text
  Line : As.U.Asu_Us;

  -- Database init status
  Words_File_Name, Nouns_File_Name : As.U.Asu_Us;
  Loading_Database : Boolean;

  -- List is words or history or anagrams
  type List_Content_List is (Words, Anagrams, History, Error, Empty);
  List_Content : List_Content_List := Empty;

  -- Us to Afpx line
  function Us2Afpx (Us : As.U.Asu_Us) return Afpx.Line_Rec is
    Rec : Afpx.Line_Rec;
    List_Width : constant Afpx.Width_Range
               := Afpx.Get_Field_Width (Afpx.List_Field_No);
    Ustr : constant Language.Unicode_Sequence
         := Language.String_To_Unicode (Us.Image);
  begin
    -- Procuste
    Rec.Str := (others => Con_Io.Space);
    if Ustr'Length >= List_Width then
      Rec.Len := List_Width;
    else
      Rec.Len := Ustr'Length;
    end if;
    Rec.Str(1 .. Rec.Len) := Ustr(1 .. Rec.Len);
    return Rec;
  end Us2Afpx;

  -- Put an error in the list
  procedure Put_Error (Msg : in String) is
  begin
      Afpx.Line_List.Delete_List;
      Afpx.Line_List.Insert (Us2Afpx (As.U.Tus ("ERROR: " & Msg & ".")));
      List_Content := Error;
      Status := Error;
  end Put_Error;

  -- Purge trailing spaces
  function Strip (Str : String) return String is (Str_Util.Strip (Str));

  -- Set Afpx selection to current item in list
  procedure Set_Current_Selection is
    Line : Afpx.Line_Rec;
  begin
    if Afpx.Line_List.Is_Empty then
      Afpx.Set_Selection ("");
    else
      Line := Afpx.Line_List.Read (Afpx.Line_List_Mng.Current);
      declare
        Str : constant String
            := Language.Unicode_To_String (Line.Str(1 ..  Line.Len));
      begin
        Afpx.Set_Selection (Strip (Lower_Str (Str)));
      end;
    end if;
  end Set_Current_Selection;

  -- Callback for change of list
  -- On scroll, re-encode IdTop and update scroll bar
  -- Left selection, Set
  procedure List_Cb (Action : in Afpx.List_Change_List;
                     Afpx_Status : in Afpx.List_Status_Rec)  is
    Percent : Afpx.Percent_Range;
    Row : Con_Io.Row_Range;
  begin
    -- Nothing if not a valid list of words
    if Status /= Found or else List_Content = History
    or else List_Content = Empty then
      return;
    end if;

    case Action is
      when Afpx.Left_Selection =>
        -- Set Con_Io selection (inter-window) to the content of Id_Selected
        -- No need to handle Afpx.Init here because this is done
        --  when building the list
        Set_Current_Selection;
      when Afpx.Right_Selection => null;
      when Afpx.Init | Afpx.Scroll =>
        -- Encode Id top
        Afpx.Clear_Field (Topnum_Fld);
        if not Afpx.Line_List.Is_Empty then
          Afpx.Encode_Field (Topnum_Fld, (0, 0),
                             Long_Longs.Image (Afpx_Status.Id_Top));
        end if;
        -- Scroll bar index
        Afpx.Clear_Field (Scroll_Fld);
        Percent := Afpx.Get_List_Percent;
        if Percent /= 0 then
          -- 0 <-> 1% and Height-1 <-> 100%
          -- (Percent-1)/99 = Row/(Height-1)
          Row := Con_Io.Row_Range(Rounds.Roundiv (
             (Afpx.Get_Field_Height (Scroll_Fld) - 1) * (Percent - 1), 99));
          Afpx.Encode_Field (Scroll_Fld, (Row => Row, Col => 0), "-");
        else
          Afpx.Encode_Field (Scroll_Fld, (0, 0), "-");
        end if;
        -- Encode percent
        Afpx.Clear_Field (Percent_Fld);
        if not Afpx.Line_List.Is_Empty then
          if Percent = 0 then
            Percent := 1;
          end if;
          Afpx.Encode_Field (Percent_Fld, (0, 0), Normal (Percent, 3));
        end if;
    end case;
  end List_Cb;

  -- Move Afpx list ot first noun (if any)
  procedure Move_To_Nouns is
    Position : Afpx.Line_List_Mng.Ll_Positive;
    Line : Afpx.Line_Rec;
    Moved : Boolean;
  begin
    if Afpx.Line_List.Is_Empty then
      return;
    end if;
    Position := Afpx.Line_List.Get_Position;
    -- Search for line in uppercase
    Afpx.Line_List.Rewind;
    loop
      Afpx.Line_List.Read (Line, Moved => Moved);
      if Line.Len /= 0
      and then Language.Is_Char (Line.Str(1))
      and then Language.Unicode_To_Char (Line.Str(1)) >= 'A'
      and then Language.Unicode_To_Char (Line.Str(1)) <= 'Z' then
        -- Found the first UPPERCASE line, move it at top of window
        if Moved then
          Afpx.Line_List.Move_To (Afpx.Line_List_Mng.Prev);
        end if;
        Afpx.Update_List (Afpx.Top_Selected);
        exit;
      end if;
      if not Moved then
        -- Reached end of list with only lowercase words
        -- Restore position and done
        Afpx.Line_List.Move_At (Position);
        exit;
      end if;
    end loop;
  end Move_To_Nouns;

  -- Scroll list according to row (of scroll button) clicked
  procedure Do_Scroll (Row : in Con_Io.Row_Range)is
    Percent : Afpx.Percent_Range;
    Position : Afpx.Line_List_Mng.Ll_Natural;
    Saved_Position : Afpx.Utils.Backup_Context;
    use type Afpx.Line_List_Mng.Ll_Natural;
  begin
    if Afpx.Line_List.Is_Empty then
      return;
    end if;
    -- 0 <-> 1% and Height-1 <-> 100%
    -- (Percent-1)/99 = Row/(Height-1)
    Percent := Rounds.Roundiv (
                 Row * 99,
                 Afpx.Get_Field_Height (Scroll_Fld) - 1) + 1;
    Position := Afpx.Get_List_Index (Percent);
    if Position = 0 then
      return;
    end if;
    -- Center to selected index but keep unchanged selected line
    Saved_Position.Backup;
    Afpx.Line_List.Move_At (Position);
    Afpx.Update_List (Afpx.Top_Selected);
    Saved_Position.Restore (Force_Position => True);
  end Do_Scroll;

  -- Is Ananouns set
  function Ananouns_Set return Boolean is
    (Afpx.Decode_Field (Ananouns_Fld, 0) /= " ");

  -- Anagrams of word
  Analen_Prefix : constant String := "-- ";
  Analen_Suffix : constant String := " --";
  -- Previous list of anagrams
  Prev_Word : As.U.Asu_Us;
  Anagrams_List : As.U.Utils.Asu_Ua.Unb_Array;
  -- In Anagrams mode
  In_Anagrams : Boolean;

  procedure Switch_To_Anagrams (To_Anagrams : in Boolean);

  -- List anagrams of word
  procedure Do_Anagrams is
    Word : As.U.Asu_Us;
    Char : Character;
    Length : Natural;
  begin
    if In_Anagrams then
      -- Quit Anagrams mode
      Switch_To_Anagrams (False);
      Afpx.Line_List.Delete_List (Deallocate => False);
      return;
    end if;
    -- Clear result
    Status := Ok;
    Afpx.Line_List.Delete_List (Deallocate => False);
    -- Get word and check it
    Word := As.U.Tus (Strip (Afpx.Decode_Field (Get_Fld, 0, False)));

    -- Check pattern
    if Word.Is_Null then
      Afpx.Line_List.Insert (Us2Afpx (
          As.U.Tus ("ERROR: Missing letter.")));
      List_Content := Error;
      Status := Error;
      return;
    end if;
    for I in 1 .. Word.Length loop
      Char := Word.Element (I);
      if Char < 'a' or else Char > 'z' then
        Afpx.Line_List.Insert (Us2Afpx (
            As.U.Tus ("ERROR: Invalid letter.")));
        List_Content := Error;
        Status := Error;
        return;
      end if;
    end loop;

    -- Get list
    Analist.List (Strip (Afpx.Decode_Field (Get_Fld, 0, False)),
                  Ananouns_Set,
                  Anagrams_List);
    History_List.Insert (Word);

    -- Copy in Afpx list
    Afpx.Line_List.Delete_List (Deallocate => False);
    Length := 0;
    for I in 1 .. Anagrams_List.Length loop
      -- Insert length
      if Anagrams_List.Element(I).Length /= Length then
        Length := Anagrams_List.Element(I).Length;
        Afpx.Line_List.Insert (Us2Afpx (As.U.Tus (
             Analen_Prefix & Images.Integer_Image (Length) & Analen_Suffix)));
      end if;
      Afpx.Line_List.Insert (Us2Afpx (Anagrams_List.Element(I)));
    end loop;
    Afpx.Line_List.Rewind;
    Afpx.Update_List(Afpx.Top);

    -- Update Status
    if Anagrams_List.Is_Null then
      List_Content := Empty;
      Status := Ok;
    else
      List_Content := Anagrams;
      Status := Found;
    end if;
    Prev_Word := Word;
    Switch_To_Anagrams (True);

  exception
    when Analist.Too_Long =>
      Put_Error ("ERROR: Word too long.");
  end Do_Anagrams;

  -- Build and launch a Words command
  procedure Do_Command (Num : Afpx.Field_Range) is
    Result : Cmd.Res_List;
    Command : Cmd.Cmd_List;
    Regex, Noun : Boolean;
    Word : As.U.Asu_Us;
    Command_Ok : Boolean;
    use type As.U.Asu_Us, Afpx.Field_Range, Afpx.Line_List_Mng.Ll_Natural,
             Cmd.Cmd_List;
  begin
    -- This can take some time
    Afpx.Set_Field_Colors (Get_Fld,
        Background => Con_Io.Color_Of ("Cyan"));
    -- Clear result
    Afpx.Line_List.Delete_List (Deallocate => False);
    Afpx.Set_Field_Activation (List_Fld, False);
    Afpx.Put;
    Afpx.Set_Field_Activation (List_Fld, True);

    -- Build command
    Word := As.U.Tus (Lower_Str (Strip (
              Afpx.Decode_Field (Get_Fld, 0, False))));
    case Num is
      when Search_Fld | Research_Fld =>
        Command := Cmd.Search;
      when Add_Word_Fld | Add_Noun_Fld =>
        Command := Cmd.Add;
      when Del_Word_Fld | Del_Noun_Fld =>
        Command := Cmd.Del;
      when others =>
        List_Content := Error;
        Status := Error;
        return;
    end case;
    Regex := Num = Research_Fld;
    Noun := Num = Add_Noun_Fld or else Num = Del_Noun_Fld;

    -- Prevent X events to interfere with the Command internal loop
    --  and execute command
    Cmd.Exec (Command, Regex, Noun, Word.Image, Command_Ok, Result);
    if Command = Cmd.Search and then Command_Ok then
      -- Append result of search noun
      Cmd.Exec (Command, Regex, True, Word.Image, Command_Ok, Result);
    end if;

    -- Set status
    if not Command_Ok then
      List_Content := Error;
      Status := Error;
    elsif Num = Search_Fld or else Num = Research_Fld then
      Status := Found;
      List_Content := Words;
    else
      Status := Ok;
    end if;

    -- Store in history if search
    if (Num = Search_Fld or else Num = Research_Fld)
    and then not Word.Is_Null then
      History_List.Insert (Word);
    end if;

    -- Log request if needed
    if Log then
      Line := Mixed_Str (Command'Img) & " "
      & (if Regex then "Regex " else "")
      & (if Noun then "Noun " else "")
      & Word;
      Basic_Proc.Put_Line_Output (Line.Image);
    end if;

    -- Reset selection on error
    if Status /= Found then
      Afpx.Set_Selection ("");
    end if;

    -- Encode result,
    if Result.Is_Empty then
      -- Set selection to search word/pattern if no result
      Afpx.Set_Selection (Lower_Str (Word.Image));
    else
      Result.Rewind;
      loop
        Result.Read (Line, Moved => Moved);
        Afpx.Line_List.Insert (Us2Afpx (Line));
        if Log then
          Basic_Proc.Put_Line_Output (Line.Image);
        end if;
        exit when not Moved;
      end loop;

      -- Move to Top
      Afpx.Line_List.Rewind;

      if Num = Search_Fld or else Num = Research_Fld then
        -- Set selection to first entry
        Set_Current_Selection;
      else
        if Afpx.Line_List.List_Length < 2 then
          -- Add/del with no result
          Afpx.Set_Selection (Lower_Str (Word.Image));
        else
          -- Add/del with result: word added/deleted is at second line
          Afpx.Line_List.Move_To;
          Set_Current_Selection;
          Afpx.Line_List.Rewind;
        end if;
      end if;

      -- Move to top
      Afpx.Update_List(Afpx.Top);
    end if;

    -- Make ready for a brand new command
    Afpx.Reset_Field (Get_Fld,
      Reset_Colors => True,
      Reset_String => Status /= Error,
      Reset_Activation => True,
      Reset_Protection => True);
  end Do_Command;

  -- Search the content of Get_Fld in the list of anagrams
  procedure Search_In_Anagrams (Is_Regex : in Boolean) is
    In_Crit, Crit, Curr : As.U.Asu_Us;
    Ok : Boolean;
    Crit_Regex : Reg_Exp.Compiled_Pattern;
    Matches : As.U.Utils.Asu_Ua.Unb_Array;
  begin
    -- Decode criteria
    In_Crit := As.U.Tus (Lower_Str (Strip (
              Afpx.Decode_Field (Get_Fld, 0, False))));
    if In_Crit.Is_Null then
      Put_Error ("Missing pattern to search");
      return;
    end if;
    Crit := In_Crit;
    if Is_Regex then
      -- Check regex
      if Reg_Exp.Match ("[A-Z]", Crit.Image, True) then
        Put_Error ("Invalid regular expression");
        return;
      end if;
    else
      -- Check pattern. '*' must be the last character
      if not Reg_Exp.Match ("[a-z?*.:]+", In_Crit.Image, True)
      or else Reg_Exp.Match (".*\*.+", In_Crit.Image, True) then
        Put_Error ("Invalid pattern");
        return;
      end if;
      -- Convert to regex
      Crit := As.U.Tus (Str_Util.Substit (Crit.Image, ":", "."));
      Crit := As.U.Tus (Str_Util.Substit (Crit.Image, "?", "."));
      Crit := As.U.Tus (Str_Util.Substit (Crit.Image, "*", ".*"));
    end if;

    -- Compile pattern
    Crit_Regex.Compile (Ok, Crit.Image);
    if not Ok then
      Put_Error ("Invalid search criteria");
      return;
    end if;

    -- Search in list of anagrams
    for I in 1 .. Anagrams_List.Length loop
      -- Read words
      Curr := As.U.Tus (Lower_Str (Anagrams_List.Element (I).Image));
      if Crit_Regex.Match (Curr.Image, True) then
        -- A real word
        Matches.Append (Curr);
      end if;
    end loop;

    -- Show result
    History_List.Insert (In_Crit);
    List_Content := Words;
    Status := Found;
    Afpx.Line_List.Delete_List (Deallocate => False);
    for I in 1 .. Matches.Length loop
      Afpx.Line_List.Insert (Us2Afpx (Matches.Element (I)));
    end loop;

    -- Ready
    Afpx.Update_List (Afpx.Top);
    if not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Rewind;
    end if;
  end Search_In_Anagrams;

  procedure Do_Recall is
  begin
    Afpx.Clear_Field (Get_Fld);
    if not History_List.Is_Empty then
      History_List.Read (Line, Cmd.Res_Mng.Current);
      Afpx.Encode_Field (Get_Fld, (0, 0), Lower_Str (Line.Image));
    end if;
  end Do_Recall;

  -- Task to load database in background
  task Load_Database is
    entry Start (Words_File_Name, Nouns_File_Name : in As.U.Asu_Us);
    entry Stop;
  end Load_Database;

  -- Database loading status: Ok, Failed or Pending
  package Protected_Trilean is new Protected_Var (Trilean.Trilean);
  Database_Loaded : Protected_Trilean.Protected_T(Mutexes.Simple);
  function Database_Ok return Boolean is
    use type Trilean.Trilean;
  begin
    return Database_Loaded.Get = Trilean.True;
  end Database_Ok;

  -- Task loading the database
  task body Load_Database is
    Words_Name, Nouns_Name : As.U.Asu_Us;
    Ok : Boolean;
  begin

    select
      -- Load: Get file name
      accept Start (Words_File_Name, Nouns_File_Name : in As.U.Asu_Us) do
        Database.Logger.Log_Debug ("Loading");
        Words_Name := Words_File_Name;
        Nouns_Name := Nouns_File_Name;
      end Start;

      -- Load dictionary
      begin
        Database.Init (Words_Name.Image, Nouns_Name.Image);
        Ok := True;
      exception
        when Database.Init_Error =>
          Ok := False;
          Basic_Proc.Put_Line_Error ("Cannot load database");
      end;
      Database.Logger.Log_Debug ("Loaded");
      -- Report completion: Ok or failure
      Database_Loaded.Set (Trilean.Boo2Tri (Ok));
      Database.Logger.Log_Debug ("Reported");
      -- Wake up main task
      Event_Mng.Send_Dummy_Signal;
      Database.Logger.Log_Debug ("Signaled");
    or
      -- Do not load
      accept Stop;
      Database_Loaded.Set (Trilean.False);
    or
      -- If main returns without calling
      terminate;
    end select;

   -- Done
  end Load_Database;

  -- Switch to Anagrams mode
  procedure Switch_To_Anagrams (To_Anagrams : in Boolean) is
  begin
    if To_Anagrams then
      Afpx.Reset_Field (Anamode_Fld);
      Afpx.Clear_Field (Get_Fld);
      Afpx.Encode_Field (Ananame_Fld, (0, 0), Prev_Word.Image);
      Afpx.Clear_Field (Anagrams_Fld);
      Afpx.Utils.Center_Field ("Reset", Anagrams_Fld, 0);
      -- Color of the Reset (Anagrams) button when in Anagrams
      Afpx.Set_Field_Colors (Anagrams_Fld, Con_Io.Color_Of ("Blue"));
      Afpx.Set_Field_Activation (Ananouns_Title_Fld, Ananouns_Set);
      Afpx.Set_Field_Activation (Ananouns_Fld, False);
      In_Anagrams := True;
    else
      Afpx.Clear_Field (Anamode_Fld);
      Afpx.Clear_Field (Get_Fld);
      Afpx.Clear_Field (Ananame_Fld);
      Afpx.Reset_Field (Anagrams_Fld, Reset_Activation => False);
      Afpx.Set_Field_Activation (Ananouns_Title_Fld, Database_Ok);
      In_Anagrams := False;
      Afpx.Set_Field_Activation (Ananouns_Fld, Database_Ok);
    end if;
  end Switch_To_Anagrams;

  -- Allow button Nouns or not
  procedure Allow_Nouns (Allow : in Boolean) is
  begin
    if Allow then
      Afpx.Reset_Field  (Nouns_Fld, Reset_String => False);
    else
      Afpx.Set_Field_Protection (Nouns_Fld, True);
      Afpx.Set_Field_Colors (Nouns_Fld,
           Foreground => Con_Io.Color_Of ("Dark_Grey"),
           Background => Con_Io.Color_Of ("Light_Grey") );
    end if;
  end Allow_Nouns;

  -- Activate the buttons that interact with database
  procedure Activate_Db (On : in Boolean) is
  begin
    Afpx.Set_Field_Activation (Anagrams_Fld, On);
    Afpx.Set_Field_Activation (Ananouns_Fld, On);
    Afpx.Set_Field_Activation (Ananouns_Title_Fld, On);
    Afpx.Set_Field_Activation (Search_Fld, On);
    Afpx.Set_Field_Activation (Research_Fld, On);
    Afpx.Set_Field_Activation (Add_Word_Fld, On);
    Afpx.Set_Field_Activation (Del_Word_Fld, On);
    Afpx.Set_Field_Activation (Add_Noun_Fld, On);
    Afpx.Set_Field_Activation (Del_Noun_Fld, On);
  end Activate_Db;

  use type Afpx.Field_Range;
begin
  Database.Logger.Init ("Xwords");
  -- Parse option for Log
  if Argument.Get_Nbre_Arg > 1 then
    Error;
    return;
  elsif Argument.Get_Nbre_Arg = 1 then
    if Argument.Get_Parameter (1) = "-l" then
      Log := True;
    else
      Error;
      return;
    end if;
  end if;

  -- Init Afpx
  Afpx.Use_Descriptor (Afpx_Xref.Main.Dscr_Num);
  Afpx.Get_Console.Set_Icon (Icon.Xwords_Xpm);

  Get_Handle:= (others => <>);
  Get_Handle.Cursor_Field := Get_Fld;

  -- Load Anagram dictio
  Database_Loaded.Set (Trilean.Other);
  begin
    -- Buttons are inactive until database is loaded OK
    Activate_Db (False);
    Words_File_Name := As.U.Tus (Environ.Getenv_If_Set (Words_Env_Name));
    Nouns_File_Name := As.U.Tus (Environ.Getenv_If_Set (Nouns_Env_Name));
    Load_Database.Start (Words_File_Name, Nouns_File_Name);
    Loading_Database := True;
  exception
    when Environ.Name_Error =>
      -- No Env variable => no dictio => no anagram, stop task
      Basic_Proc.Put_Line_Error ("No dictio => no anagram");
      Load_Database.Stop;
      Loading_Database := False;
  end;
  -- Not in anagrams
  Switch_To_Anagrams (False);

  Status := Ok;

  -- Main loop
  loop
    -- Get result of loading the dictio: polling
    if Loading_Database then
      case Database_Loaded.Get is
        when Trilean.True =>
          -- Dictio loaded OK, enable
          Database.Logger.Log_Debug ("Activated");
          Cmd.Init (Words_File_Name.Image, Nouns_File_Name.Image);
          Activate_Db (True);
          Loading_Database := False;
        when Trilean.False =>
          -- Dictio loading failed
          Basic_Proc.Put_Line_Error (
                 "Error while loading database dictionaries: "
                & Words_File_Name.Image & " and "
                & Nouns_File_Name.Image & ".");

          Basic_Proc.Set_Error_Exit_Code;
          return;
        when Trilean.Other =>
          -- Dictio not loaded yet
          null;
      end case;
    end if;

    Afpx.Clear_Field (Topof_Fld);
    if Status = Found
    and then (List_Content = Words or else List_Content = Anagrams)
    and then not Afpx.Line_List.Is_Empty then
      -- Get position of top and encode field
      Afpx.Encode_Field (Topof_Fld, (0, 0),
                         Long_Longs.Image (Afpx.Line_List.List_Length));
      Allow_Nouns (List_Content = Words);
    else
      Afpx.Clear_Field (Scroll_Fld);
      Afpx.Clear_Field (Topnum_Fld);
      Afpx.Clear_Field (Topof_Fld);
      Afpx.Clear_Field (Percent_Fld);
      Allow_Nouns (False);
    end if;

    -- Color and protection of result list according to status
    case Status is
      when Found =>
        Afpx.Reset_Field (Afpx.List_Field_No, Reset_String => False);
      when Ok =>
        Afpx.Reset_Field (Afpx.List_Field_No, Reset_String => False);
        Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
      when Error =>
        Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
        Afpx.Set_Field_Colors (Afpx.List_Field_No,
                  Background => Con_Io.Color_Of ("Red"));
    end case;

    -- Words command when in anagrams
    Afpx.Set_Field_Activation (Add_Word_Fld,
        Database_Ok and then not In_Anagrams);
    Afpx.Set_Field_Activation (Add_Noun_Fld,
        Database_Ok and then not In_Anagrams);
    Afpx.Set_Field_Activation (Del_Word_Fld,
        Database_Ok and then not In_Anagrams);
    Afpx.Set_Field_Activation (Del_Noun_Fld,
        Database_Ok and then not In_Anagrams);

    -- Set cursor at last significant char of the Get field
    Get_Handle.Cursor_Col := Afpx.Last_Index (Afpx.Decode_Field (Get_Fld, 0),
                                              True);
    Database.Logger.Log_Debug ("Calling PTG");
    Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
                       List_Change_Cb => List_Cb 'Unrestricted_Access);
    Database.Logger.Log_Debug ("PTG returning");

    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            if In_Anagrams then
              Search_In_Anagrams (False);
            else
              Do_Command (Search_Fld);
            end if;
          when Afpx.Escape_Key =>
            Do_Recall;
          when Afpx.Break_Key =>
            exit;
        end case;
      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is

          -- Double click in list, encode in Get field
          when Afpx.List_Field_No =>
            Afpx_Item := Afpx.Line_List.Read (Afpx.Line_List_Mng.Current);
            Afpx.Clear_Field (Get_Fld);
            Afpx.Encode_Field (Get_Fld, (0, 0),
                Lower_Str (Language.Unicode_To_String (
                               Afpx_Item.Str(1 .. Afpx_Item.Len))) );
          -- Scroll bar
          when Scroll_Fld =>
            Do_Scroll (Ptg_Result.Release_Pos.Row);

          -- Clear get and error
          when Clear_Fld =>
            Afpx.Clear_Field (Get_Fld);
            if Status = Error then
              Afpx.Reset_Field (Afpx.List_Field_No, Reset_String => False);
              Afpx.Line_List.Delete_List (Deallocate => False);
              Status := Ok;
            end if;

          -- Recall last request
          when Recall_Fld =>
            Do_Recall;

          -- Move to first noun
          when Nouns_Fld =>
            Move_To_Nouns;

          -- Search anagrams
          when Anagrams_Fld =>
            Do_Anagrams;
          when Ananouns_Fld =>
            -- Flip flop
            Afpx.Encode_Field (Ananouns_Fld, (0, 0),
              (if Ananouns_Set then " " else "X"));

          when Search_Fld .. Del_Noun_Fld =>
            if In_Anagrams
            and then (Ptg_Result.Field_No = Search_Fld
              or else Ptg_Result.Field_No = Research_Fld) then
              Search_In_Anagrams (Ptg_Result.Field_No = Research_Fld);
            else
              -- Words commands
              Do_Command (Ptg_Result.Field_No);
            end if;

          -- History
          when History_Fld =>
            -- Put history of search in list
            Afpx.Line_List.Delete_List (Deallocate => False);
            if not History_List.Is_Empty then
              History_List.Rewind;
              loop
                History_List.Read (Line, Moved => Moved);
                Afpx.Line_List.Insert (Us2Afpx (Line), Afpx.Line_List_Mng.Prev);
                exit when not Moved;
              end loop;
              -- Move to Bottom
              Afpx.Line_List.Rewind;
              Afpx.Update_List(Afpx.Top);
              History_List.Rewind (Cmd.Res_Mng.Prev);
            end if;
            Status := Found;
            List_Content := History;
          -- Clear list
          when Clear_List_Fld =>
            Afpx.Line_List.Delete_List (Deallocate => False);
            List_Content := Empty;
            Status := Ok;

          -- List management
          when Lmng_Fld + 0 =>
            Afpx.Update_List(Afpx.Top);
          when Lmng_Fld + 1 =>
            Afpx.Update_List(Afpx.Page_Up);
          when Lmng_Fld + 2 =>
            Afpx.Update_List(Afpx.Up);
          when Lmng_Fld + 3 =>
            Afpx.Update_List(Afpx.Center_Selected);
          when Lmng_Fld + 4 =>
            Afpx.Update_List(Afpx.Down);
          when Lmng_Fld + 5 =>
            Afpx.Update_List(Afpx.Page_Down);
          when Lmng_Fld + 6 =>
            Afpx.Update_List(Afpx.Bottom);

          -- Exit
          when Exit_Fld =>
            exit;
          when others =>
            null;
        end case;

      when others =>
        null;
    end case;
  end loop;

  -- Done
  Afpx.Release_Descriptor;

exception
  when Cmd.Terminate_Request =>
    Afpx.Release_Descriptor;
end Xwords;

