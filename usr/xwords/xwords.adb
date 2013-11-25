-- Search for words matching criteria (au:o.obile) or regexp (au.*bile)
-- Or search anagrams
with As.U.Utils, Argument, Con_Io, Afpx, Basic_Proc, Language, Many_Strings,
     Str_Util, Lower_Str, Environ, Images, Event_Mng, Afpx_Xref,
     Mutex_Manager, Protected_Var, Trilean, Rounds;
with Cmd, Analist;
procedure Xwords is

  -- Name of ENV variable for anagrams dictionary
  Dictio_Env_Name : constant String := "DICTIO_FILE";

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
  Clear_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Clear;
  Recall_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Recall;
  Nouns_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Nouns;
  Get_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Get;
  Scroll_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Scroll;
  Topnum_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Topnum;
  Topof_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Topof;
  Anagrams_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Anagrams;
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
  History : Cmd.Res_List;
  Moved : Boolean;

  -- A line of text
  Line : As.U.Asu_Us;

  -- Dictio init status
  Dictio_File_Name : As.U.Asu_Us;
  Loading_Anagrams : Boolean;

  -- List is words or history
  List_Is_Words : Boolean;

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

  -- Purge trailing spaces
  function Strip (Str : String) return String is
  begin
    return Str_Util.Strip (Str);
  end Strip;

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

    use type Afpx.List_Change_List, Afpx.List_Status_Rec;
  begin
    -- Nothing if not a valid list of words
    if Status /= Found or else not List_Is_Words then
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
        Afpx.Encode_Field (Topnum_Fld, (0, 0),
                           Images.Integer_Image (Afpx_Status.Id_Top));
        -- Scroll bar index
        Afpx.Clear_Field (Scroll_Fld);
        Percent := Afpx.Get_List_Percent;
        if Percent /= 0 then
          -- 0 <-> 1% and Height-1 <-> 100%
          -- (Percent-1)/99 = Row/(Height-1)
          Row := Con_Io.Row_Range( Rounds.Roundiv (
             (Afpx.Get_Field_Height (Scroll_Fld) - 1) * (Percent - 1), 99));
          Afpx.Encode_Field (Scroll_Fld, (Row => Row, Col => 0), "-");
        else
          Afpx.Encode_Field (Scroll_Fld, (0, 0), "-");
        end if;
    end case;
  end List_Cb;

  -- Move Afpx list ot first noun (if any)
  procedure Move_To_Nouns is
    Position : Positive;
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
    Saved_Position, Position : Natural;
  begin
    if Afpx.Line_List.Is_Empty then
      return;
    end if;
    Saved_Position := Afpx.Line_List.Get_Position;
    -- 0 <-> 1% and Height-1 <-> 100%
    -- (Percent-1)/99 = Row/(Height-1)
    Percent := Rounds.Roundiv (
                 Row * 99,
                 Afpx.Get_Field_Height (Scroll_Fld) - 1) + 1;
    Position := Afpx.Get_List_Index (Percent);
    if Position = 0 then
      return;
    end if;
    Afpx.Line_List.Move_At (Position);
    Afpx.Update_List (Afpx.Top_Selected);
    Afpx.Line_List.Move_At (Saved_Position);

  end Do_Scroll;

  -- List anagrams of word
  procedure Do_Anagrams is
    Anagrams : As.U.Utils.Asu_Ua.Unb_Array;
    Word : As.U.Asu_Us;
    Char : Character;
    Length : Natural;
  begin
    -- Clear result
    Status := Ok;
    Afpx.Line_List.Delete_List (Deallocate => False);
    -- Get word and check it
    Word := As.U.Tus (Strip (Afpx.Decode_Field (Get_Fld, 0, False)));
    if Word.Is_Null then
      return;
    end if;
    for I in 1 .. Word.Length loop
      Char := Word.Element (I);
      if Char < 'a' or else Char > 'z' then
        Afpx.Line_List.Insert (Us2Afpx (
            As.U.Tus ("ERROR: Invalid character in word.")));
        Status := Error;
        return;
      end if;
    end loop;

    -- Get list
    Analist.List (Strip (Afpx.Decode_Field (Get_Fld, 0, False)),
                  Anagrams);
    History.Insert (Word);

    -- Copy in Afpx list
    Length := 0;
    for I in 1 .. Anagrams.Length loop
      if Anagrams.Element(I).Length /= Length then
        Length := Anagrams.Element(I).Length;
        Afpx.Line_List.Insert (Us2Afpx (As.U.Tus (
             "-- " & Images.Integer_Image (Length) & " --")));
      end if;
      Afpx.Line_List.Insert (Us2Afpx (Anagrams.Element(I)));
    end loop;
    Afpx.Line_List.Rewind;
    Afpx.Update_List(Afpx.Top);

    -- Update Status
    if Anagrams.Length = 1 then
      Status := Ok;
    else
      Status := Found;
      List_Is_Words := True;
    end if;

  exception
    when Analist.Too_Long =>
      Afpx.Line_List.Insert (Us2Afpx (
          As.U.Tus ("ERROR: Word too long.")));
  end Do_Anagrams;

  -- Build and launch a Words command
  procedure Do_Command (Num : Afpx.Field_Range) is
    Result : Cmd.Res_List;
    Com, Arg : Many_Strings.Many_String;
    Word : As.U.Asu_Us;
    Command_Ok : Boolean;
    use type As.U.Asu_Us, Afpx.Field_Range;
  begin
    -- Clear result
    Afpx.Line_List.Delete_List (Deallocate => False);

    -- Build command
    Word := As.U.Tus (Lower_Str (Strip (
              Afpx.Decode_Field (Get_Fld, 0, False))));
    Com.Set ("words");
    case Num is
      when Search_Fld | Research_Fld =>
        Arg.Set ("search");
      when Add_Word_Fld | Add_Noun_Fld =>
        Arg.Set ("add");
      when Del_Word_Fld | Del_Noun_Fld =>
        Arg.Set ("delete");
      when others =>
        Status := Error;
        return;
    end case;
    if Num = Research_Fld then
      Arg.Cat ("-re");
    elsif Num = Add_Noun_Fld or else Num = Del_Noun_Fld then
      Arg.Cat ("-noun");
    end if;
    Arg.Cat ('"' & Word & '"');

    -- Prevent X events to interfere with the Command internal loop
    --  and execute command
    Afpx.Suspend;
    Cmd.Exec (Com.Image, Arg.Image, Command_Ok, Result);
    Afpx.Resume;

    -- Set status
    if not Command_Ok then
      Status := Error;
    elsif Num = Search_Fld or else Num = Research_Fld then
      Status := Found;
      List_Is_Words := True;
    else
      Status := Ok;
    end if;

    -- Add/del normal word to anagram list
    if Status = Ok then
      if Num = Add_Word_Fld then
        Analist.Add (Word);
      elsif Num = Del_Word_Fld then
        Analist.Del (Word);
      end if;
    end if;

    -- Store in history if search
    if (Num = Search_Fld or else Num = Research_Fld)
    and then not Arg.Image.Is_Null then
      History.Insert (Word);
    end if;

    -- Log request if needed
    if Log then
      Line := Com.Image;
      for I in 1 .. Arg.Nb loop
        Line := Line & " " & As.U.Asu_Us'(Arg.Nth (I));
      end loop;
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

      if (Num = Search_Fld or else Num = Research_Fld) then
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
    if Status /= Error then
      Afpx.Clear_Field (Get_Fld);
    end if;
  end Do_Command;

  procedure Do_Recall is
  begin
    Afpx.Clear_Field (Get_Fld);
    if not History.Is_Empty then
      History.Read (Line, Cmd.Res_Mng.Dyn_List.Current);
      Afpx.Encode_Field (Get_Fld, (0, 0), Lower_Str (Line.Image));
    end if;
  end Do_Recall;

  -- Task to load anagrams in background
  task Load_Anagrams is
    entry Start (File_Name : in String);
    entry Stop;
  end Load_Anagrams;

  -- Anagram loading status: Ok, Failed or Pending
  package Protected_Trilean is new Protected_Var (Trilean.Trilean);
  Anagram_Loaded : Protected_Trilean.Protected_T(Mutex_Manager.Simple);
  task body Load_Anagrams is
    File_Name : As.U.Asu_Us;
    Load : Boolean;
    Ok : Boolean;
  begin

    select
      -- Load: Get file name
      accept Start (File_Name : in String) do
        Cmd.Logger.Log_Debug ("Loading");
        Load_Anagrams.File_Name := As.U.Tus (File_Name);
      end Start;
      Load := True;
    or
      -- Do not load
      accept Stop;
      Load := False;
    or
      -- If main returns without calling
      terminate;
    end select;

    if Load then
      -- Load dictionnary
      begin
        Analist.Init (File_Name.Image);
        Ok := True;
      exception
        when Analist.Init_Error =>
          Ok := False;
      end;
      Cmd.Logger.Log_Debug ("Loaded");
      -- Report completion: Ok or failure
      Anagram_Loaded.Set (Trilean.Boo2Tri (Ok));
      Cmd.Logger.Log_Debug ("Reported");
      -- Wake up main task
      Event_Mng.Send_Dummy_Signal;
      Cmd.Logger.Log_Debug ("Signaled");

   end if;
   -- Done
  end Load_Anagrams;

  use type Afpx.Field_Range;
begin
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
  Get_Handle:= (others => <>);
  Get_Handle.Cursor_Field := Get_Fld;

  -- Load Anagram dictio
  Anagram_Loaded.Set (Trilean.Other);
  begin
    -- Button is inactive until dictio is loaded OK
    Afpx.Set_Field_Activation (Anagrams_Fld, False);
    Dictio_File_Name := As.U.Tus (Environ.Getenv_If_Set (Dictio_Env_Name));
    Load_Anagrams.Start (Dictio_File_Name.Image);
    Loading_Anagrams := True;
  exception
    when Environ.Name_Error =>
      -- No Env variable => no diction => no anagram, stop task
      Load_Anagrams.Stop;
      Loading_Anagrams := False;
  end;

  Status := Ok;

  loop
    -- Get result of loading the dictio: polling
    if Loading_Anagrams then
      case Anagram_Loaded.Get is
        when Trilean.True =>
          -- Dictio loaded OK, enable
          Cmd.Logger.Log_Debug ("Activated");
          Afpx.Set_Field_Activation (Anagrams_Fld, True);
          Loading_Anagrams := False;
        when Trilean.False =>
          -- Dictio loading failed
          Basic_Proc.Put_Line_Error (
                 "Error while loading anagrams dictionary: "
                & Dictio_File_Name.Image & ".");

          Basic_Proc.Set_Error_Exit_Code;
          return;
        when Trilean.Other =>
          -- Dictio not loaded yet
          null;
      end case;
    end if;

    if Status = Found and then List_Is_Words
    and then not Afpx.Line_List.Is_Empty then
      -- Get position of top and encode field
      Afpx.Encode_Field (Topof_Fld, (0, 0),
                         Images.Integer_Image (Afpx.Line_List.List_Length));
      Afpx.Set_Field_Protection (Nouns_Fld, False);
      Afpx.Reset_Field  (Nouns_Fld, Reset_String => False);
    else
      Afpx.Clear_Field (Scroll_Fld);
      Afpx.Set_Field_Protection (Nouns_Fld, True);
      Afpx.Set_Field_Colors (Nouns_Fld,
           Foreground => Con_Io.Color_Of ("Dark_Grey"),
           Background => Con_Io.Color_Of ("Light_Grey") );
    end if;

    -- Color and protection of result list according to status
    Afpx.Clear_Field (Topof_Fld);
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

    -- Set cursor at last significant char of the Get field
    Get_Handle.Cursor_Col := Afpx.Last_Index (Afpx.Decode_Field (Get_Fld, 0),
                                              True);
    Cmd.Logger.Log_Debug ("Calling PTG");
    Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
                       List_Change_Cb => List_Cb 'Unrestricted_Access);
    Cmd.Logger.Log_Debug ("PTG returning");

    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            Do_Command (Search_Fld);
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

          -- Words commands
          when Search_Fld .. Del_Noun_Fld =>
            Do_Command (Ptg_Result.Field_No);

          -- History
          when History_Fld =>
            -- Put history of search in list
            Afpx.Line_List.Delete_List (Deallocate => False);
            if not History.Is_Empty then
              History.Rewind;
              loop
                History.Read (Line, Moved => Moved);
                Afpx.Line_List.Insert (Us2Afpx (Line), Afpx.Line_List_Mng.Prev);
                exit when not Moved;
              end loop;
              -- Move to Bottom
              Afpx.Line_List.Rewind (True);
              Afpx.Update_List(Afpx.Top);
              History.Rewind (True, Cmd.Res_Mng.Dyn_List.Prev);
            end if;
            Status := Found;
            List_Is_Words := False;
          -- Clear list
          when Clear_List_Fld =>
            Afpx.Line_List.Delete_List (Deallocate => False);
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

