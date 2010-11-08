with As.U; use As.U;
with Argument, Con_Io, Afpx, Basic_Proc, Language, Many_Strings, String_Mng,
     Lower_Str, Environ;
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
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Ptg_Result   : Afpx.Result_Rec;
  Redisplay    : Boolean;
  Afpx_Item    : Afpx.Line_Rec;
  type Status_List is (Found, Ok, Error);
  Status       : Status_List;

  -- Fields
  Clear_Fld : constant Afpx.Field_Range := 3;
  Recall_Fld : constant Afpx.Field_Range := 4;
  Get_Fld : constant Afpx.Field_Range := 5;
  Anagrams_Fld : constant Afpx.Field_Range := 6;
  Search_Fld : constant Afpx.Field_Range := 7;
  Research_Fld : constant Afpx.Field_Range := 8;
  Add_Word_Fld : constant Afpx.Field_Range := 11;
  Add_Noun_Fld : constant Afpx.Field_Range := 12;
  Del_Word_Fld : constant Afpx.Field_Range := 13;
  Del_Noun_Fld : constant Afpx.Field_Range := 14;
  History_Fld : constant Afpx.Field_Range := 15;
  Clear_List_Fld : constant Afpx.Field_Range := 16;
  Lmng_Fld : constant Afpx.Field_Range := 18;
  Exit_Fld : constant Afpx.Field_Range := 17;

  -- History of search requests
  History : Cmd.Res_List;
  Moved : Boolean;

  -- A line of text
  Line : Asu_Us;

  -- Us to Afpx line
  function Us2Afpx (Us : Asu_Us) return Afpx.Line_Rec is
    Rec : Afpx.Line_Rec;
    List_Width : constant Afpx.Width_Range
               := Afpx.Get_Field_Width (Afpx.List_Field_No);
    Ustr : constant Language.Unicode_Sequence
         := Language.String_To_Unicode (Asu_Ts (Us));
  begin
    Rec.Len := Ustr'Length;
    -- Procuste
    Rec.Str := (others => Con_Io.Space);
    if Rec.Len > List_Width then
      Rec.Len := List_Width;
    end if;
    Rec.Str (1 .. Rec.Len) := Ustr(1 .. Rec.Len);
    return Rec;
  end Us2Afpx;

  -- Purge trailing spaces
  function Strip (Str : String) return String is
     Last : constant Natural := String_Mng.Parse_Spaces (Str, False);
  begin
    return Str(Str'First .. Last);
  end Strip;

  -- List anagrams of word
  procedure Do_Anagrams is
    Anagrams : Asu_Ua.Unb_Array;
    Word : Asu_Us;
    Char : Character;
  begin
    -- Clear result
    Status := Ok;
    Afpx.Line_List.Delete_List (Deallocate => False);
    -- Get word and check it
    Word := Asu_Tus (Strip (Afpx.Decode_Field (Get_Fld, 0, False)));
    if Asu_Is_Null (Word) then
      return;
    end if;
    for I in 1 .. Asu.Length (Word) loop
      Char := Asu.Element (Word, I);
      if Char < 'a' or else Char > 'z' then
        Afpx.Line_List.Insert (Us2Afpx (
            Asu_Tus ("ERROR: Invalid character in word.")));
        Status := Error;
        return;
      end if;
    end loop;

    -- Get list
    Analist.List (Strip (Afpx.Decode_Field (Get_Fld, 0, False)),
                  Anagrams);
    History.Insert (Word);

    -- Copy in Afpx list
    for I in 1 .. Anagrams.Length loop
      Afpx.Line_List.Insert (Us2Afpx (Anagrams.Element(I)));
    end loop;
    Afpx.Line_List.Rewind;
    Afpx.Update_List(Afpx.Top);

    -- Update Status
    if Anagrams.Length = 1 then
      Status := Ok;
    else
      Status := Found;
    end if;

  exception
    when Analist.Too_Long =>
      Afpx.Line_List.Insert (Us2Afpx (
          Asu_Tus ("ERROR: Word too long.")));
  end Do_Anagrams;

  -- Build and launch a Words command
  procedure Do_Command (Num : Afpx.Field_Range) is
    Result : Cmd.Res_List;
    Com, Word, Arg : Asu_Us;
    Command_Ok : Boolean;
    First : Boolean;
    use type Afpx.Field_Range, Asu_Us;
  begin
    -- Clear result
    Afpx.Line_List.Delete_List (Deallocate => False);

    -- Build command
    Word := Asu_Tus (Strip (Afpx.Decode_Field (Get_Fld, 0, False)));
    case Num is
      when Search_Fld | Research_Fld =>
        Com := Asu_Tus ("ws");
      when Add_Word_Fld | Add_Noun_Fld =>
        Com := Asu_Tus ("wa");
      when Del_Word_Fld | Del_Noun_Fld =>
        Com := Asu_Tus ("wd");
      when others =>
        Status := Error;
        return;
    end case;
    if Num = Research_Fld then
      Arg := Asu_Tus (Many_Strings.Cat ("-re", Asu_Ts (Word)));
    elsif Num = Add_Noun_Fld or else Num = Del_Noun_Fld then
      Arg := Asu_Tus (Many_Strings.Cat ("-noun", Asu_Ts (Word)));
    else
      Arg := Word;
    end if;

    -- Prevent X events to interfere with the Command internal loop
    --  and execute command
    Afpx.Suspend;
    Cmd.Exec (Asu_Ts (Com), Asu_Ts (Arg), Command_Ok, Result);
    Afpx.Resume;

    -- Set status
    if not Command_Ok then
      Status := Error;
    elsif Num = Search_Fld or else Num = Research_Fld then
      Status := Found;
    else
      Status := Ok;
    end if;

    -- Store in history and selection if search
    if (Num = Search_Fld or else Num = Research_Fld)
    and then not Asu_Is_Null (Arg) then
      History.Insert (Word);
    end if;

    -- Log request if needed
    if Log then
      Line := Com;
      for I in 1 .. Many_Strings.Nb (Asu_Ts (Arg)) loop
        Line := Line & " " & Many_Strings.Nth (Asu_Ts (Arg), I);
      end loop;
      Basic_Proc.Put_Line_Output (Asu_Ts (Line));
    end if;

    -- Encode result, set first word as selection
    First := True;
    if Result.Is_Empty then
      if Status = Found then
        -- Set selection to search word/pattern
        Afpx.Set_Selection (Lower_Str (Asu_Ts (Word)));
      else
        -- Reset selection for case where no result or error
        Afpx.Set_Selection ("");
      end if;
    else
      Result.Rewind;
      loop
        Result.Read (Line, Moved => Moved);
        if Status = Found and then First then
          Afpx.Set_Selection (Lower_Str (Asu_Ts (Line)));
          First := False;
        end if;
        Afpx.Line_List.Insert (Us2Afpx (Line));
        if Log then
          Basic_Proc.Put_Line_Output (Asu_Ts (Line));
        end if;
        exit when not Moved;
      end loop;

      -- Move to Top
      Afpx.Line_List.Rewind;
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
      Afpx.Encode_Field (Get_Fld, (0, 0), Lower_Str (Asu_Ts (Line)));
    end if;
  end Do_Recall;

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
  Afpx.Use_Descriptor (1);
  Cursor_Field := Get_Fld;
  Insert := False;
  Redisplay := False;

  -- Init Anagram dictio
  begin
    Analist.Init (Environ.Getenv_If_Set (Dictio_Env_Name));
  exception
    when Environ.Name_Error =>
      -- Dictio env name not set => Disable anagrams button
      Afpx.Set_Field_Activation (Anagrams_Fld, False);
    when Analist.Init_Error =>
      Basic_Proc.Put_Line_Error ("Error initializing anagrams dictionary");
      Basic_Proc.Set_Error_Exit_Code;
      return;
  end;

  Status := Ok;

  loop
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

    -- Set cursor at last significant char of the Get field
    Cursor_Col := Afpx.Last_Index (Afpx.Decode_Field (Get_Fld, 0), True);
    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result, Redisplay);
    Redisplay := False;

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

          -- Double click in list => Select for copy/paste
          when Afpx.List_Field_No =>
            Afpx.Line_List.Read (Afpx_Item, Afpx.Line_List_Mng.Current);
            declare
              Str : constant String
                  := Language.Unicode_To_String (
                          Afpx_Item.Str (1 ..  Afpx_Item.Len));
            begin
              Afpx.Set_Selection (Strip (Lower_Str (Str)));
            end;

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
                Afpx.Line_List.Insert (Us2Afpx (Line));
                exit when not Moved;
              end loop;
              -- Move to Bottom
              Afpx.Line_List.Rewind (True, Afpx.Line_List_Mng.Prev);
              Afpx.Update_List(Afpx.Bottom);
              History.Rewind (True, Cmd.Res_Mng.Dyn_List.Prev);
            end if;
            Status := Found;
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
            Afpx.Update_List(Afpx.Center);
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

      when Afpx.Refresh =>
        Redisplay := True;
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

