with Ada.Characters.Latin_1;
with As.U;
with Debug, Io_Flow, Async_Stdin;
package body Input_Dispatcher is

  -- Current input flow
  Curr_Is_Stdin : Boolean := True;

  -- Data from stdin
  Str_Stdin : As.U.Asu_Us;
  Ind_Stdin : Positive;

  -- Data from Stdin/Set_Input
  Cur_Str : As.U.Asu_Us;

  -- Is surrent string parsed (first word extracted)
  Str_Parsed : Boolean;

  -- Extracted word from current Str
  Word : As.U.Asu_Us;

  -- Get first/next word from a string
  Cur_Index : Positive;

  -- Last index of extracted word (in global variable cause
  --  also used by Error_String
  Stop_Index : Positive;

  -- Word separator
  function Is_Separator (C : in Character) return Boolean is
  begin
    -- Space and Tab, Lf (Unix standard) and Cr (cause Dos newline is CrLf)
    return     C = ' '
       or else C = Ada.Characters.Latin_1.Ht
       or else C = Ada.Characters.Latin_1.Lf
       or else C = Ada.Characters.Latin_1.Cr;
  end Is_Separator;

  -- Remove first and last string delimiters
  --  and replace the pairs of delimiters by one delimiter
  -- >"foo ""bar"" stuff"< becomes >foo "bar" stuff<
  function Parse_Substring (Str : String) return String is
    Tmp_Str : String (1 .. Str'Length);
    Tmp_Len : Natural;
    Tmp_Index : Natural;
  begin
    if Debug.Debug_Level_Array(Debug.Input) then
      Async_Stdin.Put_Line_Err ("Input_dispacher: parsing substring >"
                              & Str & "<");
    end if;
    -- Check first and last are delimiters
    if Str'Length < 2
    or else Str(Str'First) /= Sd
    or else Str(Str'Last) /= Sd then
      -- This should not occure because strings literals have
      --  already been parsed in Next_Str_Word.
      raise String_Error;
    end if;

    -- Empty string?
    if Str'Length = 2 then
      if Debug.Debug_Level_Array(Debug.Input) then
        Async_Stdin.Put_Line_Err ("Input_dispacher: empty substring");
      end if;
      return "";
    end if;
    Tmp_Str (1 .. Str'Length-2) := Str(Str'First+1 .. Str'Last-1);
    Tmp_Len := Str'Length - 2;

    -- Parse sequence of two delimiters
    Tmp_Index := Tmp_Str'First;
    while Tmp_Index <= Tmp_Len loop
      if Tmp_Str(Tmp_Index) = Sd then
        if Tmp_Index = Tmp_Len or else Tmp_Str(Tmp_Index + 1) /= Sd then
          -- Sd alone within string
          raise String_Error;
        end if;
        -- Here there are 2 Sd. Skip one.
        Tmp_Str(Tmp_Index .. Tmp_Len - 1) := Tmp_Str(Tmp_Index + 1 .. Tmp_Len);
        Tmp_Len := Tmp_Len - 1;
      end if;
      Tmp_Index := Tmp_Index + 1;
    end loop;

    -- Done
    if Debug.Debug_Level_Array(Debug.Input) then
      Async_Stdin.Put_Line_Err ("Input_dispacher: parsed substring >"
                              & Tmp_Str (1 .. Tmp_Len) & "<");
    end if;
    return Tmp_Str(1 .. Tmp_Len);
  end Parse_Substring;

  -- Extraxt next word (from Cur_Index) of Cur_Str
  function Next_Str_Word return As.U.Asu_Us is
    Tmp_Index : Positive;
    In_Lit : Boolean := False;
  begin
    -- Skip separators
    while Cur_Index <= Cur_Str.Length
    and then Is_Separator (Cur_Str.Element (Cur_Index)) loop
      Cur_Index := Cur_Index + 1;
    end loop;
    if Cur_Index > Cur_Str.Length then
      -- No more word
      return As.U.Asu_Null;
    end if;

    if Cur_Str.Element (Cur_Index) = '#' then
      -- Comment: skip line
      return As.U.Asu_Null;
    end if;

    -- Got a start of word
    Tmp_Index := Cur_Index;

    In_Lit := Cur_Str.Element (Tmp_Index) = Sd;
    if In_Lit then
      Stop_Index := Tmp_Index + 1;
      -- Parse string literal, look for Sd-Sep or Sd-End
      Parse_Lit:
      loop
        if Cur_Str.Element (Stop_Index) = Sd then
          if Stop_Index = Cur_Str.Length
          or else Is_Separator(Cur_Str.Element (Stop_Index + 1)) then
            -- End of String literal
            exit Parse_Lit;
          elsif Cur_Str.Element (Stop_Index + 1) = Sd
          and then Stop_Index + 1 /= Cur_Str.Length then
            -- Two successive Sd in the middle: keep
            Stop_Index := Stop_Index + 1;
          else
            -- One Sd in middle of string
            raise String_Error;
          end if;
        elsif Stop_Index = Cur_Str.Length then
          -- No Sd before end of line
          Stop_Index := Cur_Str.Length;
          raise String_Error;
        end if;
        -- In the middle of a string: go on
        Stop_Index := Stop_Index + 1;
      end loop Parse_Lit;

      -- This is the next start after string literal
      Cur_Index := Stop_Index + 1;

    else
      -- Parse string, look for separator or end of string
      Stop_Index := Tmp_Index + 1;
      while Stop_Index <= Cur_Str.Length
      and then not Is_Separator (Cur_Str.Element (Stop_Index)) loop
        Stop_Index := Stop_Index + 1;
      end loop;
      -- This is the next start
      Cur_Index := Stop_Index;
      -- Stop is last char of word
      Stop_Index := Stop_Index - 1;
    end if;

    return As.U.Tus (Cur_Str.Slice (Tmp_Index, Stop_Index));

  end Next_Str_Word;

  -- Next string to parse, also current string
  --  if Next_Str_Word raised String_Error
  function Current_String return String is
  begin
   return Cur_Str.Slice (Cur_Index , Stop_Index);
  end Current_String;

  -- Extract first word of current or new (Str) string
  function First_Str_Word (Str : As.U.Asu_Us := As.U.Asu_Null)
                          return As.U.Asu_Us is
  begin
    if Str.Length /= 0 then
      Cur_Str := Str;
    end if;
    Cur_Index := 1;
    Str_Parsed := True;
    return Next_Str_Word;
  end First_Str_Word;

  -- Set input flow to a new string or back to stdin if Str is empty
  -- Handle specific jump from and back to stdin
  procedure Set_Input (Str : in String) is
  begin
    if Debug.Debug_Level_Array(Debug.Input) then
      Async_Stdin.Put_Line_Err ("Input_dispacher: Setting input to >"
       & Str & "<");
    end if;
    if Str = "" then
      Curr_Is_Stdin := True;
      if Str_Stdin.Length /= 0 then
        -- Restore Cur_Index to Ind_Stdin when back from prog to stdin
        Cur_Index := Ind_Stdin;
        Cur_Str := Str_Stdin;
      end if;
    else
      if Curr_Is_Stdin then
        -- If leaving stdin for a prog, save Cur_Index in Ind_Stdin
        -- Str_Stdin is already set
        Ind_Stdin := Cur_Index;
      end if;
      Curr_Is_Stdin := False;
      Cur_Str := As.U.Tus (Str);
      Str_Parsed := False;
    end if;
    if Debug.Debug_Level_Array(Debug.Input) then
      Async_Stdin.Put_Line_Err ("Input_dispacher: Input set to >"
       & Cur_Str.Image & "< at " & Integer'Image(Cur_Index)
       & " len " & Natural'Image(Cur_Str.Length));
    end if;
  end Set_Input;

  -- Get the ungot words of current string
  -- Program_Error if current input is stdin
  --  or if no word already got from current string
  function Get_Remaining return String is
  begin
    if Curr_Is_Stdin then
      if Debug.Debug_Level_Array(Debug.Input) then
        Async_Stdin.Put_Line_Err ("Input_dispacher: Remaining on stdin.");
      end if;
      raise Program_Error;
    end if;
    if not Curr_Is_Stdin and then not Str_Parsed then
      -- Current string is not be parsed (retacal) return all
      if Debug.Debug_Level_Array(Debug.Input) then
        Async_Stdin.Put_Line_Err ("Input_dispacher: Remaining is >"
         & Cur_Str.Image & "<");
      end if;
      return Cur_Str.Image;
    else
      -- Return remaining
      if Debug.Debug_Level_Array(Debug.Input) then
        Async_Stdin.Put_Line_Err ("Input_dispacher: Remaining is >"
         & Cur_Str.Slice (Cur_Index, Cur_Str.Length) & "<");
      end if;
      return Cur_Str.Slice (Cur_Index, Cur_Str.Length);
    end if;
  end Get_Remaining;

  -- Get next word from current input
  -- Returns empty string when if end of input flow
  function Next_Word return String is
  begin
    if Curr_Is_Stdin then

      -- In stdin
      loop
        if Str_Stdin.Length = 0 then
          -- End if string. Need to get a new string
          Io_Flow.Next_Line (Str_Stdin);
          if Str_Stdin.Length = 0 then
              return "";
          end if;
          -- Got a new string, parse it, skip empty lines
          Word := First_Str_Word (Str_Stdin);
          exit when Word.Length /= 0;
        else
          Word := Next_Str_Word;
          exit when Word.Length /= 0;
          -- End of string
          Str_Stdin.Set_Null;
        end if;
      end loop;

    else

      -- In string (subprogram)
      if not Str_Parsed then
        Word := First_Str_Word;
      else
        Word := Next_Str_Word;
      end if;

    end if;

    return Word.Image;
  end Next_Word;


end Input_Dispatcher;

