with Text_Io;
with Text_Handler;
with Debug, Mcd_Mng;
package body Input_Dispatcher is

  -- Current input flow
  Curr_Is_Stdin : Boolean := True;

  -- Data from stdin
  Str_Stdin : String (1 .. Max_String_Lg);
  Len_Stdin : Natural := 0;
  Ind_Stdin : Positive;

  -- Data from STDIN/SET_INPUT
  Cur_Str : String (1 .. Max_String_Lg);
  Cur_Len : Positive;

  Str_Parsed : Boolean;

  -- Extracted from current STR
  Word : Text_Handler.Text (Max_String_Lg);

  -- Get first/next word from a string
  Cur_Index : Positive;
  Stop_Index : Positive;

  function Is_Separator (C : in Character) return Boolean is
  begin
    return C = ' ' or else C = Ascii.Ht;
  end Is_Separator;

  function Next_Str_Word return String is
    Tmp_Index : Positive;
    In_Lit : Boolean := False;
  begin
    -- Skip separators
    while Cur_Index <= Cur_Len and then Is_Separator(Cur_Str(Cur_Index)) loop
      Cur_Index := Cur_Index + 1;
    end loop;
    if Cur_Index > Cur_Len then
      -- No more word
      return "";
    end if;

    if Cur_Str(Cur_Index) = '#' then
      -- Comment: skip line
      return "";
    end if;

    -- Got a start of word
    Tmp_Index := Cur_Index;

    In_Lit := Cur_Str(Tmp_Index) = Sd;
    if In_Lit then
      Stop_Index := Tmp_Index + 1;
      -- Parse string literal, look for Sd-Sep or Sd-End
      Parse_Lit:
      loop
        if Cur_Str(Stop_Index) = Sd then
          if Stop_Index = Cur_Len
          or else Is_Separator(Cur_Str(Stop_Index + 1)) then
            -- End of String literal
            exit Parse_Lit;
          elsif Cur_Str(Stop_Index + 1) = Sd 
          and then Stop_Index + 1 /= Cur_Len then
            -- Two successive Sd in the middle: shift left by one
            Cur_Str(Stop_Index + 1 .. Cur_Len - 1) :=
                     Cur_Str(Stop_Index + 2 .. Cur_Len);
            Cur_Len := Cur_Len - 1;
          else
            -- One Sd in middle of string
            raise String_Error;
           
          end if;
        elsif Stop_Index = Cur_Len then
          -- No SD before end of line
          Stop_Index := Cur_Len;
          raise String_Error;
        end if;
        -- In the middle of a string: go on
        Stop_Index := Stop_Index + 1;
      end loop Parse_Lit;

      -- This is the next start
      Cur_Index := Stop_Index + 1;

    else
      -- Parse string, look for separator
      Stop_Index := Tmp_Index + 1;
      while Stop_Index <= Cur_Len
      and then not Is_Separator(Cur_Str(Stop_Index)) loop
        Stop_Index := Stop_Index + 1;
      end loop;
      -- This is the next start
      Cur_Index := Stop_Index;
      -- Stop is last char of word
      Stop_Index := Stop_Index - 1;
    end if;

    return Cur_Str(Tmp_Index .. Stop_Index);

  end Next_Str_Word;

  function Error_String return String is
  begin
   return Cur_Str(Cur_Index .. Stop_Index);
  end Error_String;

  function First_Str_Word (Str : String := "") return String is
  begin
    if Str /= "" then
      Cur_Len := Str'Length;
      Cur_Str(1 .. Cur_Len) := Str;
    end if;
    Cur_Index := 1;
    return Next_Str_Word;
  end First_Str_Word;

  -- Set input flow to a new string
  --  or stdin if STR is empty
  procedure Set_Input (Str : in String) is
  begin
    if Debug.Debug_Level_Array(Debug.Input) then
      Text_Io.Put_Line ("Input_dispacher: Setting input to >"
       & Str & "<");
    end if;
    if Str = "" then
      Curr_Is_Stdin := True;
      if Len_Stdin /= 0 then
        Cur_Index := Ind_Stdin;
        Cur_Len := Len_Stdin;
        Cur_Str(1 .. Cur_Len) := Str_Stdin(1 .. Len_Stdin);
      end if;
    else
      if Curr_Is_Stdin then
        Ind_Stdin := Cur_Index;
      end if;
      Curr_Is_Stdin := False;
      Cur_Len := Str'Length;
      Cur_Str(1 .. Cur_Len) := Str;
      Str_Parsed := False;
    end if;
    if Debug.Debug_Level_Array(Debug.Input) then
      Text_Io.Put_Line ("Input_dispacher: Input set to >"
       & Cur_Str(1 .. Cur_Len) & "< at " & Integer'Image(Cur_Index)
       & " len " & Integer'Image(Cur_Len));
    end if;
  end Set_Input;

  -- Get the ungot words of current string
  -- Program_Error if current input is stdin
  --  or if no word already got from current string
  function Get_Remaining return String is
  begin
    if Curr_Is_Stdin then
      if Debug.Debug_Level_Array(Debug.Input) then
        Text_Io.Put_Line ("Input_dispacher: Remaining on stdin.");
      end if;
      raise Program_Error;
    end if;
    if Debug.Debug_Level_Array(Debug.Input) then
      Text_Io.Put_Line ("Input_dispacher: Remaining is >"
       & Cur_Str(Cur_Index .. Cur_Len) & "<");
    end if;
    -- Current string may not be parsed (retacal in a function)
    if not Curr_Is_Stdin and then not Str_Parsed then
      Cur_Index := 1;
    end if;
    return Cur_Str(Cur_Index .. Cur_Len);
  end Get_Remaining;

  -- Get next word from current input
  -- Empty if end of input flow
  function Next_Word return String is
  begin
    if Curr_Is_Stdin then

      loop
        if Len_Stdin = 0 then
          -- Need to get a new string
          begin
            Text_Io.Get_Line (Str_Stdin, Len_Stdin);
          exception
            when Text_Io.End_Error =>
              return "";
          end;
          if Len_Stdin /= 0 then
            -- Str not to discard, parse it
            Text_Handler.Set(Word, First_Str_Word(Str_Stdin(1 .. Len_Stdin)));
            exit when not Text_Handler.Empty(Word);
          else
            -- Discard
            Len_Stdin := 0;
          end if;
        else
          Text_Handler.Set(Word, Next_Str_Word);
          exit when not Text_Handler.Empty(Word);
          -- End of string
          Len_Stdin := 0;
        end if;
      end loop;
    
    else

      -- In string
      if not Str_Parsed then
        Text_Handler.Set(Word, First_Str_Word);
        Str_Parsed := True;
      else
        Text_Handler.Set(Word, Next_Str_Word);
      end if;

    end if;

    return Text_Handler.Value(Word);
  end Next_Word;


end Input_Dispatcher;

