with Ada.Text_Io, Ada.Characters.Latin_1, Ada.Strings.Unbounded;
with Debug, Mcd_Mng, Io_Flow;
package body Input_Dispatcher is

  package Unb renames Ada.Strings.Unbounded;

  -- Current input flow
  Curr_Is_Stdin : Boolean := True;

  -- Data from stdin
  Str_Stdin : Unb.Unbounded_String;
  Ind_Stdin : Positive;

  -- Data from Stdin/Set_Input
  Cur_Str : Unb.Unbounded_String;

  Str_Parsed : Boolean;

  -- Extracted from current Str
  Word : Unb.Unbounded_String;

  -- Get first/next word from a string
  Cur_Index : Positive;
  Stop_Index : Positive;

  function Is_Separator (C : in Character) return Boolean is
  begin
    return     C = ' '
       or else C = Ada.Characters.Latin_1.Ht
       or else C = Ada.Characters.Latin_1.Cr;
  end Is_Separator;


  -- Remove string
  function Parse_String (Str : String) return String is
    Tmp_Str : String (1 .. Str'Length);
    Tmp_Len : Natural;
    Tmp_Index : Natural;
  begin
    -- Remove first and last delim
    if Str'Length < 2
    or else Str(Str'First) /= Sd
    or else Str(Str'Last) /= Sd then
      raise String_Error;
    end if;

    -- Empty string?
    if Str'Length = 2 then
      return "";
    end if;
    Tmp_Str (1 .. Str'Length-2) := Str(Str'First+1 .. Str'Last-1);
    Tmp_Len := Str'Length - 2;

    -- Parse sequence of two delim
    Tmp_Index := Tmp_Str'First;
    while Tmp_Index <= Tmp_Len loop
      if Tmp_Str(Tmp_Index) = Sd then
        if Tmp_Index = Tmp_Len or else Tmp_Str(Tmp_Index + 1) /= Sd then
          -- Sd alone within string
          raise String_Error;
        end if;
        Tmp_Str(Tmp_Index .. Tmp_Len - 1) := Tmp_Str(Tmp_Index + 1 .. Tmp_Len);
        Tmp_Len := Tmp_Len - 1;
      end if;
      Tmp_Index := Tmp_Index + 1;
    end loop;
    return Tmp_Str (1 .. Tmp_Len);
  end Parse_String;

  function Next_Str_Word return Unb.Unbounded_String is
    Tmp_Index : Positive;
    In_Lit : Boolean := False;
  begin
    -- Skip separators
    while Cur_Index <= Unb.Length (Cur_Str)
    and then Is_Separator(Unb.Element (Cur_Str, Cur_Index)) loop
      Cur_Index := Cur_Index + 1;
    end loop;
    if Cur_Index > Unb.Length (Cur_Str) then
      -- No more word
      return Unb.Null_Unbounded_String;
    end if;

    if Unb.Element (Cur_Str, Cur_Index) = '#' then
      -- Comment: skip line
      return Unb.Null_Unbounded_String;
    end if;

    -- Got a start of word
    Tmp_Index := Cur_Index;

    In_Lit := Unb.Element (Cur_Str, Tmp_Index) = Sd;
    if In_Lit then
      Stop_Index := Tmp_Index + 1;
      -- Parse string literal, look for Sd-Sep or Sd-End
      Parse_Lit:
      loop
        if Unb.Element (Cur_Str, Stop_Index) = Sd then
          if Stop_Index = Unb.Length (Cur_Str)
          or else Is_Separator(Unb.Element (Cur_Str, Stop_Index + 1)) then
            -- End of String literal
            exit Parse_Lit;
          elsif Unb.Element (Cur_Str, Stop_Index + 1) = Sd 
          and then Stop_Index + 1 /= Unb.Length (Cur_Str) then
            -- Two successive Sd in the middle: keep
            Stop_Index := Stop_Index + 1;
          else
            -- One Sd in middle of string
            raise String_Error;
           
          end if;
        elsif Stop_Index = Unb.Length (Cur_Str) then
          -- No Sd before end of line
          Stop_Index := Unb.Length (Cur_Str);
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
      while Stop_Index <= Unb.Length (Cur_Str)
      and then not Is_Separator(Unb.Element (Cur_Str, Stop_Index)) loop
        Stop_Index := Stop_Index + 1;
      end loop;
      -- This is the next start
      Cur_Index := Stop_Index;
      -- Stop is last char of word
      Stop_Index := Stop_Index - 1;
    end if;

    return Unb.To_Unbounded_String (Unb.Slice (Cur_Str, Tmp_Index, Stop_Index));

  end Next_Str_Word;

  function Error_String return String is
  begin
   return Unb.Slice (Cur_Str, Cur_Index , Stop_Index);
  end Error_String;

  function First_Str_Word (Str : Unb.Unbounded_String := Unb.Null_Unbounded_String)
  return Unb.Unbounded_String is
  begin
    if Unb.Length (Str) /= 0 then
      Cur_Str := Str;
    end if;
    Cur_Index := 1;
    return Next_Str_Word;
  end First_Str_Word;

  -- Set input flow to a new string
  --  or stdin if Str is empty
  procedure Set_Input (Str : in String) is
  begin
    if Debug.Debug_Level_Array(Debug.Input) then
      Ada.Text_Io.Put_Line ("Input_dispacher: Setting input to >"
       & Str & "<");
    end if;
    if Str = "" then
      Curr_Is_Stdin := True;
      if Unb.Length (Str_Stdin) /= 0 then
        Cur_Str := Str_Stdin;
      end if;
    else
      if Curr_Is_Stdin then
        Ind_Stdin := Cur_Index;
      end if;
      Curr_Is_Stdin := False;
      Cur_Str := Unb.To_Unbounded_String (Str);
      Str_Parsed := False;
    end if;
    if Debug.Debug_Level_Array(Debug.Input) then
      Ada.Text_Io.Put_Line ("Input_dispacher: Input set to >"
       & Unb.To_String (Cur_Str) & "< at " & Integer'Image(Cur_Index)
       & " len " & Natural'Image(Unb.Length (Cur_Str)));
    end if;
  end Set_Input;

  -- Get the ungot words of current string
  -- Program_Error if current input is stdin
  --  or if no word already got from current string
  function Get_Remaining return String is
  begin
    if Curr_Is_Stdin then
      if Debug.Debug_Level_Array(Debug.Input) then
        Ada.Text_Io.Put_Line ("Input_dispacher: Remaining on stdin.");
      end if;
      raise Program_Error;
    end if;
    if Debug.Debug_Level_Array(Debug.Input) then
      Ada.Text_Io.Put_Line ("Input_dispacher: Remaining is >"
       & Unb.Slice (Cur_Str, Cur_Index, Unb.Length(Cur_Str)) & "<");
    end if;
    -- Current string may not be parsed (retacal in a function)
    if not Curr_Is_Stdin and then not Str_Parsed then
      Cur_Index := 1;
    end if;
    return Unb.Slice (Cur_Str, Cur_Index, Unb.Length(Cur_Str));
  end Get_Remaining;

  -- Get next word from current input
  -- Empty if end of input flow
  function Next_Word return String is
  begin
    if Curr_Is_Stdin then

      loop
        if Unb.Length (Str_Stdin) = 0 then
          -- Need to get a new string
          Io_Flow.Next_Line (Str_Stdin);
          if Unb.Length (Str_Stdin) = 0 then
              return "";
          end if;
          -- Got str, parse it
          Word := First_Str_Word (Str_Stdin);
          exit when Unb.Length (Word) /= 0;
        else
          Word := Next_Str_Word;
          exit when Unb.Length (Word) /= 0;
          -- End of string
          Str_Stdin := Unb.Null_Unbounded_String;
        end if;
      end loop;
    
    else

      -- In string
      if not Str_Parsed then
        Word := First_Str_Word;
        Str_Parsed := True;
      else
        Word := Next_Str_Word;
      end if;

    end if;

    return Unb.To_String (Word);
  end Next_Word;


end Input_Dispatcher;

