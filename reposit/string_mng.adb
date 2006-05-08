with Ada.Strings.Unbounded;
package body String_Mng is

  -- Parces spaces and tabs (latin_1.Ht) from the head/tail of a string
  -- Returns the position of the first/last character or 0 if
  --  all the string is spaces or tabs (or empty)
  function Parse_Spaces (Str : String;
                         From_Head : Boolean := True)
           return Natural is
  begin
    if From_Head then
      -- Look forward for significant character
      for I in Str'Range loop
        if Str(I) /= ' ' and then Str(I) /= Ada.Characters.Latin_1.Ht then
          return I;
        end if;
      end loop;
      -- Not found
      return 0;
    else
      -- Look backwards for significant character
      for I in reverse Str'Range loop
        if Str(I) /= ' ' and then Str(I) /= Ada.Characters.Latin_1.Ht then
          return I;
        end if;
      end loop;
      -- Not found
      return 0;
    end if;
  end Parse_Spaces;


  -- Puts a string Str in a string of fixed length Len.
  -- If Str is shorter than Len, it is aligned at right or left and padded
  -- If Str is longer  than Len, it's head ot tail is truncated
  -- Str : String to put in the returned string
  -- Len : Number of characters of the returned string
  -- Align_Left : If string is shorter than Len characters,
  --     align it at left or at right (not Align_Left) and fill with Gap,
  -- Gap : When string is shorter than len, fill empty positions with Gap
  -- Trunc_Head : If string is longer than Len characters, trunc it's head
  --     or its tail
  -- Show_Trunc : When string is longer than Len, if Show_Trunc is set,
  --         then Str is truncated to Len-2 and starts (Trunc_Head) with " >"
  --         or ends (not Trunc_Head) with " <"
  function Procuste (Str : String;
                     Len : Positive;
                     Align_Left : Boolean := True;
                     Gap : Character := ' ';
                     Trunc_Head : Boolean := True;
                     Show_Trunc : Boolean := True)
           return String is
    L : Natural := Str'Length;
    S : String (1 .. Len);
  begin
    if L < Len then
      -- Str is shorter than Len: Pad
      if Align_Left then
        -- Copy L characters at left and pad
        S(1 .. L) := Str;
        S(L+1 .. Len) := (others => Gap);
      else
        -- Copy L characters at right and pad
        S(Len-L+1 .. Len) := Str;
        S(1 .. Len-L) := (others => Gap);
      end if;
    elsif L > Len then
      -- Str is larger than Len: Trunc
      if Trunc_Head then
        if Show_Trunc and then Len >= 2 then
          -- Copy "> " then Len-2 last characters of Str
          S := "> " & Str(Str'Last-Len+1+2 .. Str'Last);
        else
          -- Copy Len last characters of Str
          S := Str(Str'Last-Len+1 .. Str'Last);
        end if;
      else
        if Show_Trunc and then Len >= 2 then
          -- Copy Len-2 first characters of Str then " <"
          S := Str(Str'First .. Str'First+Len-1-2) & " <";
        else
          -- Copy Len first characters of Str
          S := Str(Str'First .. Str'First+Len-1);
        end if;
      end if;
    else
      -- Str is as Len characters: copy
      S := Str;
    end if;
    return S;
  end Procuste;

  -- Locate Nth occurence of a fragment within a string
  --  starting at From index, and from head or tail
  -- Returns index of fragment start
  --  or 0 if not found or if Within or Fragment is empty
  function Locate (Within     : String;
                   From_Index : Positive;
                   Fragment   : String;
                   Occurence  : Positive := 1;
                   From_Head  : Boolean := True)
           return Natural is
    Found_Occurence : Natural := 0;
  begin
    if Within'Length = 0
    or else Fragment'Length = 0
    or else From_Index not in Within'First .. Within'Last then
      return 0;
    end if;
    if From_Head then
      for I in From_Index .. Within'Last - Fragment'Length + 1 loop
        if Within(I .. I + Fragment'Length - 1) = Fragment then
          Found_Occurence := Found_Occurence + 1;
          if Found_Occurence = Occurence then
            return I;
          end if;
        end if;
      end loop;
    else
      for I in reverse From_Index .. Within'Last - Fragment'Length + 1 loop
        if Within(I .. I + Fragment'Length - 1) = Fragment then
          Found_Occurence := Found_Occurence + 1;
          if Found_Occurence = Occurence then
            return I;
          end if;
        end if;
      end loop;
    end if;
    return 0;
  end Locate;

  -- Remove Nb_Char characters from the string From at index At_Index.
  -- If Gap is No_Gap, then the string is shorten by Nb_Char, either
  --  those after At_Index if Shift_Left is True, or those before
  --  At_Index if Shift_Left is False.
  -- If Gap is set, then the string From keeps its length and is padded
  --  by Gap, either at the end (if Shift_Left is True) or at the
  --  beginning (if Shift_Left is False).
  -- Return the remaining string.
  -- Raises Constraint_Error if At_Index is not within From'First .. From'Last.
  function Remove (From : String;
                   At_Index : Positive;
                   Nb_Char : Natural;
                   Shift_Left : Boolean := True;
                   Gap : Character := No_Gap)
          return String is

    -- Generate a Padding string if need
    function Do_Pad (Nb_Gap : Integer) return String is
      subtype Pad_Str is String (1 .. Nb_Gap);
    begin
      if Gap = No_Gap or else Nb_Gap <= 0 then
        return "";
      else
        return Pad_Str'( (others => Gap));
      end if;
    end Do_Pad;

  begin
    -- Error or limit situations
    if At_Index < From'First or else At_Index > From'Last then
      raise Constraint_Error;
    end if;
    if Nb_Char = 0 then
      return From;
    end if;

    if Shift_Left then
      -- Check if Nb_Char can be removed at At_Index
      if At_Index + Nb_Char - 1 >= From'Last then
        -- No tail to keep, return head (and pad)
        return From(From'First .. At_Index-1)
             & Do_Pad(From'Last-At_Index+1);
      else
        -- Cat tail to head and return it (and pad)
        return From(From'First .. At_Index-1)
             & From(At_Index+Nb_Char .. From'Last)
             & Do_Pad(Nb_Char);
      end if;
    else
      -- Check if Nb_Char can be removed from At_Index
      if At_Index <= Nb_Char then
        -- No head to keep, return head (and pad)
        return Do_Pad(At_Index-From'First+1)
             & From(At_Index+1 .. From'Last);
      else
        return Do_Pad(Nb_Char)
             & From(From'First .. At_Index-Nb_Char)
             & From(At_Index+1 .. From'Last);
      end if;
    end if;
  end Remove;


  -- If To_Right is True, extract Nb_Char characters of From from At_Index
  -- If To_Right is False, extract Nb_Char characters of From up to At_Index
  -- Return the extracted substring.
  -- Raises Constraint_Error
  --  if At_Index+Nb_Char-1 is more than From'Last  (when To_Right is True)
  --  or At_Index-Nb_Char+1 is less than From'First (when To_Right is False).
  function Slice (From : String;
                  At_Index : Positive;
                  Nb_Char : Natural;
                  To_Right : Boolean := True)
           return String is
  begin
    -- Check len of result
    if (To_Right and then At_Index + Nb_Char - 1 > From'Last)
    or else (not To_Right and then At_Index - Nb_Char + 1 < From'First) then
      raise Constraint_Error;
    end if;
    -- Extract slice
    if To_Right then
      return From (At_Index .. At_Index + Nb_Char - 1);
    else
      return From (At_Index - Nb_Char + 1 .. At_Index);
    end if;
  end Slice;

  -- Extract the Nb_Char first (if Head is set to True) or last characters
  --   (if Head is set to False) of From string.
  -- Return the remaining string.
  function Cut (From : String;
                Nb_Char : Natural;
                Head : Boolean := True)
           return String is
  begin
    if Head then
      return Remove (From, From'First, Nb_Char, True);
    else
      return Remove (From, From'Last,  Nb_Char, False);
    end if;
  end Cut;

  -- Extract the Nb_Char first (if Head is set to True) or last characters
  --    (if Head is set to False) of From string.
  -- Return the extracted substring.
  -- Raises Constraint_Error if Nb_Char is more than From'Length.
  function Extract (From : String;
                    Nb_Char : Natural;
                    Head : Boolean := True)
           return String is
  begin
    if Head then
      return Slice (From, From'First, Nb_Char, True);
    else
      return Slice (From, From'Last,  Nb_Char, False);
    end if;
  end Extract;

  -- Swap the characters of string
  -- Example: ABCD -> DCBA
  function Swap (Str : String) return String is
    Res : String (1 .. Str'Length);
  begin
    for I in reverse Str'Range loop
      Res (Str'Last - I + 1) := Str(I);
    end loop;
    return Res;
  end Swap;

  -- Remove any multiple occurence of a character from string.
  -- Check from head or tail and return string.
  -- Example ABCAD, Head -> ABCD  and  ABCAD, Tail -> BCAD
  function Unique (From : String;
                   From_Head : Boolean := True)
           return String is
    Used : array (Character) of Boolean := (others => False);
    Input, Output : String (1 .. From'Length);
    Last : Natural;
    C : Character;
  begin
    if From_Head then
      Input := From;
    else
      Input := Swap (From);
    end if;
    Last := 0;
    for I in Input'Range loop
      C := Input(I);
      if not Used(C) then
        Last := Last + 1;
        Output(Last) := C;
        Used(C) := True;
      end if;
    end loop;
    if From_Head then
      return Output(1 .. Last);
    else
      return Swap (Output(1 .. Last));
    end if;
  end Unique;

  -- Replace recursively all variables by their values provided by the
  --  Resolv callback.
  -- A variable name is identified because it is within delimiters (strings).
  -- Start and stop delimiters must be non empty and different (e.g. "(" and ")",
  --  or "${" and "}"), otherwise Inv_Delimiter is raised.
  -- Variables may be defined recursively (e.g. ${Foo${Bar}}).
  -- Delimiter number must match (as many stop as start and in consistent
  --  sequence e.g. {}}{ s forbidden), otherwise the exception
  --  Delimiter_Mismatch is raised.
  -- If no callback is set (Resolv = null) then variables are replaced by
  --  empty strings.
  package Asu renames Ada.Strings.Unbounded;
  function Eval_Variables (Str : String;
                           Start_Delimiter, Stop_Delimiter : in String;
                           Resolv : Resolv_Access)
           return String is


    -- The string to work on
    Ustr : Asu.Unbounded_String;
    -- Last index of Ustr
    Last_Index : Natural;

    -- Check that Str_Index + Delimiter Length fits in Ustr and check if
    --  Ustr (Str_Index...) matches Delimiter
    function Match (Str_Index : in Positive; Delimiter : in String) return Boolean is
      Stop_Index : Natural;
    begin
      -- Compute index in Ustr of character matching Delimiter'Last
      begin
        Stop_Index := Str_Index + Delimiter'Length - 1;
      exception
        when Constraint_Error =>
          -- All these strings are definitely too long
          return False;
      end;
      -- Check enough characters remain in Ustr
      if Stop_Index > Last_Index then
        return False;
      end if;
      -- Check match
      return Asu.Slice (Ustr, Str_Index, Stop_Index) = Delimiter;
    end Match;

    -- Nested level of delimiters
    Level : Natural;
    -- Substitution has occured
    Subst_Occured : Boolean;
    -- Current index in Ustr
    Curr_Index : Natural;
    -- Index of start and stop of variable definition (delimiters included)
    -- Start = 0 when no current valid start (so second stop of { { } }
    --  is skipped)
    Start_Index : Natural; -- 0 if no current start
    Stop_Index : Positive;
    -- Index of stat and stop of variable name
    Start_Var, Stop_Var : Positive;

  begin
    -- Check Delimiters are non empty and different
    if Start_Delimiter = ""
    or else Stop_Delimiter = ""
    or else Start_Delimiter = Stop_Delimiter then
      raise Inv_Delimiter;
    end if;

    -- Store input string and its last index
    Ustr := Asu.To_Unbounded_String (Str);
    Last_Index := Str'Length;
    Level := 0;

    -- Start substitutions
    Subst_Occured := True;
    -- Loop as long as substitution occured and didn't lead to emtpy result
    while Subst_Occured and then Last_Index /= 0 loop
      -- Init for one pass
      Subst_Occured := False;
      Curr_Index := 1;
      Start_Index := 0;
      Level := 0;
      loop
        -- Look for delimiters
        if Match (Curr_Index, Start_Delimiter) then
          -- Found start delimiter: jump after it
          Level := Level + 1;
          Start_Index := Curr_Index;
          Curr_Index := Curr_Index + Start_Delimiter'Length - 1;
          Start_Var := Curr_Index + 1;
        elsif Match (Curr_Index, Stop_Delimiter) then
          if Level = 0 then
            -- More Stop than Start
            raise Delimiter_Mismatch;
          end if;
          Level := Level - 1;
          if Start_Index /= 0 then
            -- Found stop delimiter: jump after it
            Stop_Var := Curr_Index - 1;
            Curr_Index := Curr_Index + Stop_Delimiter'Length - 1;
            Stop_Index := Curr_Index;
            -- Substitute
            if Resolv /= null then
              declare
                -- Variable value
                Val : constant String
                    := Resolv (Asu.Slice (Ustr, Start_Var, Stop_Var));
                -- Correction to current and last index
                Offset : Integer
                       := Val'Length - (Stop_Index - Start_Index + 1);
              begin
                Asu.Replace_Slice (Ustr, Start_Index, Stop_Index, Val);
                Curr_Index := Curr_Index + Offset;
                Last_Index := Last_Index + Offset;
              end;
            else
              -- No resolving => empty string
              Asu.Replace_Slice (Ustr, Start_Index, Stop_Index, "");
              Curr_Index := Curr_Index - (Stop_Index - Start_Index + 1);
              Last_Index := Last_Index - (Stop_Index - Start_Index + 1);
            end if;
            -- Go on trying to substitute
            Subst_Occured := True;
            -- Following stop delimiter will not be processed if no
            -- new start has been found
            Start_Index := 0;
          end if;
        end if;
        -- End of this pass?
        exit when Curr_Index = Last_Index;
        -- Search delimiters in next char
        Curr_Index := Curr_Index + 1;
      end loop;
    end loop;

    -- Final level must be 0
    if Level /= 0 then
      raise Delimiter_Mismatch;
    end if;

    -- Done
    return Asu.To_String (Ustr);
  end Eval_Variables;
  -- No_Variable : exception;

  -- Locate an escape sequence within the Within string,
  --  starting searching from From_Index.
  -- An escape sequence is one escape char followed by one escaped char,
  --  (e.g. Esc='\' and Escaped="na" will detect "\n" or "\a").
  -- The first char of the Escape string defines the escape character
  --  and the rest of the string the possible escaped characters.
  -- Returns the index of the escaped char matching, or 0 if not found.
  -- Also returns 0 if Escape is empty.
  function Locate_Escape (Within_Str : String;
                          From_Index : Positive;
                          Escape     : String) return Natural is
    Esc  : Character;
    Start : Natural;
  begin
    -- Empty Escape
    if Escape = "" then
      return 0;
    end if;
    Esc := Escape(Escape'First);
    
    -- Locate escape sequence
    for I in From_Index .. Natural'Pred(Within_Str'Last) loop
      if Within_Str(I) = Esc then
        -- Count back the Esc (at least I is Esc)
        for J in reverse Within_Str'First .. I loop
          exit when Within_Str(J) /= Esc;
          Start := J;
        end loop;
        -- Check I = J modulo 2 => odd number of Esc
        if (I - Start) rem 2 = 0 then
          -- Odd number of Esc is an escape: "\\\n" -> '\''\n'
          -- Check if the following letter is within the Escape chars
          for K in Escape'Range loop
            if Within_Str(I + 1) = Escape(K) then
              -- The character following '\' is one of the expected
              return I + 1;
            end if;
          end loop;
           -- Not one of the expected characters
        end if;
        -- Even number of Esc is not an escape: "\\n" -> '\''n'
      end if;
    end loop;
    return 0;
  end Locate_Escape;

  -- Locate where to cut Str so that is best matches the requested line Length
  -- Looks for separator character
  -- Default Separator function, True for Space and Latin.Ht.
  function Is_Separator (Char : Character) return Boolean is
  begin
    return Char = ' ' or else Char = Ada.Characters.Latin_1.Ht;
  end Is_Separator;
  -- If Str is shorter or equal to Length, return Str'Last
  -- Else try to find a separator before Length, up to Mini
  -- Else try to find a separator after  Length, up to Maxi
  -- Else try to find a separator before Mini,   up to 1
  -- Else try to find a separator after  Maxi,   up to Str'Length
  -- Prerequisits Mini <= Length <= Maxi. Beware that they are not
  --  relative to Str indexes but that the returned value is.
  -- Returns 0 only if Str is empty.
  function Truncate (Str : in String;
                     Length : Positive;
                     Mini, Maxi : Positive;
                     Separating : Separator_Access := Is_Separator'Access)
           return Natural is
    Strlen : constant Natural := Str'Length;
    -- Corresponding index in Str
    function Indof (I : Positive) return Positive is
    begin
      return I - 1 + Str'First;
    end Indof;
  begin
    if Mini > Length or else Length > Maxi then
      raise Constraint_Error;
    end if;
    -- Handle trivial cases
    if Strlen = 0 then
      return 0;
    elsif Strlen <= Length then
      return Str'Last;
    end if;
    -- Else try to find a separator before Length, up to Mini
    for I in reverse Mini .. Length loop
      if Separating (Str (Indof (I))) then
        return Indof (I);
      end if;
    end loop;
    -- Else try to find a separator after  Length, up to Maxi
    for I in Length + 1 .. Maxi loop
      if Separating (Str (Indof (I))) then
        return Indof (I);
      end if;
    end loop;
    -- Else try to find a separator before Mini,   up to 1
    for I in reverse 1 .. Mini - 1 loop
      if Separating (Str (Indof (I))) then
        return Indof (I);
      end if;
    end loop;
    -- Else try to find a separator after  Maxi,   up to Str'Length
    for I in Maxi + 1 .. Strlen loop
      if Separating (Str (Indof (I))) then
        return Indof (I);
      end if;
    end loop;
    -- No separator found
    return Str'Last;
  end Truncate;

end String_Mng;

