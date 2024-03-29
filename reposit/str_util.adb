with As.U;
package body Str_Util is

  -- Return a String (1 .. N)
  function Normalize (Str : String) return String is
  begin
    if Str'First = 1 then
      -- Optim: no copy if not needed
      return Str;
    end if;
    declare
      Lstr : constant String (1 .. Str'Length) := Str;
    begin
      return Lstr;
    end;
  end Normalize;

  -- Swap the characters of string
  -- Example: ABCD -> DCBA
  function Swap (Str : String) return String is
    Res : String (1 .. Str'Length);
  begin
    for I in reverse Str'Range loop
      Res(Str'Last - I + 1) := Str(I);
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
    Output : String (1 .. From'Length);
    First, Last : Natural;
    C : Character;
  begin
    if From_Head then
      Last := 0;
      for I in From'Range loop
        C := From(I);
        if not Used(C) then
          Last := Last + 1;
          Output(Last) := C;
          Used(C) := True;
        end if;
      end loop;
      return Output(1 .. Last);
    else
      -- Cannot init First to From'Length + 1, cause may raise Constraint_Error
      for I in reverse From'Range loop
        C := From(I);
        if not Used(C) then
          if I = From'Last then
            First := From'Length;
          else
            First := First - 1;
          end if;
          Output(First) := C;
          Used(C) := True;
        end if;
      end loop;
      return Normalize (Output(First .. Output'Last));
    end if;
  end Unique;

  -- Overwrite a part of a string by a new one
  -- Do nothing if New_Str is empty
  -- Append New_Item if Position = Source'Last + 1
  -- Extend Source if Position + New_Str'Length - 1 > Source'Last
  -- Raises Constraint_Error if Position < Source'First
  --                         or Position > Source'Last + 1
  function Overwrite (Source   : String;
                      Position : Positive;
                      New_Str  : String) return String is
    -- Index in New_Str of last overwritting char (others are appended)
    Lo : Natural;
    Result : String (1 .. Source'Length);
  begin
    if Position < Source'First or else Position > Source'Last + 1 then
      raise Constraint_Error;
    end if;
    Lo := (if Position + New_Str'Length - 1 > Source'Last then
             New_Str'First + Source'Last - Position
           else New_Str'Last);
    Result := Source;
    -- Overwrite by Lo chars from Position
    Result(Position - Source'First + 1
        .. Position - Source'First + 1 + Lo - New_Str'First) :=
      New_Str(New_Str'First .. Lo);
    -- Append others
    return Result & New_Str(Lo + 1 .. New_Str'Last);
  end Overwrite;

  -- Copy the string Val at the beginning of the string To
  -- To(To'First .. To'First - Val'Length) := Val;
  procedure Copy (Val : in String; To : in out String) is
  begin
    To(To'First .. To'First + Val'Length - 1) := Val;
  end Copy;

  -- Replace a slice by a new string
  -- Delete chars if By is empty (except if High < Low)
  -- Insert By before Low if High < Low
  -- Append By if Low = Source'Last + 1 (and High < Low)
  -- Raises Constraint_Error if Low < Source'First
  --                         or Low > Source'Last + 1 or High > Source'Last
  function Replace (Source   : String;
                    Low      : Positive;
                    High     : Natural;
                    By       : String) return String is
    Start_Tail : Positive;
  begin
    if Low < Source'First or else Low > Source'Last + 1
    or else High > Source'Last then
      raise Constraint_Error;
    end if;
    Start_Tail := (if Low <= High then High + 1 -- Replace
                   else Low);                   -- Insert
    return Normalize (Source(Source'First .. Low - 1)
                    & By
                    & Source(Start_Tail .. Source'Last));
  end Replace;

  -- Insert a string before a given position
  -- Append if Before = Source'Last + 1
  -- Raises Constraint_Error if Before < Source'First
  --                         or Before > Source'Last + 1
  function Insert (Source  : String;
                   Before  : Positive;
                   New_Str : String) return String is
  begin
    if Before < Source'First or else Before > Source'Last + 1 then
      raise Constraint_Error;
    end if;
    return Normalize (if Before = Source'Last + 1 then
                        Source & New_Str
                      else
                        Source(Source'First .. Before - 1)
                        & New_Str
                        & Source(Before .. Source'Last));
  end Insert;

  -- Delete some characters
  -- Do nothing if Through < From
  -- Raises Constraint_Error if Through >= From and
  --  From < Source'First or From > Source'Last
  --  or Through > Source'Last
  function Delete (Source  : String;
                   From    : Positive;
                   Through : Natural) return String is
  begin
   if Through < From then
      return Normalize (Source);
    end if;
    if From < Source'First or else From > Source'Last
    or else Through > Source'Last then
      raise Constraint_Error;
    end if;
    if Source'Last - Source'First = Through - From then
      return "";
    end if;
    return Normalize (if Through = Source'Last then
                        Source(Source'First .. From - 1)
                      else
                        Source(Source'First .. From - 1)
                      & Source(Through + 1 .. Source'Last));
  end Delete;

  -- Delete Number characters from From included
  --  or as many characters as possible
  -- May raise Constraint_Error if From > Source.Length
  function Delete_Nb (Source : String;
                      From   : Positive;
                      Number : Natural) return String is
  begin
    if From + Number - 1 <= Source'Last then
      -- We can delete Number characters
      return Delete (Source, From, From + Number - 1);
    else
      -- We delete from From to Last
      return Delete (Source, From, Source'Last);
    end if;
  end Delete_Nb;

  -- Remove tailing spaces and tabs
  function Strip (Str : String; From : Strip_Kind := Tail) return String is
    -- Parses spaces and tabs (Ht) from the head/tail of a string
    -- Returns the position of the first/last character or 0 if
    --  all the string is spaces or tabs (or empty)
   function Parse_Spaces (Str : String;
                           From_Head : Boolean := True)
             return Natural is
    begin
      if From_Head then
        -- Look forward for significant character
        for I in Str'Range loop
          if not Is_Separator (Str(I)) then
            return I;
          end if;
        end loop;
        -- Not found
        return 0;
      else
        -- Look backwards for significant character
        for I in reverse Str'Range loop
          if not Is_Separator (Str(I)) then
            return I;
          end if;
        end loop;
        -- Not found
        return 0;
      end if;
    end Parse_Spaces;

    Start, Stop : Natural;
  begin
    case From is
      when Tail =>
        Start := Str'First;
        Stop  := Parse_Spaces (Str, False);
      when Head =>
        Start := Parse_Spaces (Str, True);
        Stop  := Str'Last;
      when Both =>
        Start := Parse_Spaces (Str, True);
        Stop  := Parse_Spaces (Str, False);
    end case;
    if Start = 0 then
      return "";
    else
      return Normalize (Str(Start .. Stop));
    end if;
  end Strip;

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
      return (if Gap = No_Gap or else Nb_Gap <= 0 then ""
              else Pad_Str'( (others => Gap)));
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
      return Normalize ((if At_Index + Nb_Char - 1 >= From'Last then
                           -- No tail to keep, return head (and pad)
                           From(From'First .. At_Index-1)
                           & Do_Pad(From'Last-At_Index+1)
                         else
                           -- Cat tail to head and return it (and pad)
                           From(From'First .. At_Index-1))
                           & From(At_Index+Nb_Char .. From'Last)
                           & Do_Pad(Nb_Char));
    else
      -- Check if Nb_Char can be removed from At_Index
      return Normalize (if At_Index <= Nb_Char then
                          -- No head to keep, return head (and pad)
                          Do_Pad(At_Index-From'First+1)
                          & From(At_Index+1 .. From'Last)
                        else
                          Do_Pad(Nb_Char)
                          & From(From'First .. At_Index-Nb_Char)
                          & From(At_Index+1 .. From'Last));
    end if;
  end Remove;

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
    return Normalize (if To_Right then
                        From(At_Index .. At_Index + Nb_Char - 1)
                      else
                        From(At_Index - Nb_Char + 1 .. At_Index));
  end Slice;

  -- Extract the Nb_Char first (if Head is set to True) or last characters
  --    (if Head is set to False) of From string.
  -- Return the extracted substring.
  -- Raises Constraint_Error if Nb_Char is more than From'Length.
  function Extract (From : String;
                    Nb_Char : Natural;
                    Head : Boolean := True) return String is
    (if Head then Slice (From, From'First, Nb_Char, True)
     else Slice (From, From'Last,  Nb_Char, False));

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
                     Show_Trunc : Boolean := True;
                     Head_Mark : Str2 := "> ";
                     Tail_Mark : Str2 := " <")
           return String is
    L : constant Natural := Str'Length;
    S : String (1 .. Len);
  begin
    if L < Len then
      -- Str is shorter than Len: Pad
      if Align_Left then
        -- Copy L characters at left and pad
        S(1 .. L) := Str;
        S(L + 1 .. Len) := (others => Gap);
      else
        -- Copy L characters at right and pad
        S(Len - L + 1 .. Len) := Str;
        S(1 .. Len - L) := (others => Gap);
      end if;
    elsif L > Len then
      -- Str is larger than Len: Trunc
      if Trunc_Head then
        if Show_Trunc and then Len >= 2 then
          -- Copy "> " then Len-2 last characters of Str
          S := Head_Mark & Str(Str'Last - Len + 1 + 2 .. Str'Last);
        else
          -- Copy Len last characters of Str
          S := Str(Str'Last - Len + 1 .. Str'Last);
        end if;
      else
        if Show_Trunc and then Len >= 2 then
          -- Copy Len-2 first characters of Str then " <"
          S := Str(Str'First .. Str'First + Len - 1 - 2) & Tail_Mark;
        else
          -- Copy Len first characters of Str
          S := Str(Str'First .. Str'First + Len - 1);
        end if;
      end if;
    else
      -- Str is as Len characters: copy
      S := Str;
    end if;
    return S;
  end Procuste;

  -- Locate where to cut Str so that is best matches the requested line Length
  -- Looks for separator character
  -- Default Separator function, True for Space and Ht.
  function Is_Separator (Char : Character) return Boolean is
    (Char = Aski.Spc or else Char = Aski.Ht);

  -- If Str is shorter or equal to Length, return Str'Last
  -- Else try to find a separator before Length, up to Mini
  -- Else try to find a separator after  Length, up to Maxi
  -- Else try to find a separator before Mini,   up to 1
  -- Else try to find a separator after  Maxi,   up to Str'Length
  -- Prerequisits Mini <= Length <= Maxi. Beware that they are not
  --  relative to Str indexes but that the returned value is.
  -- Returns 0 only if Str is empty.
  function Truncate (Str : String;
                     Length : Positive;
                     Mini, Maxi : Positive;
                     Separating : not null access
    function (Char : Character) return Boolean := Is_Separator'Access)
  return String is
    Strlen : constant Natural := Str'Length;
    -- Corresponding index in Str
    function Indof (I : Positive) return Positive is (I - 1 + Str'First);
  begin
    if Mini > Length or else Length > Maxi then
      raise Constraint_Error;
    end if;
    -- Handle trivial cases
    if Strlen = 0 then
      return "";
    elsif Strlen <= Length then
      return Normalize (Str);
    end if;
    -- Else try to find a separator before Length, up to Mini
    for I in reverse Mini .. Length loop
      if Separating (Str(Indof (I))) then
        return Normalize (Str(Str'First .. Indof (I)));
      end if;
    end loop;
    -- Else try to find a separator after  Length, up to Maxi
    for I in Length + 1 .. Maxi loop
      if Separating (Str(Indof (I))) then
        return Normalize (Str(Str'First .. Indof (I)));
      end if;
    end loop;
    -- Else try to find a separator before Mini,   up to 1
    for I in reverse 1 .. Mini - 1 loop
      if Separating (Str(Indof (I))) then
        return Normalize (Str(Str'First .. Indof (I)));
      end if;
    end loop;
    -- Else try to find a separator after  Maxi,   up to Str'Length
    for I in Maxi + 1 .. Strlen loop
      if Separating (Str(Indof (I))) then
        return Normalize (Str(Str'First .. Indof (I)));
      end if;
    end loop;
    -- No separator found
    return Normalize (Str);
  end Truncate;

  -- Center a String Str in a fixed size
  -- if Str <= Size pad with Gap after then before Str
  -- if Str > Size  raise Constraint_Error
  function Center (Str : String;
                   Len : Positive;
                   Gap : Character := ' ';
                   Offset : Integer := 0) return String is
    Start : Integer;
  begin
    if Str'Length > Len then
      raise Constraint_Error;
    end if;
    -- Start position
    Start := (Len - Str'Length) / 2 + 1 + Offset;
    if Start <= 0 then
      Start := 1;
    end if;
    if Start + Str'Length > Len  + 1 then
      Start := Len - Str'Length + 1;
    end if;
    -- Copy
    declare
      Res : String (1 .. Len) := (others => Gap);
    begin
      Copy (Str, Res(Start .. Len));
      return Res;
    end;
  end Center;

  -- Check if Within starts or ends with Fragment (More efficient than Locate)
  -- Returns False if Fragment is empty
  function Start_With (Within     : String;
                       Fragment   : String;
                       Forward    : Boolean := True) return Boolean is
  begin
    -- Handle incorrect values
    if Fragment'Length = 0 or else Within'Length < Fragment'Length then
      return False;
    end if;
    if Forward then
      return Within(Within'First .. Within'First + Fragment'Length - 1)
             = Fragment;
    else
      return Within(Within'Last - Fragment'Length + 1 .. Within'Last)
             = Fragment;
    end if;
  end Start_With;

  -- Locate Nth occurence of a fragment within a string,
  --  between a given index (first/last if 0) and the end/beginning of string,
  --  searching forward or backward
  -- Returns index in Within of char matching start of Fragment
  --  or 0 if not found or if Within or Fragment is empty
  function Locate (Within     : String;
                   Fragment   : String;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1)
           return Natural is
    Index : Natural;
    Found_Occurence : Natural := 0;
  begin
    -- Fix Index
    Index := (if From_Index = 0 then
               (if Forward then Within'First else Within'Last)
              else From_Index);

    -- Handle limit or incorrect values
    if Within'Length = 0
    or else Fragment'Length = 0
    or else Index not in Within'First .. Within'Last then
      return 0;
    end if;
    if Forward then
      for I in Index .. Within'Last - Fragment'Length + 1 loop
        if Within(I .. I + Fragment'Length - 1) = Fragment then
          Found_Occurence := Found_Occurence + 1;
          if Found_Occurence = Occurence then
            return I;
          end if;
        end if;
      end loop;
    else
      for I in reverse Within'First .. Index - Fragment'Length + 1 loop
        if Within(I .. I + Fragment'Length - 1) = Fragment then
          Found_Occurence := Found_Occurence + 1;
          if Found_Occurence = Occurence then
            return I;
          end if;
        end if;
      end loop;
    end if;
    return 0;
  exception
    when Constraint_Error =>
      return 0;
  end Locate;

  -- Replace all variables by their values provided by the Resolv callback.
  -- A variable name is identified because it is within delimiters (strings).
  -- Start and stop delimiters must be non empty and different (e.g. "(" and ")"
  --  or "${" and "}"), otherwise Inv_Delimiter is raised.
  -- Variables may be defined recursively (e.g. ${Foo${Bar}}).
  -- Delimiter number must match (as many stop as start and in consistent
  --  sequence e.g. {}}{ s forbidden), otherwise the exception
  --  Delimiter_Mismatch is raised.
  -- On option Recursive, loops re-avaluating as long as possible (otherwise
  --  only one pass)
  -- On option No_Check_Stop, extra stops are accepted ({}} is OK)
  -- If no callback is set (Resolv = null) then variables are replaced by
  --  empty strings.
  function Eval_Variables (Str : String;
                           Start_Delimiter, Stop_Delimiter : in String;
                           Resolv : access
    function (Variable_Name : String) return String;
                           Muliple_Passes : Boolean;
                           No_Check_Stop : Boolean;
                           Skip_Backslashed : Boolean) return String is


    -- The string to work on
    Ustr : As.U.Asu_Us;
    -- Last index of Ustr
    Last_Index : Natural;

    -- Check that Str_Index + Delimiter Length fits in Ustr and check if
    --  Ustr (Str_Index...) matches Delimiter
    function Match (Str_Index : in Positive; Delimiter : in String)
                   return Boolean is
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
      if Ustr.Slice (Str_Index, Stop_Index) /= Delimiter then
        -- Not match
        return False;
      end if;
      if Skip_Backslashed
      and then Is_Backslashed (Ustr.Image, Str_Index) then
        -- Matches but backslashed
        return False;
      end if;
      return True;
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

    -- Replace one occurence by the value
    procedure Substitute is
    begin
      if Resolv /= null then
        declare
          -- Variable value
          Val : constant String
              := Resolv (Ustr.Slice (Start_Var, Stop_Var));
          -- Correction to current and last index
          Offset : constant Integer
                 := Val'Length - (Stop_Index - Start_Index + 1);
        begin
          Ustr.Replace (Start_Index, Stop_Index, Val);
          Curr_Index := Curr_Index + Offset;
          Last_Index := Last_Index + Offset;
        end;
      else
        -- No resolving => empty string
        Ustr.Replace (Start_Index, Stop_Index, "");
        Curr_Index := Curr_Index - (Stop_Index - Start_Index + 1);
        Last_Index := Last_Index - (Stop_Index - Start_Index + 1);
      end if;
    end Substitute;

  begin
    -- Check Delimiters are non empty and different
    if Start_Delimiter = ""
    or else Stop_Delimiter = ""
    or else Start_Delimiter = Stop_Delimiter then
      raise Inv_Delimiter;
    end if;

    -- Store input string and its last index
    Ustr := As.U.Tus (Str);
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
          if Level = 0 and then No_Check_Stop then
            -- Skip this extra Stop
            null;
          else
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
              Substitute;
              -- Go on trying to substitute
              Subst_Occured := True;
              -- Following stop delimiter will not be processed if no
              -- new start has been found
              Start_Index := 0;
            end if;
          end if;
        end if;
        -- End of this pass?
        exit when Curr_Index = Last_Index;
        -- Search delimiters in next char
        Curr_Index := Curr_Index + 1;
      end loop;
      exit when not Muliple_Passes;
    end loop;

    -- Final level must be 0
    if Level /= 0 then
      raise Delimiter_Mismatch;
    end if;

    -- Remove backslash for delimiters if they have been skipped
    --  and replace '\\' by '\'
    if Skip_Backslashed then
      Curr_Index := 1;
      Last_Index := Ustr.Length;
      while Curr_Index < Ustr.Length loop
        if Match (Curr_Index, "\\") then
            -- Remove first '\' and skip second
            Ustr.Delete (Curr_Index, Curr_Index);
            Last_Index := Last_Index - 1;
            Curr_Index := Curr_Index + 1;
          elsif Match (Curr_Index, "\" & Start_Delimiter) then
            -- Remove leading '\' and skip  delimiter
            Ustr.Delete (Curr_Index, Curr_Index);
            Last_Index := Last_Index - 1;
            Curr_Index := Curr_Index + Start_Delimiter'Length;
          elsif Match (Curr_Index, "\" & Stop_Delimiter) then
            -- Remove leading '\' and skip  delimiter
            Ustr.Delete (Curr_Index, Curr_Index);
            Last_Index := Last_Index - 1;
            Curr_Index := Curr_Index + Stop_Delimiter'Length;
          else
            -- One step forward
            Curr_Index := Curr_Index + Stop_Delimiter'Length;
          end if;
       end loop;
    end if;

    -- Done
    return Ustr.Image;
  end Eval_Variables;

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
        for J in reverse From_Index .. I loop
          exit when Within_Str(J) /= Esc;
          Start := J;
        end loop;
        -- Check I = J modulo 2 => odd number of Esc
        if (I - Start) rem 2 = 0 then
          -- Odd number of Esc is an escape: "\\\n" -> '\''\n'
          -- Check if the following letter is within the Escape chars
          for Esc of Escape loop
            if Within_Str(I + 1) = Esc then
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

  -- Check if the character at Index of Str is backslashed
  --  (the number of '\' before it is odd)
  -- Raises Constraint_Error if Index is out of Str
  function Is_Backslashed (Str   : String;
                           Index : Positive) return Boolean is
    Backslashed : Boolean := False;
  begin
    if Index < Str'First or else Index > Str'Last then
      raise Constraint_Error;
    end if;
    for I in reverse Str'First .. Index - 1 loop
      if Str(I) = '\' then
        Backslashed := not Backslashed;
      else
        exit;
      end if;
    end loop;
    return Backslashed;
  end Is_Backslashed;

  -- Split Str according to Separator
  -- Replaces Separator by Many_String.Separator but skips
  --  '\' & Separator (using Is_Backslashed),
  --  then replaces '\' & Separator by Separator
  function Split (Str       : String;
                  Separator : Character) return Many_Strings.Many_String is
    Result : As.U.Asu_Us;
    Index : Natural;
  begin
    Result := As.U.Tus (Str);
    -- Do in reverse so the result of subst does no affect
    --  Is_Backslashed or index
    Index := Result.Length;
    while Index /= 0 loop
      if Result.Element (Index) = Separator then
        if Is_Backslashed (Result.Image, Index) then
          -- Replace '\' & Separator by Separator
          Result.Replace (Index - 1, Result.Length,
                          Result.Slice (Index, Result.Length) );
          Index := Index - 1;
        else
          -- Replace Separator by Many_Strings.Separator
          Result.Replace_Element (Index, Many_Strings.Separator);
        end if;
      end if;
      Index := Index - 1;
    end loop;
    return Many_Strings.Set (Result);
  end Split;

  -- Replace occurences of What by By in Str. One pass.
  function Substit (Str, What, By : String;
                    Skip_Backslashed: Boolean := False) return String is
    Len : constant Natural := What'Length;
    Last : constant Natural := Str'Last;
    I : Positive;
    Result : As.U.Asu_Us;
  begin
    -- Nothing if what is empty or Str too short
    if Len = 0 or else Str'Length < Len then
      return Str;
    end if;
    I := Str'First;
    loop
      -- See if there are enough chars remaining. If yes, check if match
      if I + Len - 1 > Last then
        -- Str cannot match any more (not enough chars)
        Result.Append (Str(I .. Last));
        exit;
      elsif Str(I .. I + Len - 1) = What
      and then (not Skip_Backslashed or else not Is_Backslashed (Str, I)) then
        -- Match, replace
        Result.Append (By);
        I := I + Len;
      else
        -- No match, move one char forward
        Result.Append (Str(I));
        I := I + 1;
      end if;
    end loop;
    return Result.Image;
  end Substit;

end Str_Util;

