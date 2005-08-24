package body String_Mng is

  -- Parces spaces and tabs (latin_1.Ht) from the head/tail of a string
  -- Returns the position of the first/last character or 0 if
  --  all the string is spaces or tabs (or empty)
  function Parse_Spaces (Str : String; From_Head : Boolean := True)
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
  function Procuste (Str : String; Len : Positive;
           Align_Left : Boolean := True; Gap : Character := ' ';
           Trunc_Head : Boolean := True; Show_Trunc : Boolean := True)
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
  -- Returns index of fragment start
  --  or 0 if not found or if Within or Fragment is empty
  function Locate (Within : String;
                   Fragment : String;
                   Occurence : Positive := 1) return Natural is
    Found_Occurence : Natural := 0;
  begin
    if Within'Length = 0 or else Fragment'Length = 0 then
      return 0;
    end if;
    for I in 1 .. Within'Length - Fragment'Length + 1 loop
      if Within(I .. I+Fragment'Length-1) = Fragment then
        Found_Occurence := Found_Occurence + 1;
        if Found_Occurence = Occurence then
          return I;
        end if;
      end if;
    end loop;
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
  function Remove (From : String; At_Index : Positive; Nb_Char : Natural;
                   Shift_Left : Boolean := True;
                   Gap : Character := No_Gap) return String is

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
  function Slice (From : String; At_Index : Positive; Nb_Char : Natural;
                  To_Right : Boolean := True) return String is
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
  function Cut (From : String; Nb_Char : Natural;
                Head : Boolean := True) return String is
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
  function Extract (From : String; Nb_Char : Natural;
                    Head : Boolean := True) return String is
  begin
    if Head then
      return Slice (From, From'First, Nb_Char, True);
    else
      return Slice (From, From'Last,  Nb_Char, False);
    end if;
  end Extract;

end String_Mng;

