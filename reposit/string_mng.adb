with Ada.Characters.Latin_1;
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

end String_Mng;

