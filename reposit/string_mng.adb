package body String_Mng is

  -- Parces spaces and tabs (ASCII.HT) from the head/tail of a string
  -- Returns the position of the first/last character or 0 if
  --  all the string is spaces or tabs (or empty)
  function Parse_Spaces (Str : String; From_Head : Boolean := True)
                        return Natural is
  begin
    if From_Head then
      -- Look forward for significant character
      for I in Str'Range loop
        if Str(I) /= ' ' and then Str(I) /= Ascii.Ht then
          return I;
        end if;
      end loop;
      -- Not found
      return 0;
    else
      -- Look backwards for significant character
      for I in reverse Str'Range loop
        if Str(I) /= ' ' and then Str(I) /= Ascii.Ht then
          return I;
        end if;
      end loop;
      -- Not found
      return 0;
    end if;
  end Parse_Spaces;


  -- Puts a string STR in a string of fixed length LEN.
  -- If STR is shorter than LEN, it is aligned at right or left and padded
  -- If STR is longer  than LEN, it's head ot tail is truncated

  -- STR : STRING to put in the returned string
  -- LEN : Number of characters of the returned string
  -- ALIGN_LEFT : If string is shorter than LEN characters,
  --     align it at left or at right (not LEFT) and fill with GAP,
  -- GAP : When string is shorter than len, fill empty positions with GAP
  -- TRUNC_HEAD : If string is longer than LEN characters, trunc it's head
  --     or its tail
  -- SHOW_TRUNC : When string is longer than LEN, if SHOW_TRUNC is set,
  --         then STR is truncated to LEN-2 and starts (TRUNC_HEAD) with " >"
  --         or ends (not TRUNC_HEAD) with " <"
  function Procuste (Str : String; Len : Positive;
           Align_Left : Boolean := True; Gap : Character := ' ';
           Trunc_Head : Boolean := True; Show_Trunc : Boolean := True)
           return String is
    L : Natural := Str'Length;
    S : String (1 .. Len);
  begin
    if L < Len then
      -- STR is shorter than LEN: Pad
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
      -- STR is larger than LEN: Trunc
      if Trunc_Head then
        if Show_Trunc and then Len >= 2 then
          -- Copy "> " then LEN-2 last characters of STR
          S := "> " & Str(Str'Last-Len+1+2 .. Str'Last);
        else
          -- Copy LEN last characters of STR
          S := Str(Str'Last-Len+1 .. Str'Last);
        end if;
      else
        if Show_Trunc and then Len >= 2 then
          -- Copy LEN-2 first characters of STR then " <"
          S := Str(Str'First .. Str'First+Len-1-2) & " <";
        else
          -- Copy LEN first characters of STR
          S := Str(Str'First .. Str'First+Len-1);
        end if;
      end if;
    else
      -- STR is as LEN characters: copy
      S := Str;
    end if;
    return S;
  end Procuste;

end String_Mng;

