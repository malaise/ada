package body STRING_MNG is

  -- Parces spaces and tabs (ASCII.HT) from the head/tail of a string
  -- Returns the position of the first/last character or 0 if
  --  all the string is spaces or tabs (or empty)
  function PARSE_SPACES (STR : STRING; FROM_HEAD : BOOLEAN := TRUE)
                        return NATURAL is
  begin
    if FROM_HEAD then
      -- Look forward for significant character
      for I in STR'RANGE loop
        if STR(I) /= ' ' and then STR(I) /= ASCII.HT then
          return I;
        end if;
      end loop;
      -- Not found
      return 0;
    else
      -- Look backwards for significant character
      for I in reverse STR'RANGE loop
        if STR(I) /= ' ' and then STR(I) /= ASCII.HT then
          return I;
        end if;
      end loop;
      -- Not found
      return 0;
    end if;
  end PARSE_SPACES;


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
  function PROCUSTE (STR : STRING; LEN : POSITIVE;
           ALIGN_LEFT : BOOLEAN := TRUE; GAP : CHARACTER := ' ';
           TRUNC_HEAD : BOOLEAN := TRUE; SHOW_TRUNC : BOOLEAN := TRUE)
           return STRING is
    L : NATURAL := STR'LENGTH;
    S : STRING (1 .. LEN);
  begin
    if L < LEN then
      -- STR is shorter than LEN: Pad
      if ALIGN_LEFT then
        -- Copy L characters at left and pad
        S(1 .. L) := STR;
        S(L+1 .. LEN) := (others => GAP);
      else
        -- Copy L characters at right and pad
        S(LEN-L+1 .. LEN) := STR;
        S(1 .. LEN-L) := (others => GAP);
      end if;
    elsif L > LEN then
      -- STR is larger than LEN: Trunc
      if TRUNC_HEAD then
        if SHOW_TRUNC and then LEN >= 2 then
          -- Copy "> " then LEN-2 last characters of STR
          S := "> " & STR(STR'LAST-LEN+1+2 .. STR'LAST);
        else
          -- Copy LEN last characters of STR
          S := STR(STR'LAST-LEN+1 .. STR'LAST);
        end if;
      else
        if SHOW_TRUNC and then LEN >= 2 then
          -- Copy LEN-2 first characters of STR then " <"
          S := STR(STR'FIRST .. STR'FIRST+LEN-1-2) & " <";
        else
          -- Copy LEN first characters of STR
          S := STR(STR'FIRST .. STR'FIRST+LEN-1);
        end if;
      end if;
    else
      -- STR is as LEN characters: copy
      S := STR;
    end if;
    return S;
  end PROCUSTE;

end STRING_MNG;

