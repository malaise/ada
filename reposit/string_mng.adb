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
  -- RIGHT : If string is shorter than LEN characters, align it at RIGHT
  --          or at left (not RIGHT) and fill with GAP,
  --         If string is longer than LEN characters, trunc it's head (RIGHT)
  --          or its tail (not RIGHT)
  -- GAP : When string is shorter than len, fill empty positions with GAP

  function PROCUSTE (STR : STRING; LEN : POSITIVE;
   RIGHT : BOOLEAN := TRUE; GAP : CHARACTER := ' ') return STRING is

    L : POSITIVE := STR'LENGTH;
    S : STRING (1 .. LEN);
  begin
    if L < LEN then
      -- STR is shorter than LEN: Pad
      if RIGHT then
        -- Copy L characters at right and pad
        S(LEN-L+1 .. LEN) := STR;
        S(LEN-L+2 .. LEN) := (others => GAP);
      else
        -- Copy L characters at left and pad
        S(1 .. L) := STR;
        S(L+1 .. LEN) := (others => GAP);
      end if;
    elsif L > LEN then
      -- STR is larger than LEN: Trunc
      if RIGHT then
        -- Copy L last characters of STR
        S := STR(STR'LAST-L+1 .. STR'LAST);
      else
        -- Copy L first characters of STR
        S := STR(STR'FIRST .. STR'FIRST+L-1);
      end if;
    else
      -- STR is as LEN characters: copy
      S := STR;
    end if;
    return S;
  end PROCUSTE;

end STRING_MNG;

