package STRING_MNG is

  -- Parces spaces and tabs (ASCII.HT) from the head/tail of a string
  -- Returns the position of the first/last character or 0 if
  --  all the string is spaces or tabs (or empty)
  function PARSE_SPACES (STR : STRING; FROM_HEAD : BOOLEAN := TRUE)
                        return NATURAL;

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
   RIGHT : BOOLEAN := TRUE; GAP : CHARACTER := ' ') return STRING;

end STRING_MNG;

