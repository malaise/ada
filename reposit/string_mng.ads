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
           return STRING;

end STRING_MNG;

