with MATH;
-- Puts an INTEGER in a string of fixed length.
-- If I is shorter than max, it is aligned at right or left
-- If I is longer, it is rounded if possible (or truncated)

-- I : Integer value to put in the returned string
-- LEN : Number of characters of the returned string
-- RIGHT : If string is shorter than LEN character, align it at RIGHT
--   or at left (not RIGHT) and fill with GAP
-- GAP : When string is shorter than len, fill empty positions with GAP

function NORMAL (I : INTEGER; LEN : POSITIVE; RIGHT : BOOLEAN := TRUE;
 GAP : CHARACTER := ' ') return STRING is
  L : POSITIVE := INTEGER'IMAGE(I)'LAST;
  SI : STRING (1 .. L) :=  INTEGER'IMAGE(I);
  SM : STRING (1 .. LEN);
begin
  -- skip first char if space
  if SI(1) = ' ' then
    L := L - 1;
    SI (1 .. L) := SI (2 .. L + 1);
  end if;
  if L > LEN then
    -- round I at LEN digits
    declare
      -- round I at len digits
      R : MATH.REAL := MATH.REAL(I) / (10.0 ** (L-LEN) );
      NI : INTEGER := INTEGER(MATH.ROUND (R));
      NL : INTEGER := INTEGER'IMAGE(NI)'LAST;
    begin
      -- correction when i.e. 999.6 is rounded to 1000 : keep 999
      if INTEGER'IMAGE(NI)(1) = ' ' then
        NL := NL - 1;
      end if;
      if NL > LEN then
        NI := INTEGER(MATH.TRUNC (R) );
      end if;
      -- should be OK now
      SM := NORMAL (NI, LEN, RIGHT);
    end;
  end if;
  if L <= LEN then
    -- gap with gap_character, in SM
    if RIGHT then
      SM (1 .. LEN-L) := (others => GAP);
      SM (LEN-L+1 .. LEN) := SI (1 ..L);
    else
      SM (1 .. L) := SI (1 .. L);
      SM (L+1 .. LEN) := (others => GAP);
    end if;
  end if;
  return SM;
end NORMAL;

