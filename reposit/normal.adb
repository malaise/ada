-- Puts an INTEGER in a string of fixed length.
-- If I is shorter than max, it is aligned at right or left
-- If I is longer, it is rounded if possible (or truncated)

-- I : Integer value to put in the returned string
-- LEN : Number of characters of the returned string
-- RIGHT : If string is shorter than LEN character, align it at RIGHT
--   or at left (not RIGHT) and fill with GAP
-- GAP : When string is shorter than len, fill empty positions with GAP

function NORMAL (I : INTEGER; LEN : POSITIVE;
 RIGHT : BOOLEAN := TRUE; GAP : CHARACTER := ' ') return STRING is
  L : POSITIVE := INTEGER'IMAGE(I)'LAST;
  SI : STRING (1 .. L) := INTEGER'IMAGE(I);
  SM : STRING (1 .. LEN);

  -- Real -> integer : round or trunc
  function TRUNC (X : in FLOAT) return INTEGER is
    INT : INTEGER;
  begin
    INT := INTEGER (X);
    -- Adjust to 1
    if X > 0.0 then
      -- If x>0 error is 1 too much
      if FLOAT (INT) > X then INT := INT - 1; end if;
      return INT;
    else
      -- If x<0 error is 1 too less
      if FLOAT (INT) < X then INT := INT + 1; end if;
      return INT;
    end if;
  exception
    when others => raise CONSTRAINT_ERROR;
  end TRUNC;

  function ROUND (X : in FLOAT) return INTEGER is
    RESULTAT : INTEGER;
  begin
    if X > 0.0 then
      RESULTAT := TRUNC  (X + 0.5);
    else
      RESULTAT := TRUNC  (X - 0.5);
    end if;
    return RESULTAT;
  exception
    when others => raise CONSTRAINT_ERROR;
  end ROUND;


begin
  -- Skip first char if space
  if SI(1) = ' ' then
    L := L - 1;
    SI (1 .. L) := SI (2 .. L + 1);
  end if;
  if L > LEN then
    -- Round I at LEN digits
    declare
      -- Round I at len digits
      R : FLOAT := FLOAT(I) / (10.0 ** (L-LEN) );
      NI : INTEGER := INTEGER(ROUND (R));
      NL : INTEGER := INTEGER'IMAGE(NI)'LAST;
    begin
      -- Correction when i.e. 999.6 is rounded to 1000 : keep 999
      if INTEGER'IMAGE(NI)(1) = ' ' then
        NL := NL - 1;
      end if;
      if NL > LEN then
        NI := INTEGER(TRUNC (R) );
      end if;
      -- Should be OK now
      return NORMAL (NI, LEN, RIGHT);
    end;
  else -- L <= LEN
    -- Gap with gap_character, in SM
    if RIGHT then
      SM (1 .. LEN-L) := (others => GAP);
      SM (LEN-L+1 .. LEN) := SI (1 ..L);
    else
      SM (1 .. L) := SI (1 .. L);
      SM (L+1 .. LEN) := (others => GAP);
    end if;
    return SM;
  end if;
end NORMAL;

