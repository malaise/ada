-- Puts an Integer in a string of fixed length.
-- If I is shorter than max, it is aligned at right or left
-- If I is longer, it is rounded if possible (or truncated)

-- I : Integer value to put in the returned string
-- Len : Number of characters of the returned string
-- Right : If string is shorter than Len character, align it at Right
--   or at left (not Right) and fill with Gap
-- Gap : When string is shorter than len, fill empty positions with Gap

function Normal (I : Integer; Len : Positive;
 Right : Boolean := True; Gap : Character := ' ') return String is
  L : Positive := Integer'Image(I)'Last;
  Si : String (1 .. L) := Integer'Image(I);
  Sm : String (1 .. Len);

  -- Real -> integer : round or trunc
  function Trunc (X : in Float) return Integer is
    Int : Integer;
  begin
    Int := Integer (X);
    -- Adjust to 1
    if X > 0.0 then
      -- If x>0 error is 1 too much
      if Float (Int) > X then Int := Int - 1; end if;
      return Int;
    else
      -- If x<0 error is 1 too less
      if Float (Int) < X then Int := Int + 1; end if;
      return Int;
    end if;
  exception
    when others => raise Constraint_Error;
  end Trunc;

  function Round (X : in Float) return Integer is
    Resultat : Integer;
  begin
    if X > 0.0 then
      Resultat := Trunc  (X + 0.5);
    else
      Resultat := Trunc  (X - 0.5);
    end if;
    return Resultat;
  exception
    when others => raise Constraint_Error;
  end Round;


begin
  -- Skip first char if space
  if Si(1) = ' ' then
    L := L - 1;
    Si (1 .. L) := Si (2 .. L + 1);
  end if;
  if L > Len then
    -- Round I at Len digits
    declare
      -- Round I at len digits
      R : Float := Float(I) / (10.0 ** (L-Len) );
      Ni : Integer := Integer(Round (R));
      Nl : Integer := Integer'Image(Ni)'Last;
    begin
      -- Correction when i.e. 999.6 is rounded to 1000 : keep 999
      if Integer'Image(Ni)(1) = ' ' then
        Nl := Nl - 1;
      end if;
      if Nl > Len then
        Ni := Integer(Trunc (R) );
      end if;
      -- Should be Ok now
      return Normal (Ni, Len, Right);
    end;
  else -- L <= Len
    -- Gap with gap_character, in Sm
    if Right then
      Sm (1 .. Len-L) := (others => Gap);
      Sm (Len-L+1 .. Len) := Si (1 ..L);
    else
      Sm (1 .. L) := Si (1 .. L);
      Sm (L+1 .. Len) := (others => Gap);
    end if;
    return Sm;
  end if;
end Normal;

