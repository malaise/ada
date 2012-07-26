-- Puts an Integer in a string of fixed length.
-- If I is shorter than max, it is aligned at right or left
-- If I is longer, it is rounded if possible (or truncated)

-- I : Integer value to put in the returned string
-- Len : Number of characters of the returned string
-- Right : If string is shorter than Len character, align it at Right
--   or at left (not Right) and fill with Gap
-- Gap : When string is shorter than len, fill empty positions with Gap
function Normal_Gen (I     : Int;
                     Len   : Positive;
                     Right : Boolean := True;
                     Gap   : Character := ' ') return String is
  L : Positive := Int'Image(I)'Last;
  Si : String (1 .. L) := Int'Image(I);
  Sm : String (1 .. Len);
  Warning_Char : constant Character := '!';

  -- Real -> Int : round or trunc
  function Trunc (X : in Float) return Int is
    I : Int;
  begin
    I := Int (X);
    -- Adjust to 1
    if X > 0.0 then
      -- If x>0 error is 1 too much
      if Float (I) > X then I := I - 1; end if;
      return I;
    else
      -- If x<0 error is 1 too less
      if Float (I) < X then I := I + 1; end if;
      return I;
    end if;
  exception
    when others => raise Constraint_Error;
  end Trunc;

  function Round (X : in Float) return Int is
    Result : Int;
  begin
    if X > 0.0 then
      Result := Trunc  (X + 0.5);
    else
      Result := Trunc  (X - 0.5);
    end if;
    return Result;
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
    -- I is longer than the requested Len
    -- Round I at Len - 1 digits and cat the warning char
    declare
      R : constant Float := Float(I) / (10.0 ** (L-Len+1) );
      I : constant Int := Round (R);
      Imi : constant String := Int'Image(I);
      Fi : Natural := 1;
      Li : Natural := Imi'Last;
    begin
      -- Skip first char if space
      if Imi(1) = ' ' then
        Fi := Fi + 1;
      end if;
      if Li - Fi + 1 = (Len - 1) + 1 then
        -- Round has generated an extra digit (e.g. 99.8 -> 100)
        -- Skip last digit
        Li := Li - 1;
      elsif Li - Fi + 1 = (Len - 1) then
        -- Round has not generated extra digit (e.g. 99.4 -> 99)
        null;
      else
        -- Bug: Round should have lead to (Len - 1) or (Len - 1) + 1
        raise Program_Error;
      end if;
      -- Cat warning char
      if Right then
        return Warning_Char & Imi(Fi .. Li);
      else
        return Imi(Fi .. Li) & Warning_Char;
      end if;
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
end Normal_Gen;

