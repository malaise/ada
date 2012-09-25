-- Image of a duration (without leading space)
-- Round at Nb_Digits decimals
-- If Plus and Dur>=0 then put a '+' sign before result
with As.U, My_Math, Round_At, Str_Util;
function Dur_Image (Dur : Duration; Nb_Digits : Natural; Plus : Boolean)
                    return String is
   D : Duration;
   Str : As.U.Asu_Us;
   Dot : Natural;
begin
  -- Round at Nb_Digits
  D := Duration (Round_At (My_Math.Real (Dur), -Nb_Digits));

  -- Locate Dot
  Str := As.U.Tus (D'Img);
  Dot := Str_Util.Locate (Str.Image, ".");
  if Dot = 0 then
    -- No dot!?
    return Str.Image;
  end if;

  -- Remove tail
  if Nb_Digits /= 0 then
    -- 0 digits => Remove dot
    -- Else remove from Dot + NbDigits + 1
    Dot := Dot + Nb_Digits + 1;
    if Dot > Str.Length then
      -- Less digits than required
      Dot := 0;
    end if;
  end if;
  if Dot /= 0 then
    Str.Delete (Dot, Str.Length);
  end if;

  -- Strip leading space or put a '+'
  if Str.Element (1) = ' ' then
    if Plus then
      Str.Replace_Element (1, '+');
    else
      Str.Delete (1, 1);
    end if;
  end if;
  return Str.Image;
end Dur_Image;

