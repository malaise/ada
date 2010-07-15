-- Image of a duration (without leading space)
with As.U; use As.U;
with My_Math, Round_At, String_Mng;
function Dur_Image (Dur : Duration; Nb_Digits : Natural; Plus : Boolean)
                    return String is
   D : Duration;
   Str : Asu_Us;
   Dot : Natural;
begin
  -- Round at Nb_Digits
  D := Duration (Round_At (My_Math.Real (Dur), -Nb_Digits));

  -- Locate Dot
  Str := Asu_Tus (D'Img);
  Dot := String_Mng.Locate (Asu_Ts (Str), ".");
  if Dot = 0 then
    -- No dot!?
    return Asu_Ts (Str);
  end if;

  -- Remove tail
  if Nb_Digits /= 0 then
    -- 0 digits => Remove dot
    -- Else remove from Dot + NbDigits + 1
    Dot := Dot + Nb_Digits + 1;
    if Dot > Asu.Length (Str) then
      -- Less digits than required
      Dot := 0;
    end if;
  end if;
  if Dot /= 0 then
    Asu.Delete (Str, Dot, Asu.Length (Str));
  end if;

  -- Strip leading space or put a '+'
  if Asu.Element (Str, 1) = ' ' then
    if Plus then
      Asu.Replace_Element (Str, 1, '+');
    else
      Asu.Delete (Str, 1, 1);
    end if;
  end if;
  return Asu_Ts (Str);
end Dur_Image;

