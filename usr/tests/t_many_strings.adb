with Ada.Text_Io;
with As.U; use As.U;
with Argument, Many_Strings;

procedure T_Many_Strings is
  Str : Asu_Us;
  L : Positive;
begin

  -- Build String (may raise Constraint_Error if args too long)
  for I in 1 .. Argument.Get_Nbre_Arg loop
    if I = 1 then
      Str := Asu_Tus (Argument.Get_Parameter (Occurence => I));
    else
      Many_Strings.Cat (Str, Argument.Get_Parameter (Occurence => I));
    end if;
  end loop;

  -- Decode number
  L := Many_Strings.Nb (Asu_Ts (Str));
  Ada.Text_Io.Put_Line ("Got" & L'Img & " substrings:");

  -- Decode substrings
  for I in 1 .. L loop
    Ada.Text_Io.Put_Line ('>' &
         Many_Strings.Nth (Asu_Ts (Str), I) & '<');
  end loop;

  -- Should raise String_Error
  Ada.Text_Io.Put_Line ("This should raise String_Error");
  Ada.Text_Io.Put_Line ('>' &
     Many_Strings.Nth (Asu_Ts (Str), L + 1) & '<');

end T_Many_Strings;

