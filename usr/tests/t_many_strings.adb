with Ada.Text_Io;
with Argument, Many_Strings;

procedure T_Many_Strings is
  Str : Many_Strings.Many_String;
  L : Positive;
begin

  -- Build String (may raise Constraint_Error if args too long)
  for I in 1 .. Argument.Get_Nbre_Arg loop
    Str.Cat (Argument.Get_Parameter (Occurence => I));
  end loop;

  -- Decode number
  L := Many_Strings.Nb (Str);
  Ada.Text_Io.Put_Line ("Got" & L'Img & " substrings:");

  -- Decode substrings
  for I in 1 .. L loop
    Ada.Text_Io.Put_Line ('>' &
         Many_Strings.Nth (Str, I) & '<');
  end loop;

  -- Should raise String_Error
  Ada.Text_Io.Put_Line ("This should raise String_Error");
  Ada.Text_Io.Put_Line ('>' &
     Many_Strings.Nth (Str, L + 1) & '<');

end T_Many_Strings;

