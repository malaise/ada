with Ada.Text_Io;
with Text_Handler, Argument, Many_Strings;

procedure T_Many_Strings is
  Str : Text_Handler.Text(1024);
  L : Positive; 
begin

  -- Build String (may raise Constraint_Error if args too long)
  for I in 1 .. Argument.Get_Nbre_Arg loop
    if I = 1 then
      Text_Handler.Set (Str, Argument.Get_Parameter (Occurence => I));
    else
      Many_Strings.Cat (Str, Argument.Get_Parameter (Occurence => I));
    end if;
  end loop;

  -- Decode number
  L := Many_Strings.Nb (Text_Handler.Value (Str));
  Ada.Text_Io.Put_Line ("Got" & L'Img & " substrings:");

  -- Decode substrings
  for I in 1 .. L loop
    Ada.Text_Io.Put_Line ('>' &
         Many_Strings.Nth (Text_Handler.Value(Str), I) & '<');
  end loop;

  -- Should raise String_Error
  Ada.Text_Io.Put_Line ("This should raise String_Error");
  Ada.Text_Io.Put_Line ('>' &
     Many_Strings.Nth (Text_Handler.Value(Str), L + 1) & '<');

end T_Many_Strings;

