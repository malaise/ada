with Basic_Proc, Argument, Many_Strings, As.U;

procedure T_Many_Strings is
  Str : Many_Strings.Many_String;
  L : Positive;

  procedure Put (S : in Many_Strings.Many_String) is
  begin
    -- Decode substrings
    for I in 1 ..  S.Nb loop
      Basic_Proc.Put_Line_Output ('>' & S.Nth (I) & '<');
    end loop;
  end Put;


begin

  -- Build String (may raise Constraint_Error if args too long)
  for I in 1 .. Argument.Get_Nbre_Arg loop
    Str.Cat (Argument.Get_Parameter (Occurence => I));
  end loop;

  -- Decode number
  L := Str.Nb;
  Basic_Proc.Put_Line_Output ("Got" & L'Img & " substrings:");
  -- Decode substrings
  Put (Str);

  -- Split and set back
  declare
    A : constant As.U.Asu_Array := Str.Split;
  begin
    Str.Reset;
    Str.Set (A);
  end;
  -- Decode substrings
  Basic_Proc.Put_Line_Output ("After split:");
  Put (Str);

  -- Should raise String_Error
  begin
    Basic_Proc.Put_Line_Output ("This should raise String_Error");
    Basic_Proc.Put_Line_Output ('>' &
       Many_Strings.Nth (Str, L + 1) & '<');
  exception
    when Many_Strings.String_Error =>
      Basic_Proc.Put_Line_Output ("String_Error raised.");
  end;

end T_Many_Strings;

