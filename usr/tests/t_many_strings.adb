with Basic_Proc, Argument, Many_Strings, As.U;

procedure T_Many_Strings is
  -- Automatic test is:
  -- Empty, Empty, "toto", Empty, "titi", Empty, Empty, Empty
  Auto_Res : constant As.U.Asu_Array
           := (As.U.Asu_Null, As.U.Asu_Null, As.U.Tus ("toto"), As.U.Asu_Null,
               As.U.Tus ("titi"), As.U.Asu_Null, As.U.Asu_Null, As.U.Asu_Null);
  Str : Many_Strings.Many_String;
  L : Positive;

  procedure Put (S : in Many_Strings.Many_String) is
  begin
    -- Decode substrings
    for I in 1 ..  S.Nb loop
      Basic_Proc.Put_Line_Output ('>' & S.Nth (I) & '<');
    end loop;
  end Put;

  use type As.U.Asu_Us, As.U.Asu_Array;

begin
  -- The automatic part
  -------------------------------------
  -- Set Empty, Empty, "toto", Empty,
  -- then "titi", Empty, Empty, Empty
  Str.Set (Many_Strings.Separator & Many_Strings.Separator & "toto"
         & Many_Strings.Separator);
  Str.Cat ("titi" & Many_Strings.Separator);
  Str.Cat ("" & Many_Strings.Separator);

  -- test words
  for I in 1 .. Str.Nb loop
    if Str.Nth (I) /= Auto_Res(I) then
      Basic_Proc.Put_Line_Error ("ERROR: element No" & I'Img
        & ">" & Str.Nth (I) & "< differs from expected >" & Auto_Res(I).Image
        & "<");
      Basic_Proc.Set_Error_Exit_Code;
      return;
    end if;
  end loop;

  -- Test Split
  declare
    A : constant As.U.Asu_Array := Str.Split;
  begin
    if A /= Auto_Res then
      Basic_Proc.Put_Line_Error ("ERROR: Split does not match");
      Put (Str);
      Basic_Proc.Set_Error_Exit_Code;
      return;
    end if;
  end;
  Basic_Proc.Put_Line_Output ("Autotest OK.");

  -- The part that depends on arguments
  -------------------------------------
  Str.Reset;
  -- Build String (may raise Constraint_Error if args too long)
  for I in 1 .. Argument.Get_Nbre_Arg loop
    Str.Cat (Argument.Get_Parameter (Occurence => I));
  end loop;

  -- Decode number
  L := Str.Nb;
  Basic_Proc.Put_Line_Output ("Got" & L'Img & " substrings:");
  -- Decode substrings
  Put (Str);

  if L >= 2 then
    Basic_Proc.Put_Line_Output ("2nd substring >" & Str.Nth(2) & "<");
  end if;

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
  Basic_Proc.Put_Line_Output ("Arg test OK.");

end T_Many_Strings;

