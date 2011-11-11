with Argument, Arbitrary, Rnd, Integer_Image, Mixed_Str, My_Math, Basic_Proc;
procedure T_Arbitrary is


  Abort_Error : exception;
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                                    & " [ <num1> [ <num2> ] ]");
    raise Abort_Error;
  end Usage;

  function Set (Occ : Positive) return Arbitrary.Number is
  begin
    return Arbitrary.Set (Argument.Get_Parameter(Occurence => Occ));
  exception
    when others =>
      Basic_Proc.Put_Line_Output ("Invalid_Number "
                               & Argument.Get_Parameter(Occurence => Occ));
      Usage;
      raise Abort_Error;
  end Set;

  use type Arbitrary.Number;

  Max : constant Positive
      := Positive(My_Math.Sqrt (My_Math.Real(Integer'Last))) - 2;
  function Random return Integer is
  begin
    return Rnd.Int_Random (-Max, Max);
  end Random;

  function Image (I : Integer) return String is
  begin
    if I < 0 then
      return Integer_Image (I);
    else
      -- Add the '+'
      return '+' & Integer_Image (I);
    end if;
  end Image;

  function Image (B : Boolean) return String is
  begin
   return Mixed_Str (B'Img);
  end Image;

  procedure Check (
           A : in Arbitrary.Number;
           Oper : in String;
           B : in Arbitrary.Number;
           N : in Arbitrary.Number;
           I : in Integer) is
  begin
    if Arbitrary.Image (N) /= Image (I) then
      Basic_Proc.Put_Output ("ERROR on " &
        Arbitrary.Image (A) & " " & Oper & " " & Arbitrary.Image (B) & ". ");
      Basic_Proc.Put_Line_Output ("Expected " & Image (I)
             & ", got " & Arbitrary.Image (N) & ".");
      raise Abort_Error;
    end if;
  end Check;

  procedure Check (
           A : in Arbitrary.Number;
           Oper : in String;
           B : in Arbitrary.Number;
           Nb : in Boolean;
           Ib : in Boolean) is
  begin
    if Nb /= Ib then
      Basic_Proc.Put_Output ("ERROR on " &
        Arbitrary.Image (A) & " " & Oper & " " & Arbitrary.Image (B) & ". ");
      Basic_Proc.Put_Line_Output ("Expected " & Image (Ib)
             & ", got " & Image (Nb) & ".");
      raise Abort_Error;
    end if;
  end Check;

  A, B, C, D : Arbitrary.Number;

  Ia, Ib : Integer;
  Na, Nb : Arbitrary.Number;

  Nul : Arbitrary.Number;

  I : Integer;
  Ok : Boolean;
begin

  if Argument.Get_Nbre_Arg > 2 then
    Usage;
    return;
  end if;


  if Argument.Get_Nbre_Arg >= 1 then
    -- One or two args
    A := Set (1);

    Basic_Proc.Put_Line_Output ("A is        " & Arbitrary.Image(A));
    Basic_Proc.Put_Line_Output ("abs A is    " & Arbitrary.Image(abs A));
    Basic_Proc.Put_Line_Output ("-A is       " & Arbitrary.Image(-A));
    Basic_Proc.Put_Line_Output ("A positive  "
                              & Image(Arbitrary.Is_Positive(A)));
    begin
      Basic_Proc.Put_Line_Output ("Sqrt(A)     "
                                & Arbitrary.Image(Arbitrary.Sqrt(A)));
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Constraint_Error on Sqrt(A)");
    end;
    -- Digits of A
    Basic_Proc.Put_Output ("Digits: ");
    for I in 1 .. Arbitrary.Nb_Digits (A) - 1 loop
      Basic_Proc.Put_Output (Arbitrary.Nth_Digit (A, I)'Img);
    end loop;
    Basic_Proc.Put_Output (" and" & Arbitrary.Last_Digit (A)'Img);
    Basic_Proc.New_Line_Output;
  end if;

  if Argument.Get_Nbre_Arg = 2 then
    -- Two args
    Basic_Proc.New_Line_Output;
    B := Set (2);
    Basic_Proc.Put_Line_Output ("B is        " & Arbitrary.Image(B));
    Basic_Proc.Put_Line_Output ("abs B is    " & Arbitrary.Image(abs B));
    Basic_Proc.Put_Line_Output ("-B is       " & Arbitrary.Image(-B));
    Basic_Proc.Put_Line_Output ("B positive  " & Image(Arbitrary.Is_Positive(A)));
    begin
      Arbitrary.Sqrt(B, C, D);
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Constraint_Error on Sqrt(B)");
    end;
    Basic_Proc.Put_Line_Output ("Sqrt(B)     " & Arbitrary.Image(C)
                        & " remaining "  & Arbitrary.Image(D));

    Basic_Proc.Put_Line_Output ("A =  B is   " & Boolean'Image(A = B));
    Basic_Proc.Put_Line_Output ("A <  B is   " & Boolean'Image(A < B));
    Basic_Proc.Put_Line_Output ("A <= B is   " & Boolean'Image(A <= B));
    Basic_Proc.Put_Line_Output ("A >  B is   " & Boolean'Image(A > B));
    Basic_Proc.Put_Line_Output ("A >= B is   " & Boolean'Image(A >= B));

    Basic_Proc.Put_Line_Output ("A + B is   " &  Arbitrary.Image(A + B));
    Basic_Proc.Put_Line_Output ("A - B is   " &  Arbitrary.Image(A - B));
    Basic_Proc.Put_Line_Output ("A * B is   " &  Arbitrary.Image(A * B));
    begin
      Arbitrary.Div (A, B, C, D);
      Basic_Proc.Put_Line_Output ("A / B is   " &  Arbitrary.Image(C));
      Basic_Proc.Put_Line_Output ("A % B is   " &  Arbitrary.Image(D));
      Basic_Proc.Put_Line_Output ("A rem B is " &  Arbitrary.Image(A rem B));
      Basic_Proc.Put_Line_Output ("A mod B is " &  Arbitrary.Image(A mod B));
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Constraint_Error on division");
    end;
    begin
      Basic_Proc.Put_Line_Output ("A ** B is  " &  Arbitrary.Image(A ** B));
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Constraint_Error on **");
    end;
  end if;

  if Argument.Get_Nbre_Arg /= 0 then
    -- Done if any arg
    return;
  end if;

  -- No arg => automatic test
  -- Setting from different types
  A := Arbitrary.Set(Integer'(21));
  A := Arbitrary.Set(Long_Integer'(21));
  A := Arbitrary.Set(Long_Long_Integer'(21));

  -- Setting empty
  begin
    A := Arbitrary.Set("");
    Basic_Proc.Put_Line_Output ("ERROR: Set("""") should have raised "
                        & "Constraint_Error");
    raise Abort_Error;
  exception
    when Constraint_Error =>
      Basic_Proc.Put_Line_Output ("Set("""")  raises Constraint_Error, OK.");
  end;

  if Arbitrary.Is_Set (A) then
    Basic_Proc.Put_Line_Output ("A is set, OK.");
  else
    Basic_Proc.Put_Line_Output ("ERROR: A is not set.");
    raise Abort_Error;
  end if;

  if not Arbitrary.Is_Set (Nul) then
    Basic_Proc.Put_Line_Output ("Nul is not set, OK.");
  else
    Basic_Proc.Put_Line_Output ("ERROR: Nul is set.");
    raise Abort_Error;
  end if;
  delay 1.0;

  -- Loop of comparisons
  Rnd.Randomize;

  loop
    Ia := Random;
    Na := Arbitrary.Set (Ia);
    Ib := Random;
    if Ib > 0 and then abs Ib < 100 then
      Ib := 0;
    end if;
    Nb := Arbitrary.Set (Ib);
    Basic_Proc.Put_Line_Output (Arbitrary.Image (Na)
              & " and " & Arbitrary.Image (Nb));

    -- Unary operators
    Check (Nul, "abs", Na, abs Na, abs Ia);
    Check (Nul, "-", Na, -Na, -Ia);

    -- Comparisons
    Check (Na, "=",  Nb, Na = Nb,  Ia = Ib);
    Check (Na, "<",  Nb, Na < Nb,  Ia < Ib);
    Check (Na, "<=", Nb, Na <= Nb, Ia <= Ib);
    Check (Na, ">",  Nb, Na > Nb,  Ia > Ib);
    Check (Na, ">=", Nb, Na >= Nb, Ia >= Ib);

    -- Basic operations
    Check (Na, "+",  Nb, Na + Nb,  Ia + Ib);
    Check (Na, "-",  Nb, Na - Nb,  Ia - Ib);
    Check (Na, "*",  Nb, Na * Nb,  Ia * Ib);

    -- Basic operations where B /= 0
    if Ib /= 0 then
      Check (Na, "/",  Nb, Na / Nb,  Ia / Ib);
      Check (Na, "rem",  Nb, Na rem Nb,  Ia rem Ib);
      Check (Na, "mod",  Nb, Na mod Nb,  Ia mod Ib);
    else
      -- Shall raise Constraint_Error
      begin
        A := Na / Nb;
        Basic_Proc.Put_Line_Output ("ERROR: / should have raised Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
      begin
        A := Na rem Nb;
        Basic_Proc.Put_Line_Output ("ERROR: rem should have raised Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
      begin
        A := Na mod Nb;
        Basic_Proc.Put_Line_Output ("ERROR: mod should have raised Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
    end if;

    -- Pow
    begin
      I := Ia ** Ib;
      Ok := True;
    exception
      when Constraint_Error =>
        -- Integers too big
        Ok := False;
    end;
    if Ok then
      Check (Na, "**",  Nb, Na ** Nb,  I);
    end if;

    -- Sqrt
    if Ib >= 0 then
      I := Integer(My_Math.Trunc (My_Math.Sqrt (My_Math.Real(Ib))));
      Check (Nul, "Sqrt", Nb, Arbitrary.Sqrt (Nb), I);
    else
      begin
        A := Arbitrary.Sqrt (Nb);
        Basic_Proc.Put_Line_Output ("ERROR: Sqrt should have raised " &
                              "Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
    end if;

    -- Sleep a bit when B is 0
    if Ib = 0 then
      Basic_Proc.Put_Line_Output ("Waiting a bit");
      delay 1.0;
    end if;
  end loop;


exception
  when Abort_Error =>
    null;
end T_Arbitrary;

