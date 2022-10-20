with Argument, Arbitrary.Factors, Rnd, Int_Img, Mixed_Str,
     My_Math, Basic_Proc, Key_Pressed;
procedure T_Arbitrary is
  Abort_Error : exception;
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                                    & " [ <num1> [ <num2> ] ]");
    Basic_Proc.Put_Line_Output ("   or: " & Argument.Get_Program_Name
                                    & " -a [ <nb_loops> ]");
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
  function Random return Integer is (Rnd.Gen.Int_Random (-Max, Max));

  function Image (I : Integer) return String is
    (if I < 0 then Int_Img (I)
     -- Add the '+'
     else '+' & Int_Img (I));

  function Image (B : Boolean) return String is (Mixed_Str (B'Img));

  procedure Check (
           A : in Arbitrary.Number;
           Oper : in String;
           B : in Arbitrary.Number;
           N : in Arbitrary.Number;
           I : in Integer) is
  begin
    if N.Image /= Image (I) then
      Basic_Proc.Put_Output_Again ("ERROR on " &
        Arbitrary.Image (A) & " " & Oper & " " & B.Image & ". ");
      Basic_Proc.Put_Line_Output_Again ("Expected " & Image (I)
             & ", got " & N.Image & ".");
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
        A.Image & " " & Oper & " " & B.Image & ". ");
      Basic_Proc.Put_Line_Output ("Expected " & Image (Ib)
             & ", got " & Image (Nb) & ".");
      raise Abort_Error;
    end if;
  end Check;

  A, B, C, D : Arbitrary.Number;

  L : Arbitrary.Factors.Nb_List_Mng.List_Type;
  Moved : Boolean;

  Ia, Ib, Ic : Integer;
  Na, Nb : Arbitrary.Number;

  Nul : Arbitrary.Number;

  I, Nb_Loops : Natural;
  Ok : Boolean;

begin

  if Argument.Get_Nbre_Arg < 1
  or else Argument.Get_Nbre_Arg > 2 then
    Usage;
    return;
  end if;


  if Argument.Get_Parameter(1) /= "-a" then
    -- One or two args
    A := Set (1);

    Basic_Proc.Put_Line_Output ("A is        " & A.Image);
    Basic_Proc.Put_Line_Output ("abs A is    " & Arbitrary.Image(abs A));
    Basic_Proc.Put_Line_Output ("-A is       " & Arbitrary.Image(-A));
    Basic_Proc.Put_Line_Output ("Incr (A) is " & Arbitrary.Image(A.Incr));
    Basic_Proc.Put_Line_Output ("Decr (A) is " & Arbitrary.Image(A.Decr));
    Basic_Proc.Put_Line_Output ("A positive  "
                              & Image(Arbitrary.Is_Positive(A)));
    begin
      Basic_Proc.Put_Line_Output ("Sqrt(A)     "
                                & A.Sqrt.Image);
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Constraint_Error on Sqrt(A)");
    end;
    -- Digits of A
    Basic_Proc.Put_Output ("Digits:");
    for I in 1 .. Arbitrary.Nb_Digits (A) - 1 loop
      Basic_Proc.Put_Output (Arbitrary.Nth_Digit (A, I)'Img);
    end loop;
    if Arbitrary.Nb_Digits (A) /= 1 then
      Basic_Proc.Put_Output (" and");
    end if;
    Basic_Proc.Put_Output (Arbitrary.Last_Digit (A)'Img);
    Basic_Proc.New_Line_Output;

    if Argument.Get_Nbre_Arg = 2 then
      -- Two args
      Basic_Proc.New_Line_Output;
      B := Set (2);
      Basic_Proc.Put_Line_Output ("B is        " & Arbitrary.Image(B));
      Basic_Proc.Put_Line_Output ("abs B is    " & Arbitrary.Image(abs B));
      Basic_Proc.Put_Line_Output ("-B is       " & Arbitrary.Image(-B));
      Basic_Proc.Put_Line_Output ("Incr (B) is " & Arbitrary.Image(B.Incr));
      Basic_Proc.Put_Line_Output ("Decr (B) is " & Arbitrary.Image(B.Decr));
      Basic_Proc.Put_Line_Output ("B positive  "
                                & Image(Arbitrary.Is_Positive(A)));
      begin
        Arbitrary.Sqrt(B, C, D);
      exception
        when Constraint_Error =>
          Basic_Proc.Put_Line_Output ("Constraint_Error on Sqrt(B)");
      end;
      Basic_Proc.Put_Line_Output ("Sqrt(B)     " & Arbitrary.Image(C)
                          & " remaining "  & Arbitrary.Image(D));
      begin
        Arbitrary.Factors.Decompose (B, L);
        Basic_Proc.Put_Output ("Prime factors: ");
        loop
          L.Read (C, Moved => Moved);
          if Moved then
            Basic_Proc.Put_Output (" ");
          elsif L.List_Length > Arbitrary.One then
            Basic_Proc.Put_Output (" and " );
          end if;
          Basic_Proc.Put_Output (Arbitrary.Image (C));
          exit when not Moved;
        end loop;
        Basic_Proc.New_Line_Output;
      exception
        when Constraint_Error =>
          Basic_Proc.Put_Line_Output ("Constraint_Error on decomposition " &
                    "into prime factors");
      end;

      Basic_Proc.New_Line_Output;

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
        Basic_Proc.Put_Line_Output ("Roundiv(A, B) is "
                                 &  Arbitrary.Image (Arbitrary.Roundiv (A, B)));
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

    -- Done if 1st arg is not -a
    return;
  end if;

  -- -a [ <nb_loop> ] => automatic test
  Nb_Loops := 0;
  if Argument.Get_Nbre_Arg = 2 then
    begin
      Nb_Loops := Positive'Value (Argument.Get_Parameter(2));
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Invalid value for nb_loops");
        Basic_Proc.Set_Error_Exit_Code;
        return;
    end;
  end if;
  Key_Pressed.Open (False);

  -- Setting from different types
  A.Set(Integer'(21));
  A.Set(Long_Integer'(21));
  A.Set(Long_Long_Integer'(21));

  -- Setting empty
  begin
    A.Set("");
    Basic_Proc.Put_Line_Output_Again ("ERROR: Set("""") should have raised "
                        & "Constraint_Error");
    raise Abort_Error;
  exception
    when Constraint_Error =>
      Basic_Proc.Put_Line_Output ("Set("""")  raises Constraint_Error, OK.");
  end;

  if A.Is_Set then
    Basic_Proc.Put_Line_Output_Again ("A is set, OK.");
  else
    Basic_Proc.Put_Line_Output_Again ("ERROR: A is not set.");
    raise Abort_Error;
  end if;

  if not Nul.Is_Set then
    Basic_Proc.Put_Line_Output_Again ("Nul is not set, OK.");
  else
    Basic_Proc.Put_Line_Output_Again ("ERROR: Nul is set.");
    raise Abort_Error;
  end if;
  delay 1.0;

  -- Loop of comparisons
  Rnd.Gen.Randomize;
  I := 0;

  loop
    Ia := Random;
    Na.Set (Ia);
    Ib := Random;
    if Ib > 0 and then abs Ib < 100 then
      Ib := 0;
    end if;
    Nb.Set (Ib);
    Basic_Proc.Put_Line_Output_Again (Na.Image & " and " & Nb.Image);

    -- Unary operators
    Check (Nul, "abs", Na, abs Na, abs Ia);
    Check (Nul, "-", Na, -Na, -Ia);
    Check (Nul, "Incr",  Na, Na.Incr,  Ia + 1);
    Check (Nul, "Decr",  Na, Na.Decr,  Ia - 1);

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
      Check (Na, "roundiv",  Nb, Arbitrary.Roundiv (Na, Nb),
             Integer (My_Math.Roundiv (My_Math.Inte(Ia),
                                       My_Math.Inte(Ib))));
      Check (Na, "rem",  Nb, Na rem Nb,  Ia rem Ib);
      Check (Na, "mod",  Nb, Na mod Nb,  Ia mod Ib);
    else
      -- Shall raise Constraint_Error
      begin
        A := Na / Nb;
        Basic_Proc.Put_Line_Output_Again
            ("ERROR: / should have raised Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
      begin
        A := Na rem Nb;
        Basic_Proc.Put_Line_Output_Again
            ("ERROR: rem should have raised Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
      begin
        A := Na mod Nb;
        Basic_Proc.Put_Line_Output_Again
            ("ERROR: mod should have raised Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
    end if;

    -- Pow
    begin
     Ic := Ia ** Ib;
      Ok := True;
    exception
      when Constraint_Error =>
        -- Integer too big
        Ok := False;
    end;
    if Ok then
      Check (Na, "**",  Nb, Na ** Nb,  Ic);
    end if;

    -- Sqrt
    if Ib >= 0 then
      Ic := Integer(My_Math.Trunc (My_Math.Sqrt (My_Math.Real(Ib))));
      Check (Nul, "Sqrt", Nb, Nb.Sqrt, Ic);
    else
      begin
        A := Nb.Sqrt;
        Basic_Proc.Put_Line_Output_Again
            ("ERROR: Sqrt should have raised " & "Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
    end if;

    -- Prime factors
    L.Delete_List (False);
    if Ia > 1 and then Ia <= 2121 then
      Arbitrary.Factors.Decompose (Na, L);
      C := Arbitrary.One;
      loop
        L.Read (D, Moved => Moved);
        C := C * D;
        exit when not Moved;
      end loop;
      if C /= Na then
        Basic_Proc.Put_Line_Output_Again
            ("ERROR: Prime decomposition of " &
             Na.Image & " does not mutiply to itself");
        raise Abort_Error;
      end if;
    elsif Ia <= 1 then
      begin
        Arbitrary.Factors.Decompose (Na, L);
        Basic_Proc.Put_Line_Output_Again
            ("ERROR: decomposition into prime factors " &
             "should have raised Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
    end if;

    exit when Key_Pressed.Key_Pressed;
    -- Sleep a bit
    if Nb_Loops = 0 and then I /= 0 and then I mod 1000 = 0 then
      Basic_Proc.Put_Line_Output_Again ("Waiting a bit, hit a key to stop...");
      delay 1.0;
      exit when Key_Pressed.Key_Pressed;
    end if;
    I := I + 1;
    exit when I = Nb_Loops;
  end loop;

  Key_Pressed.Close;

exception
  when Abort_Error =>
    Key_Pressed.Close;
    Basic_Proc.Set_Error_Exit_Code;
  when others =>
    Key_Pressed.Close;
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end T_Arbitrary;

