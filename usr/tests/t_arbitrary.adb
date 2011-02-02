with Ada.Text_Io;
with Argument, Arbitrary, Rnd, Integer_Image, Mixed_Str, My_Math;
procedure T_Arbitrary is


  Abort_Error : exception;
  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                                    & " [ <num1> [ <num2> ] ]");
    raise Abort_Error;
  end Usage;

  function Set (Occ : Positive) return Arbitrary.Number is
  begin
    return Arbitrary.Set (Argument.Get_Parameter(Occurence => Occ));
  exception
    when others =>
      Ada.Text_Io.Put_Line ("Invalid_Number " & Argument.Get_Parameter(Occurence => Occ));
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
      Ada.Text_Io.Put ("ERROR on " &
        Arbitrary.Image (A) & " " & Oper & " " & Arbitrary.Image (B) & ". ");
      Ada.Text_Io.Put_Line ("Expected " & Image (I)
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
      Ada.Text_Io.Put ("ERROR on " &
        Arbitrary.Image (A) & " " & Oper & " " & Arbitrary.Image (B) & ". ");
      Ada.Text_Io.Put_Line ("Expected " & Image (Ib)
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

    Ada.Text_Io.Put_Line ("A is        " & Arbitrary.Image(A));
    Ada.Text_Io.Put_Line ("abs A is    " & Arbitrary.Image(abs A));
    Ada.Text_Io.Put_Line ("-A is       " & Arbitrary.Image(-A));
    Ada.Text_Io.Put_Line ("A positive  " & Image(Arbitrary.Is_Positive(A)));
    begin
      Ada.Text_Io.Put_Line ("Sqrt(A)     " & Arbitrary.Image(Arbitrary.Sqrt(A)));
    exception
      when Constraint_Error =>
        Ada.Text_Io.Put_Line ("Constraint_Error on Sqrt(A)");
    end;
    -- Digits of A
    Ada.Text_Io.Put ("Digits: ");
    for I in 1 .. Arbitrary.Nb_Digits (A) - 1 loop
      Ada.Text_Io.Put (Arbitrary.Nth_Digit (A, I)'Img);
    end loop;
    Ada.Text_Io.Put (" and" & Arbitrary.Last_Digit (A)'Img);
    Ada.Text_Io.New_Line;
  end if;

  if Argument.Get_Nbre_Arg = 2 then
    -- Two args
    Ada.Text_Io.New_Line;
    B := Set (2);
    Ada.Text_Io.Put_Line ("B is        " & Arbitrary.Image(B));
    Ada.Text_Io.Put_Line ("abs B is    " & Arbitrary.Image(abs B));
    Ada.Text_Io.Put_Line ("-B is       " & Arbitrary.Image(-B));
    Ada.Text_Io.Put_Line ("B positive  " & Image(Arbitrary.Is_Positive(A)));
    begin
      Arbitrary.Sqrt(B, C, D);
    exception
      when Constraint_Error =>
        Ada.Text_Io.Put_Line ("Constraint_Error on Sqrt(B)");
    end;
    Ada.Text_Io.Put_Line ("Sqrt(B)     " & Arbitrary.Image(C)
                        & " remaining "  & Arbitrary.Image(D));

    Ada.Text_Io.Put_Line ("A =  B is   " & Boolean'Image(A = B));
    Ada.Text_Io.Put_Line ("A <  B is   " & Boolean'Image(A < B));
    Ada.Text_Io.Put_Line ("A <= B is   " & Boolean'Image(A <= B));
    Ada.Text_Io.Put_Line ("A >  B is   " & Boolean'Image(A > B));
    Ada.Text_Io.Put_Line ("A >= B is   " & Boolean'Image(A >= B));

    Ada.Text_Io.Put_Line ("A + B is   " &  Arbitrary.Image(A + B));
    Ada.Text_Io.Put_Line ("A - B is   " &  Arbitrary.Image(A - B));
    Ada.Text_Io.Put_Line ("A * B is   " &  Arbitrary.Image(A * B));
    begin
      Arbitrary.Div (A, B, C, D);
      Ada.Text_Io.Put_Line ("A / B is   " &  Arbitrary.Image(C));
      Ada.Text_Io.Put_Line ("A % B is   " &  Arbitrary.Image(D));
      Ada.Text_Io.Put_Line ("A rem B is " &  Arbitrary.Image(A rem B));
      Ada.Text_Io.Put_Line ("A mod B is " &  Arbitrary.Image(A mod B));
    exception
      when Constraint_Error =>
        Ada.Text_Io.Put_Line ("Constraint_Error on division");
    end;
    begin
      Ada.Text_Io.Put_Line ("A ** B is  " &  Arbitrary.Image(A ** B));
    exception
      when Constraint_Error =>
        Ada.Text_Io.Put_Line ("Constraint_Error on **");
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
    Ada.Text_Io.Put_Line ("ERROR: Set("""") should have raised "
                        & "Constraint_Error");
    raise Abort_Error;
  exception
    when Constraint_Error =>
      Ada.Text_Io.Put_Line ("Set("""")  raises Constraint_Error, OK.");
  end;

  if Arbitrary.Is_Set (A) then
    Ada.Text_Io.Put_Line ("A is set, OK.");
  else
    Ada.Text_Io.Put_Line ("ERROR: A is not set.");
    raise Abort_Error;
  end if;

  if not Arbitrary.Is_Set (Nul) then
    Ada.Text_Io.Put_Line ("Nul is not set, OK.");
  else
    Ada.Text_Io.Put_Line ("ERROR: Nul is set.");
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
    Ada.Text_Io.Put_Line (Arbitrary.Image (Na)
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
        Ada.Text_Io.Put_Line ("ERROR: / should have raised Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
      begin
        A := Na rem Nb;
        Ada.Text_Io.Put_Line ("ERROR: rem should have raised Constraint_Error");
        raise Abort_Error;
      exception
        when Constraint_Error =>
          -- OK
          null;
      end;
      begin
        A := Na mod Nb;
        Ada.Text_Io.Put_Line ("ERROR: mod should have raised Constraint_Error");
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
        Ada.Text_Io.Put_Line ("ERROR: Sqrt should have raised " &
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
      Ada.Text_Io.Put_Line ("Waiting a bit");
      delay 1.0;
    end if;
  end loop;


exception
  when Abort_Error =>
    null;
end T_Arbitrary;

