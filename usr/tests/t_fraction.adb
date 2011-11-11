with Basic_Proc, Argument, Arbitrary.Fractions, String_Mng;
procedure T_Fraction is

  subtype Fraction is Arbitrary.Fractions.Fraction;
  A, B : Fraction;

  Abort_Error : exception;
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                                    & " <frac1> <frac2>");
    Basic_Proc.Put_Line_Output ("<frac> ::= <num>:<denom>");
    raise Abort_Error;
  end Usage;

  function Set (Occ : Positive) return Fraction is
    N, D : Arbitrary.Number;
  begin
    declare
      Str : constant String := Argument.Get_Parameter(Occurence => Occ);
      I : constant Natural := String_Mng.Locate (Str, ":");
    begin
      N := Arbitrary.Set (Str(1 .. I - 1));
      D := Arbitrary.Set (Str(I+1 .. Str'Last));
    end;
    return Arbitrary.Fractions.Set (N, D);
  exception
    when others =>
      Basic_Proc.Put_Line_Output ("Invalid_Number " & Argument.Get_Parameter(Occurence => Occ));
      Usage;
      raise Abort_Error;
  end Set;

  use type Arbitrary.Fractions.Fraction;
begin

  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  A := Set (1);
  B := Set (2);

  Basic_Proc.Put_Line_Output ("A is        " & Arbitrary.Fractions.Image(A));
  Basic_Proc.Put_Line_Output ("abs A is    " & Arbitrary.Fractions.Image(abs A));
  Basic_Proc.Put_Line_Output ("-A is       " & Arbitrary.Fractions.Image(-A));

  Basic_Proc.Put_Line_Output ("B is        " & Arbitrary.Fractions.Image(B));
  Basic_Proc.Put_Line_Output ("abs B is    " & Arbitrary.Fractions.Image(abs B));
  Basic_Proc.Put_Line_Output ("-B is       " & Arbitrary.Fractions.Image(-B));

  Basic_Proc.Put_Line_Output ("A =  B is   " & Boolean'Image(A = B));
  Basic_Proc.Put_Line_Output ("A <  B is   " & Boolean'Image(A < B));
  Basic_Proc.Put_Line_Output ("A <= B is   " & Boolean'Image(A <= B));
  Basic_Proc.Put_Line_Output ("A >  B is   " & Boolean'Image(A > B));
  Basic_Proc.Put_Line_Output ("A >= B is   " & Boolean'Image(A >= B));

  Basic_Proc.Put_Line_Output ("A + B is   " & Arbitrary.Fractions.Image(A + B));
  Basic_Proc.Put_Line_Output ("A - B is   " & Arbitrary.Fractions.Image(A - B));
  Basic_Proc.Put_Line_Output ("A * B is   " & Arbitrary.Fractions.Image(A * B));
  Basic_Proc.Put_Line_Output ("A / B is   " & Arbitrary.Fractions.Image(A / B));
  Basic_Proc.Put_Line_Output ("A ** B.Num is "
    & Arbitrary.Fractions.Image(A ** Arbitrary.Fractions.Numerator (B)));

  begin
    A := Arbitrary.Fractions.Set(Arbitrary.One, Arbitrary.Zero);
    Basic_Proc.Put_Line_Output ("Set (1,0) SHOULD HAVE RAISED Constraint_Error");
  exception
    when Constraint_Error =>
      Basic_Proc.Put_Line_Output ("Set (1,0) raises Constraint_Error, OK.");
  end;

exception
  when Abort_Error =>
    null;
end T_Fraction;

