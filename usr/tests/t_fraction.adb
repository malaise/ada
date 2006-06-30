with Ada.Text_Io;
with Argument, Arbitrary.Fractions, String_Mng;
procedure T_Fraction is

  subtype Fraction is Arbitrary.Fractions.Fraction;
  A, B, C, D : Fraction;

  Abort_Error : exception;
  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                                    & " <frac1> <frac2>");
    Ada.Text_Io.Put_Line ("<frac> ::= <num>:<denom>");
    raise Abort_Error;
  end Usage;

  function Set (Occ : Positive) return Fraction is
    N, D : Arbitrary.Number;
  begin
    declare
      Str : constant String := Argument.Get_Parameter(Occurence => Occ);
      I : constant Natural := String_Mng.Locate (Str, 1, ":");
    begin
      N := Arbitrary.Set (Str(1 .. I - 1));
      D := Arbitrary.Set (Str(I+1 .. Str'Last));
    end;
    return Arbitrary.Fractions.Set (N, D);
  exception
    when others =>
      Ada.Text_Io.Put_Line ("Invalid_Number " & Argument.Get_Parameter(Occurence => Occ));
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

  Ada.Text_Io.Put_Line ("A is        " & Arbitrary.Fractions.Image(A));
  Ada.Text_Io.Put_Line ("abs A is    " & Arbitrary.Fractions.Image(abs A));
  Ada.Text_Io.Put_Line ("-A is       " & Arbitrary.Fractions.Image(-A));

  Ada.Text_Io.Put_Line ("B is        " & Arbitrary.Fractions.Image(B));
  Ada.Text_Io.Put_Line ("abs B is    " & Arbitrary.Fractions.Image(abs B));
  Ada.Text_Io.Put_Line ("-B is       " & Arbitrary.Fractions.Image(-B));

  Ada.Text_Io.Put_Line ("A =  B is   " & Boolean'Image(A = B));
  Ada.Text_Io.Put_Line ("A <  B is   " & Boolean'Image(A < B));
  Ada.Text_Io.Put_Line ("A <= B is   " & Boolean'Image(A <= B));
  Ada.Text_Io.Put_Line ("A >  B is   " & Boolean'Image(A > B));
  Ada.Text_Io.Put_Line ("A >= B is   " & Boolean'Image(A >= B));

  Ada.Text_Io.Put_Line ("A + B is   " & Arbitrary.Fractions.Image(A + B));
  Ada.Text_Io.Put_Line ("A - B is   " & Arbitrary.Fractions.Image(A - B));
  Ada.Text_Io.Put_Line ("A * B is   " & Arbitrary.Fractions.Image(A * B));
  Ada.Text_Io.Put_Line ("A / B is   " & Arbitrary.Fractions.Image(A / B));
  Ada.Text_Io.Put_Line ("A ** B.Num is "
    & Arbitrary.Fractions.Image(A ** Arbitrary.Fractions.Numerator (B)));

  begin
    A := Arbitrary.Fractions.Set(Arbitrary.One, Arbitrary.Zero);
    Ada.Text_Io.Put_Line ("Set (1,0) SHOULD HAVE RAISED Constraint_Error");
  exception
    when Constraint_Error =>
      Ada.Text_Io.Put_Line ("Set (1,0) raises Constraint_Error, OK.");
  end;

exception
  when Abort_Error =>
    null;
end T_Fraction;

