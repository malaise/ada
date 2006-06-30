with Ada.Text_Io;
with Argument, Arbitrary;
procedure T_Arbitrary is

  A, B, C, D : Arbitrary.Number;

  Abort_Error : exception;
  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                                    & " <num1> <num2>");
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

begin

  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  A := Arbitrary.Set(Integer'(21));
  A := Arbitrary.Set(Long_Integer'(21));
  A := Arbitrary.Set(Long_Long_Integer'(21));

  A := Set (1);
  B := Set (2);

  declare
    use type Arbitrary.Number;
  begin
    Ada.Text_Io.Put_Line ("A is        " & Arbitrary.Image(A));
    Ada.Text_Io.Put_Line ("abs A is    " & Arbitrary.Image(abs A));
    Ada.Text_Io.Put_Line ("-A is       " & Arbitrary.Image(-A));

    Ada.Text_Io.Put_Line ("B is        " & Arbitrary.Image(B));
    Ada.Text_Io.Put_Line ("abs B is    " & Arbitrary.Image(abs B));
    Ada.Text_Io.Put_Line ("-B is       " & Arbitrary.Image(-B));

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
  end;

  begin
    A := Arbitrary.Set("");
    Ada.Text_Io.Put_Line ("Set("""") SHOULD HAVE RAISED Constraint_Error");
  exception
    when Constraint_Error =>
      Ada.Text_Io.Put_Line ("Set("""")  raises Constraint_Error, OK.");
  end;

exception
  when Abort_Error =>
    null;
end T_Arbitrary;

