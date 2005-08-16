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

  A := Set (1);
  B := Set (2);

  declare
    use type Arbitrary.Number;
  begin
    Ada.Text_Io.Put_Line ("A is     " & Arbitrary.Image(A));
    Ada.Text_Io.Put_Line ("abs A is " & Arbitrary.Image(abs A));
    Ada.Text_Io.Put_Line ("-A is    " & Arbitrary.Image(-A));

    Ada.Text_Io.Put_Line ("B is     " & Arbitrary.Image(B));
    Ada.Text_Io.Put_Line ("abs B is " & Arbitrary.Image(abs B));
    Ada.Text_Io.Put_Line ("-B is    " & Arbitrary.Image(-B));

    Ada.Text_Io.Put_Line ("A =  B is  " & Boolean'Image(A = B));
    Ada.Text_Io.Put_Line ("A <  B is  " & Boolean'Image(A < B));
    Ada.Text_Io.Put_Line ("A <= B is  " & Boolean'Image(A <= B));
    Ada.Text_Io.Put_Line ("A >  B is  " & Boolean'Image(A > B));
    Ada.Text_Io.Put_Line ("A >= B is  " & Boolean'Image(A >= B));

    Ada.Text_Io.Put_Line ("A + B is  " &  Arbitrary.Image(A + B));
    Ada.Text_Io.Put_Line ("A - B is  " &  Arbitrary.Image(A - B));
  end;
    
exception
  when Abort_Error =>
    null;
end T_Arbitrary;

