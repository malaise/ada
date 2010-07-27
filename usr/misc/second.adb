-- Purpose: solve a.x2 + b.x + c = 0
with Argument, My_Io, My_Math, Get_Float;
use My_Math;
procedure Second is

  A, B, C : My_Math.Real;

  -- Discriminant
  D : My_Math.Real;

  package Real_Io renames My_Math.Real_Io;

begin

  -- Check number of arguments
  if Argument.Get_Nbre_Arg /= 3 then
    My_Io.Put_Line ("ERROR. 3 arguments expected, a, b and c.");
    return;
  end if;

  -- Parse arguments
  begin
    A := My_Math.Real (Get_Float.Get_Float (Argument.Get_Parameter(1)));
    B := My_Math.Real (Get_Float.Get_Float (Argument.Get_Parameter(2)));
    C := My_Math.Real (Get_Float.Get_Float (Argument.Get_Parameter(3)));
  exception
    when others =>
      My_Io.Put_Line ("ERROR in an argument. 3 arguments expected, a, b and c.");
      return;
  end;

  Real_Io.Put(A); My_Io.Put(" * x2 + ");
  Real_Io.Put(B); My_Io.Put(" * x + ");
  Real_Io.Put(C); My_Io.Put(" = 0");
  My_Io.New_Line;

  -- A = 0 ?
  if A = 0.0 then
    if B = 0.0 then
      if C = 0.0 then
        My_Io.Put_Line ("Inifinity of solutions.");
      else
        My_Io.Put_Line ("No solution.");
      end if;
    else
      My_Io.Put ("One solution: ");
      Real_Io.Put (-C/B);
        My_Io.New_Line;
    end if;
  else
    D := B * B - 4.0 * A * C;
    if D >= 0.0 then
      My_Io.Put ("Two solutions: ");
      Real_Io.Put ((-B + My_Math.Sqrt(D)) / (2.0 * A));
      My_Io.Put (" and ");
      Real_Io.Put ((-B - My_Math.Sqrt(D)) / (2.0 * A));
      My_Io.New_Line;
    else
      My_Io.Put_Line ("Two complex solutions: ");
      Real_Io.Put ((-B) / (2.0 * A) );
      My_Io.Put (" +/-");
      Real_Io.Put (My_Math.Sqrt(-D) / (2.0 * A));
      My_Io.Put (" * i ");
      My_Io.New_Line;
    end if;
  end if;

end Second;

