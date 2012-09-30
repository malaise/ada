-- Purpose: solve a.x2 + b.x + c = 0
with Argument, Basic_Proc, My_Math, Get_Float;
use My_Math;
procedure Second is

  A, B, C : My_Math.Real;

  -- Discriminant
  D : My_Math.Real;

  package Real_Io renames My_Math.Real_Io;

begin

  -- Check number of arguments
  if Argument.Get_Nbre_Arg /= 3 then
    Basic_Proc.Put_Line_Output ("ERROR. 3 arguments expected, a, b and c.");
    return;
  end if;

  -- Parse arguments
  begin
    A := My_Math.Real (Get_Float.Get_Float (Argument.Get_Parameter(1)));
    B := My_Math.Real (Get_Float.Get_Float (Argument.Get_Parameter(2)));
    C := My_Math.Real (Get_Float.Get_Float (Argument.Get_Parameter(3)));
  exception
    when others =>
      Basic_Proc.Put_Line_Output ("ERROR in an argument. 3 arguments expected, a, b and c.");
      return;
  end;

  Real_Io.Put (A); Basic_Proc.Put_Output (" * x2 + ");
  Real_Io.Put (B); Basic_Proc.Put_Output (" * x + ");
  Real_Io.Put (C); Basic_Proc.Put_Output (" = 0");
  Basic_Proc.New_Line_Output;

  -- A = 0 ?
  if A = 0.0 then
    -- First degree
    if B = 0.0 then
      if C = 0.0 then
        Basic_Proc.Put_Line_Output ("Inifinity of solutions.");
      else
        Basic_Proc.Put_Line_Output ("No solution.");
      end if;
    else
      Basic_Proc.Put_Output ("One solution: ");
      Real_Io.Put (-C/B);
        Basic_Proc.New_Line_Output;
    end if;
  else
    -- A /= 0
    -- Compute discriminant, see if it is >= 0
    D := B * B - 4.0 * A * C;
    if D >= 0.0 then
      Basic_Proc.Put_Output ("Two real solutions: ");
      Real_Io.Put ((-B + My_Math.Sqrt(D)) / (2.0 * A));
      Basic_Proc.Put_Output (" and ");
      Real_Io.Put ((-B - My_Math.Sqrt(D)) / (2.0 * A));
      Basic_Proc.New_Line_Output;
    else
      Basic_Proc.Put_Line_Output ("Two complex solutions: ");
      Real_Io.Put ((-B) / (2.0 * A) );
      Basic_Proc.Put_Output (" +/-");
      Real_Io.Put (My_Math.Sqrt(-D) / (2.0 * A));
      Basic_Proc.Put_Output (" * i ");
      Basic_Proc.New_Line_Output;
    end if;
  end if;

end Second;

