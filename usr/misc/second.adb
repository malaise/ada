-- Purpose: solve a.x2 + b.x + c = 0
with Argument, Basic_Proc, My_Math, Gets, Images;
use My_Math;
procedure Second is

  function Real_Image is new Images.Flo_Image (My_Math.Real);

  A, B, C : My_Math.Real;

  -- Discriminant
  D : My_Math.Real;

begin

  -- Check number of arguments
  if Argument.Get_Nbre_Arg /= 3 then
    Basic_Proc.Put_Line_Output ("ERROR. 3 arguments expected, a, b and c.");
    return;
  end if;

  -- Parse arguments
  begin
    A := My_Math.Real (Gets.Get_Float (Argument.Get_Parameter(1)));
    B := My_Math.Real (Gets.Get_Float (Argument.Get_Parameter(2)));
    C := My_Math.Real (Gets.Get_Float (Argument.Get_Parameter(3)));
  exception
    when others =>
      Basic_Proc.Put_Line_Output (
         "ERROR in an argument. 3 Float arguments expected, a, b and c.");
      return;
  end;

  Basic_Proc.Put_Output (Real_Image (A)); Basic_Proc.Put_Output (" * x2 + ");
  Basic_Proc.Put_Output (Real_Image (B)); Basic_Proc.Put_Output (" * x + ");
  Basic_Proc.Put_Output (Real_Image (C)); Basic_Proc.Put_Output (" = 0");
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
      Basic_Proc.Put_Output (Real_Image (-C/B) );
      Basic_Proc.New_Line_Output;
    end if;
  else
    -- A /= 0
    -- Compute discriminant, see if it is >= 0
    D := B * B - 4.0 * A * C;
    if D >= 0.0 then
      Basic_Proc.Put_Output ("Two real solutions: ");
      Basic_Proc.Put_Output (Real_Image ((-B + My_Math.Sqrt(D)) / (2.0 * A) ));
      Basic_Proc.Put_Output (" and ");
      Basic_Proc.Put_Output (Real_Image ((-B - My_Math.Sqrt(D)) / (2.0 * A) ));
      Basic_Proc.New_Line_Output;
    else
      Basic_Proc.Put_Line_Output ("Two complex solutions: ");
      Basic_Proc.Put_Output (Real_Image ((-B) / (2.0 * A) ));
      Basic_Proc.Put_Output (" +/-");
      Basic_Proc.Put_Output (Real_Image (My_Math.Sqrt(-D) / (2.0 * A) ));
      Basic_Proc.Put_Output (" * i ");
      Basic_Proc.New_Line_Output;
    end if;
  end if;

end Second;

