-- Purpose: solve a.x2 + b.x + c = 0
with TEXT_IO;
with ARGUMENT, MY_IO, MY_MATH, GET_FLOAT;
use MY_MATH;
procedure SECOND is

  A, B, C : MY_MATH.REAL;

  -- Discriminant
  D : MY_MATH.REAL;

  package REAL_IO is new TEXT_IO.FLOAT_IO (MY_MATH.REAL);
  
begin

  -- Check number of arguments
  if ARGUMENT.GET_NBRE_ARG /= 3 then
    MY_IO.PUT_LINE ("ERROR. 3 arguments expected, a, b and c.");
    return;
  end if;

  -- Parse arguments
  begin
    A := MY_MATH.REAL (GET_FLOAT.GET_FLOAT (ARGUMENT.GET_PARAMETER(1)));
    B := MY_MATH.REAL (GET_FLOAT.GET_FLOAT (ARGUMENT.GET_PARAMETER(2)));
    C := MY_MATH.REAL (GET_FLOAT.GET_FLOAT (ARGUMENT.GET_PARAMETER(3)));
  exception
    when others =>
      MY_IO.PUT_LINE ("ERROR in an argument. 3 arguments expected, a, b and c.");
      return;
  end;

  REAL_IO.PUT(A); MY_IO.PUT(" * x2 + ");
  REAL_IO.PUT(B); MY_IO.PUT(" * x + ");
  REAL_IO.PUT(C); MY_IO.PUT(" = 0");
  MY_IO.NEW_LINE;

  -- A = 0 ?
  if A = 0.0 then
    if B = 0.0 then
      if C = 0.0 then
        MY_IO.PUT_LINE ("Inifinity of solutions.");
      else
        MY_IO.PUT_LINE ("No solution.");
      end if;
    else
      MY_IO.PUT ("One solution: ");
      REAL_IO.PUT (-C/B);
        MY_IO.NEW_LINE;
    end if;
  else
    D := B * B - 4.0 * A * C;
    if D >= 0.0 then
      MY_IO.PUT ("Two solutions: ");
      REAL_IO.PUT ((-B + MY_MATH.SQRT(D)) / (2.0 * A));
      MY_IO.PUT (" and ");
      REAL_IO.PUT ((-B - MY_MATH.SQRT(D)) / (2.0 * A));
      MY_IO.NEW_LINE;
    else
      MY_IO.PUT_LINE ("Two complex solutions: ");
      REAL_IO.PUT ((-B) / (2.0 * A) );
      MY_IO.PUT (" +/-");
      REAL_IO.PUT (MY_MATH.SQRT(-D) / (2.0 * A));
      MY_IO.PUT (" * i ");
      MY_IO.NEW_LINE;
    end if;
  end if;

end SECOND;

