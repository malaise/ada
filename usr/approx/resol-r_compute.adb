with My_Math;
separate (Resol)
package body R_Compute is
  use My_Syslin;

  subtype Number is Points.P_T_Coordinate;

  -- Given the points array, build A matrix (n points, degree d).
  -- Xi is Points(i).X

  -- X1**0   X2**0   ... Xn-1**0   Xn**0
  -- X1**1   X2**1   ... Xn-1**1   Xn**1
  -- ...     ...     ... ...       ...
  -- X1**d-2 X2**d-2 ... Xn-1**d-2 Xn**d-2
  -- X1**d-1 X2**d-1 ... Xn-1**d-1 Xn**d-1

  -- Return At * A (At = transposed of A) and A * Y
  --  At * A is a square matrix d*d
  --  A * Y is a vector d
  procedure Do_Matrixes (The_Points : in Points.P_T_The_Points;
    T_A_A : out Matrix; A_Y : out Vector) is
    subtype Index_Point is Positive range The_Points'Range;

    subtype Index_Degree is Positive range 1..The_Degree;
    -- Allocate on heap
    type Mattrix_Access is access  R_Compute.Matrix;
    A_Loc : constant Mattrix_Access := new Matrix (Index_Degree, Index_Point);
    use My_Math;
  begin
    -- Build local A matrix
    for Column in Index_Point loop
      A_Loc (Index_Degree'First, Column) := 1.0;
    end loop;
    for Row in Index_Degree range
     Index_Degree'Succ(Index_Degree'First) .. Index_Degree'Last loop
      for Column in Index_Point loop
        A_Loc (Row, Column) :=
         A_Loc (Index_Degree'Pred(Row), Column) * The_Points(Column).X;
      end loop;
    end loop;

    -- Build At * A mattrix : T_A_A (R,C) = Sum[K=1..n] (A(R,K)*A(C,K))
    for Row_A in Index_Degree loop
      for Column_Ta in Index_Degree loop
        declare
          Bubble : Number := 0.0;
        begin
          for Column_A_Row_Ta in Index_Point loop
            Bubble := Bubble + A_Loc (Row_A, Column_A_Row_Ta)
            * A_Loc (Column_Ta, Column_A_Row_Ta);
          end loop;
          T_A_A (Row_A, Column_Ta) := Bubble;
        end;
      end loop;
    end loop;

    -- Build Y * Y vector : A_Y (I) = Sum[K=1..n] A(I,K)*Y(K)
    -- Y(K) is Points(K).Y
    for Row_A in Index_Degree loop
      declare
        Bubble : Number := 0.0;
      begin
        for Column_A in Index_Point loop
          Bubble := Bubble + A_Loc (Row_A, Column_A)
          * The_Points (Column_A).Y;
        end loop;
        A_Y (Row_A) := Bubble;
      end;
    end loop;

  end Do_Matrixes;

  -- Use my_syslin to solve A*X=B
  function System (A : Matrix; B : Vector) return Vector is
  begin
    return My_Syslin.Gauss (A,B);
  exception
    when others => raise R_Resol_Error;
  end System;

end R_Compute;
