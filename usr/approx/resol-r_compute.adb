separate (RESOL)
package body R_COMPUTE is
  use MY_SYSLIN;

  subtype NUMBER is POINTS.P_T_COORDINATE;

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
  procedure DO_MATRIXES (THE_POINTS : in POINTS.P_T_THE_POINTS;
    T_A_A : out MATRIX; A_Y : out VECTOR) is
    subtype INDEX_POINT is POSITIVE range THE_POINTS'RANGE;

    subtype INDEX_DEGREE is POSITIVE range 1..THE_DEGREE;
    A_LOC : MATRIX (INDEX_DEGREE, INDEX_POINT);
    T_A_A_LOC : MATRIX (INDEX_DEGREE, INDEX_DEGREE);
    A_Y_LOC : VECTOR (INDEX_DEGREE);
  begin
    -- Build local A matrix
    for COLUMN in INDEX_POINT loop
      A_LOC (INDEX_DEGREE'FIRST, COLUMN) := 1.0;
    end loop;
    for ROW in INDEX_DEGREE range
     INDEX_DEGREE'SUCC(INDEX_DEGREE'FIRST) .. INDEX_DEGREE'LAST loop
      for COLUMN in INDEX_POINT loop
        A_LOC (ROW, COLUMN) :=
         A_LOC (INDEX_DEGREE'PRED(ROW), COLUMN) * THE_POINTS(COLUMN).X;
      end loop;
    end loop;

    -- Build At * A mattrix : T_A_A (R,C) = SUM[K=1..n] (A(R,K)*A(C,K))
    for ROW_A in INDEX_DEGREE loop
      for COLUMN_TA in INDEX_DEGREE loop
        declare
          BUBBLE :NUMBER := 0.0;
        begin
          for COLUMN_A_ROW_TA in INDEX_POINT loop
            BUBBLE := BUBBLE + A_LOC (ROW_A, COLUMN_A_ROW_TA)
            * A_LOC (COLUMN_TA, COLUMN_A_ROW_TA);
          end loop;
          T_A_A_LOC (ROW_A, COLUMN_TA) := BUBBLE;
        end;
      end loop;
    end loop;
    T_A_A := T_A_A_LOC;

    -- Build Y * Y vector : A_Y (I) = SUM[K=1..n] A(I,K)*Y(K)
    -- Y(K) is Points(K).Y
    for ROW_A in INDEX_DEGREE loop
      declare
        BUBBLE : NUMBER := 0.0;
      begin
        for COLUMN_A in INDEX_POINT loop
          BUBBLE := BUBBLE + A_LOC (ROW_A, COLUMN_A)
          * THE_POINTS (COLUMN_A).Y;
        end loop;
        A_Y_LOC (ROW_A) := BUBBLE;
      end;
    end loop;
    A_Y := A_Y_LOC;

  end DO_MATRIXES;

  -- Use my_syslin to solve A*X=B
  function SYSTEM (A : MATRIX; B : VECTOR) return VECTOR is
  begin
    return MY_SYSLIN.GAUSS (A,B);
  exception
    when others => raise R_RESOL_ERROR;
  end SYSTEM;

end R_COMPUTE;
