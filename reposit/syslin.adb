package body SYSLIN is

  -- The heart (art) of solving
  function GAUSS (A : MATRIX; B : VECTOR) return VECTOR is

    -- Dimension of the system
    subtype DIMENSION is POSITIVE range 1 ..  B'LENGTH;
    -- The solution
    SOLUTION    : VECTOR(DIMENSION);
    -- The pivot and its line
    PIVOT       : NUMBER;
    PIVOT_LINE  : DIMENSION;
    -- The number above which we consider a number as nul
    EPSILON     : constant NUMBER := NUMBER'EPSILON;
    -- Working matrix and vector for triangulation
    A_T         : MATRIX(DIMENSION, DIMENSION);
    B_T         : VECTOR(DIMENSION);
  begin

    -- A has to be square
    if A'LENGTH(1) /= A'LENGTH(2) then
      raise DIMENSION_ERROR;
    end if;

    -- A and B must have same length
    if A'LENGTH(1) /= B'LENGTH then
      raise DIMENSION_ERROR;
    end if;

    -- B (and thus A) of dimension > 0
    if B'LENGTH < 1 then
      raise DIMENSION_ERROR;
    end if;

    -- A and B indexes starting from 1
    if      A'FIRST(1) /= DIMENSION'FIRST
    or else A'FIRST(2) /= DIMENSION'FIRST
    or else B'FIRST    /= DIMENSION'FIRST then
      raise DIMENSION_ERROR;
    end if;

    -- Case where dimension is 1
    if B'LENGTH = 1 then
      -- Test wether pivot (A(1,1)) is not nul
      if A(A'FIRST(1), A'FIRST(2)) < EPSILON then
        raise DISCRIMINENT_ERROR;
      end if;
      SOLUTION(SOLUTION'FIRST) := B(B'FIRST) / A(A'FIRST(1), A'FIRST(2));
      return SOLUTION;
    end if;

    -- Load working matrix and vector
    A_T := A;
    B_T := B;

    -- Triangulation of the matrix
    for LINE in DIMENSION'FIRST .. DIMENSION'PRED(DIMENSION'LAST) loop

      -- Search for biggest pivot as possible
      --  from current line to last line, in current column (which is current line index)
      PIVOT := abs A_T(LINE, LINE);
      PIVOT_LINE := LINE;
      for SUB_LINE in DIMENSION'SUCC(LINE) .. DIMENSION'LAST loop
        if abs(A_T(SUB_LINE, LINE)) > PIVOT then
          PIVOT := abs(A_T(SUB_LINE, LINE));
          PIVOT_LINE := SUB_LINE;
        end if;
      end loop;

      -- Test wether pivot is not nul
      if PIVOT < EPSILON then
        raise DISCRIMINENT_ERROR;
      end if;

      -- Exchange current line with pivot line (in matrix and vector)
      if PIVOT_LINE /= LINE then
        declare
          subtype SLICE is DIMENSION range LINE .. DIMENSION'LAST;
          NUMBER_AUX : NUMBER;
        begin
          for COLUMN in SLICE loop
            NUMBER_AUX := A_T(LINE, COLUMN);
            A_T(LINE, COLUMN) := A_T(PIVOT_LINE, COLUMN);
            A_T(PIVOT_LINE, COLUMN) := NUMBER_AUX;
          end loop;
          NUMBER_AUX := B_T(LINE);
          B_T(LINE) := B_T(PIVOT_LINE);
          B_T(PIVOT_LINE) := NUMBER_AUX;
        end;
      end if;

      -- Modification of lines below current line
      -- For each sub_line s above l,
      --  factor = A(s,l) / A (l,l)
      --  for each column c on line s, substract factor * A(l,c) to A(s,c)
      --  substract factor * B(l) to B(s)
      -- So all A(s,l+1) = 0
      for SUB_LINE in DIMENSION'SUCC(LINE) .. DIMENSION'LAST loop
        declare
          FACTOR : NUMBER := A_T(SUB_LINE, LINE) / A_T(LINE, LINE);
        begin
          for COLUMN in DIMENSION'SUCC(LINE) .. DIMENSION'LAST loop
            A_T(SUB_LINE, COLUMN) := A_T(SUB_LINE, COLUMN) - FACTOR * A_T(LINE, COLUMN);
          end loop;
          B_T(SUB_LINE) := B_T(SUB_LINE) - FACTOR * B_T(LINE);
        end;
      end loop;

    end loop;

    -- The mattrix A is now triangular

    -- Test wether last component is not nul
    if abs(A_T(DIMENSION'LAST, DIMENSION'LAST)) < EPSILON then
      raise DISCRIMINENT_ERROR;
    end if;

    -- Resolution "in stairs"
    -- Last line is A(l,l) * x(l) = B(l)  => x(l)
    -- then A(l-1, l-1) * x(l-1) + A(l-1, l) * x(l) = B(l-1) => x(l-1)
    -- then A(l-2, l-2) * x(l-2) + A(l-2, l-1) * x(l-1) +  A(l-2, l * x(l) = B(l-2)
    --                            <----------- computed in TMP ----------->
    SOLUTION(DIMENSION'LAST) := B_T(DIMENSION'LAST) / A_T(DIMENSION'LAST, DIMENSION'LAST);
    for LINE in reverse DIMENSION'FIRST .. DIMENSION'PRED(DIMENSION'LAST) loop
      declare
        TMP : NUMBER := 0.0;
      begin
        for COLUMN in DIMENSION'SUCC(LINE) .. DIMENSION'LAST loop
          TMP := TMP + A_T(LINE, COLUMN) * SOLUTION(COLUMN);
        end loop;
        SOLUTION(LINE) := (B_T(LINE) - TMP) / A_T(LINE, LINE);
      end;
    end loop;

    return SOLUTION;

  end GAUSS;

end SYSLIN;
