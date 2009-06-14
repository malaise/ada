package body Syslin is

  -- The heart (art) of solving
  function Gauss (A : Matrix; B : Vector) return Vector is

    -- Dimension of the system
    subtype Dimension is Positive range 1 ..  B'Length;
    -- The solution
    Solution    : Vector(Dimension);
    -- The pivot and its line
    Pivot       : Number;
    Pivot_Line  : Dimension;
    -- The number above which we consider a number as nul
    Epsilon     : constant Number := Number'Epsilon;
    -- Working matrix and vector for triangulation
    A_T         : Matrix(Dimension, Dimension);
    B_T         : Vector(Dimension);
  begin

    -- A has to be square
    if A'Length(1) /= A'Length(2) then
      raise Dimension_Error;
    end if;

    -- A and B must have same length
    if A'Length(1) /= B'Length then
      raise Dimension_Error;
    end if;

    -- B (and thus A) of dimension > 0
    if B'Length < 1 then
      raise Dimension_Error;
    end if;

    -- A and B indexes starting from 1
    if      A'First(1) /= Dimension'First
    or else A'First(2) /= Dimension'First
    or else B'First    /= Dimension'First then
      raise Dimension_Error;
    end if;

    -- Case where dimension is 1
    if B'Length = 1 then
      -- Test wether pivot (A(1,1)) is not nul
      if A(A'First(1), A'First(2)) < Epsilon then
        raise Discriminent_Error;
      end if;
      Solution(Solution'First) := B(B'First) / A(A'First(1), A'First(2));
      return Solution;
    end if;

    -- Load working matrix and vector
    A_T := A;
    B_T := B;

    -- Triangulation of the matrix
    for Line in Dimension'First .. Dimension'Pred(Dimension'Last) loop

      -- Search for biggest pivot as possible
      --  from current line to last line, in current column (which is current line index)
      Pivot := abs A_T(Line, Line);
      Pivot_Line := Line;
      for Sub_Line in Dimension'Succ(Line) .. Dimension'Last loop
        if abs(A_T(Sub_Line, Line)) > Pivot then
          Pivot := abs(A_T(Sub_Line, Line));
          Pivot_Line := Sub_Line;
        end if;
      end loop;

      -- Test wether pivot is not nul
      if Pivot < Epsilon then
        raise Discriminent_Error;
      end if;

      -- Exchange current line with pivot line (in matrix and vector)
      if Pivot_Line /= Line then
        declare
          subtype Slice is Dimension range Line .. Dimension'Last;
          Number_Aux : Number;
        begin
          for Column in Slice loop
            Number_Aux := A_T(Line, Column);
            A_T(Line, Column) := A_T(Pivot_Line, Column);
            A_T(Pivot_Line, Column) := Number_Aux;
          end loop;
          Number_Aux := B_T(Line);
          B_T(Line) := B_T(Pivot_Line);
          B_T(Pivot_Line) := Number_Aux;
        end;
      end if;

      -- Modification of lines below current line
      -- For each sub_line s above l,
      --  factor = A(s,l) / A (l,l)
      --  for each column c on line s, substract factor * A(l,c) to A(s,c)
      --  substract factor * B(l) to B(s)
      -- So all A(s,l+1) = 0
      for Sub_Line in Dimension'Succ(Line) .. Dimension'Last loop
        declare
          Factor : constant Number := A_T(Sub_Line, Line) / A_T(Line, Line);
        begin
          for Column in Dimension'Succ(Line) .. Dimension'Last loop
            A_T(Sub_Line, Column) := A_T(Sub_Line, Column) - Factor * A_T(Line, Column);
          end loop;
          B_T(Sub_Line) := B_T(Sub_Line) - Factor * B_T(Line);
        end;
      end loop;

    end loop;

    -- The mattrix A is now triangular

    -- Test wether last component is not nul
    if abs(A_T(Dimension'Last, Dimension'Last)) < Epsilon then
      raise Discriminent_Error;
    end if;

    -- Resolution "in stairs"
    -- Last line is A(l,l) * x(l) = B(l)  => x(l)
    -- then A(l-1, l-1) * x(l-1) + A(l-1, l) * x(l) = B(l-1) => x(l-1)
    -- then A(l-2, l-2) * x(l-2) + A(l-2, l-1) * x(l-1) +  A(l-2, l * x(l) = B(l-2)
    --                            <----------- computed in Tmp ----------->
    Solution(Dimension'Last) := B_T(Dimension'Last) / A_T(Dimension'Last, Dimension'Last);
    for Line in reverse Dimension'First .. Dimension'Pred(Dimension'Last) loop
      declare
        Tmp : Number := 0.0;
      begin
        for Column in Dimension'Succ(Line) .. Dimension'Last loop
          Tmp := Tmp + A_T(Line, Column) * Solution(Column);
        end loop;
        Solution(Line) := (B_T(Line) - Tmp) / A_T(Line, Line);
      end;
    end loop;

    return Solution;

  end Gauss;

end Syslin;
