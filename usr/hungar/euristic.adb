with Rnd, Sorts, My_Io, Normal;
with Debug;

package body Euristic is

  -- Dump Mattrix
  procedure Put_Mattrix (Mattrix : in Types.Mattrix_Rec) is
    subtype Index_Range is Types.Index_Range range 1 .. Mattrix.Dim;
  begin
    for Row in Index_Range loop
      for Col in Index_Range loop
        if Mattrix.Notes(Row, Col) / 100 = 100 then
          My_Io.Put ("** ");
        else
          My_Io.Put(Normal(Mattrix.Notes(Row, Col) / 100, 2) & " ");
        end if;
      end loop;
      My_Io.New_Line;
    end loop;
  end Put_Mattrix;

  -- State of a zero in zero descriptors
  -- Used also in zero transfer
  type Zero_State_List is (Free, Slashed, Squared);
  -- transfer of state of zeros after euristic (for reduction or result)
  -- No zero are free at this point, so this state is used to describe non-zero cell
  type Zero_Transfer_Tab is array (Types.Index_Range range <>, Types.Index_Range range <>) of Zero_State_List;
  -- Have at least 1 zero/row and 1 zero/col
  procedure Init_Search (Mattrix : in out Types.Mattrix_Rec) is
    Lowest : Natural;
  begin
    -- One zero in each row
    for Row in 1 .. Mattrix.Dim loop
      -- Search lowest elem of row
      Lowest := Types.Cell_Range'Last;
      for Col in 1 .. Mattrix.Dim loop
        if Mattrix.Notes(Row, Col) < Lowest then Lowest := Mattrix.Notes(Row, Col); end if;
      end loop;
      -- Substract it from each element
      if Lowest /= 0 then
        for Col in 1 .. Mattrix.Dim loop
          Mattrix.Notes(Row, Col)  := Mattrix.Notes(Row, Col) - Lowest;
        end loop;
      end if;
    end loop;

    -- One zero in each col
    for Col in 1 .. Mattrix.Dim loop
      -- Search lowest elem of col
      Lowest := Types.Cell_Range'Last;
      for Row in 1 .. Mattrix.Dim loop
        if Mattrix.Notes(Row, Col) < Lowest then Lowest := Mattrix.Notes(Row, Col); end if;
      end loop;
      -- Substract it from each element
      if Lowest /= 0 then
        for Row in 1 .. Mattrix.Dim loop
          Mattrix.Notes(Row, Col)  := Mattrix.Notes(Row, Col) - Lowest;
        end loop;
      end if;
    end loop;
  end Init_Search;

  -- Try to find a euristic solution
  -- If OK, Done is set to True
  -- Else, Transfer is set for reduction
  procedure Euristic_Search (Mattrix : in Types.Mattrix_Rec;
   Done : out Boolean; Transfer : out Zero_Transfer_Tab) is

    -- For each square with 0, keep its pos and the number of zeros in
    -- its row and col
    type Zero_Desc_Rec is record
      Row : Types.Index_Range;
      Col : Types.Index_Range;
      Sigma : Positive;
      State : Zero_State_List := Free;
    end record;
    -- To sort zero dscriptors
    function "<" (Left, Right : Zero_Desc_Rec) return Boolean;

    type Zero_Desc_Tab_Gen is array (Positive range <>) of Zero_Desc_Rec;

    package Zero_Desc_Sort is new Sorts (
     Typ_Object => Zero_Desc_Rec,
     Typ_Index  => Positive,
     "<"        => "<",
     Typ_Array  => Zero_Desc_Tab_Gen);

    -- Index in mattrix
    subtype Index_Range is Types.Index_Range range 1 .. Mattrix.Dim;
    -- Zero_Desc_Rec for all the mattrix
    subtype Zero_Desc_Range is Positive range 1 .. Mattrix.Dim * Mattrix.Dim;
    subtype Zero_Desc_Tab is Zero_Desc_Tab_Gen(Zero_Desc_Range);
    Zero_Desc : Zero_Desc_Tab;
    Nb_Zero : Natural range 0 .. Zero_Desc_Range'Last;

    -- Instantiate sorting
    -- Free zeros first, in order of crescent Sigma, then slashed then squared zeros
    function "<" (Left, Right : Zero_Desc_Rec) return Boolean is
    begin
      if Left.State = Free then
        if Right.State /= Free then
          -- Left Free and not Right
          return True;
        else
          -- Left and Right Free
          return Left.Sigma < Right.Sigma;
        end if;
      else
        return Left.State = Slashed and then Right.State = Squared;
      end if;
    end "<";

  begin

    Init_Zeros:
    declare
      -- Number of zeros/row, number of zeros/col
      Zero_Row : array (1 .. Mattrix.Dim) of Natural := (others => 0);
      Zero_Col : array (1 .. Mattrix.Dim) of Natural := (others => 0);
    begin
      -- Count zeros in rows and cols and store zeros in desc
      -- Sigmas are unknown yet
      Nb_Zero := 0;
      for Row in Index_Range loop
        for Col in Index_Range loop
          if Mattrix.Notes(Row, Col) = 0 then
            -- Number of zeros in this row and col
            Zero_Row(Row) := Zero_Row(Row) + 1;
            Zero_Col(Col) := Zero_Col(Col) + 1;
            -- Descriptor of the zero: all is set but sigma
            Nb_Zero := Nb_Zero + 1;
            Zero_Desc(Nb_Zero).Row := Row;
            Zero_Desc(Nb_Zero).Col := Col;
            Zero_Desc(Nb_Zero).State := Free;
          end if;
        end loop;
      end loop;

      -- Set sigmas of zero descriptors
      for No_Zero in 1 .. Nb_Zero loop
        -- Number of zeros in its row + col
        Zero_Desc(No_Zero).Sigma := Zero_Row(Zero_Desc(No_Zero).Row)
                                  + Zero_Col(Zero_Desc(No_Zero).Col)
                                  - 1;
      end loop;
    end Init_Zeros;

    -- Init random serial list
    Rnd.Randomize;

    Euristic_Loop:
    loop


      Try_To_Solve:
      declare
        -- Mattrix of zeros : each zero has either an index in zero desc
        --  or 0 if content of Mattrix is not zero
        Cross_Ref : array (1 .. Mattrix.Dim, 1 .. Mattrix.Dim) of Natural
                  := (others => (others => 0));
        -- The zero selected (one of the zeros with min sigma)
        Selected_Zero : Positive;
        -- Number of zeros with min zigma
        Nb_Min_Zero : Positive := 1;
        -- Index in Zero_Desc
        Index_Desc : Natural;
        -- Row and column index storage
        Row_Tmp, Col_Tmp : Index_Range;
        -- Is there any zero free remaining
        Zero_Free_Remaining : Boolean;
      begin
        -- Sort the list of zeros
        Sort_Zero_Desc:
        begin
          Zero_Desc_Sort.Quick_Sort(Zero_Desc(1 .. Nb_Zero));
        end Sort_Zero_Desc;

        -- Set the cross reference mattrix
        for I in 1 .. Nb_Zero loop
          Cross_Ref(Zero_Desc(I).Row, Zero_Desc(I).Col) := I;
        end loop;

        -- Count the number of zeros which are free and have the Sigma minimum
        for I in 2 .. Nb_Zero loop
          exit when Zero_Desc(I).State /= Free or else Zero_Desc(I).Sigma /= Zero_Desc(1).Sigma;
          Nb_Min_Zero := I;
        end loop;

        -- Select one randomly
        Selected_Zero := Rnd.Int_Random (1, Nb_Min_Zero);
        Zero_Desc(Selected_Zero).State := Squared;

        -- Propagate the choice
        -- Sigma of slashed and squared zeros are not used
        -- Slash all zero of the same row and keep up to date the sigma of zeros of their columns
        if Debug.On then
          My_Io.Put ("Square " & Normal(Zero_Desc(Selected_Zero).Sigma, 4)
                   & " at "
                   & Normal(Zero_Desc(Selected_Zero).Row, 2)
                   & "-"
                   & Normal(Zero_Desc(Selected_Zero).Col, 2)
                   & ",Slash ");
        end if;
        for Col in Index_Range loop
          Index_Desc := Cross_Ref(Zero_Desc(Selected_Zero).Row, Col);
          if Index_Desc /= 0 and then Zero_Desc(Index_Desc).State = Free then
            Zero_Desc(Index_Desc).State := Slashed;
            Col_Tmp := Zero_Desc(Index_Desc).Col;
            if Debug.On then
              My_Io.Put (Normal(Zero_Desc(Index_Desc).Row, 2)
                 & "-" & Normal(Zero_Desc(Index_Desc).Col, 2) & ",");
            end if;
            for Row in Index_Range loop
              Index_Desc := Cross_Ref(Row, Col_Tmp);
              if Index_Desc /= 0 and then Zero_Desc(Index_Desc).State = Free then
                Zero_Desc(Index_Desc).Sigma := Zero_Desc(Index_Desc).Sigma - 1;
              end if;
            end loop;
          end if;
        end loop;
        -- Slash all zero of the same col and keep up to date the sigma of zeros of their rows
        for Row in Index_Range loop
          Index_Desc := Cross_Ref(Row, Zero_Desc(Selected_Zero).Col);
          if Index_Desc /= 0 and then Zero_Desc(Index_Desc).State = Free then
            Zero_Desc(Index_Desc).State := Slashed;
            Row_Tmp := Zero_Desc(Index_Desc).Row;
            if Debug.On then
              My_Io.Put (Normal(Zero_Desc(Index_Desc).Row, 2)
                 & "-" & Normal(Zero_Desc(Index_Desc).Col, 2) & ",");
            end if;
            for Col in Index_Range loop
              Index_Desc := Cross_Ref(Row_Tmp, Col);
              if Index_Desc /= 0 and then Zero_Desc(Index_Desc).State = Free then
                Zero_Desc(Index_Desc).Sigma := Zero_Desc(Index_Desc).Sigma - 1;
              end if;
            end loop;
          end if;
        end loop;
        if Debug.On then
          My_Io.New_Line;
        end if;

        -- Count nb of free zero remaining.
        Zero_Free_Remaining := False;
        for I in 1 .. Nb_Zero loop
          if Zero_Desc(I).State = Free then
            Zero_Free_Remaining := True;
            exit;
          end if;
        end loop;

        -- Euristic search done when no more zero free
        exit Euristic_Loop when not Zero_Free_Remaining;
      end Try_To_Solve;

    end loop Euristic_Loop;

    -- Check final success
    Test_Done:
    declare
      -- Number of squared zeros
      Nb_Squared_Zeros : Types.Index_Range := 0;
    begin
      -- Done if the number of squared zeros is Dim
      for I in 1 .. Nb_Zero loop
        if Zero_Desc(I).State = Squared then
          Nb_Squared_Zeros := Nb_Squared_Zeros + 1;
        end if;
      end loop;
      Done := Nb_Squared_Zeros = Mattrix.Dim;
      if Debug.On then
        My_Io.Put_Line ("Nb zeros "
                       & Types.Index_Range'Image(Nb_Squared_Zeros)
                       & "/" & Types.Index_Range'Image(Nb_Zero));
      end if;
    end Test_Done;

    -- Dump Rows and Cols with no squared zero
    Dump_No_Zero:
    declare
      Found : Boolean;
    begin
      if not Done and then Debug.On then
        for Row in Index_Range loop
          Found := False;
          for I in 1 .. Nb_Zero loop
            if Zero_Desc(I).State = Squared and then Zero_Desc(I).Row = Row then
              Found := True;
              exit;
            end if;
          end loop;
          if not Found then
            My_Io.Put_Line("Row " & Index_Range'Image(Row) & " has no zero");
          end if;
        end loop;
        for Col in Index_Range loop
          Found := False;
          for I in 1 .. Nb_Zero loop
            if Zero_Desc(I).State = Squared and then Zero_Desc(I).Col = Col then
              Found := True;
              exit;
            end if;
          end loop;
          if not Found then
            My_Io.Put_Line("Col " & Index_Range'Image(Col) & " has no zero");
          end if;
        end loop;
      end if;
    end Dump_No_Zero;

    -- Set zero transfer tab (free means not a zero)
    Transfer := (others => (others => Free));
    for I in 1 .. Nb_Zero loop
      -- No zero free remain in Zero_Desc
      Transfer(Zero_Desc(I).Row, Zero_Desc(I).Col) := Zero_Desc(I).State;
    end loop;

    -- Dump transfer tab
    if Debug.On then
      My_Io.Put_Line("End of search.");
      My_Io.Put_Line("Mattrix:");
      Put_Mattrix(Mattrix);
      My_Io.Put_Line("Zero transfer tab:");
      for Row in Index_Range loop
        for Col in Index_Range loop
          case Transfer(Row, Col) is
            when Squared =>
              My_Io.Put("*");
            when Slashed =>
              My_Io.Put("/");
            when Free =>
              My_Io.Put(".");
          end case;
        end loop;
        My_Io.New_Line;
      end loop;
    end if;

  end Euristic_Search;

  procedure Reduce (Mattrix : in out Types.Mattrix_Rec; Transfer : in Zero_Transfer_Tab) is
    -- Index in mattrix
    subtype Index_Range is Types.Index_Range range 1 .. Mattrix.Dim;
    -- Rows and colums marqued
    Marked_Row : array (Index_Range) of Boolean := (others => False);
    Marked_Col : array (Index_Range) of Boolean := (others => False);
    -- Does the current row/col satisfy the criteria
    Ok : Boolean;
    -- At least one mark should be done
    Nb_Mark : Natural;
  begin
    -- Mark rows with no squared zero
    for Row in Index_Range loop
      -- No squared zero ?
      Ok := True;
      for Col in Index_Range loop
        if Transfer(Row, Col) = Squared then
          -- Squared zero
          Ok := False;
          exit;
        end if;
      end loop;
      if Ok then
        Marked_Row(Row) := True;
        if Debug.On then
          My_Io.Put(" Mark Row " & Index_Range'Image(Row));
        end if;
      end if;
    end loop;
    if Debug.On then
      My_Io.New_Line;
    end if;

    -- Iterative marking of rows and cols
    Marking:
    loop

      Nb_Mark := 0;

      -- For each marked row, mark each col with a slashed zero in this row
      for Row in Index_Range loop
        if Marked_Row(Row) then
          -- Mark each col with a slashed zero in this row
          for Col in Index_Range loop
            if Transfer(Row, Col) = Slashed and then not Marked_Col(Col) then
              Marked_Col(Col) := True;
              Nb_Mark := Nb_Mark + 1;
              if Debug.On then
                My_Io.Put(" Mark Col " & Index_Range'Image(Col));
              end if;
            end if;
          end loop;
        end if;
      end loop;

      -- For each marked col, mark each row with a squared zero in this col
      for Col in Index_Range loop
        if Marked_Col(Col) then
          -- Mark each row with a squared zero in this col
          for Row in Index_Range loop
            if Transfer(Row, Col) = Squared and then not Marked_Row(Row) then
              Marked_Row(Row) := True;
              Nb_Mark := Nb_Mark + 1;
              if Debug.On then
                My_Io.Put(" Mark Row " & Index_Range'Image(Row));
              end if;
            end if;
          end loop;
        end if;
      end loop;
      if Debug.On then
        My_Io.Put_Line (" --- Nb_Mark " & Natural'Image(Nb_Mark));
      end if;

      exit Marking when Nb_Mark = 0;
    end loop Marking;

    if Debug.On then
      My_Io.Put("Not marked rows: ");
      for Row in Index_Range loop
        if not Marked_Row(Row) then
          My_Io.Put(Index_Range'Image(Row));
        end if;
      end loop;
      My_Io.New_Line;
      My_Io.Put("Not marked cols: ");
      for Col in Index_Range loop
        if not Marked_Col(Col) then
          My_Io.Put(Index_Range'Image(Col));
        end if;
      end loop;
      My_Io.New_Line;
    end if;

    Sub_Lowest:
    declare
      Lowest : Types.Cell_Range := Types.Cell_Range'Last;
      Nb_Change : Natural := 0;
    begin
      -- Search lowest value amoung marked lines and not marked columns
      for Row in Index_Range loop
        if Marked_Row(Row) then
          for Col in Index_Range loop
            if not Marked_Col(Col)
            and then Mattrix.Notes(Row, Col) < Lowest then
              Lowest:= Mattrix.Notes(Row, Col);
            end if;
          end loop;
        end if;
      end loop;
      if Debug.On then
        My_Io.Put_Line ("  Lowest " & Types.Cell_Range'Image(Lowest));
      end if;

      -- Substract Lowest from cells which row     marked and col not marked
      -- Add       Lowest to   cells which row not marked and col     marked
      for Row in Index_Range loop
        for Col in Index_Range loop
          if Marked_Row(Row) and then not Marked_Col(Col) then
            Nb_Change := Nb_Change + 1;
            Mattrix.Notes(Row, Col) := Mattrix.Notes(Row, Col) - Lowest;
            if Debug.On then
              My_Io.Put_Line ("    Sub");
            end if;
          elsif not Marked_Row(Row) and then Marked_Col(Col) then
            Nb_Change := Nb_Change + 1;
            -- Add only if Mattrix.Notes(Row, Col) + Lowest <= Cell_Range'Last
            if Types.Cell_Range'Last - Mattrix.Notes(Row, Col) >= Lowest then
              Mattrix.Notes(Row, Col) := Mattrix.Notes(Row, Col) + Lowest;
              if Debug.On then
                My_Io.Put_Line ("    Add");
              end if;
            else
              Mattrix.Notes(Row, Col) := Types.Cell_Range'Last;
              if Debug.On then
                My_Io.Put_Line ("    Max");
              end if;
            end if;
          end if;
        end loop;
      end loop;

      if Debug.On and then Nb_Change = 0 then
        My_Io.Put_Line ("No Change");
        Put_Mattrix(Mattrix);
      end if;

    end Sub_Lowest;

  end Reduce;

  procedure Search (Mattrix : in out Types.Mattrix_Rec; Nb_Iterations : out Positive) is
    Done : Boolean;
    Transfer : Zero_Transfer_Tab (1 .. Mattrix.Dim, 1 .. Mattrix.Dim);
    Nb_Loop  : Positive := 1;
  begin
    -- Init for search : one zero/row and / col
    Init_Search (Mattrix);
    loop
       if not Debug.On then
         My_Io.Put (".");
         My_Io.Flush;
       end if;
       -- Try to search
       Euristic_Search (Mattrix, Done, Transfer);
       exit when Done;
       -- Euristic failed : reduce
       Reduce (Mattrix, Transfer);
       Nb_Loop := Nb_Loop + 1;
    end loop;
    My_Io.New_Line;
    -- Euristic success: set mattrix to 1 (affected) or 0 (not affected)
    -- Affected if Transfer is squared
    for Row in 1 .. Mattrix.Dim loop
      for Col in 1 .. Mattrix.Dim loop
        if Transfer(Row, Col) = Squared then
          Mattrix.Notes(Row, Col) := 1;
        else
          Mattrix.Notes(Row, Col) := 0;
        end if;
      end loop;
    end loop;
    Nb_Iterations := Nb_Loop;
  end Search;


end Euristic;

