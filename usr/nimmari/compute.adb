with Rnd, My_Math;
package body Compute is

  procedure Init is
  begin
    Rnd.Randomize;
  end Init;

  -- Remove How_Many random bars from From
  procedure Remove_From (From : in out Common.Bar_Status_Array;
                         How_Many : in Common.Full_Bar_Range) is
    Nb_Removed : Common.Full_Bar_Range := 0;
    Remove_Index : Common.Bar_Range;
  begin
    loop
      exit when Nb_Removed = How_Many;
      Remove_One:
      loop
        Remove_Index := Rnd.Int_Random (Common.Bar_Range'First,
                                        Common.Bar_Range'Last);
        if From(Remove_Index) then
          -- Leave this one
          From(Remove_Index) := False;
          exit Remove_One;
        end if;
      end loop Remove_One;
      Nb_Removed := Nb_Removed + 1;
    end loop;
  end Remove_From;

  -- The state: 0s and 1s
  subtype Power_Range is Positive range 1 .. 3;
  type Mattrix_Tab is array (Common.Row_Range, Power_Range) of Boolean;
  Mattrix : Mattrix_Tab;
  Sigma : array (Power_Range) of Boolean;

  procedure Play (Row    : out Common.Row_Range;
                  Remove : out Common.Bar_Status_Array;
                  Result : out Common.Result_List) is

    Col : Power_Range;
    Winning : Boolean;
    New_Sum : Common.Full_Bar_Range;
    Sums : array (Common.Row_Range) of Common.Full_Bar_Range := (others => 0);
    Sums_Sums : Natural := 0;
    Nb_Rows_More_One : Natural := 0;
    use type Common.Game_Kind_List;
  begin
    Result := Common.Played;
    -- Compute Mattrix and Sigma
    declare
      Sum : Common.Full_Bar_Range;
    begin
      Mattrix := (others => (others => False));
      Sigma := (others => False);
      for I in Common.Row_Range loop
        -- Nb of bars
        Sum := Common.Nb_Bars (Common.Get_Bars (I));
        Sums(I) := Sum;
        Sums_Sums := Sums_Sums + Sum;
        -- Number of rows with more than one for marienbad end
        if Sum > 1 then
          Nb_Rows_More_One := Nb_Rows_More_One + 1;
        end if;
        -- Mattrix of powers of 2 (7 -> III, 5->101...)
        -- Logical Sigma per col
        for J in reverse Power_Range loop
          if Sum >= 2 ** (J - 1) then
            Col := Power_Range'Last - J + 1;
            Mattrix(I, Col) := True;
            Sigma(Col) := not Sigma(Col);
            Sum := Sum - 2 ** (J - 1);
          end if;
        end loop;
      end loop;
    end;

    -- Finished ?
    if Sums_Sums = 0 then
      if Common.Get_Game_Kind = Common.Nim then
        Result := Common.Lost;
      else
        Result := Common.Won;
      end if;
      Row := Common.Row_Range'First;
      Remove := (others => False);
      return;
    end if;

    -- Special ending for marienbad
    if Common.Get_Game_Kind = Common.Marienbad
    and then Nb_Rows_More_One = 1 then
      -- Last decision point
      declare
        Nb_Rows_One : Natural := 0;
      begin
        -- Find the row with more than one, count rows with one
        for I in Common.Row_Range loop
          if Sums(I) = 1 then
            Nb_Rows_One := Nb_Rows_One + 1;
          elsif Sums(I) > 1 then
            Row := I;
          end if;
        end loop;
        -- Leave one or zero bar on the selected row
        Remove := Common.Get_Bars (Row);
        if Nb_Rows_One mod 2 = 0 then
          -- Leave only one (random) bar
          Remove_From (Remove, 1);
        end if;
        return;
      end;
    end if;

    -- Can we win (sigma not all false)
    -- if yes keep first True Col in Sigma
    Winning := False;
    for J in Power_Range loop
      if Sigma(J) then
        Winning := True;
        Col := J;
        exit;
      end if;
    end loop;

    if Winning then
      declare
        -- Number of True in Mattrix (x, Col)
        Nb_True, Nb_Row : Natural;
      begin
        -- Compute Nb_True the number of True in the column Col
        Nb_True := 0;
        for I in Common.Row_Range loop
          if Mattrix(I, Col) then
            Nb_True := Nb_True + 1;
          end if;
        end loop;
        -- Select one randomly
        Nb_True := Rnd.Int_Random (1, Nb_True);
        -- Select row, the Nb_True matching
        Nb_Row := 0;
        for I in Common.Row_Range loop
          Row := I;
          if Mattrix(I, Col) then
            Nb_Row := Nb_Row + 1;
            exit when Nb_Row = Nb_True;
          end if;
        end loop;
      end;

      -- Play: for each True in Sigma, change corresponding col in row
      for I in Col .. Power_Range'Last loop
        if Sigma(I) then Mattrix(Row, I) := not Mattrix(Row, I);
        end if;
      end loop;

      -- Compute new amount and remove corresponding Nb of bars
      New_Sum := 0;
      for J in Power_Range loop
        if Mattrix(Row, Power_Range'Last - J + 1) then
          New_Sum := New_Sum + 2 ** (J - 1);
        end if;
      end loop;

    else

      -- Loosing : play random
      -- Choose a non empty row
      declare
        -- Number of Rows with Sum /= 0
        Nb_Non0, Curr_Non0 : Natural;
      begin
        -- Compute R
        Nb_Non0 := 0;
        for I in Common.Row_Range loop
          if Sums (I) /= 0 then
            Nb_Non0 := Nb_Non0 + 1;
          end if;
        end loop;
        Nb_Non0 := Rnd.Int_Random (1, Nb_Non0);
        -- Select row, the Nb_Non0 th matching
        Curr_Non0 := 0;
        for I in Common.Row_Range loop
          Row := I;
          if Sums(I) /= 0 then
            Curr_Non0 := Curr_Non0 + 1;
            exit when Curr_Non0 = Nb_Non0;
          end if;
        end loop;
      end;

      -- Number of bars to remove
      declare
        R : Float;
        -- The idea is: the more bars there is, the more we tend to remove
        Factor : constant Float := Float(Common.Nb_Init_Bars (Row))
                                 / Float(Sums(Row));
      begin
       -- 1.0 <= R < N + 1.0
       R := Rnd.Float_Random (1.0, Float(Sums(Row)) + 1.0);
       R := R / Factor;
       New_Sum := Integer(My_Math.Trunc (My_Math.Real (R)));
      end;
      -- Eh! Play!
      if New_Sum = 0 then
        New_Sum := 1;
      elsif New_Sum > Sums (Row) then
        -- Protection, should not enter here, but doesn't hurt :-)
        New_Sum := Sums(Row);
      end if;

      -- Number of bars to leave
      New_Sum := Sums(Row) - New_Sum;
    end if;

    -- Now remove Random bars to get the new Sum
    Remove := Common.Get_Bars (Row);
    Remove_From (Remove, Sums(Row) - New_Sum);

    -- Recompute number of bars
    Sums_Sums := Sums_Sums - Sums(Row) + New_Sum;

    -- Finished ?
    if Sums_Sums = 0 then
      if Common.Get_Game_Kind = Common.Nim then
        Result := Common.Played_And_Won;
      else
        Result := Common.Played_And_Lost;
      end if;
    end if;

  end Play;

end Compute;
