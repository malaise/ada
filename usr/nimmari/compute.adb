with Rnd, My_Math;
with Screen;
package body Compute is
  use Common;

  procedure Init is
  begin
    Rnd.Randomize;
  end Init;

  subtype Power_Range is Positive range 1 .. 3;
  type Mattrix_Tab is array (Common.Row_Range, Power_Range) of Boolean;
  Mattrix : Mattrix_Tab;
  Sigma : array (Power_Range) of Boolean;

  procedure Play (Game : in Common.Game_List;
                  Result : out Result_List;
                  Row : out Common.Row_Range;
                  Bars : out Common.Full_Bar_Range) is
    Col : Power_Range;
    Winning : Boolean;
    Selected_Row : Common.Row_Range;
    Sums : array (Common.Row_Range) of Common.Full_Bar_Range := (others => 0);
    Sums_Sums : Natural := 0;
    Nb_Bars : Common.Full_Bar_Range;
    Nb_Rows_More_One : Natural := 0;
  begin
    Result := Played;
    -- Compute MATTRIX and SIGMA
    declare
      Sum : Natural;
    begin
      Mattrix := (others => (others => False));
      Sigma := (others => False);
      for I in Common.Row_Range loop
        Sum := Screen.Content (I);
        Sums (I) := Sum;
        Sums_Sums := Sums_Sums + Sum;
        -- Number of rows with more than one for marienbad end
        if Sum > 1 then
          Nb_Rows_More_One := Nb_Rows_More_One + 1;
        end if;
        for J in reverse Power_Range loop
          if Sum >= 2 ** (J - 1) then
            Col := Power_Range'Last - J + 1;
            Mattrix (I, Col) := True;
            Sigma (Col) := not Sigma (Col);
            Sum := Sum - 2 ** (J - 1);
          end if;
        end loop;
      end loop;
    end;

    -- Finished ?
    if Sums_Sums = 0 then
      if Game = Common.Nim then
        Result := Lost;
      else
        Result := Won;
      end if;
      Row := Common.Row_Range'First;
      Bars := 0;
      return;
    end if;

    -- Special ending for marienbad
    if Game = Common.Marienbad and then Nb_Rows_More_One = 1 then
      -- Last decision point
      -- Choose row with more than one, count rows with one
      declare
        Nb_Rows_One : Natural := 0;
      begin
        for I in Common.Row_Range loop
          if Sums (I) = 1 then
            Nb_Rows_One := Nb_Rows_One + 1;
          elsif Sums (I) > 1 then
            Row := I;
          end if;
        end loop;
        -- Leave one or zero on the selected row
        if Nb_Rows_One mod 2 = 0 then
          Bars := 1;
        else
          Bars := 0;
        end if;
        return;
      end;
    end if;

    -- Can we win (sigma not all false)
    -- if yes keep first true COL in SIGMA
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
        -- number of TRUE in MATTRIX (x, COL)
        R, N : Natural;
      begin
        -- Compute R
        R := 0;
        for I in Common.Row_Range loop
          if Mattrix (I, Col) then
            R := R + 1;
          end if;
        end loop;
        R := Rnd.Int_Random (1, R);
        -- Select row, the Rth matching
        N := 0;
        for I in Common.Row_Range loop
          Selected_Row := I;
          if Mattrix (I, Col) then
            N := N + 1;
            exit when N = R;
          end if;
        end loop;
      end;

      -- play: for each TRUE in sigma, change corresponding col in selected row
      for I in Col .. Power_Range'Last loop
        if Sigma (I) then
           Mattrix (Selected_Row, I) := not Mattrix (Selected_Row, I);
        end if;
      end loop;

      -- Compute new amount 
      Nb_Bars := 0;
      for J in Power_Range loop
        if Mattrix (Selected_Row, Power_Range'Last - J + 1) then
          Nb_Bars := Nb_Bars + 2 ** (J - 1);
        end if;
      end loop;

    else

      -- Loosing : play random
      -- Choose a non empty row
      declare
        -- number of ROWS with SUM /= 0
        R, N : Natural;
      begin
        -- Compute R
        R := 0;
        for I in Common.Row_Range loop
          if Sums (I) /= 0 then
            R := R + 1;
          end if;
        end loop;
        R := Rnd.Int_Random (1, R);
        -- Select row, the Rth matching
        N := 0;
        for I in Common.Row_Range loop
          Selected_Row := I;
          if Sums (I) /= 0 then
            N := N + 1;
            exit when N = R;
          end if;
        end loop;
      end;

      -- Number of bars to remove
      declare
        R : Float;
        -- The idea is: the more bars there is, the more we tend to remove
        Factor : constant Float := Float(Common.Bar_Per_Row(Selected_Row))
                                 / Float(Sums (Selected_Row));
      begin
       -- 1.0 <= R < N + 1.0
       R := Rnd.Float_Random (1.0, Float(Sums (Selected_Row)) + 1.0);
       R := R / Factor;
       Nb_Bars := Integer(My_Math.Trunc(My_Math.Real(R)));
      end;
      -- Eh! Play!
      if Nb_Bars = 0 then
        Nb_Bars := 1;
      elsif Nb_Bars > Sums (Selected_Row) then
        -- Protection, should not enter here, but doesn't hurt :-)
        Nb_Bars := Sums (Selected_Row);
      end if;

      -- Number of bars to leave
      Nb_Bars := Sums (Selected_Row) - Nb_Bars;
    end if;
    Bars := Nb_Bars;
    Row := Selected_Row;

    -- Recompute number of bars
    Sums_Sums := Sums_Sums - Sums(Selected_Row) + Nb_Bars;

    -- Finished ?
    if Sums_Sums = 0 then
      if Game = Common.Nim then
        Result := Played_And_Won;
      else
        Result := Played_And_Lost;
      end if;
    end if;

  end Play;

end Compute;
