with RND, MATH;
with SCREEN;
package body COMPUTE is
  use COMMON;

  procedure INIT is
  begin
    RND.RANDOMIZE;
  end INIT;

  subtype POWER_RANGE is POSITIVE range 1 .. 3;
  type MATTRIX_TAB is array (COMMON.ROW_RANGE, POWER_RANGE) of BOOLEAN;
  MATTRIX : MATTRIX_TAB;
  SIGMA : array (POWER_RANGE) of BOOLEAN;

  procedure PLAY (GAME : in COMMON.GAME_LIST;
                  RESULT : out RESULT_LIST;
                  ROW : out COMMON.ROW_RANGE;
                  BARS : out COMMON.FULL_BAR_RANGE) is
    COL : POWER_RANGE;
    WINNING : BOOLEAN;
    SELECTED_ROW : COMMON.ROW_RANGE;
    SUMS : array (COMMON.ROW_RANGE) of COMMON.FULL_BAR_RANGE := (others => 0);
    SUMS_SUMS : NATURAL := 0;
    NB_BARS : COMMON.FULL_BAR_RANGE;
    NB_ROWS_MORE_ONE : NATURAL := 0;
  begin
    RESULT := PLAYED;
    -- Compute MATTRIX and SIGMA
    declare
      SUM : NATURAL;
    begin
      MATTRIX := (others => (others => FALSE));
      SIGMA := (others => FALSE);
      for I in COMMON.ROW_RANGE loop
        SUM := SCREEN.CONTENT (I);
        SUMS (I) := SUM;
        SUMS_SUMS := SUMS_SUMS + SUM;
        -- Number of rows with more than one for marienbad end
        if SUM > 1 then
          NB_ROWS_MORE_ONE := NB_ROWS_MORE_ONE + 1;
        end if;
        for J in reverse POWER_RANGE loop
          if SUM >= 2 ** (J - 1) then
            COL := POWER_RANGE'LAST - J + 1;
            MATTRIX (I, COL) := TRUE;
            SIGMA (COL) := not SIGMA (COL);
            SUM := SUM - 2 ** (J - 1);
          end if;
        end loop;
      end loop;
    end;

    -- Finished ?
    if SUMS_SUMS = 0 then
      if GAME = COMMON.NIM then
        RESULT := LOST;
      else
        RESULT := WON;
      end if;
      ROW := COMMON.ROW_RANGE'FIRST;
      BARS := 0;
      return;
    end if;

    -- Special ending for marienbad
    if GAME = COMMON.MARIENBAD and then NB_ROWS_MORE_ONE = 1 then
      -- Last decision point
      -- Choose row with more than one, count rows with one
      declare
        NB_ROWS_ONE : NATURAL := 0;
      begin
        for I in COMMON.ROW_RANGE loop
          if SUMS (I) = 1 then
            NB_ROWS_ONE := NB_ROWS_ONE + 1;
          elsif SUMS (I) > 1 then
            ROW := I;
          end if;
        end loop;
        -- Leave one or zero on the selected row
        if NB_ROWS_ONE mod 2 = 0 then
          BARS := 1;
        else
          BARS := 0;
        end if;
        return;
      end;
    end if;

    -- Can we win (sigma not all false)
    -- if yes keep first true COL in SIGMA
    WINNING := FALSE;
    for J in POWER_RANGE loop
      if SIGMA(J) then
        WINNING := TRUE;
        COL := J;
        exit;
      end if;
    end loop;
    
    if WINNING then

      declare
        -- number of TRUE in MATTRIX (x, COL)
        R, N : NATURAL;
      begin
        -- Compute R
        R := 0;
        for I in COMMON.ROW_RANGE loop
          if MATTRIX (I, COL) then
            R := R + 1;
          end if;
        end loop;
        R := RND.INT_RANDOM (1, R);
        -- Select row, the Rth matching
        N := 0;
        for I in COMMON.ROW_RANGE loop
          SELECTED_ROW := I;
          if MATTRIX (I, COL) then
            N := N + 1;
            exit when N = R;
          end if;
        end loop;
      end;

      -- play: for each TRUE in sigma, change corresponding col in selected row
      for I in COL .. POWER_RANGE'LAST loop
        if SIGMA (I) then
           MATTRIX (SELECTED_ROW, I) := not MATTRIX (SELECTED_ROW, I);
        end if;
      end loop;

      -- Compute new amount 
      NB_BARS := 0;
      for J in POWER_RANGE loop
        if MATTRIX (SELECTED_ROW, POWER_RANGE'LAST - J + 1) then
          NB_BARS := NB_BARS + 2 ** (J - 1);
        end if;
      end loop;

    else

      -- Loosing : play random
      -- Choose a non empty row
      declare
        -- number of ROWS with SUM /= 0
        R, N : NATURAL;
      begin
        -- Compute R
        R := 0;
        for I in COMMON.ROW_RANGE loop
          if SUMS (I) /= 0 then
            R := R + 1;
          end if;
        end loop;
        R := RND.INT_RANDOM (1, R);
        -- Select row, the Rth matching
        N := 0;
        for I in COMMON.ROW_RANGE loop
          SELECTED_ROW := I;
          if SUMS (I) /= 0 then
            N := N + 1;
            exit when N = R;
          end if;
        end loop;
      end;

      -- Number of bars to remove
      declare
        R : FLOAT;
        -- The idea is: the more bars there is, the more we tend to remove
        FACTOR : constant FLOAT := FLOAT(COMMON.BAR_PER_ROW(SELECTED_ROW))
                                 / FLOAT(SUMS (SELECTED_ROW));
      begin
       -- 1.0 <= R < N + 1.0
       R := RND.FLOAT_RANDOM (1.0, FLOAT(SUMS (SELECTED_ROW)) + 1.0);
       R := R / FACTOR;
       NB_BARS := INTEGER(MATH.TRUNC(MATH.REAL(R)));
      end;
      -- Eh! Play!
      if NB_BARS = 0 then
        NB_BARS := 1;
      elsif NB_BARS > SUMS (SELECTED_ROW) then
        -- Protection, should not enter here, but doesn't hurt :-)
        NB_BARS := SUMS (SELECTED_ROW);
      end if;

      -- Number of bars to leave
      NB_BARS := SUMS (SELECTED_ROW) - NB_BARS;
    end if;
    BARS := NB_BARS;
    ROW := SELECTED_ROW;

    -- Recompute number of bars
    SUMS_SUMS := SUMS_SUMS - SUMS(SELECTED_ROW) + NB_BARS;

    -- Finished ?
    if SUMS_SUMS = 0 then
      if GAME = COMMON.NIM then
        RESULT := PLAYED_AND_WON;
      else
        RESULT := PLAYED_AND_LOST;
      end if;
    end if;

  end PLAY;

end COMPUTE;
