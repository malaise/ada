with RND, SORTS;
with MY_IO;

package body EURISTIC is

  -- State of a zero in zero descriptors
  -- Used also in zero transfer
  type ZERO_STATE_LIST is (FREE, SLASHED, SQUARED);
  -- transfer of state of zeros after euristic (for reduction or result)
  -- No zero are free at this point, so this state is used to describe non-zero cell
  type ZERO_TRANSFER_TAB is array (TYPES.INDEX_RANGE range <>, TYPES.INDEX_RANGE range <>) of ZERO_STATE_LIST;
  -- Have at least 1 zero/row and 1 zero/col
  procedure INIT_SEARCH (MATTRIX : in out TYPES.MATTRIX_REC) is
      LOWEST : NATURAL;
  begin
    -- One zero in each row
    for ROW in 1 .. MATTRIX.DIM loop
      -- Search lowest elem of row
      LOWEST := TYPES.CELL_RANGE'LAST;
      for COL in 1 .. MATTRIX.DIM loop
        if MATTRIX.NOTES(ROW, COL) < LOWEST then LOWEST := MATTRIX.NOTES(ROW, COL); end if;
      end loop;
      -- Substract it from each element
      if LOWEST /= 0 then
        for COL in 1 .. MATTRIX.DIM loop
          MATTRIX.NOTES(ROW, COL)  := MATTRIX.NOTES(ROW, COL) - LOWEST;
        end loop;
      end if;
    end loop;

    -- One zero in each COL
    for COL in 1 .. MATTRIX.DIM loop
      -- Search lowest elem of col
      LOWEST := TYPES.CELL_RANGE'LAST;
      for ROW in 1 .. MATTRIX.DIM loop
        if MATTRIX.NOTES(ROW, COL) < LOWEST then LOWEST := MATTRIX.NOTES(ROW, COL); end if;
      end loop;
      -- Substract it from each element
      if LOWEST /= 0 then
        for ROW in 1 .. MATTRIX.DIM loop
          MATTRIX.NOTES(ROW, COL)  := MATTRIX.NOTES(ROW, COL) - LOWEST;
        end loop;
      end if;
    end loop;
  end INIT_SEARCH;

  -- Try to find a euristic solution
  -- If OK, DONE is set to TRUE
  -- Else, TRANSFER is set for reduction 
  procedure EURISTIC_SEARCH (MATTRIX : in TYPES.MATTRIX_REC;
   DONE : out BOOLEAN; TRANSFER : out ZERO_TRANSFER_TAB) is

    -- For each square with 0, keep its pos and the number of zeros in
    -- its row and col
    type ZERO_DESC_REC is record
      ROW : TYPES.INDEX_RANGE;
      COL : TYPES.INDEX_RANGE;
      SIGMA : POSITIVE;
      STATE : ZERO_STATE_LIST := FREE;
    end record;
    -- To sort zero dscriptors
    function "<" (LEFT, RIGHT : ZERO_DESC_REC) return BOOLEAN;
  
    type ZERO_DESC_TAB_GEN is array (POSITIVE range <>) of ZERO_DESC_REC;

    package ZERO_DESC_SORT is new SORTS (
     TYP_OBJECT => ZERO_DESC_REC,
     TYP_INDEX  => POSITIVE,
     "<"        => "<",
     TYP_ARRAY  => ZERO_DESC_TAB_GEN);

    -- Index in mattrix
    subtype INDEX_RANGE is TYPES.INDEX_RANGE range 1 .. MATTRIX.DIM;
    -- ZERO_DESC_REC for all the mattrix
    subtype ZERO_DESC_RANGE is POSITIVE range 1 .. MATTRIX.DIM * MATTRIX.DIM;
    subtype ZERO_DESC_TAB is ZERO_DESC_TAB_GEN(ZERO_DESC_RANGE);
    ZERO_DESC : ZERO_DESC_TAB;
    NB_ZERO : NATURAL range 0 .. ZERO_DESC_RANGE'LAST;
    -- Local copy of DONE
    LOC_DONE : BOOLEAN;

    -- Instantiate sorting
    -- Free zeros first, in order of crescent SIGMA, then slashed then squared zeros
    function "<" (LEFT, RIGHT : ZERO_DESC_REC) return BOOLEAN is
    begin
      if LEFT.STATE = FREE then
        if RIGHT.STATE /= FREE then
          -- LEFT FREE and not RIGHT
          return TRUE;
        else
          -- LEFT and RIGHT FREE
          return LEFT.SIGMA < RIGHT.SIGMA;
        end if;
      else
        return LEFT.STATE = SLASHED and then RIGHT.STATE = SQUARED;
      end if;
    end "<";

  begin

    INIT_ZEROS:
    declare
      -- Number of zeros/row, number of zeros/col
      ZERO_ROW : array (1 .. MATTRIX.DIM) of NATURAL := (others => 0);
      ZERO_COL : array (1 .. MATTRIX.DIM) of NATURAL := (others => 0);
    begin
      -- Count zeros in rows and cols and store zeros in desc
      -- Sigmas are unknown yet
      NB_ZERO := 0;
      for ROW in INDEX_RANGE loop
        for COL in INDEX_RANGE loop
          if MATTRIX.NOTES(ROW, COL) = 0 then
            -- Number of zeros in this row and col
            ZERO_ROW(ROW) := ZERO_ROW(ROW) + 1;
            ZERO_COL(COL) := ZERO_COL(COL) + 1;
            -- Descriptor of the zero: all is set but sigma
            NB_ZERO := NB_ZERO + 1;
            ZERO_DESC(NB_ZERO).ROW := ROW;
            ZERO_DESC(NB_ZERO).COL := COL;
            ZERO_DESC(NB_ZERO).STATE := FREE;
          end if;
        end loop;
      end loop;

      -- Set sigmas of zero descriptors
      for NO_ZERO in 1 .. NB_ZERO loop
        -- Number of zeros in its row + col
        ZERO_DESC(NO_ZERO).SIGMA := ZERO_ROW(ZERO_DESC(NO_ZERO).ROW)
                                  + ZERO_COL(ZERO_DESC(NO_ZERO).COL);
      end loop;
    end INIT_ZEROS;

    -- Init random serial list
    RND.RANDOMIZE;

    EURISTIC_LOOP:
    loop
    
      -- Sort the list of zeros
      SORT_ZERO_DESC:
      begin
        ZERO_DESC_SORT.QUICK_SORT(ZERO_DESC(1 .. NB_ZERO));
      end SORT_ZERO_DESC;

      TRY_TO_SOLVE:
      declare
        -- Mattrix of zeros : each zero has either an index in zero desc or 0 if content of MATTRIX is not zero
        CROSS_REF : array (1 .. MATTRIX.DIM, 1 .. MATTRIX.DIM) of NATURAL := (others => (others => 0));
        -- The zero selected (one of the zeros with min sigma)
        SELECTED_ZERO : POSITIVE;
        -- Number of zeros with min zigma
        NB_MIN_ZERO : POSITIVE := 1;
        -- Index in ZERO_DESC
        INDEX_DESC : NATURAL;
        -- Row and column index storage
        ROW_TMP, COL_TMP : INDEX_RANGE;
        -- Is ther any zero free remaining
        ZERO_FREE_REMAINING : BOOLEAN;
      begin
        -- Set the cross reference mattrix
        for I in 1 .. NB_ZERO loop
          CROSS_REF(ZERO_DESC(I).ROW, ZERO_DESC(I).COL) := I;
        end loop;

        -- Count the number of zeros which are free and have the SIGMA minimum
        for I in 2 .. NB_ZERO loop
          exit when ZERO_DESC(I).STATE /= FREE or else ZERO_DESC(I).SIGMA /= ZERO_DESC(1).SIGMA;
          NB_MIN_ZERO := I;
        end loop;

        -- Select one randomly
        SELECTED_ZERO := RND.INT_RANDOM (1, NB_MIN_ZERO);
        ZERO_DESC(SELECTED_ZERO).STATE := SQUARED;

        -- Propagate the choice
        -- Sigma of slashed and squared zeros are not used
        -- Slash all zero of the same row and keep up to date the sigma of zeros of their columns
        for COL in INDEX_RANGE loop
          INDEX_DESC := CROSS_REF(ZERO_DESC(SELECTED_ZERO).ROW, COL);
          if INDEX_DESC /= 0 and then ZERO_DESC(INDEX_DESC).STATE = FREE then
            ZERO_DESC(INDEX_DESC).STATE := SLASHED;
            COL_TMP := ZERO_DESC(INDEX_DESC).COL;
            for ROW in INDEX_RANGE loop
              INDEX_DESC := CROSS_REF(ROW, COL_TMP);
              if INDEX_DESC /= 0 and then ZERO_DESC(INDEX_DESC).STATE = FREE then
                ZERO_DESC(INDEX_DESC).SIGMA := ZERO_DESC(INDEX_DESC).SIGMA - 1;
              end if;
            end loop;
          end if;
        end loop;
        -- Slash all zero of the same col and keep up to date the sigma of zeros of their rows
        for ROW in INDEX_RANGE loop
          INDEX_DESC := CROSS_REF(ROW, ZERO_DESC(SELECTED_ZERO).COL);
          if INDEX_DESC /= 0 and then ZERO_DESC(INDEX_DESC).STATE = FREE then
            ZERO_DESC(INDEX_DESC).STATE := SLASHED;
            ROW_TMP := ZERO_DESC(INDEX_DESC).ROW;
            for COL in INDEX_RANGE loop
              INDEX_DESC := CROSS_REF(ROW_TMP, COL);
              if INDEX_DESC /= 0 and then ZERO_DESC(INDEX_DESC).STATE = FREE then
                ZERO_DESC(INDEX_DESC).SIGMA := ZERO_DESC(INDEX_DESC).SIGMA - 1;
              end if;
            end loop;
          end if;
        end loop;

        -- Count nb of free zero remaining.
        ZERO_FREE_REMAINING := FALSE;
        for I in 1 .. NB_ZERO loop
          if ZERO_DESC(I).STATE = FREE then
            ZERO_FREE_REMAINING := TRUE;
            exit;
          end if;
        end loop;

        -- Euristic search done when no more zero free
        exit EURISTIC_LOOP when not ZERO_FREE_REMAINING;
      end TRY_TO_SOLVE;

    end loop EURISTIC_LOOP;

    TEST_DONE:
    declare
      -- Number of squared zeros
      NB_SQUARED_ZEROS : TYPES.INDEX_RANGE := 0;
    begin
      -- Done if the number of squared zeros is DIM
      for I in 1 .. NB_ZERO loop
        if ZERO_DESC(I).STATE = SQUARED then
          NB_SQUARED_ZEROS := NB_SQUARED_ZEROS + 1;
        end if;
      end loop;
      LOC_DONE :=  NB_SQUARED_ZEROS = MATTRIX.DIM;
      DONE := LOC_DONE;
    end TEST_DONE;

    -- Set zero transfer tab (free means not a zero)
    TRANSFER := (others => (others => FREE));
    for I in 1 .. NB_ZERO loop
      -- No zero free remain in ZERO_DESC
      TRANSFER(ZERO_DESC(I).ROW, ZERO_DESC(I).COL) := ZERO_DESC(I).STATE;
    end loop;
    
  end EURISTIC_SEARCH;

  procedure REDUCE (MATTRIX : in out TYPES.MATTRIX_REC; TRANSFER : in ZERO_TRANSFER_TAB) is
    -- Index in mattrix
    subtype INDEX_RANGE is TYPES.INDEX_RANGE range 1 .. MATTRIX.DIM;
    -- Rows and colums marqued
    MARKED_ROW : array (INDEX_RANGE) of BOOLEAN := (others => FALSE);
    MARKED_COL : array (INDEX_RANGE) of BOOLEAN := (others => FALSE);
    -- Does the current row/col satify the criteria
    OK : BOOLEAN;
    -- At least one mark done
    ONE_MARK : BOOLEAN;
  begin
    -- Mark rows with no squared zero
    for ROW in INDEX_RANGE loop
      -- No squared zero ?
      OK := TRUE;
      for COL in INDEX_RANGE loop
        if TRANSFER(ROW, COL) = SQUARED then
          -- Squared zero
          OK := FALSE;
          exit;
        end if;
      end loop;
      if OK then
        MARKED_ROW(ROW) := TRUE;
      end if;
    end loop;

    -- Iterative marking of rows and cols
    MARKING:
    loop

      ONE_MARK := FALSE;

      -- For each marked row, mark each col with a slashed zero in this row
      for ROW in INDEX_RANGE loop
        if MARKED_ROW(ROW) then
          -- Mark each col with a slashed zero in this row
          for COL in INDEX_RANGE loop
            if TRANSFER(ROW, COL) = SLASHED and then not MARKED_COL(COL) then
              MARKED_COL(COL) := TRUE;
              ONE_MARK := TRUE;
            end if;
          end loop;
        end if;
      end loop;

      -- For each marked col, mark each row with a squared zero in this col
      for COL in INDEX_RANGE loop
        if MARKED_COL(COL) then
          -- Mark each row with a squared zero in this col
          for ROW in INDEX_RANGE loop
            if TRANSFER(ROW, COL) = SQUARED and then not MARKED_ROW(ROW) then
              MARKED_ROW(ROW) := TRUE;
              ONE_MARK := TRUE;
            end if;
          end loop;
        end if;
      end loop;

      exit MARKING when not ONE_MARK;
    end loop MARKING;

    SUB_LOWEST:
    declare
      LOWEST : TYPES.CELL_RANGE := TYPES.CELL_RANGE'LAST;
    begin
      -- Search lowest value amoung marked lines and not marked columns
      for ROW in INDEX_RANGE loop
        if MARKED_ROW(ROW) then
          for COL in INDEX_RANGE loop
            if not MARKED_COL(COL) and then MATTRIX.NOTES(ROW, COL) < LOWEST then
              LOWEST:= MATTRIX.NOTES(ROW, COL);
            end if;
          end loop;
        end if;
      end loop;

      -- Substract LOWEST from cells which row     marked and col not marked
      -- Add       LOWEST to   cells which row not marked and col     marked
      for ROW in INDEX_RANGE loop
        for COL in INDEX_RANGE loop
          if MARKED_ROW(ROW) and then not MARKED_COL(COL) then
            MATTRIX.NOTES(ROW, COL) := MATTRIX.NOTES(ROW, COL) - LOWEST;
          elsif not MARKED_ROW(ROW) and then MARKED_COL(COL) then
            -- Add only if MATTRIX.NOTES(ROW, COL) + LOWEST <= CELL_RANGE'LAST
            if TYPES.CELL_RANGE'LAST - MATTRIX.NOTES(ROW, COL) >= LOWEST then
              MATTRIX.NOTES(ROW, COL) := MATTRIX.NOTES(ROW, COL) + LOWEST;
            end if;
          end if;
        end loop;
      end loop;
    end SUB_LOWEST;

  end REDUCE;
    
  procedure SEARCH (MATTRIX : in out TYPES.MATTRIX_REC; NB_ITERATIONS : out POSITIVE) is
    DONE : BOOLEAN;
    TRANSFER : ZERO_TRANSFER_TAB (1 .. MATTRIX.DIM, 1 .. MATTRIX.DIM);
    NB_LOOP  : POSITIVE := 1;
  begin
    -- Init for search : one zero/row and / col
    INIT_SEARCH (MATTRIX);
    loop
       MY_IO.PUT (".");
       MY_IO.FLUSH;
       -- Try to search
       EURISTIC_SEARCH (MATTRIX, DONE, TRANSFER);
       exit when DONE;
       -- Euristic failed : reduce
       REDUCE (MATTRIX, TRANSFER);
       NB_LOOP := NB_LOOP + 1;
    end loop;
    MY_IO.NEW_LINE;
    -- Euristic success: set mattrix to 1 (affected) or 0 (not affected) 
    -- Affected if TRANSFER is squared
    for ROW in 1 .. MATTRIX.DIM loop
      for COL in 1 .. MATTRIX.DIM loop
        if TRANSFER(ROW, COL) = SQUARED then
          MATTRIX.NOTES(ROW, COL) := 1;
        else
          MATTRIX.NOTES(ROW, COL) := 0;
        end if;
      end loop;
    end loop;
    NB_ITERATIONS := NB_LOOP;
  end SEARCH;


end EURISTIC;

