with TEXT_IO; with CALENDAR;
with NORMAL, TEXT_HANDLER, SORTS;
package body COMMON is
  -- The dimension of square
  DIM : DIM_RANGE;
  DM1 : NATURAL;

  -- Array of content of square
  MAX_LEN : constant := MAX_DIM * MAX_DIM;
  subtype LEN_RANGE is POSITIVE range 1 .. MAX_LEN;

  -- Curent length of array, current array
  LEN : LEN_RANGE;
  type LIS_ARRAY is array (LEN_RANGE range <>) of LEN_RANGE;
  LIS : LIS_ARRAY (LEN_RANGE);

  -- Expected sum of each row, column and diag
  SIGMA : POSITIVE;
  -- Number of magic squares found
  NB_SQUARE : NATURAL;

  -- Output file
  FILE : TEXT_IO.FILE_TYPE;
  FILE_NAME : TEXT_HANDLER.TEXT(80);

  -- Real -> integer : round or trunc
  function TRUNC (X : in FLOAT) return INTEGER is
    INT : INTEGER;
  begin
    INT := INTEGER (X);
    -- Adjust to 1
    if X > 0.0 then
      -- If x>0 error is 1 too much
      if FLOAT (INT) > X then INT := INT - 1; end if;
      return INT;
    else
      -- If x<0 error is 1 too less
      if FLOAT (INT) < X then INT := INT + 1; end if;
      return INT;
    end if;
  exception
    when others => raise CONSTRAINT_ERROR;
  end TRUNC;

  function ROUND (X : in FLOAT) return INTEGER is
    RESULTAT : INTEGER;
  begin
    if X > 0.0 then
      RESULTAT := TRUNC  (X + 0.5);
    else
      RESULTAT := TRUNC  (X - 0.5);
    end if;
    return RESULTAT;
  exception
    when others => raise CONSTRAINT_ERROR;
  end ROUND;

  function FRAC (X : in FLOAT) return FLOAT is
  begin
    return X - FLOAT(TRUNC(X));
  end FRAC;

  -- Recursive procedure to try a a level
  procedure TRY (CUR : LEN_RANGE);

  -- Init of array and file and start first try
  procedure SEARCH (DIM : in DIM_RANGE) is
    START_TIME : CALENDAR.TIME;
    SEARCH_DURATION : FLOAT;
    use CALENDAR;
  begin
    COMMON.DIM := DIM;
    DM1 := DIM - 1;

    -- Compute len and sigma : sigma := (len * (len+1)) / 2 / dim
    LEN := DIM * DIM;
    SIGMA := (DIM * (LEN + 1) ) / 2;

    -- Initialise array
    for I in 1 .. LEN loop
      LIS(I) := I;
    end loop;
    NB_SQUARE := 0;

    TEXT_HANDLER.SET(FILE_NAME, NORMAL (DIM, 1, TRUE) & "_MAGIC.DAT");
    begin
      TEXT_IO.OPEN (FILE, TEXT_IO.OUT_FILE, TEXT_HANDLER.VALUE(FILE_NAME));
    exception
      when TEXT_IO.NAME_ERROR =>
        TEXT_IO.CREATE (FILE, TEXT_IO.OUT_FILE, TEXT_HANDLER.VALUE(FILE_NAME));
    end;

    -- Start searching
    START_TIME := CALENDAR.CLOCK;
    TRY(1);
    SEARCH_DURATION := FLOAT (CALENDAR.CLOCK - START_TIME);

    -- Done
    TEXT_IO.PUT_LINE (NATURAL'IMAGE(NB_SQUARE)
                   &  " squares of "
                   & DIM_RANGE'IMAGE(DIM)
                   & " found in "
                   & NATURAL'IMAGE(TRUNC(SEARCH_DURATION))
                   & "."
                   & NORMAL (ROUND(FRAC(SEARCH_DURATION) * 1000.0), 3, GAP => '0')
                   & " s." );

    TEXT_IO.CLOSE(FILE);
  end SEARCH;

  -- Check if, up to N, the array content may be a magic square
  function CHECK (N : LEN_RANGE) return BOOLEAN is separate;

  -- Display and log array (square) content
  procedure DUMP;

  -- To sort a part of the array
  package SORT is new SORTS (
   TYP_OBJECT => LEN_RANGE,
   TYP_INDEX  => LEN_RANGE,
   "<"        => "<",
   TYP_ARRAY  => LIS_ARRAY);

  -- Supposing that LIS is sorted from CUR to LEN
  -- Tries all possibilities of numbers in LIS(CUR) .. LIS (LEN)
  procedure TRY (CUR : LEN_RANGE) is
    REST : LEN_RANGE;
    TMP : LEN_RANGE;
  begin

    if CUR = LEN then
      -- Square is complete
      if CHECK(LEN) then
        -- Square is complete and magic
        NB_SQUARE := NB_SQUARE + 1;
        DUMP;
      end if;
      -- No more possibility
      return;
    end if;


    -- We will put, at CUR position, one after one, each  number of
    -- LIS(CUR) .. LIS(LEN)
    REST := CUR;

    loop

      -- No change for try with REST= CUR : LIS(CUR)
      if REST /= CUR then
        -- Exchange LIS(CUR) <-> LIS(REST)
        TMP := LIS(CUR);
        LIS(CUR) := LIS(REST);
        LIS(REST) := TMP;

        -- Sort CUR+1 .. LEN for try at CUR+1
        SORT.QUICK_SORT(LIS(CUR+1 .. LEN));
      end if;

      if CHECK(CUR) then
        -- Optim : try next level if and only if current level is convenient
        TRY(CUR+1);
        -- Done for CUR if REST=LEN
        exit when REST = LEN;
        -- Optim : sort only if try has been done
        -- Sort CUR+1 .. LEN for next exchange
        SORT.QUICK_SORT(LIS(CUR+1 .. LEN));
      else
        -- Done for CUR if REST=LEN
        exit when REST = LEN;
      end if;

      -- Next REST
      REST := REST + 1;

    end loop;

  end TRY;

  procedure DUMP is
    I : LEN_RANGE;
    J : NATURAL;
  begin
    -- Dump lines
    for LIN in 1 .. DIM loop
      J := LIN * DIM - DIM;
      for COL in 1 .. DIM loop
        I := LIS(J + COL);
        TEXT_IO.PUT (NORMAL(I, 2) & " ");
        TEXT_IO.PUT (FILE, NORMAL(I, 2) & " ");
      end loop;
      TEXT_IO.NEW_LINE;
      TEXT_IO.NEW_LINE (FILE);
    end loop;
    TEXT_IO.NEW_LINE;
    TEXT_IO.NEW_LINE(FILE);
  end DUMP;


end COMMON;

