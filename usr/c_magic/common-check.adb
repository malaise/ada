separate (COMMON)

-- Supposing that CHECK(N-1) is true,
-- check if, up to N, the square is potentialy magic
function CHECK (N : LEN_RANGE) return BOOLEAN is
  SUM : NATURAL;
  NM1 : constant NATURAL := N - 1;
begin

  -- Check last (even incomplete) line
  declare
    OFFSET : constant NATURAL := (NM1 / DIM) * DIM;
  begin
    -- Compute sum of line
    SUM := 0;
    for COL in 1 .. NM1 rem DIM  + 1 loop
      SUM := SUM + LIS(OFFSET + COL);
    end loop;
  end;
  if NM1 rem DIM = DM1 then
    -- Last line if it is complete
    if SUM /= SIGMA then
      -- Wrong sum for this line. Test fails.
      return FALSE;
    end if;
  else
    -- Incomplete last line
    if SUM >= SIGMA then
      -- Sum already too big for this line. Test fails.
      return FALSE;
    end if;
  end if;

  if NM1 = LEN - DIM then
    -- Only first column completed
    -- Check diag 1,n .. n,1 (COL = DIM - LIN + 1)
    SUM := 0;
    for LIN in 1 .. DIM loop
      SUM := SUM + LIS(LIN * DIM - LIN + 1);
    end loop;
    if SUM /= SIGMA then
      return FALSE;
    end if;
  elsif NM1 < LEN - DIM then
    -- No column completed
    if N > DIM then
      -- Last column has at least 2 rows
      declare
        -- Top of this column
        OFFSET : constant NATURAL := NM1 rem DIM + 1;
      begin
        SUM := 0;
        for LIN in 0 .. NM1 / DIM loop
          SUM := SUM + LIS(LIN * DIM + OFFSET);
        end loop;
      end;
      -- No column completed. Check last (incomplete) column
      if SUM >= SIGMA then
        -- Sum already too big for this column. Test fails.
        return FALSE;
      else
        -- No more check possible
        return TRUE;
      end if;
    else
      -- Last column does not have 2 rows: no more check possible
      return TRUE;
    end if;
  end if;

  -- Some columns completed.
  -- Check last column (it is complete)
  declare
    COL : constant DIM_RANGE := ((N-1) rem DIM) + 1;
  begin
    SUM := 0;
    for LIN in 1 .. DIM loop
      SUM := SUM + LIS(LIN * DIM - DIM + COL);
    end loop;
  end;
  if SUM /= SIGMA then
    return FALSE;
  end if;

  if N /= LEN then
    -- No second diag (last column not complete)
    return TRUE;
  end if;

  -- Check diag 1,1 .. n,n (COL = LIN)
  SUM := 0;
  for LIN in 1 .. DIM loop
    SUM := SUM + LIS(LIN * DIM - DIM + LIN);
  end loop;
  if SUM /= SIGMA then
    return FALSE;
  end if;

  -- This square is magic
  return TRUE;

end CHECK;
