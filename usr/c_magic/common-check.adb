separate (Common)

-- Supposing that Check(N-1) is true,
-- check if, up to N, the square is potentialy magic
function Check (N : Len_Range) return Boolean is
  Sum : Natural;
  Nm1 : constant Natural := N - 1;
begin

  -- Check last (even incomplete) line
  declare
    Offset : constant Natural := (Nm1 / Dim) * Dim;
  begin
    -- Compute sum of line
    Sum := 0;
    for Col in 1 .. Nm1 rem Dim  + 1 loop
      Sum := Sum + Lis(Offset + Col);
    end loop;
  end;
  if Nm1 rem Dim = Dm1 then
    -- Last line if it is complete
    if Sum /= Sigma then
      -- Wrong sum for this line. Test fails.
      return False;
    end if;
  else
    -- Incomplete last line
    if Sum >= Sigma then
      -- Sum already too big for this line. Test fails.
      return False;
    end if;
  end if;

  if Nm1 = Len - Dim then
    -- Only first column completed
    -- Check diag 1,n .. n,1 (Col = Dim - Lin + 1)
    Sum := 0;
    for Lin in 1 .. Dim loop
      Sum := Sum + Lis(Lin * Dim - Lin + 1);
    end loop;
    if Sum /= Sigma then
      return False;
    end if;
  elsif Nm1 < Len - Dim then
    -- No column completed
    if N > Dim then
      -- Last column has at least 2 rows
      declare
        -- Top of this column
        Offset : constant Natural := Nm1 rem Dim + 1;
      begin
        Sum := 0;
        for Lin in 0 .. Nm1 / Dim loop
          Sum := Sum + Lis(Lin * Dim + Offset);
        end loop;
      end;
      -- No column completed. Check last (incomplete) column
      if Sum >= Sigma then
        -- Sum already too big for this column. Test fails.
        return False;
      else
        -- No more check possible
        return True;
      end if;
    else
      -- Last column does not have 2 rows: no more check possible
      return True;
    end if;
  end if;

  -- Some columns completed.
  -- Check last column (it is complete)
  declare
    Col : constant Dim_Range := ((N-1) rem DiM) + 1;
  begin
    Sum := 0;
    for Lin in 1 .. Dim loop
      Sum := Sum + Lis(Lin * Dim - Dim + Col);
    end loop;
  end;
  if Sum /= Sigma then
    return False;
  end if;

  if N /= Len then
    -- No second diag (last column not complete)
    return True;
  end if;

  -- Check diag 1,1 .. n,n (Col = Lin)
  Sum := 0;
  for Lin in 1 .. Dim loop
    Sum := Sum + Lis(Lin * Dim - Dim + Lin);
  end loop;
  if Sum /= Sigma then
    return False;
  end if;

  -- This square is magic
  return True;

end Check;

