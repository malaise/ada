package body Limited_Pool is

  -- type Pool_Type is tagged limited private;

  -- Check if pool is not empty, get number of elements in pool
  function Is_Empty (Pool : in Pool_Type) return Boolean is
  begin
    return Pool.Pool.Is_Empty;
  end Is_Empty;

  function Length (Pool : in Pool_Type) return Natural is
  begin
    return Natural (Pool.Pool.Length);
  end Length;

  -- Add in pool
  procedure Push (Pool : in out Pool_Type; Data : in Data_Type) is
  begin
   if Pool.Length = Natural'Last then
      raise Pool_Full;
    end if;
    Pool.Pool.Push (Data);
  end Push;

  -- Get from pool last pushed (Lifo) or first pushed (Fifo)
  procedure Pop (Pool : in out Pool_Type; Data : out Data_Type) is
  begin
    Pool.Pool.Pop (Data);
  end Pop;

  procedure Pop (Pool : in out Pool_Type) is
  begin
    Pool.Pool.Pop;
  end Pop;

  -- Read from pool last pushed (Lifo) or first pushed (Fifo)
  procedure Front (Pool : in out Pool_Type; Data : out Data_Type) is
  begin
    Pool.Pool.Front (Data);
  end Front;

  -- Clear the pool (deallocates)
  procedure Clear (Pool : in out Pool_Type) is
  begin
    Pool.Pool.Clear;
  end Clear;

end Limited_Pool;

