package body Limited_Pool is

  -- Check if pool is not empty, get number of elements in pool
  function Is_Empty (Pool : in Pool_Type) return Boolean is
    (Pool.Pool.Is_Empty);

  function Length (Pool : in Pool_Type) return Natural is
    (Natural (Pool.Pool.Length));

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

  function Pop (Pool : in out Pool_Type) return Data_Type is (Pool.Pool.Pop);

  procedure Pop (Pool : in out Pool_Type) is
  begin
    Pool.Pool.Pop;
  end Pop;

  -- Read from pool last pushed (Lifo) or first pushed (Fifo)
  procedure Look (Pool : in out Pool_Type; Data : out Data_Type) is
  begin
    Pool.Pool.Look (Data);
  end Look;

  function  Look (Pool : in out Pool_Type) return Data_Type is
  begin
    return Data : Data_Type do
      Look (Pool, Data);
    end return;
  end Look;

  -- Clear the pool (deallocates)
  procedure Clear (Pool : in out Pool_Type) is
  begin
    Pool.Pool.Clear;
  end Clear;

end Limited_Pool;

