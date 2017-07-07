package body Long_Long_Limited_Pool is

  -- Check if pool is not empty, get number of elements in pool
  function Is_Empty (Pool : in Pool_Type) return Boolean is
    -- The one of the list
    (Pool_List_Mng.List_Type(Pool).Is_Empty);

  function Length (Pool : in Pool_Type) return Ll_Natural is (Pool.List_Length);

  -- Add in beginning of list
  procedure Push (Pool : in out Pool_Type; Data : in Data_Type) is
  begin
    Pool.Insert (Data, Pool_List_Mng.Prev);
  exception
    when Pool_List_Mng.Full_List =>
      raise Pool_Full;
  end Push;

  -- Get from pool, move to next
  procedure Pop (Pool : in out Pool_Type; Data : out Data_Type) is
  begin
    if Pool.Is_Empty then
      raise Empty_Pool;
    end if;
    if Lifo then
      -- Lifo means pop first and move to next
      Pool.Get(Data);
    else
      -- Fifo means pop last (and go to previous) then rewind to first
      Pool.Rewind (Pool_List_Mng.Prev);
      Pool.Get (Data, Pool_List_Mng.Prev);
      Pool.Rewind (Check_Empty => False);
    end if;
  end Pop;

  function Pop (Pool : in out Pool_Type) return Data_Type is
  begin
    return Data : Data_Type do
      Pop (Pool, Data);
    end return;
  end Pop;

  procedure Pop (Pool : in out Pool_Type) is
    Dummy_Data : Data_Type;
  begin
    Pop (Pool, Dummy_Data);
  end Pop;

  -- Read from pool, remain at current pos
  procedure Look (Pool : in out Pool_Type; Data : out Data_Type) is
  begin
    if Pool.Is_Empty then
      raise Empty_Pool;
    end if;
    if Lifo then
      -- Lifo means read first
      Pool.Read (Data, Pool_List_Mng.Current);
    else
      -- Fifo means read last then rewind to first
      Pool.Rewind (Pool_List_Mng.Prev);
      Pool.Read (Data, Pool_List_Mng.Current);
      Pool.Rewind;
    end if;
  end Look;

  function  Look (Pool : in out Pool_Type) return Data_Type is
  begin
    return Data : Data_Type do
      Look (Pool, Data);
    end return;
  end Look;


  -- Clear the pool
  procedure Clear (Pool : in out Pool_Type) is
  begin
    Pool.Delete_List;
  end Clear;

end Long_Long_Limited_Pool;

