--  type Data_Type is limited private;
--  type Data_Access_Type is access Data_Type;

package body Unlimited_Pool is

  procedure Set (To : out Data_Type; Val : in Data_Type) is
  begin
    To := Val;
  end Set;

  -- Check if pool is not empty, get number of elements in pool
  function Is_Empty (Pool : in Pool_Type) return Boolean is
  begin
    -- The one of the list
    return Pool_List_Mng.List_Type(Pool).Is_Empty;
  end Is_Empty;

  function Length (Pool : in Pool_Type) return Natural is
  begin
    return Pool.List_Length;
  end Length;


  -- Add in beginning of list
  procedure Push (Pool : in out Pool_Type; Data : in Data_Type) is
  begin
    Pool.Insert(Data, Pool_List_Mng.Prev);
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
    if not Lifo then
      -- Fifo means pop last (and go to previous) then rewind to first
      Pool.Rewind (True, Pool_List_Mng.Prev);
      Pool.Get (Data, Pool_List_Mng.Prev);
      Pool.Rewind (False);
    else
      -- Lifo means pop first and move to next
      Pool.Get(Data);
    end if;
  end Pop;

  procedure Pop (Pool : in out Pool_Type) is
  begin
    if Pool.Is_Empty then
      raise Empty_Pool;
    end if;
    if not Lifo then
      -- Fifo means pop last (and go to previous) then rewind to first
      Pool.Rewind (True, Pool_List_Mng.Prev);
      Pool.Delete (Pool_List_Mng.Prev);
      Pool.Rewind (False);
    else
      -- Lifo means pop first and move to next
      Pool.Delete;
    end if;
  end Pop;

  -- Read from pool, remain at current pos
  procedure Read (Pool : in out Pool_Type; Data : out Data_Type) is
  begin
    if Pool.Is_Empty then
      raise Empty_Pool;
    end if;
    if not Lifo then
      -- Fifo means read last then rewind to first
      Pool.Rewind (True, Pool_List_Mng.Prev);
      Pool.Read (Data, Pool_List_Mng.Current);
      Pool.Rewind (True);
    else
      -- Lifo means read first
      Pool.Read (Data, Pool_List_Mng.Current);
    end if;
  end Read;

  -- Clear the pool
  procedure Clear (Pool : in out Pool_Type) is
  begin
    Pool.Delete_List;
  end Clear;

end Unlimited_Pool;

