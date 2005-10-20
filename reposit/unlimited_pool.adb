--  type Data_Type is limited private;
--  type Data_Access_Type is access Data_Type;

package body Unlimited_Pool is

  procedure Set (To : out Data_Type; Val : in Data_Type) is
  begin
    To := Val;
  end Set;

  -- Check if pool is not empty, get number og elements in pool
  function Is_Empty (Pool : in Pool_Type) return Boolean is
  begin
    return Pool_List_Mng.Is_Empty(Pool_List_Mng.List_Type(Pool));
  end Is_Empty;

  function Length (Pool : in Pool_Type) return Natural is
  begin
    return Pool_List_Mng.List_Length(Pool_List_Mng.List_Type(Pool));
  end Length;

  -- Add in beginning of list
  procedure Push (Pool : in out Pool_Type; Data : in Data_Type) is
  begin
    Pool_List_Mng.Insert(Pool_List_Mng.List_Type(Pool),
                         Data,
                         Pool_List_Mng.Prev);
  exception
    when Pool_List_Mng.Full_List =>
      raise Pool_Full;
  end Push;

  -- Get from pool, move to next
  procedure Pop (Pool : in out Pool_Type; Data : out Data_Type) is
  begin
    if Is_Empty(Pool) then
      raise Empty_Pool;
    else
      Pool_List_Mng.Get(Pool_List_Mng.List_Type(Pool), Data);
    end if;
  end Pop;

end Unlimited_Pool;

