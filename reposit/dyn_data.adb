with Unlimited_Pool;
package body Dyn_Data is

  -- A list of Data_Access pointing to free data
  package Pool_Mng is new Unlimited_Pool (Data_Access_Type);
  Data_Pool : Pool_Mng.Pool_Type;

  function Allocate return Data_Access_Type is
    Returned_Data_Access : Data_Access_Type;
  begin
    -- Try to use a free data in the pool
    if not Pool_Mng.Is_Empty (Data_Pool) then
      -- Use data from the pool
      Pool_Mng.Pop (Data_Pool, Returned_Data_Access);
    else
      -- Allocate data
      Returned_Data_Access := new Data_Type;
    end if;
    return Returned_Data_Access;
  end Allocate;

  function Allocate (Data : Data_Type) return Data_Access_Type is
    Returned_Data_Access : Data_Access_Type;
  begin
    Returned_Data_Access := Allocate;
    Returned_Data_Access.all := Data;
    return Returned_Data_Access;
  end Allocate;

  procedure Free (Data_Access : in out Data_Access_Type) is
  begin
    -- Check if Data_Access is not null
    if Data_Access = null then
      raise Constraint_Error;
    end if;

    -- Insert this data (access) in pool
    Pool_Mng.Push (Data_Pool, Data_Access);
    -- Reset data access
    Data_Access := null;
  end Free;

  -- Clear the list of free data
  procedure Clear is
  begin
    Pool_Mng.Clear (Data_Pool);
  end Clear;

end Dyn_Data;

