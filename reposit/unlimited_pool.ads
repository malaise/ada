-- Generic simple unlimited pool of objects
-- Simply push data in pool, and pop data as long as it is not empty
with Limited_List;
generic
  type Data_Type is private;
package Unlimited_Pool is

  type Pool_Type is limited private; 

  -- Check if pool is not empty, get number og elements in pool
  function Is_Empty (Pool : in Pool_Type) return Boolean;
  function Length (Pool : in Pool_Type) return Natural;

  -- Add in pool
  Pool_Full : exception;
  procedure Push (Pool : in out Pool_Type; Data : in Data_Type);

  -- Get from pool and null if pool is empty
  Empty_Pool : exception;
  procedure Pop (Pool : in out Pool_Type; Data : out Data_Type);

private
  procedure Set (To : in out Data_Type; Val : in Data_Type);

  package Pool_List_Mng is new Limited_List (Data_Type, Set);
  type Pool_Type is new Pool_List_Mng.List_Type;

end Unlimited_Pool;

