-- Generic simple unlimited pool of objects
-- Simply push data in pool, and pop data as long as it is not empty
-- Default policy is Lifo (Last In First Out)
--  but Fifo (First In First Out) is also possible
with Limited_List;
generic
  type Data_Type is private;
  Lifo : Boolean := True;
package Unlimited_Pool is

  type Pool_Type is tagged limited private;

  -- Check if pool is not empty, get number of elements in pool
  function Is_Empty (Pool : in Pool_Type) return Boolean;
  function Length (Pool : in Pool_Type) return Natural;

  -- Add in pool
  Pool_Full : exception;
  procedure Push (Pool : in out Pool_Type; Data : in Data_Type);

  -- Get from pool last pushed (Lifo) or first pushed (Fifo)
  Empty_Pool : exception;
  procedure Pop (Pool : in out Pool_Type; Data : out Data_Type);
  procedure Pop (Pool : in out Pool_Type);

  -- Read from pool last pushed (Lifo) or first pushed (Fifo)
  procedure Read (Pool : in out Pool_Type; Data : out Data_Type);

  -- Clear the pool (deallocates)
  procedure Clear (Pool : in out Pool_Type);

private
  type Data_Type_Access is access all Data_Type;
  procedure Set (To : out Data_Type; Val : in Data_Type);

  package Pool_List_Mng is new Limited_List (Data_Type, Data_Type_Access, Set);
  type Pool_Type is new Pool_List_Mng.List_Type with null record;

end Unlimited_Pool;

