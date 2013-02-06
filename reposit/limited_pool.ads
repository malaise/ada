-- Generic simple dynamic pool of limited private objects
-- Simply push data in pool, and pop data as long as it is not empty
-- Default policy is Lifo (Last In First Out)
--  but Fifo (First In First Out) is also possible
with Long_Long_Limited_Pool;
generic
  type Data_Type is limited private;
  Lifo : Boolean := True;
  with procedure Set (To : out Data_Type; Val : in Data_Type);
package Limited_Pool is

  package My_Pool is new Long_Long_Limited_Pool (Data_Type, Lifo, Set);

  type Pool_Type is tagged limited private;

  -- Check if pool is not empty, get number of elements in pool
  function Is_Empty (Pool : in Pool_Type) return Boolean;
  function Length (Pool : in Pool_Type) return Natural;

  -- Add in pool
  Pool_Full : exception renames My_Pool.Pool_Full;
  procedure Push (Pool : in out Pool_Type; Data : in Data_Type);

  -- Get from pool last pushed (Lifo) or first pushed (Fifo)
  Empty_Pool : exception renames My_Pool.Empty_Pool;
  procedure Pop (Pool : in out Pool_Type; Data : out Data_Type);
  procedure Pop (Pool : in out Pool_Type);

  -- Read from pool the next to be popped
  --  i.e. last pushed (Lifo) or first pushed (Fifo)
  procedure Front (Pool : in out Pool_Type; Data : out Data_Type);

  -- Clear the pool (deallocates)
  procedure Clear (Pool : in out Pool_Type);

private

  type Pool_Type is tagged limited record
    Pool : My_Pool.Pool_Type;
  end record;

end Limited_Pool;

