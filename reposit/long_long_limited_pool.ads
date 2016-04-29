-- Generic simple dynamic pool of limited private objects
-- Simply push data in pool, and pop data as long as it is not empty
-- Default policy is Lifo (Last In First Out)
--  but Fifo (First In First Out) is also possible
private with Long_Long_Limited_List;
with Long_Longs;
generic
  type Data_Type is limited private;
  Lifo : Boolean := True;
  with procedure Set (To : out Data_Type; Val : in Data_Type);
package Long_Long_Limited_Pool is

  type Pool_Type is tagged limited private;

  -- Check if pool is not empty, get number of elements in pool
  function Is_Empty (Pool : in Pool_Type) return Boolean;
  subtype Ll_Natural is Long_Longs.Llu_Natural;
  function Length (Pool : in Pool_Type) return Ll_Natural;

  -- Add in pool
  Pool_Full : exception;
  procedure Push (Pool : in out Pool_Type; Data : in Data_Type);

  -- Get from pool last pushed (Lifo) or first pushed (Fifo)
  Empty_Pool : exception;
  procedure Pop (Pool : in out Pool_Type; Data : out Data_Type);
  function Pop (Pool : in out Pool_Type) return Data_Type;
  procedure Pop (Pool : in out Pool_Type);

  -- Read from pool the next to be popped
  --  i.e. last pushed (Lifo) or first pushed (Fifo)
  procedure Look (Pool : in out Pool_Type; Data : out Data_Type);
  function  Look (Pool : in out Pool_Type) return Data_Type;

  -- Clear the pool (deallocates)
  procedure Clear (Pool : in out Pool_Type);

private

  package Pool_List_Mng is new Long_Long_Limited_List (Data_Type, Set);
  type Pool_Type is new Pool_List_Mng.List_Type with null record;

end Long_Long_Limited_Pool;

