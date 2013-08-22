-- Exclusive access to data in a pool
with Mutex_Manager, Long_Long_Limited_List, Long_Long_Limited_Pool;
generic
  type Key_Type is private;
package Control_Pool is

  -- A controlled pool
  type Controlled_Pool_Type is tagged limited private;

  -- Get exclusive access to a data
  --  If delay is negative, wait until access is granted
  --  If delay is null, try and give up if not free
  --  If delay is positive, try during the specified delay
  function Get (Pool : in out Controlled_Pool_Type;
                Key : in Key_Type;
                Waiting_Time : in Duration) return Boolean;

  -- Release access to data
  -- Key_Not_Got is raised if the access was not got
  procedure Release (Pool : in out Controlled_Pool_Type;
                     Key : in Key_Type);
  Key_Not_Got : exception;

  -- Clear (from free list) the unused accesses
  procedure Clear (Pool : in out Controlled_Pool_Type);

private
  -- Pool of used mutexes
  -----------------------
  -- The item in the pool
  type Mutex_Access is access Mutex_Manager.Mutex;
  type Cell_Type is record
    Data_Mutex : Mutex_Access;
    Waiters : Positive;
    Key : Key_Type;
  end record;
  -- Affectation of cell
  procedure Set_Cell (To : out Cell_Type; Val : in Cell_Type);
  -- Pool management
  package Used_Mutex_List is new Long_Long_Limited_List (Cell_Type, Set_Cell);

  -- Pool of free mutexes
  -----------------------
  procedure Set (To : out Mutex_Access; Val : in Mutex_Access);
  package Free_Mutex_Pool is new Long_Long_Limited_Pool
      (Mutex_Access, Set => Set);

  -- A controlled pool
  type Controlled_Pool_Type is tagged limited record
    Global_Mutex : Mutex_Manager.Simple_Mutex;
    Used_Mutexes : Used_Mutex_List.List_Type;
    Free_Mutexes : Free_Mutex_Pool.Pool_Type;
  end record;

end Control_Pool;

