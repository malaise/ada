with Mutex_Manager, Long_Long_Limited_List, Long_Long_Limited_Pool;
package body Control_Pool is

  -- The global mutex protecting the pool
  -------------------
  Global_Mutex : Mutex_Manager.Simple_Mutex;


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
  procedure Set_Cell (To : out Cell_Type; Val : in Cell_Type) is
  begin
    To := Val;
  end Set_Cell;

  -- Pool management
  package Pool_Mng is new Long_Long_Limited_List (Cell_Type, Set_Cell);
  Pool : Pool_Mng.List_Type;

  -- Search in pool for a key
  function Same_Key (Current, Criteria : Cell_Type) return Boolean is
  begin
    return Current.Key = Criteria.Key;
  end Same_Key;
  function Search is new Pool_Mng.Search(Same_Key);


  -- Pool of free mutexes
  -----------------------
  procedure Set (To : out Mutex_Access; Val : in Mutex_Access) is
  begin
    To := Val;
  end Set;
  package Free_Mutex_Pool is new Long_Long_Limited_Pool
      (Mutex_Access, Set => Set);
  Free_Mutexes : Free_Mutex_Pool.Pool_Type;

  function Get_Mutex return Mutex_Access is
    Mut_Acc : Mutex_Access;
  begin
    -- Try to get one from the pool
    if not Free_Mutex_Pool.Is_Empty (Free_Mutexes) then
      Free_Mutex_Pool.Pop (Free_Mutexes, Mut_Acc);
    else
      Mut_Acc := new Mutex_Manager.Simple_Mutex;
    end if;
    return Mut_Acc;
  end Get_Mutex;

  -- Decrement Nb users of a mutex, access was or not granted
  procedure Release (Key : in Key_Type; Granted : in Boolean);

  -- Get exclusive access
  -----------------------
  function Get (Key : Key_Type;
                Waiting_Time : Duration) return Boolean is
    Got : Boolean;
    Cell : Cell_Type;
  begin
    -- Global lock
    Global_Mutex.Get;
    -- Look for this key in pool
    Cell.Key := Key;
    if Search (Pool, Cell, From => Pool_Mng.Absolute) then
      -- Read cell, increment counter, store
      Pool.Read (Cell, Pool_Mng.Current);
      Cell.Waiters := Cell.Waiters + 1;
      Pool.Modify (Cell, Pool_Mng.Current);
      -- Unlock Global mutex and wait for data mutex
      Global_Mutex.Release;
      Got := Mutex_Manager.Get (Cell.Data_Mutex.all, Waiting_Time);
      -- Release cell if mutex not got
      if not Got then
        Release (Key, False);
      end if;
      return Got;
    else
      -- Get and lock data mutex (not blocking cause we are first)
      Cell.Data_Mutex := Get_Mutex;
      Cell.Data_Mutex.Get;
      -- Store cell
      Cell.Key := Key;
      Cell.Waiters := 1;
      Pool.Insert (Cell);
      -- Unlock Global mutex and return success
      Global_Mutex.Release;
      return True;
    end if;
  end Get;


  -- Release access
  -----------------
  procedure Release (Key : in Key_Type) is
  begin
    Release(Key, True);
  end Release;

  procedure Release (Key : in Key_Type; Granted : in Boolean) is
    Cell : Cell_Type;
    Moved : Boolean;
  begin
    -- Global lock
    Global_Mutex.Get;
    -- Look for this key in pool
    Cell.Key := Key;
    if not Search (Pool, Cell, From => Pool_Mng.Absolute) then
      -- Exception if not found
      raise Key_Not_Got;
    end if;
    -- Decrease counter or remove cell
    Pool.Read (Cell, Pool_Mng.Current);
    if Cell.Waiters /= 1 then
      Cell.Waiters := Cell.Waiters - 1;
      Pool.Modify (Cell, Pool_Mng.Current);
    else
      Pool.Delete (Moved => Moved);
    end if;
    -- Release data mutex if it was granted
    if Granted then
      Cell.Data_Mutex.Release;
    end if;
    -- Unlock Global mutex
    Global_Mutex.Release;
  end Release;

end Control_Pool;

