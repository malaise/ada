with Ada.Unchecked_Deallocation;
package body Control_Pool is

  -- Pool of used mutexes
  -----------------------
  -- Affectation of cell
  procedure Set_Cell (To : out Cell_Type; Val : in Cell_Type) is
  begin
    To := Val;
  end Set_Cell;

  -- Search in pool for a key
  function Same_Key (Current, Criteria : Cell_Type) return Boolean is
  begin
    return Current.Key = Criteria.Key;
  end Same_Key;
  function Search is new Used_Mutex_List.Search(Same_Key);

  -- Pool of free mutexes
  -----------------------
  procedure Set (To : out Mutex_Access; Val : in Mutex_Access) is
  begin
    To := Val;
  end Set;

  function Get_Mutex (Pool : Controlled_Pool_Type) return Mutex_Access is
    Mut_Acc : Mutex_Access;
  begin
    -- Try to get one from the pool
    if not Pool.Free_Mutexes.Is_Empty then
      Pool.Free_Mutexes.Pop (Mut_Acc);
    else
      -- No more free mutex, create a new one
      Mut_Acc := new Mutex_Manager.Simple_Mutex;
    end if;
    return Mut_Acc;
  end Get_Mutex;

  -- Decrement Nb users of a mutex, access was or not granted
  procedure Release (Pool : in Controlled_Pool_Type;
                     Key : in Key_Type;
                     Granted : in Boolean);

  -- Get exclusive access
  -----------------------
  function Get (Pool : Controlled_Pool_Type;
                Key : Key_Type;
                Waiting_Time : Duration) return Boolean is
    Got : Boolean;
    Cell : Cell_Type;
  begin
    -- Global lock
    Pool.Global_Mutex.Get;
    -- Look for this key in pool
    Cell.Key := Key;
    if Search (Pool.Used_Mutexes.all, Cell,
               From => Used_Mutex_List.Absolute) then
      -- Read cell, increment counter, store
      Pool.Used_Mutexes.Read (Cell, Used_Mutex_List.Current);
      Cell.Waiters := Cell.Waiters + 1;
      Pool.Used_Mutexes.Modify (Cell, Used_Mutex_List.Current);
      -- Unlock Global mutex and wait for data mutex
      Pool.Global_Mutex.Release;
      Got := Cell.Data_Mutex.Get (Waiting_Time);
      -- Release cell if mutex not got
      if not Got then
        Release (Pool, Key, False);
      end if;
      return Got;
    else
      -- Get and lock data mutex (not blocking cause we are first)
      Cell.Data_Mutex := Get_Mutex (Pool);
      Cell.Data_Mutex.Get;
      -- Store cell
      Cell.Key := Key;
      Cell.Waiters := 1;
      Pool.Used_Mutexes.Insert (Cell);
      -- Unlock Global mutex and return success
      Pool.Global_Mutex.Release;
      return True;
    end if;
  end Get;


  -- Release access
  -----------------
  procedure Release (Pool : in Controlled_Pool_Type; Key : in Key_Type) is
  begin
    Release(Pool, Key, True);
  end Release;

  procedure Release (Pool : in Controlled_Pool_Type;
                     Key : in Key_Type;
                     Granted : in Boolean) is
    Cell : Cell_Type;
    Moved : Boolean;
  begin
    -- Global lock
    Pool.Global_Mutex.Get;
    -- Look for this key in pool
    Cell.Key := Key;
    if not Search (Pool.Used_Mutexes.all,
                   Cell, From => Used_Mutex_List.Absolute) then
      -- Exception if not found
      raise Key_Not_Got;
    end if;
    -- Decrease counter or remove cell
    Pool.Used_Mutexes.Read (Cell, Used_Mutex_List.Current);
    if Cell.Waiters /= 1 then
      Cell.Waiters := Cell.Waiters - 1;
      Pool.Used_Mutexes.Modify (Cell, Used_Mutex_List.Current);
    else
      Pool.Free_Mutexes.Push (Cell.Data_Mutex);
      Pool.Used_Mutexes.Delete (Moved => Moved);
    end if;
    -- Release data mutex if it was granted
    if Granted then
      Cell.Data_Mutex.Release;
    end if;
    -- Unlock Global mutex
    Pool.Global_Mutex.Release;
  end Release;

  -- Clear (from free list) the unused accesses
  procedure Clear (Pool : in Controlled_Pool_Type) is
  begin
    Pool.Free_Mutexes.Clear;
  end Clear;

  -- Finalization
  procedure Deallocate is new Ada.Unchecked_Deallocation
    (Used_Mutex_List.List_Type, Used_Mutex_List_Access);
  procedure Deallocate is new Ada.Unchecked_Deallocation
    (Free_Mutex_Pool.Pool_Type, Free_Mutex_List_Access);
  overriding procedure Finalize   (Pool : in out Controlled_Pool_Type) is
  begin
    Deallocate (Pool.Used_Mutexes);
    Deallocate (Pool.Free_Mutexes);
  end Finalize;

end Control_Pool;

