with Mod_Image;
package body Protected_Pool is

  -- Conversion from/to string
  function Mod_Key_Image is new Mod_Image (Key_Type);
  function Key_Image (Key : Key_Type) return String renames Mod_Key_Image;
  function Key_Value (Str : String) return Key_Type is
  begin
    return Key_Type'Value (Str);
  end Key_Value;

  function Match (Current, Criteria : Cell_Type) return Boolean is
  begin
    return Current.Key = Criteria.Key;
  end Match;

  procedure Search is new Elt_List_Mng.Search(Match);

  -- Store a new element in the pool, return the key to access it
  function Store (Pool : Pool_Type; Element : Element_Type) return Key_Type is
    Cell : Cell_Type;
    Found : Boolean;
  begin
    -- Lock mutex
    Mutex_Manager.Get (Pool.Mutex);
    -- Find next available (not used) key
    Cell.Key := Pool.Next_Key;
    Cell.Data := Element;
    loop
      Search (Pool.List.all, Found, Cell, From => Elt_List_Mng.Absolute);
      exit when not Found;
      Cell.Key := Cell.Key + 1;
      if Cell.Key = Pool.Next_Key then
        -- No available key
        Mutex_Manager.Release (Pool.Mutex);
        raise Pool_Full;
      end if;
    end loop;
    -- Store
    Pool.List.Insert (Cell);
    -- Release Mutex
    Mutex_Manager.Release (Pool.Mutex);
    -- Done
    return Cell.Key;
  end Store;

  -- Locate the key in the pool, Raise Not_Found
  procedure Locate (Pool : Pool_Type; Key : Key_Type) is
    Cell : Cell_Type;
    Found : Boolean;
  begin
    Cell.Key := Pool.Next_Key;
    Search (Pool.List.all, Found, Cell, From => Elt_List_Mng.Absolute);
    if not Found then
      raise Not_Found;
    end if;
   end Locate;

  -- Get/Read/Delete from the pool the element of key
  function Get (Pool : Pool_Type; Key : Key_Type) return Element_Type is
    Cell : Cell_Type;
    Done : Boolean;
  begin
    Mutex_Manager.Get (Pool.Mutex);
    Locate (Pool, Key);
    Pool.List.all.Get (Cell, Done => Done);
    Mutex_Manager.Release (Pool.Mutex);
    return Cell.Data;
  exception
    when Not_Found =>
      Mutex_Manager.Release (Pool.Mutex);
      raise;
  end Get;

  function Read (Pool : Pool_Type; Key : Key_Type) return Element_Type is
    Cell : Cell_Type;
    Done : Boolean;
  begin
    Mutex_Manager.Get (Pool.Mutex);
    Locate (Pool, Key);
    Pool.List.all.Read (Cell, Done => Done);
    Mutex_Manager.Release (Pool.Mutex);
    return Cell.Data;
  exception
    when Not_Found =>
      Mutex_Manager.Release (Pool.Mutex);
      raise;
  end Read;

  procedure Delete (Pool : Pool_Type; Key : Key_Type) is
    Done : Boolean;
  begin
    Mutex_Manager.Get (Pool.Mutex);
    Locate (Pool, Key);
    Pool.List.all.Delete (Done => Done);
    Mutex_Manager.Release (Pool.Mutex);
  exception
    when Not_Found =>
      Mutex_Manager.Release (Pool.Mutex);
      raise;
  end Delete;

  -- Delete the whole pool
  procedure Delete_Pool (Pool : Pool_Type) is
  begin
    Mutex_Manager.Get (Pool.Mutex);
    Pool.List.all.Delete_List;
    Mutex_Manager.Release (Pool.Mutex);
  end Delete_Pool;

  procedure Finalize (Pool : in out Pool_Type) is
  begin
    Mutex_Manager.Get (Pool.Mutex);
    Pool.List.all.Delete_List;
    Mutex_Manager.Release (Pool.Mutex);
  end Finalize;

end Protected_Pool;


