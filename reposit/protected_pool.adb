with Ada.Unchecked_Deallocation;
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
    Pool.Mutex.Get;
    -- Find next available (not used) key
    Cell.Key := Pool.Next_Key.all;
    Cell.Data := Element;
    loop
      Search (Pool.List.all, Found, Cell, From => Elt_List_Mng.Absolute);
      exit when not Found;
      Cell.Key := Cell.Key + 1;
      if Cell.Key = Pool.Next_Key.all then
        -- No available key
        Pool.Mutex.Release;
        raise Pool_Full;
      end if;
    end loop;
    -- Store
    Pool.List.Insert (Cell);
    -- Update next key
    Pool.Next_Key.all := Cell.Key + 1;
    -- Release Mutex
    Pool.Mutex.Release;
    -- Done
    return Cell.Key;
  end Store;

  -- Locate the key in the pool, Raise Not_Found
  procedure Locate (Pool : Pool_Type; Key : Key_Type) is
    Cell : Cell_Type;
    Found : Boolean;
  begin
    Cell.Key := Key;
    Search (Pool.List.all, Found, Cell, From => Elt_List_Mng.Absolute);
    if not Found then
      raise Not_Found;
    end if;
   end Locate;

  -- Get/Read/Delete from the pool the element of key
  function Get (Pool : Pool_Type; Key : Key_Type) return Element_Type is
    Cell : Cell_Type;
    Moved : Boolean;
  begin
    Pool.Mutex.Get;
    Locate (Pool, Key);
    Pool.List.all.Get (Cell, Moved => Moved);
    Pool.Mutex.Release;
    return Cell.Data;
  exception
    when Not_Found =>
      Pool.Mutex.Release;
      raise;
  end Get;

  function Read (Pool : Pool_Type; Key : Key_Type) return Element_Type is
    Cell : Cell_Type;
    Moved : Boolean;
  begin
    Pool.Mutex.Get;
    Locate (Pool, Key);
    Pool.List.all.Read (Cell, Moved => Moved);
    Pool.Mutex.Release;
    return Cell.Data;
  exception
    when Not_Found =>
      Pool.Mutex.Release;
      raise;
  end Read;

  procedure Delete (Pool : Pool_Type; Key : Key_Type) is
    Moved : Boolean;
  begin
    Pool.Mutex.Get;
    Locate (Pool, Key);
    Pool.List.all.Delete (Moved => Moved);
    Pool.Mutex.Release;
  exception
    when Not_Found =>
      Pool.Mutex.Release;
      raise;
  end Delete;

  -- Delete the whole pool
  -- Clears most of the memory
  procedure Delete_Pool (Pool : Pool_Type) is
  begin
    Pool.Mutex.Get;
    Pool.List.all.Delete_List;
    Pool.Mutex.Release;
  end Delete_Pool;

  procedure Deallocate is new Ada.Unchecked_Deallocation
   (Key_Type, Key_Access);
  procedure Deallocate is new Ada.Unchecked_Deallocation
   (Elt_List_Mng.List_Type, List_Access);

  overriding procedure Finalize (Pool : in out Pool_Type) is
  begin
    Pool.Mutex.Get;
    Pool.List.all.Delete_List;
    Deallocate (Pool.Next_Key);
    Deallocate (Pool.List);
    Pool.Mutex.Release;
  end Finalize;

end Protected_Pool;


