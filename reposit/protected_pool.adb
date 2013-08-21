with Images;
package body Protected_Pool is

  -- Cell affectation
  procedure Set (To : out Cell_Type; Val : in Cell_Type) is
  begin
    To := Val;
  end Set;

  -- Key incrementation (mod has a max of Max_Int)
  function Next_Key (Key : Key_Type) return Key_Type is
  begin
    return (if Key /= Key_Type'Last then Key_Type'Succ (Key)
            else Key_Type'First);
  end Next_Key;

  -- Conversion from/to string
  function Key_Img is new Images.Int_Image (Key_Type);
  function Key_Image (Key : Key_Type) return String renames Key_Img;
  function Key_Value (Str : String) return Key_Type is
  begin
    return Key_Type'Value (Str);
  end Key_Value;

  function Match (Current, Criteria : Cell_Type) return Boolean is
  begin
    return Current.Key = Criteria.Key;
  end Match;

  function Search is new Elt_List_Mng.Search(Match);

  -- Store a new element in the pool, return the key to access it
  function Store (Pool : in out Pool_Type;
                  Element : in Element_Type) return Key_Type is
    Cell : Cell_Type;
  begin
    -- Lock mutex
    Pool.Mutex.Get;
    -- Find next available (not used) key
    Cell.Key := Pool.Next_Key;
    Cell.Data := Element;
    loop
      exit when not Search (Pool.List, Cell,
                            From => Elt_List_Mng.Absolute);
      Cell.Key := Next_Key (Cell.Key);
      if Cell.Key = Pool.Next_Key then
        -- No available key
        Pool.Mutex.Release;
        raise Pool_Full;
      end if;
    end loop;
    -- Store
    Pool.List.Insert (Cell);
    -- Update next key
    Pool.Next_Key := Next_Key (Cell.Key);
    -- Release Mutex
    Pool.Mutex.Release;
    -- Done
    return Cell.Key;
  end Store;

  -- Locate the key in the pool, Raise Not_Found
  procedure Locate (Pool : in out Pool_Type; Key : in Key_Type) is
    Cell : Cell_Type;
  begin
    Cell.Key := Key;
    if not Search (Pool.List, Cell, From => Elt_List_Mng.Absolute) then
      raise Not_Found;
    end if;
   end Locate;

  -- Get/Read/Delete from the pool the element of key
  function Get (Pool : in out Pool_Type; Key : in Key_Type)
               return Element_Type is
    Cell : Cell_Type;
    Moved : Boolean;
  begin
    Pool.Mutex.Get;
    Locate (Pool, Key);
    Pool.List.Get (Cell, Moved => Moved);
    Pool.Mutex.Release;
    return Cell.Data;
  exception
    when Not_Found =>
      Pool.Mutex.Release;
      raise;
  end Get;

  function Read (Pool : in out Pool_Type; Key : in Key_Type)
                return Element_Type is
    Cell : Cell_Type;
    Moved : Boolean;
  begin
    Pool.Mutex.Get;
    Locate (Pool, Key);
    Pool.List.Read (Cell, Moved => Moved);
    Pool.Mutex.Release;
    return Cell.Data;
  exception
    when Not_Found =>
      Pool.Mutex.Release;
      raise;
  end Read;

  procedure Delete (Pool : in out Pool_Type; Key : in Key_Type) is
    Moved : Boolean;
  begin
    Pool.Mutex.Get;
    Locate (Pool, Key);
    Pool.List.Delete (Moved => Moved);
    Pool.Mutex.Release;
  exception
    when Not_Found =>
      Pool.Mutex.Release;
      raise;
  end Delete;

  -- Delete the whole pool
  -- Clears most of the memory
  procedure Delete_Pool (Pool : in out Pool_Type) is
  begin
    Pool.Mutex.Get;
    Pool.List.Delete_List;
    Pool.Mutex.Release;
  end Delete_Pool;

end Protected_Pool;

