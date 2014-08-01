package body Protected_Pool is

  -- Cell affectation
  procedure Set (To : out Element_Type; Val : in Element_Type) is
  begin
    To := Val;
  end Set;

  -- Store a new element in the pool
  procedure Store (Pool : in out Pool_Type;
                   Element : in Element_Type) is
  begin
    Pool.Mutex.Get;
    Pool.List.Insert (Element);
    Pool.Mutex.Release;
  exception
    when Elt_Uniq_Mng.Full_List =>
      Pool.Mutex.Release;
      raise Pool_Full;
    when others =>
      Pool.Mutex.Release;
      raise;
  end Store;

  -- Get/Read/Delete from the pool the element of key
  procedure Get (Pool : in out Pool_Type; Elt : in out Element_Type) is
  begin
    Pool.Mutex.Get;
    Pool.List.Read (Elt);
    Pool.List.Delete_Current;
    Pool.Mutex.Release;
  exception
    when Not_Found =>
      Pool.Mutex.Release;
      raise Not_Found;
    when others =>
      Pool.Mutex.Release;
      raise;
  end Get;

  procedure Read (Pool : in out Pool_Type; Elt : in out Element_Type) is
  begin
    Pool.Mutex.Get;
    Pool.List.Read (Elt);
    Pool.Mutex.Release;
  exception
    when Not_Found =>
      Pool.Mutex.Release;
      raise Not_Found;
    when others =>
      Pool.Mutex.Release;
      raise;
  end Read;

  procedure Delete (Pool : in out Pool_Type; Elt : in Element_Type) is
  begin
    Pool.Mutex.Get;
    Pool.List.Delete (Elt);
    Pool.Mutex.Release;
  exception
    when Not_Found =>
      Pool.Mutex.Release;
      raise Not_Found;
    when others =>
      Pool.Mutex.Release;
      raise;
  end Delete;

  -- Delete the whole pool
  procedure Delete_Pool (Pool : in out Pool_Type) is
  begin
    Pool.Mutex.Get;
    Pool.List.Delete_List;
    Pool.Mutex.Release;
  end Delete_Pool;

end Protected_Pool;

