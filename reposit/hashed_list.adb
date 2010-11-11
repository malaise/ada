package body Hashed_List is

  -- Element hashing
  procedure Dump (Data : in Element_Access) is
  begin
    null;
  end Dump;

  -- Search in hashing the element matching criteria, returns null if not found
  procedure Locate (List : in out List_Type;
                    Crit : in Element_Type;
                    Reset : in Boolean;
                    Element : out Element_Access) is
    Index : constant Hash_Mng.Hash_Range := Hash_Func (Key_Image (Crit));
    Data_Found : Hash_Mng.Found_Rec;
  begin
    -- Search by hashing
    if Reset then
      List.Table.Reset_Find (Index);
    end if;
    loop
      -- Loop withing matching (same image) until data = criteria
      List.Table.Find_Next (Index, Data_Found);
      if Data_Found.Found and then Data_Found.Data.all = Crit then
        -- Found the correct element, store its access (for further access)
        --  and its hash index (for deletion)
        Element := Data_Found.Data;
        List.Current := Data_Found.Data;
        List.Hash_Index := Index;
        return;
      elsif not Data_Found.Found then
        -- No (more) element with this criteria
        exit;
      end if;
      -- Check next elements with this criteria
    end loop;
    -- Not found
    Element := null;
    List.Current := null;
  end Locate;

  -- Check if an element exists in the list
  procedure Search_First (List : in out List_Type;
                          Crit : in Element_Type;
                          Found : out Boolean) is
    Acc : Element_Access;
  begin
    Locate (List, Crit, True, Acc);
    Found := Acc /= null;
  end Search_First;
  procedure Search_Next (List : in out List_Type;
                         Crit : in Element_Type;
                         Found : out Boolean) is
    Acc : Element_Access;
  begin
    Locate (List, Crit, False, Acc);
    Found := Acc /= null;
  end Search_Next;
  procedure Find_First (List : in out List_Type;
                        Crit : in Element_Type) is
    Acc : Element_Access;
  begin
    Locate (List, Crit, True, Acc);
    if Acc = null then
      raise Not_In_List;
    end if;
  end Find_First;
  procedure Find_Next (List : in out List_Type;
                       Crit : in Element_Type) is
    Acc : Element_Access;
  begin
    Locate (List, Crit, False, Acc);
    if Acc = null then
      raise Not_In_List;
    end if;
  end Find_Next;

  -- Insert an item
  -- May raise Full_List (no more memory)
  procedure Insert (List : in out List_Type;
                    Item : in Element_Type) is
  begin
    -- Insert new element in list (append for more determinism) and in hashing
    List.List.Rewind (False, List_Mng.Prev);
    List.List.Insert (Item);
    Hash_Mng.Store (List.Table,
                    Key_Image(Item),
                    Element_Access (List.List.Access_Current));
  exception
    when List_Mng.Full_List =>
      raise Full_List;
  end Insert;

  -- Read the element matching in the list
  -- May raise Not_In_List
  procedure Read_Current (List : in List_Type;
                          Item : out Element_Type) is
  begin
    -- Set element or raise Not_In_List
    Set (Item, Get_Access_Current (List).all);
  end Read_Current;

  -- Get direct access to element matching in the list
  -- May raise Not_In_List
  procedure Get_Access_Current (List : in List_Type;
                                Item_Access : out Element_Access) is
  begin
    if List.Current = null then
      raise Not_In_List;
    end if;
    Item_Access := List.Current;
  end Get_Access_Current;
  function Get_Access_Current (List : List_Type) return Element_Access is
  begin
    if List.Current = null then
      raise Not_In_List;
    end if;
    return List.Current;
  end Get_Access_Current;

  -- Suppress the last element found (which is reset)
  -- May raise Not_In_List
  procedure Delete_Current (List : in out List_Type) is
    Acc : Element_Access;
    Dummy : Boolean;
  begin
    Get_Access_Current (List, Acc);

    -- Locate in list the item that has this access
    List.List.Rewind;
    loop
      exit when Element_Access(List.List.Access_Current) = Acc;
      List.List.Move_To;
    end loop;

    -- Delete this item from hash table (without re-hashing data)
    Hash_Mng.Remove (List.Table, List.Hash_Index);

    -- Delete this item from list
    List.List.Delete (Moved => Dummy);
    List.Current := null;
  exception
    when List_Mng.Not_In_List =>
      -- Move_To reached the end without finding
      raise Internal_Error;
  end Delete_Current;

  -- Read the last element searched/found
  -- May raise Not_Equal if Item is not "=" to the element searched/found
  -- May raise Not_In_List
  procedure Replace_Current (List : in out List_Type;
                             Item : in Element_Type) is
    Acc : Element_Access;
    First_Element : Element_Type;
    Position : Positive;
  begin
    Get_Access_Current (List, Acc);

    -- Check that Item is "=" to current element
    if Item /= Acc.all then
      raise Not_Equal;
    end if;

    -- Overwrite current element
    Set (Acc.all, Item);

    -- Replace first element by itself so that the list is (marked as) modified
    -- The list is not empty for sure (Get_Access passed)
    Position := List.List.Get_Position;
    List.List.Rewind;
    List.List.Read (First_Element, List_Mng.Current);
    List.List.Modify (First_Element, List_Mng.Current);
    List.List.Move_At (Position);
  end Replace_Current;

  -- Delete the full list
  --  deallocate or not the free list
  procedure Delete_List (List       : in out List_Type;
                         Deallocate : in Boolean := True) is
  begin
    List.List.Delete_List (Deallocate);
    Hash_Mng.Clear_All (List.Table);
    List.Current := null;
  end Delete_List;

  -- Return without exception
  function Is_Empty (List : List_Type) return Boolean is
  begin
    return List.List.Is_Empty;
  end Is_Empty;

  -- Return the number of elements in the list (0 if empty, no exception)
  function List_Length (List : List_Type) return Natural is
  begin
    return List.List.List_Length;
  end List_Length;

  -- These two calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and reset it for further
  --  testing
  function Is_Modified (List : List_Type) return Boolean is
  begin
    return List.List.Is_Modified;
  end Is_Modified;

  procedure Modification_Ack (List : in out List_Type) is
  begin
     List.List.Modification_Ack;
  end Modification_Ack;


  -- Call Iteration on all items
  procedure Iterate (List      : in out List_Type;
                     Iteration : access
     procedure (Current : in Element_Type;
                Go_On   : in out Boolean);
                     From      : in Reference := From_First) is
    Item : Element_Type;
    Moved : Boolean;
    Go_On : Boolean;
  begin
    if List.List.Is_Empty then
      return;
    end if;
    -- Prepare loop on all items
    if From = From_First then
      List.List.Rewind (True, List_Mng.Next);
    else
      List.List.Rewind (True, List_Mng.Prev);
    end if;
    Go_On := True;
    loop
      if From = From_First then
        List.List.Read (Item, List_Mng.Next, Moved => Moved);
      else
        List.List.Read (Item, List_Mng.Prev, Moved => Moved);
      end if;
      Iteration (Item, Go_On);
      -- Callback requests to stop or en of list
      exit when not Go_On or else not Moved;
    end loop;
  end Iterate;

 -- Rewind internal list and read successive items
  procedure Rewind (List : in out List_Type;
                    From : in Reference := From_First) is
  begin
    if List.List.Is_Empty then
      raise Not_In_List;
    end if;
    if From = From_First then
      List.List.Rewind (True, List_Mng.Next);
    else
      List.List.Rewind (True, List_Mng.Prev);
    end if;
  end Rewind;

  procedure Read_Next (List : in out List_Type;
                       Item : out Element_Type;
                       Moved : out Boolean;
                      From : in Reference := From_First) is
  begin
    if List.List.Is_Empty then
      raise Not_In_List;
    end if;
    if From = From_First then
      List.List.Read (Item, List_Mng.Next, Moved);
    else
      List.List.Read (Item, List_Mng.Prev, Moved);
    end if;
  end Read_Next;

  overriding procedure Finalize (List : in out List_Type) is
  begin
    Delete_List (List, Deallocate => True);
  end Finalize;

end Hashed_List;

