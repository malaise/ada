package body Unique_List is

  -- Element hashing
  procedure Dump (Data : in Element_Access) is
  begin
    null;
  end Dump;

  -- Search in hashing the element matching criteria, returns null if not found
  procedure Locate (List : in out List_Type;
                    Crit : in Element_Type;
                    Element : out Element_Access) is
    Key : constant String := Key_Image (Crit);
    Data_Found : Hash_Mng.Found_Rec;
  begin
    -- Search by hashing
    Hash_Mng.Reset_Find (List.Table, Key);
    loop
      -- Loop withing matching (same image) until data = criteria
      Hash_Mng.Find_Next(List.Table, Key, Data_Found);
      if Data_Found.Found and then Data_Found.Data.all = Crit then
        -- Found the correct element
        Element := Data_Found.Data;
        return;
      elsif not Data_Found.Found then
        -- No (more) element with this criteria
        exit;
      end if;
      -- Check next elements with this criteria
    end loop;
    Element := null;
  end Locate;

  -- Check if an element exists in the list
  procedure Search (List : in out List_Type;
                    Crit : in Element_Type;
                    Found : out Boolean) is
    Acc : Element_Access;
  begin
    Locate (List, Crit, Acc);
    Found := Acc /= null;
  end Search;

  -- Insert or replace an item
  -- Optionally drops new Item if one already exists
  -- May raise Full_List (no more memory)
  procedure Insert (List : in out List_Type;
                    Item : in Element_Type;
                    Drop : in Boolean := False) is
    Acc : Element_Access;
  begin
    Locate (List, Item, Acc);
    if Acc = null then
      -- Insert new element in list and hashing
      List.List.Insert (Item);
      Hash_Mng.Store (List.Table,
                      Key_Image(Item),
                      Element_Access (List.List.Access_Current));
    elsif not Drop then
      -- This element exists, overwrite it
      Set (Acc.all, Item);
    end if;
  exception
    when List_Mng.Full_List =>
      raise Full_List;
  end Insert;

  -- Read the element matching in the list
  -- May raise Not_In_List
  procedure Read (List : in out List_Type;
                  Crit : in Element_Type;
                  Item : out Element_Type) is
    Acc : Element_Access;
  begin
    Get_Access (List, Crit, Acc);
    -- This element exists, return it
    Set (Item, Acc.all);
  end Read;

  -- Get direct access to element matching in the list
  -- May raise Not_In_List
  procedure Get_Access (List : in out List_Type;
                        Crit : in Element_Type;
                        Item_Access : out Element_Access) is
  begin
    Locate (List, Crit, Item_Access);
    if Item_Access = null then
      -- Not found
      raise Not_In_List;
    end if;
  end Get_Access;

  -- Suppress the element matching in the list
  -- May raise Not_In_List
  procedure Delete (List : in out List_Type;
                    Crit : in Element_Type) is
    Done : Boolean;
  begin
    Delete (List, Crit, Done);
    if not Done then
      raise Not_In_List;
    end if;
  end Delete;

  -- Delete anyway. Set Done to True if matching item was found
  --  and deletion was done
  procedure Search is new List_Mng.Search ("=");
  procedure Delete (List : in out List_Type;
                    Crit : in Element_Type;
                    Done : out Boolean) is
    Dummy : Boolean;
    Acc : Element_Access;
    Key : constant String := Key_Image (Crit);
  begin
    -- Locate in list the item matching the criteria
    Search (List.List, Done, Crit, From => List_Mng.Absolute);
    if not Done then
      -- data not found
      return;
    end if;
    -- Locate in hash the item matching the criteria
    Locate (List, Crit, Acc);
    -- Check it has been found and same item
    if Acc /= Element_Access(List.List.Access_Current) then
      raise Internal_Error;
    end if;

    -- Delete this item from list and hash table
    List.List.Delete (Moved => Dummy);
    Hash_Mng.Remove (List.Table, Key);
  end Delete;

  -- Delete the full list
  --  deallocate or not the free list
  procedure Delete_List (List       : in out List_Type;
                         Deallocate : in Boolean := True) is
  begin
    List.List.Delete_List (Deallocate);
    Hash_Mng.Clear_All (List.Table);
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

end Unique_List;

