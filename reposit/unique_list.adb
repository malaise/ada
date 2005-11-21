with Limited_List, Hash;
package body Unique_List is

  subtype Element_Access is List_Mng.Element_Access;
  use type List_Mng.Element_Access;

  -- Element hashing
  procedure Dump (Data : in Element_Access) is
  begin
    null;
  end Dump;

  -- Search in hashing the element matching criteria, returns null if not found
  procedure Locate (List : in out List_Type;
                    Crit : in Element_Type;
                    Element : out Element_Access) is
    Key : constant String := Image (Crit);
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

  -- Insert or replace an item
  -- May raise Full_List (no more memory)
  procedure Insert (List : in out List_Type;
                    Item : in Element_Type) is
    Acc : Element_Access;
  begin
    Locate (List, Item, Acc);
    if Acc /= null then
      -- This element exists, overwrite it
      Set (Acc.all, Item);
    else
      -- Insert new element in list and hashing
      List_Mng.Insert (List.List, Item);
      Hash_Mng.Store (List.Table,
                      Image(Item),
                      List_Mng.Access_Current (List.List));
    end if;
  exception
    when List_Mng.Full_List =>
      raise Full_List;
  end Insert;

  -- Read the  the element matching in the list
  -- May raise Not_In_List
  procedure Read (List : in out List_Type;
                  Crit : in Element_Type;
                  Item : out Element_Type) is
    Acc : Element_Access;
  begin
    Locate (List, Crit, Acc);
    if Acc /= null then
      -- This element exists, return it
      Set (Item, Acc.all);
    else
      -- not found
      raise Not_In_List;
    end if;
  end Read;

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
    Key : constant String := Image (Crit);
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
    if Acc /= List_Mng.Access_Current (List.List) then
      raise Internal_Error;
    end if;

    -- Delete this item from list and hash table
    List_Mng.Delete (List.List, Done => Dummy);
    Hash_Mng.Remove (List.Table, Key);
  end Delete;

  -- Delete the full list
  --  deallocate or not the free list
  procedure Delete_List (List       : in out List_Type;
                         Deallocate : in Boolean := True) is
  begin
    List_Mng.Delete_List (List.List, Deallocate);
    Hash_Mng.Clear_All (List.Table);
  end Delete_List;

  -- Return without exception
  function Is_Empty (List : List_Type) return Boolean is
  begin
    return List_Mng.Is_Empty (List.List);
  end Is_Empty;

  -- Return the number of elements in the list (0 if empty, no exception)
  function List_Length (List : List_Type) return Natural is
  begin
    return List_Mng.List_Length (List.List);
  end List_Length;

  -- These two calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and reset it for further
  --  testing
  function Is_Modified (List : List_Type) return Boolean is
  begin
    return List_Mng.Is_Modified (List.List);
  end Is_Modified;

  procedure Modification_Ack (List : in out List_Type) is
  begin
     List_Mng.Modification_Ack (List.List);
  end Modification_Ack;


  -- Call Iteration on all items
  procedure Iterate (List      : in out List_Type;
                     Iteration : in Iteration_Access;
                     From      : in Reference := From_First) is
    Item : Element_Type;
    Moved : Boolean;
    Go_On : Boolean;
  begin
    if List_Mng.Is_Empty (List.List) then
      return;
    end if;
    -- Prepare loop on all items
    List_Mng.Rewind (List.List);
    Go_On := True;
    loop
      List_Mng.Read (List.List, Item, Done => Moved);
      Iteration (Item, Go_On);
      -- Callback requests to stop or en of list
      exit when not Go_On or else not Moved;
    end loop;
  end Iterate;

end Unique_List;

