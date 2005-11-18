with Limited_List, Hash;
package body Unique_List is

  -- The limited list of items
  package List_Mng is new Limited_List (Element_Type, Set);
  List : List_Mng.List_Type;
  subtype Element_Access is List_Mng.Element_Access;
  use type List_Mng.Element_Access;

  -- Element hashing
  procedure Dump (Data : in Element_Access) is
  begin
    null;
  end Dump;
  package Element_Hashing is
          new Hash.Hash_Mng (Hash.Max_Hash_Value, Element_Access, Dump);

  -- Search in hashing the element matching criteria, returns null if not found
  function Locate (Crit : in Element_Type) return Element_Access is
    Key : constant String := Image (Crit);
    Data_Found : Element_Hashing.Found_Rec;
  begin
    -- Search by hashing
    Element_Hashing.Reset_Find (Key);
    loop
      -- Loop withing matching (same image) until data = criteria
      Data_Found := Element_Hashing.Find_Next(Key);
      if Data_Found.Found and then Data_Found.Data.all = Crit then
        -- Found the correct element
        return Data_Found.Data;
      elsif not Data_Found.Found then
        -- No (more) element with this criteria
        exit;
      end if;
      -- Check next elements with this criteria
    end loop;
    return null;
  end Locate;

  -- Insert or replace an item
  -- May raise Full_List (no more memory)
  procedure Insert (Item  : in Element_Type) is
    Acc : Element_Access := Locate (Item);
  begin
    if Acc /= null then
      -- This element exists, overwrite it
      Set (Acc.all, Item);
    else
      -- Insert new element in list and hashing
      List_Mng.Insert (List, Item);
      Element_Hashing.Store (Image(Item), List_Mng.Access_Current (List));
    end if;
  exception
    when List_Mng.Full_List =>
      raise Full_List;
  end Insert;

  -- Read the  the element matching in the list
  -- May raise Not_In_List
  procedure Read (Crit : in Element_Type; Item : out Element_Type) is
    Acc : Element_Access := Locate (Crit);
  begin
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
  procedure Delete (Crit : in Element_Type) is
    Done : Boolean;
  begin
    Delete (Crit, Done);
    if not Done then
      raise Not_In_List;
    end if;
  end Delete;

  -- Delete anyway. Set Done to True if matching item was found
  --  and deletion was done
  procedure Search is new List_Mng.Search ("=");
  procedure Delete (Crit : in Element_Type;
                    Done : out Boolean) is
    Dummy : Boolean;
  begin
    -- Locate the item matching the criteria
    Search (List, Done, Crit, From => List_Mng.Absolute);
    if Done then
      -- Delete this item
      List_Mng.Delete (List, Done => Dummy);
    end if;
  end Delete;

  -- Delete the full list
  --  deallocate or not the free list
  procedure Delete_List (Deallocate : in Boolean := True) is
  begin
    List_Mng.Delete_List (List, Deallocate);
  end Delete_List;

  -- Return without exception
  function Is_Empty return Boolean is
  begin
    return List_Mng.Is_Empty (List);
  end Is_Empty;

  -- Return the number of elements in the list (0 if empty, no exception)
  function List_Length return Natural is
  begin
    return List_Mng.List_Length (List);
  end List_Length;

  -- These two calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and reset it for further
  --  testing
  function Is_Modified return Boolean is
  begin
    return List_Mng.Is_Modified (List);
  end Is_Modified;

  procedure Modification_Ack is
  begin
     List_Mng.Modification_Ack (List);
  end Modification_Ack;


  -- Call Iteration on all items
  procedure Iterate (Iteration : in Iteration_Access;
                     From      : in Reference := From_First) is
    Item : Element_Type;
    Moved : Boolean;
    Go_On : Boolean;
  begin
    if List_Mng.Is_Empty (List) then
      return;
    end if;
    -- Prepare loop on all items
    List_Mng.Rewind (List);
    Go_On := True;
    loop
      List_Mng.Read (List, Item, Done => Moved);
      Iteration (Item, Go_On);
      -- Callback requests to stop or en of list
      exit when not Go_On or else not Moved;
    end loop;
  end Iterate;

end Unique_List;

