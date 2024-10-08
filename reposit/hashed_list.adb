package body Hashed_List is

  -- Check if we are in callback
  procedure Check_Callback (List : in out List_Type) is
  begin
    if List.In_Cb then
      raise In_Callback;
    end if;
  end Check_Callback;

  -- Search in hashing the element matching criteria, returns null if not found
  procedure Locate (List      : in out List_Type;
                    Crit      : in Element_Type;
                    Reset     : in Boolean;
                    Element   : out Element_Acc;
                    Direction : in Direction_List := Forward) is
    Index : constant Hash_Mng.Hash_Range := Hash_Func (Key_Image (Crit));
    Data_Found : Hash_Mng.Found_Rec;
  begin
    -- Search by hashing
    if Reset then
      List.Table.Reset_Find (Index);
    end if;
    loop
      -- Loop withing matching (same image) until data = criteria
      List.Table.Find_Next (Index, Data_Found,
                            Hashing.Direction_List(Direction));
      if Data_Found.Found and then Data_Found.Data.Elt.all = Crit then
        -- Found a matching element, store its access (for further access)
        --  and its hash index (for deletion)
        Element := Data_Found.Data.Elt;
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
    List.Current := Null_Cell;
  end Locate;

  -- Check if an element exists in the list
  procedure Search_First (List      : in out List_Type;
                          Crit      : in Element_Type;
                          Found     : out Boolean;
                          Direction : in Direction_List := Forward) is
  begin
    Found := Search_First (List, Crit, Direction);
  end Search_First;

  function Search_First (List      : in out List_Type;
                         Crit      : in Element_Type;
                         Direction : in Direction_List:= Forward)
           return Boolean is
    Acc : Element_Acc;
  begin
    Locate (List, Crit, True, Acc, Direction);
    return Acc /= null;
  end Search_First;

  procedure Search_Next (List      : in out List_Type;
                         Crit      : in Element_Type;
                         Found     : out Boolean;
                         Direction : in Direction_List := Forward) is
  begin
    Found := Search_Next (List, Crit, Direction);
  end Search_Next;

  function Search_Next (List      : in out List_Type;
                        Crit      : in Element_Type;
                        Direction : in Direction_List := Forward)
           return Boolean is
    Acc : Element_Acc;
  begin
    Locate (List, Crit, False, Acc, Direction);
    return  Acc /= null;
  end Search_Next;

  procedure Find_First (List : in out List_Type;
                        Crit : in Element_Type;
                        Direction : in Direction_List := Forward) is
    Acc : Element_Acc;
  begin
    Locate (List, Crit, True, Acc, Direction);
    if Acc = null then
      raise Not_In_List;
    end if;
  end Find_First;

  procedure Find_Next (List : in out List_Type;
                       Crit : in Element_Type;
                       Direction : in Direction_List := Forward) is
    Acc : Element_Acc;
  begin
    Locate (List, Crit, False, Acc, Direction);
    if Acc = null then
      raise Not_In_List;
    end if;
  end Find_Next;

  -- Insert an item
  -- May raise Full_List (no more memory)
  procedure Insert (List : in out List_Type;
                    Item : in Element_Type;
                    Where : in Where_Insert_List := Last) is
  begin
    Check_Callback (List);
    -- Insert new element in list and in hashing
    case Where is
      when First =>
        List.List.Rewind (List_Mng.Next, False);
        List.List.Insert (Item, List_Mng.Prev);
      when Last =>
        List.List.Rewind (List_Mng.Prev, False);
        List.List.Insert (Item, List_Mng.Next);
      when After_Curr =>
        List.List.Insert (Item, List_Mng.Next);
      when Before_Curr =>
        List.List.Insert (Item, List_Mng.Prev);
    end case;
    Hash_Mng.Store (List.Table,
                    Key_Image(Item),
                    (Elt  => List.List.Access_Current,
                     Cell => List.List.Cell_Access_Current),
                    Hashing.Where_Insert_List(Where));
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

  function  Read_Current (List : List_Type) return Element_Type is
  begin
    return Item : Element_Type do
      Read_Current (List, Item);
    end return;
  end Read_Current;

  -- Get direct access to element matching in the list
  -- May raise Not_In_List
  procedure Get_Access_Current (List : in List_Type;
                                Item_Access : out Element_Access) is
  begin
    Item_Access := Get_Access_Current (List);
  end Get_Access_Current;

  function Get_Access_Current (List : List_Type) return Element_Access is
  begin
    if List.Current = Null_Cell then
      raise Not_In_List;
    end if;
    return Element_Access (List.Current.Elt);
  end Get_Access_Current;

  -- Suppress the last element found (which is reset)
  -- May raise Not_In_List
  procedure Delete_Current (List : in out List_Type) is
  begin
    Check_Callback (List);
    if List.Current = Null_Cell then
      raise Not_In_List;
    end if;

    -- Delete this item from hash table (without re-hashing data)
    Hash_Mng.Remove (List.Table, List.Hash_Index);

    -- Remove the cell that has this access
    List.List.Delete_Current_Rewind (List.Current.Cell);
    List.Current := Null_Cell;
  end Delete_Current;

  -- Read the last element searched/found
  -- May raise Not_Equal if Item is not "=" to the element searched/found
  -- May raise Not_In_List
  procedure Replace_Current (List : in out List_Type;
                             Item : in Element_Type) is
    Acc : Element_Acc;
  begin
    Check_Callback (List);
    Acc := Element_Acc (Get_Access_Current (List));

    -- Check that Item is "=" to current element
    if Item /= Acc.all then
      raise Not_Equal;
    end if;

    -- Overwrite current element
    Set (Acc.all, Item);

    -- Mark the list is (marked as) modified
    List.List.Set_Modified;
  end Replace_Current;

  -- Delete the full list
  --  deallocate or not the free list
  procedure Delete_List (List       : in out List_Type;
                         Deallocate : in Boolean := True) is
  begin
    Check_Callback (List);
    List.List.Delete_List (Deallocate);
    Hash_Mng.Clear_All (List.Table);
    List.Current := Null_Cell;
  end Delete_List;

  -- Return without exception
  function Is_Empty (List : List_Type) return Boolean is (List.List.Is_Empty);

  -- Return the number of elements in the list (0 if empty, no exception)
  function List_Length (List : List_Type) return Ll_Natural is
    (List.List.List_Length);

  -- These two calls allow sharing the same list among several
  --  software layers. Each time the list is modified, a flag is set
  --  which allow another layer to test it and reset it for further
  --  testing
  function Is_Modified (List : List_Type) return Boolean is
    (List.List.Is_Modified);

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
      List.List.Rewind (List_Mng.Next);
    else
      List.List.Rewind (List_Mng.Prev);
    end if;
    Go_On := True;
    loop
      List.List.Read (
          Item,
          (if From = From_First then List_Mng.Next else List_Mng.Prev),
          Moved => Moved);
      if Iteration /= null then
        List.In_Cb := True;
        begin
          Iteration (Item, Go_On);
        exception
          when others =>
            List.In_Cb := False;
            raise Iteration_Error;
        end;
        List.In_Cb := False;
      end if;
      -- Callback requests to stop or end of list
      exit when not Go_On or else not Moved;
    end loop;
  exception
    when Iteration_Error =>
      raise;
    when others =>
      List.In_Cb := False;
      raise;
  end Iterate;

 -- Rewind internal list and read successive items
  procedure Rewind (List : in out List_Type;
                    From : in Reference := From_First) is
  begin
    if List.List.Is_Empty then
      raise Not_In_List;
    end if;
    List.List.Rewind ( (if From = From_First then List_Mng.Next
                        else List_Mng.Prev));
  end Rewind;

  -- Move internal list to current access
  function Search_Access (List : in out List_Type) return Boolean is
  begin
    if List.List.Is_Empty then
      raise Not_In_List;
    end if;
    return List.List.Search_Access (List.Current.Elt);
  end Search_Access;

  procedure Read_Next (List : in out List_Type;
                       Item : out Element_Type;
                       Moved : out Boolean;
                       Direction : in Direction_List := Forward) is
  begin
    if List.List.Is_Empty then
      raise Not_In_List;
    end if;
    List.List.Read (Item,
                    (if Direction = Forward then List_Mng.Next
                     else List_Mng.Prev),
                    Moved);
  end Read_Next;

   -- Read the item at a given position
  procedure Read_At (List      : in out List_Type;
                     Position  : in Long_Longs.Llu_Positive;
                     Direction : in Direction_List := Forward;
                     Item      : out Element_Type) is
  begin
    List.List.Move_At (Position,
                       (if Direction = Forward then List_Mng.Next
                        else List_Mng.Prev));
    List.List.Read (Item, List_Mng.Current);
  end Read_At;

end Hashed_List;

