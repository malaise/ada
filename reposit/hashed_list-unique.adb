package body Hashed_List.Unique is

  -----------------
  -- UNIQUE LIST --
  -----------------
  -- A unique list is hashed list where at most one element of a given value
  --  (in the sense of "=") is stored.
  -- Storing an element leads to either
  --  - insertion (when the element is new)
  --  - replacement of the previous element by the new
  --  - drop the new value and keep the original
  -- type Unique_List_Type is new List_Type with null record;

  -- Get access to the element matching in the list
  -- May raise Not_In_List
  procedure Get_Access (List : in out Unique_List_Type;
                        Item : in Element_Type;
                        Item_Access : out Element_Access) is
    Acc : Element_Access;
  begin
    Locate (List_Type(List), Item, True, Acc);
    if Acc = null then
      raise Not_In_List;
    end if;
    Item_Access := List.Current;
  end Get_Access;

  -- Insert an item if does not already exists
  -- May raise Full_List (no more memory)
  procedure Insert_If_New (List : in out Unique_List_Type;
                           Item : in Element_Type) is
    Acc : Element_Access;
  begin
    Locate (List_Type(List), Item, True, Acc);
    if Acc = null then
      Insert (List_Type(List), Item);
      -- Else drop
    end if;
  end Insert_If_New;

  -- Insert or replace an item
  -- May raise Full_List (no more memory)
  -- This ensures that Hased_List.Insert is not called on a Unique_List
  overriding procedure Insert (List : in out Unique_List_Type;
                       Item : in Element_Type) is
    Acc : Element_Access;
  begin
    Locate (List_Type(List), Item, True, Acc);
    if Acc = null then
      Insert (List_Type(List), Item);
    else
      Replace_Current (List_Type(List), Item);
    end if;
  end Insert;

  -- Read the element matching in the list
  -- May raise Not_In_List
  procedure Read (List : in out Unique_List_Type;
                  Item : in out Element_Type) is
    Acc : Element_Access;
  begin
    -- Find (List, Item);
    Locate (List_Type(List), Item, True, Acc);
    if Acc = null then
      raise Not_In_List;
    end if;
    -- Read (List, Item);
    Set (Item, Get_Access_Current (List_Type(List)).all);
  end Read;

  -- Suppress the element matching in the list
  -- May raise Not_In_List
  procedure Delete (List : in out Unique_List_Type;
                    Crit : in Element_Type) is
    Acc : Element_Access;
  begin
    -- Find (List, Item);
    Locate (List_Type(List), Crit, True, Acc);
    if Acc = null then
      raise Not_In_List;
    end if;
    Delete_Current (List_Type(List));
  end Delete;

  -- Delete anyway. Set Done to True if matching item was found
  --  and deletion was done
  procedure Delete (List : in out Unique_List_Type;
                    Crit : in Element_Type;
                    Done : out Boolean) is
    Acc : Element_Access;
  begin
    -- Find (List, Item);
    Locate (List_Type(List), Crit, True, Acc);
    if Acc = null then
      Done := False;
      return;
    end if;
    Delete_Current (List_Type(List));
    Done := True;
  end Delete;

end Hashed_List.Unique;

