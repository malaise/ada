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

  procedure Search (List : in out Unique_List_Type;
                    Crit : in Element_Type;
                    Found : out Boolean) is
  begin
    Search_First (List_Type (List), Crit, Found);
  end Search;

  function Search (List : in out Unique_List_Type;
                   Crit : in Element_Type) return Boolean is
    (Search_First (List_Type (List), Crit));

  procedure Find (List : in out Unique_List_Type;
                  Crit : in Element_Type) is
  begin
    Find_First (List_Type (List), Crit);
  end Find;

  -- Search in hashing the element matching criteria, returns null if not found
  -- Optim: See if last found is the one
  procedure Locate_Optim (List : in out Unique_List_Type;
                          Crit : in Element_Type;
                          Element : out Element_Acc) is
    Index : constant Hash_Mng.Hash_Range := Hash_Func (Key_Image (Crit));
    use type Hash_Mng.Hash_Range;
  begin
    -- See if previously found is the one
    if List.Current /= Null_Cell and then List.Hash_Index = Index
    and then List.Current.Elt.all = Crit then
      -- Yes
      Element := List.Current.Elt;
    else
      Locate (List_Type(List), Crit, True, Element, Forward);
    end if;
  end Locate_Optim;

  -- Get access to the element matching in the list
  -- May raise Not_In_List
  procedure Get_Access (List : in out Unique_List_Type;
                        Item : in Element_Type;
                        Item_Access : out Element_Access) is
  begin
    Item_Access := Get_Access (List, Item);
  end Get_Access;

  function Get_Access (List : in out Unique_List_Type;
                       Item : in Element_Type)
           return Element_Access is
    Acc : Element_Acc;
  begin
    Locate_Optim (List, Item, Acc);
    if Acc = null then
      raise Not_In_List;
    end if;
    return Element_Access (Acc);
  end Get_Access;

  -- Insert an item if does not already exists
  -- May raise Full_List (no more memory)
  function  Insert_If_New (List : in out Unique_List_Type;
                           Item : in Element_Type;
                           Where : in Where_Insert_List := Last)
            return Boolean is

    Acc : Element_Acc;
  begin
    Check_Callback (List_Type(List));
    Locate (List_Type(List), Item, True, Acc);
    if Acc = null then
      Insert (List_Type(List), Item, Where);
      return True;
    else
      -- Else drop
      return False;
    end if;
  end Insert_If_New;
  procedure Insert_If_New (List : in out Unique_List_Type;
                           Item : in Element_Type;
                           Where : in Where_Insert_List := Last) is
    Dummy : Boolean;
  begin
    Dummy := Insert_If_New (List, Item, Where);
  end Insert_If_New;

  -- Insert or replace an item
  -- May raise Full_List (no more memory)
  -- This ensures that Hased_List.Insert is not called on a Unique_List
  overriding procedure Insert (List : in out Unique_List_Type;
                               Item : in Element_Type;
                               Where : in Where_Insert_List := Last) is
    Acc : Element_Acc;
  begin
    Check_Callback (List_Type(List));
    Locate_Optim (List, Item, Acc);
    if Acc = null then
      Insert (List_Type(List), Item, Where);
    else
      Replace_Current (List_Type(List), Item);
    end if;
  end Insert;

  -- Read the element matching in the list
  -- May raise Not_In_List
  procedure Read (List : in out Unique_List_Type;
                  Item : in out Element_Type) is
    Acc : Element_Acc;
  begin
    -- Find (List, Item);
    Locate_Optim (List, Item, Acc);
    if Acc = null then
      raise Not_In_List;
    end if;
    -- Read (List, Item);
    Set (Item, Get_Access_Current (List_Type(List)).all);
  end Read;

  function Read (List : in out Unique_List_Type;
                 Item : in Element_Type) return Element_Type is
  begin
    return Result : Element_Type do
      Set (Result, Item);
      Read (List, Result);
    end return;
  end Read;

  -- Suppress the element matching in the list
  -- May raise Not_In_List
  procedure Delete (List : in out Unique_List_Type;
                    Crit : in Element_Type) is
    Acc : Element_Acc;
  begin
    Check_Callback (List_Type(List));
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
    Acc : Element_Acc;
  begin
    Check_Callback (List_Type(List));
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

