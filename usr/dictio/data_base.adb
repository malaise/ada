with Dynamic_List;
package body Data_Base is

  package Item_List_Mng is new Dynamic_List (Item_Rec);
  Item_List : Item_List_Mng.list_type;

  function Name_Match (Elt1, Elt2 : Item_Rec) return Boolean is
  begin
    return Elt1.Name = Elt2.Name;
  end Name_Match;
  procedure Search_Name is new Item_List_Mng.Search (Name_Match);

  procedure Set (Item : in Item_Rec) is
    Itm : Item_Rec;
  begin
    Itm := Item;
    begin
      Search_Name (Item_List, Itm, From_Current => False);
      Item_List_Mng.Modify (Item_List, Itm, Item_List_Mng.Current);
    exception
      when Item_List_Mng.Not_In_List =>
        if not Item_List_Mng.Is_Empty (Item_List) then
          Item_List_Mng.Move_To (Item_List, Item_List_Mng.Prev, 0, False);
        end if;
        Item_List_Mng.Insert (Item_List, Itm);
    end;
  end Set;

  procedure Get (Name : in Item_Name; Item : out Item_Rec) is
    Itm : Item_Rec;
  begin
    Itm.Name := Name;
    begin
      Search_Name (Item_List, Itm, From_Current => False);
      Item_List_Mng.Read (Item_List, Item, Item_List_Mng.Current);
    exception
      when Item_List_Mng.Not_In_List =>
        Item := No_Item;
    end;
  end Get;

  procedure Reset is
  begin
    Item_List_Mng.Delete_List (Item_List);
  end Reset;

  function Nb_Item return Natural is
  begin
    return Item_List_Mng.List_Length (Item_List);
  end Nb_Item;

  
  -- Item_Name is empty when no more item
  procedure Read_First (Item : out Item_Rec) is
  begin
    if Item_List_Mng.Is_Empty (Item_List) then
      Item := No_Item;
      return;
    end if;
    Item_List_Mng.Move_To (Item_List, Item_List_Mng.Next, 0, False);
    Item_List_Mng.Read (Item_List, Item, Item_List_Mng.Current);
  end Read_First;

  procedure Read_Next (Item : out Item_Rec) is
  begin
    Item_List_Mng.Move_To (Item_List);
    Item_List_Mng.Read (Item_List, Item, Item_List_Mng.Current);
  exception
    when Item_List_Mng.Not_In_List =>
      Item := No_Item;
  end Read_Next;

end Data_Base;

