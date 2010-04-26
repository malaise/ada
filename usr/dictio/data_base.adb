with Dynamic_List, Normal, Crc_10, Hash;
with Parse;
pragma Elaborate (Hash);
package body Data_Base is

  package Item_Dyn_List_Mng is new Dynamic_List (Item_Rec);
  package Item_List_Mng renames Item_Dyn_List_Mng.Dyn_List;
  Item_List : Item_List_Mng.List_Type;

  -- Name and kind match
  function Name_Match (Elt1, Elt2 : Item_Rec) return Boolean is
  begin
    return Elt1.Kind = Elt2.Kind and then Elt1.Name = Elt2.Name;
  end Name_Match;
  procedure Search_Name is new Item_List_Mng.Search (Name_Match);


  -- Hash on: Item.Kind & Item.Name (not Parsed)
  H_Use : constant Boolean := True;
  procedure H_Dump (Data : in Item_Dyn_List_Mng.Element_Access) is
  begin
    null;
  end H_Dump;
  package H_Item is new Hash.Hash_Mng (
        Data_Acess => Item_Dyn_List_Mng.Element_Access,
        Dump => H_Dump);
  H_Table : H_Item.Hash_Table;
  function H_Get (Kind : Item_Kind; Name : Item_Name)
                 return Item_Dyn_List_Mng.Element_Access is
    R : H_Item.Found_Rec;
  begin
    H_Item.Reset_Find (H_Table, Kind & Name);
    loop
      H_Item.Find_Next (H_Table, Kind & Name, R);
      if not R.Found then
        return null;
      end if;
      exit when R.Data.Kind = Kind and then R.Data.Name = Name;
    end loop;
    return R.Data;
  end H_Get;

  -- Crc image
  function Image (Crc : Crc_10.Max_Crc_Range) return Item_Crc is
  begin
    return Normal (Integer(Crc), Item_Crc'Length, Gap => '0');
  end Image;

  -- Crc of a string
  function Crc_Of (Str : String) return Item_Crc is
    Crc : Crc_10.Crc_Type;
  begin
    Crc.Add (Str);
    return Image (Crc.Get);
  end Crc_Of;

  procedure Set (Item : in Item_Rec) is
    Itm : Item_Rec;

    procedure Append_Itm is
    begin
      Item_List.Rewind (False, Item_List_Mng.Prev);
      Item_List.Insert (Itm);
    end Append_Itm;

    Acc : Item_Dyn_List_Mng.Element_Access;
    Found : Boolean;
    use type Item_Dyn_List_Mng.Element_Access;
  begin
    -- The one to store
    Itm := Item;

    if H_Use then
      Acc := H_Get (Item.Kind, Item.Name);
      if Acc /= null then
        Acc.all := Itm;
      else
        Append_Itm;
        H_Item.Store (H_Table,
                      Item.Kind & Item.Name,
                      Item_List.Access_Current);
      end if;
    else
      Search_Name (Item_List, Found, Itm, From => Item_List_Mng.Absolute);
      if Found then
        Item_List.Modify (Itm, Item_List_Mng.Current);
      else
        Append_Itm;
      end if;
    end if;
  end Set;

  -- Set item, update Crc
  procedure Set_Then_Get_Crc (Item : in out Item_Rec) is
  begin
    Item.Crc := Crc_Of(Parse(Item.Name) & Item.Kind
                    & Item.Data(1 .. Item.Data_Len));

    Set (Item);
  end Set_Then_Get_Crc;

  procedure Get (Name : in Item_Name; Kind : in Item_Kind; Item : out Item_Rec) is
    Itm : Item_Rec;
    Acc : Item_Dyn_List_Mng.Element_Access;
    Found : Boolean;
    use type Item_Dyn_List_Mng.Element_Access;
  begin
    if H_Use then
      Acc := H_Get (Kind, Name);
      if Acc = null then
        Item := No_Item;
      else
        Item := Acc.all;
      end if;
    else
      Itm.Name := Name;
      Itm.Kind := Kind;
      Search_Name (Item_List, Found, Itm, From => Item_List_Mng.Absolute);
      if Found then
        Item_List.Read (Item, Item_List_Mng.Current);
      else
        Item := No_Item;
      end if;
    end if;
  end Get;

  procedure Reset is
  begin
    if H_Use then
      H_Item.Clear_All (H_Table);
    end if;
    Item_List.Delete_List;
  end Reset;

  function Nb_Item return Natural is
  begin
    return Item_List.List_Length;
  end Nb_Item;


  -- Item_Name is empty when no more item
  procedure Read_First (Item : out Item_Rec) is
  begin
    if Item_List.Is_Empty then
      Item := No_Item;
      return;
    end if;
    Item_List.Rewind;
    Item_List.Read (Item, Item_List_Mng.Current);
  end Read_First;

  procedure Read_Next (Item : out Item_Rec) is
  begin
    Item_List.Move_To;
    Item_List.Read (Item, Item_List_Mng.Current);
  exception
    when Item_List_Mng.Not_In_List =>
      Item := No_Item;
  end Read_Next;


  -- Crc of empty data base
  Default_Crc : constant Item_Crc := (others => '0');

  function Get_Crc return Item_Crc is
    Pos : Positive;
    Item : Item_Rec;
    Crc : Crc_10.Crc_Type;
  begin
    if Item_List.Is_Empty then
      return Default_Crc;
    end if;

    Pos := Item_List.Get_Position;
    Read_First (Item);
    loop
      exit when Item = No_Item;
      Crc.Add (Item.Crc);
      Read_Next (Item);
    end loop;

    Item_List.Move_At (Pos);
    return Image (Crc.Get);
  end Get_Crc;

end Data_Base;

