with Dynamic_List, Normal, Crc_10, Hash;
with Parse;
package body Data_Base is

  package Item_List_Mng is new Dynamic_List (Item_Rec);
  Item_List : Item_List_Mng.List_Type;

  -- Name and kind match
  function Name_Match (Elt1, Elt2 : Item_Rec) return Boolean is
  begin
    return Elt1.Kind = Elt2.Kind and then Elt1.Name = Elt2.Name;
  end Name_Match;
  procedure Search_Name is new Item_List_Mng.Search (Name_Match);


  -- Hash on: Item.Kind & Item.Name (not Parsed)
  H_Use : constant Boolean := True;
  procedure H_Dump (Data : in Item_List_Mng.Element_Access) is
  begin
    null;
  end H_Dump;
  package H_Item is new Hash.Hash_Mng (Data_Acess => Item_List_Mng.Element_Access,
                                       Dump => H_Dump);
  function H_Get (Kind : Item_Kind; Name : Item_Name)
                 return Item_List_Mng.Element_Access is
    R : H_Item.Found_Rec;
  begin
    H_Item.Reset_Find (Kind & Name);
    loop
      R := H_Item.Find_Next (Kind & Name);
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
  begin
    Crc_10.Rst;
    Crc_10.Add (Str);
    return Image (Crc_10.Get);
  end Crc_Of;

  procedure Set (Item : in Item_Rec) is
    Itm : Item_Rec;

    procedure Append_Itm is
    begin
      if not Item_List_Mng.Is_Empty (Item_List) then
        Item_List_Mng.Move_To (Item_List, Item_List_Mng.Prev, 0, False);
      end if;
      Item_List_Mng.Insert (Item_List, Itm);
    end Append_Itm;

    Acc : Item_List_Mng.Element_Access;
    use type Item_List_Mng.Element_Access;
  begin
    -- The one to store
    Itm := Item;

    if H_Use then
      Acc := H_Get (Item.Kind, Item.Name);
      if Acc /= null then
        Acc.all := Itm;
      else
        Append_Itm;
        H_Item.Store (Item.Kind & Item.Name,
                      Item_List_Mng.Access_Current(Item_List));
      end if;
    else
      begin
        Search_Name (Item_List, Itm, From => Item_List_Mng.Absolute);
        Item_List_Mng.Modify (Item_List, Itm, Item_List_Mng.Current);
      exception
        when Item_List_Mng.Not_In_List =>
          Append_Itm;
      end;
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
    Acc : Item_List_Mng.Element_Access;
    use type Item_List_Mng.Element_Access;
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
      begin
        Search_Name (Item_List, Itm, From => Item_List_Mng.Absolute);
        Item_List_Mng.Read (Item_List, Item, Item_List_Mng.Current);
      exception
        when Item_List_Mng.Not_In_List =>
          Item := No_Item;
      end;
    end if;
  end Get;

  procedure Reset is
  begin
    if H_Use then
      H_Item.Clear_All;
    end if;
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


  -- Crc of empty data base
  Default_Crc : constant Item_Crc := (others => '0');

  function Get_Crc return Item_Crc is
    Pos : Positive;
    Item : Item_Rec;
  begin
    if Item_List_Mng.Is_Empty (Item_List) then
      return Default_Crc;
    end if;

    Pos := Item_List_Mng.Get_Position (Item_List);
    Read_First (Item);
    Crc_10.Rst;
    loop
      exit when Item = No_Item;
      Crc_10.Add (Item.Crc);
      Read_Next (Item);
    end loop;

    Item_List_Mng.Move_To (Item_List, Item_List_Mng.Next,
                           Pos-1, From_Current => False);
    return Image (Crc_10.Get);
  end Get_Crc;
    
end Data_Base;

