with Dynamic_List, Normal, Hash_Function, Hash;
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

  type Item_Access is access all Item_Rec;
  -- Hash on: Item.Kind & Item.Name (not Parsed)
  H_Use : constant Boolean := True;
  procedure H_Dump (Data : in Item_Access) is
  begin
    null;
  end H_Dump;
  package H_Item is new Hash.Hash_Mng (
        Data_Access => Item_Access, Dump => H_Dump);
  H_Table : H_Item.Hash_Table;
  function H_Get (Kind : Item_Kind; Name : Item_Name)
                 return Item_Access is
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
  function Image (Crc : Hash.Hash_Range) return Item_Crc is
  begin
    return Normal (Integer(Crc), Item_Crc'Length, Gap => '0');
  end Image;

  -- Crc of a string
  function Crc_Of (Str : String) return Item_Crc is
  begin
    return Image (Hash.Hash_Def_Func (Str));
  end Crc_Of;

  procedure Set (Item : in Item_Rec) is
    Itm : Item_Rec;

    procedure Append_Itm is
    begin
      Item_List.Rewind (False, Item_List_Mng.Prev);
      Item_List.Insert (Itm);
    end Append_Itm;

    Acc : Item_Access;
    Found : Boolean;
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
                      Item_Access(Item_List.Access_Current));
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
    Acc : Item_Access;
    Found : Boolean;
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
    Buffer : Hash_Function.Hash_Buffer;
    use type Hash_Function.Hash_Range;
  begin
    if Item_List.Is_Empty then
      return Default_Crc;
    end if;

    Pos := Item_List.Get_Position;
    Read_First (Item);
    loop
      exit when Item = No_Item;
      Buffer.Add (Item.Crc);
      Read_Next (Item);
    end loop;

    Item_List.Move_At (Pos);
    return Image (Buffer.Get rem Hash.Hash_Range'Last);
  end Get_Crc;

end Data_Base;

