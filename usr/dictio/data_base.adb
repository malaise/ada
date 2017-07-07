with Dynamic_List, Normal, Hash_Function, Hash;
with Parse;
pragma Elaborate_All (Hash);
package body Data_Base is

  package Item_Dyn_List_Mng is new Dynamic_List (Item_Rec);
  package Item_List_Mng renames Item_Dyn_List_Mng.Dyn_List;
  Item_List : Item_List_Mng.List_Type;

  type Item_Access is access all Item_Rec;
  -- Hash on: Item.Kind & Item.Name (not Parsed)
  H_Use : constant Boolean := True;
  package H_Item is new Hash.Hash_Mng (Data_Access => Item_Access);
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
    (Normal (Integer(Crc), Item_Crc'Length, Gap => '0'));

  -- Crc of a string
  function Crc_Of (Str : String) return Item_Crc is
    (Image (Hash.Hash_Def_Func (Str)));

  procedure Set (Item : in Item_Rec) is
    Itm : Item_Rec;

    procedure Append_Itm is
    begin
      Item_List.Rewind (Item_List_Mng.Prev, False);
      Item_List.Insert (Itm);
    end Append_Itm;

    Acc : Item_Access;
  begin
    -- The one to store
    Itm := Item;

    Acc := H_Get (Item.Kind, Item.Name);
    if Acc /= null then
      Acc.all := Itm;
    else
      Append_Itm;
      H_Item.Store (H_Table,
                    Item.Kind & Item.Name,
                    Item_Access(Item_List.Access_Current));
    end if;
  end Set;

  -- Set item, update Crc
  procedure Set_Then_Get_Crc (Item : in out Item_Rec) is
  begin
    Item.Crc := Crc_Of(Parse(Item.Name) & Item.Kind
                    & Item.Data(1 .. Item.Data_Len));

    Set (Item);
  end Set_Then_Get_Crc;

  function Get (Name : in Item_Name; Kind : in Item_Kind) return Item_Rec is
    Item : Item_Rec;
    Acc : Item_Access;
  begin
    Acc := H_Get (Kind, Name);
    if Acc = null then
      Item := No_Item;
    else
      Item := Acc.all;
    end if;
    return Item;
  end Get;

  procedure Reset is
  begin
    if H_Use then
      H_Item.Clear_All (H_Table);
    end if;
    Item_List.Delete_List;
  end Reset;

  function Nb_Item return Natural is (Item_List.List_Length);


  -- Item_Name is empty when no more item
  function Read_First return Item_Rec is
    Item : Item_Rec;
  begin
    if Item_List.Is_Empty then
      return No_Item;
    end if;
    Item_List.Rewind;
    Item_List.Read (Item, Item_List_Mng.Current);
    return Item;
  end Read_First;

  function Read_Next return Item_Rec is
    Item : Item_Rec;
  begin
    Item_List.Move_To;
    Item_List.Read (Item, Item_List_Mng.Current);
    return Item;
  exception
    when Item_List_Mng.Not_In_List =>
      return No_Item;
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
    Item := Read_First;
    while Item /= No_Item loop
      Buffer.Add (Item.Crc);
      Item := Read_Next;
    end loop;

    Item_List.Move_At (Pos);
    return Image (Buffer.Get rem Hash.Hash_Range'Last);
  end Get_Crc;

end Data_Base;

