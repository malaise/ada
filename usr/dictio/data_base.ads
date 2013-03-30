package Data_Base is
  -- Item name
  subtype Item_Name is String (1 .. 32);
  -- Item kind: "d" for data, "a" for alias
  subtype Item_Kind is String (1 .. 1);
  subtype Item_Data is String (1 .. 256);
  subtype Item_Crc  is String (1 .. 4);

  Data_Kind  : constant Item_Kind := (others => 'd');
  Alias_Kind : constant Item_Kind := (others => 'a');
  Trace_Kind : constant Item_Kind := (others => 't');

  type Item_Rec is record
    Data_Len : Natural := 0;  -- Of data
    Name : Item_Name := (others => ' ');
    Kind : Item_Kind := Data_Kind;
    Crc  : Item_Crc := (others => ' ');
    Data : Item_Data := (others => ' ');
  end record;
  No_Item : constant Item_Rec := (0,
                                  (others => ' '),
                                  (others => ' '),
                                  (others => ' '),
                                  (others => ' '));

  -- Set item,
  procedure Set (Item : in Item_Rec);
  -- Set item, update Crc
  procedure Set_Then_Get_Crc (Item : in out Item_Rec);

  -- Get item
  function Get (Name : in Item_Name; Kind : in Item_Kind) return Item_Rec;

  -- Clear all
  procedure Reset;

  -- Item is No_Item when no more item
  function Nb_Item return Natural;
  function Read_First return Item_Rec;
  function Read_Next  return Item_Rec;

  -- Get Crc of all data base
  function Get_Crc return Item_Crc;

end Data_Base;

