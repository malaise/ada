package Data_Base is
  -- Host and item names are space padded

  subtype Item_Name is String (1 .. 32);
  subtype Item_Data is String (1 .. 256);
  subtype Item_Crc  is String (1 .. 4);

  type Item_Rec is record
    Crc  : Item_Crc := (others => ' ');
    Name : Item_Name := (others => ' ');
    Data_Len : Natural := 0;  -- Of data
    Data : Item_Data := (others => ' ');
  end record;
  No_Item : constant Item_Rec := ((others => ' '),
                                  (others => ' '),
                                  0,
                                  (others => ' '));

  procedure Set (Item : in Item_Rec);
  procedure Get (Name : in Item_Name; Item : out Item_Rec);

  procedure Reset;

  -- Item is No_Item when no more item
  function Nb_Item return Natural;
  procedure Read_First (Item : out Item_Rec);
  procedure Read_Next (Item : out Item_Rec);

  -- Get Crc of all data base
  function Get_Crc return Item_Crc;

end Data_Base;

