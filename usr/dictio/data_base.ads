package Data_Base is
  -- Item name
  subtype Item_Name is String (1 .. 32);
  -- Item kind: "d" for data, "a" for alias
  subtype Item_Kind is String (1 .. 1);
  subtype Item_Data is String (1 .. 256);
  subtype Item_Crc  is String (1 .. 4);

  type Item_Rec is record
    Data_Len : Natural := 0;  -- Of data
    Name : Item_Name := (others => ' ');
    Kind : Item_Kind := (others => 'd');
    Data : Item_Data := (others => ' ');
    Crc  : Item_Crc := (others => ' ');
  end record;
  No_Item : constant Item_Rec := (0,
                                  (others => ' '),
                                  (others => ' '),
                                  (others => ' '),
                                  (others => ' '));

  procedure Set (Item : in Item_Rec);
  procedure Get (Name : in Item_Name; Kind : in Item_Kind; Item : out Item_Rec);

  procedure Reset;

  -- Item is No_Item when no more item
  function Nb_Item return Natural;
  procedure Read_First (Item : out Item_Rec);
  procedure Read_Next (Item : out Item_Rec);

  -- Get Crc of all data base
  function Get_Crc return Item_Crc;

end Data_Base;

