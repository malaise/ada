package Data_Base is
  -- Host and item names are space padded

  subtype Item_Name is String (1 .. 32);
  subtype Item_Data is String (1 .. 256);

  type Item_Rec is record
    Name : Item_Name := (others => ' ');
    Data_Len : Natural := 0;  -- Of data
    Data : Item_Data := (others => ' ');
  end record;
  No_Item : constant Item_Rec := ((others => ' '), 0, (others => ' '));

  procedure Set (Item : in Item_Rec);
  procedure Get (Name : in Item_Name; Item : out Item_Rec);

  -- Item_Name is empty when no more item
  procedure Read_First (Item : out Item_Rec);
  procedure Read_Next (Item : out Item_Rec);

end Data_Base;

