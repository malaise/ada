with Status, Intra_Dictio, Data_Base;
package body Sync_Mng is
  
  procedure Send is
    Item : Data_Base.Item_Rec;
    use type Data_Base.Item_Rec;
  begin
    Data_Base.Read_First (Item);
    while Item /= Data_Base.No_Item loop
      Intra_Dictio.Send_Sync_Data (Item);
      Data_Base.Read_Next (Item);
    end loop;
  end Send;

end Sync_Mng;

