with Status, Intra_Dictio, Data_Base, Debug;
package body Sync_Mng is

  Max_Retry : constant := 3;
  First_Timeout : constant Duration := 0.1;
  Timeout_Factor : constant Duration := 2.0;
  
  procedure Send is
    Item : Data_Base.Item_Rec;
    Result : Intra_Dictio.Reply_Result_List;
    Timeout : Duration;
    use type Data_Base.Item_Rec, Intra_Dictio.Reply_Result_List;
  begin
    Data_Base.Read_First (Item);

    Items:
    while Item /= Data_Base.No_Item loop
      Timeout := First_Timeout;

      Retries:
      for I in 1 .. Max_Retry loop
        Result := Intra_Dictio.Reply_Sync_Data (Item);
        -- Ok or Error. Not Overflow.
        exit when Result /= Intra_Dictio.Overflow;
        delay Timeout;
        -- Increase timeout for next retry
        Timeout := Timeout * Timeout_Factor;
      end loop Retries;

      if Result /= Intra_Dictio.Ok then
        -- Give up if too many overlows or other error
        if Debug.Level_Array(Debug.Sync) then
          Debug.Put ("Sync: Giving up due to " & Result'Img);
        end if; 
        exit;
      end if;
      Data_Base.Read_Next (Item);
    end loop Items;

  end Send;

end Sync_Mng;

